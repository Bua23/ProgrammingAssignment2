import sys
import unittest
from datetime import datetime, timezone
from pathlib import Path
from unittest import mock

sys.path.insert(0, str(Path(__file__).resolve().parent.parent))

import workday_client as w  # noqa: E402

NOW = datetime(2026, 11, 9, 13, 0, tzinfo=timezone.utc)
DAY = datetime(2026, 11, 9, tzinfo=timezone.utc)


class ParseUrlTests(unittest.TestCase):
    def test_plain(self):
        self.assertEqual(
            w.parse_workday_url("https://crowdstrike.wd5.myworkdayjobs.com/crowdstrikecareers"),
            ("crowdstrike.wd5.myworkdayjobs.com", "crowdstrike", "crowdstrikecareers"),
        )

    def test_locale_prefix_and_job_path(self):
        self.assertEqual(
            w.parse_workday_url("https://adobe.wd5.myworkdayjobs.com/en-US/external_experienced/job/London/x_R1"),
            ("adobe.wd5.myworkdayjobs.com", "adobe", "external_experienced"),
        )

    def test_rejects_other_hosts(self):
        with self.assertRaises(ValueError):
            w.parse_workday_url("https://careers.example.com/jobs")


class PostedOnTests(unittest.TestCase):
    def test_relative_dates(self):
        self.assertEqual(w.parse_posted_on("Posted Today", NOW), DAY)
        self.assertEqual(w.parse_posted_on("Posted Yesterday", NOW), datetime(2026, 11, 8, tzinfo=timezone.utc))
        self.assertEqual(w.parse_posted_on("Posted 3 Days Ago", NOW), datetime(2026, 11, 6, tzinfo=timezone.utc))
        self.assertEqual(w.parse_posted_on("Posted 30+ Days Ago", NOW), datetime(2026, 10, 10, tzinfo=timezone.utc))

    def test_unknown(self):
        self.assertIsNone(w.parse_posted_on("", NOW))
        self.assertIsNone(w.parse_posted_on("Sometime", NOW))


class FacetTests(unittest.TestCase):
    def test_country_facet_preferred(self):
        facets = [{"facetParameter": "locationMainGroup", "values": [
            {"facetParameter": "locationCountry", "descriptor": "Location Country", "values": [
                {"descriptor": "Germany", "id": "de"}, {"descriptor": "United Kingdom", "id": "uk"}]},
            {"facetParameter": "locations", "descriptor": "Locations", "values": [
                {"descriptor": "United Kingdom - London", "id": "ldn"}]},
        ]}]
        self.assertEqual(w.uk_applied_facets(facets), {"locationCountry": ["uk"]})

    def test_falls_back_to_uk_location_values(self):
        facets = [
            {"facetParameter": "jobFamilyGroup", "descriptor": "Job Category", "values": [{"descriptor": "UK Sales", "id": "x"}]},
            {"facetParameter": "locations", "descriptor": "Locations", "values": [
                {"descriptor": "Tel Aviv, Israel", "id": "tlv"},
                {"descriptor": "London, United Kingdom", "id": "ldn"},
                {"descriptor": "Remote - United Kingdom", "id": "rem"},
            ]},
        ]
        self.assertEqual(w.uk_applied_facets(facets), {"locations": ["ldn", "rem"]})

    def test_no_uk_facet(self):
        self.assertEqual(w.uk_applied_facets([{"facetParameter": "locations", "values": [{"descriptor": "Paris", "id": "p"}]}]), {})


class ParseJobsTests(unittest.TestCase):
    def test_parse_listing(self):
        jobs = w.parse_workday_jobs(
            [{"title": " AI Lead ", "externalPath": "/job/London/AI-Lead_R42", "locationsText": "United Kingdom - London",
              "postedOn": "Posted 2 Days Ago", "bulletFields": ["R42"]},
             {"title": "no path"}],
            "x.wd5.myworkdayjobs.com", "x", "Site", NOW)
        self.assertEqual(len(jobs), 1)
        job = jobs[0]
        self.assertEqual(job["title"], "AI Lead")
        self.assertEqual(job["external_id"], "R42")
        self.assertEqual(job["url"], "https://x.wd5.myworkdayjobs.com/en-US/Site/job/London/AI-Lead_R42")
        self.assertEqual(job["posted_at"], datetime(2026, 11, 7, tzinfo=timezone.utc))
        self.assertEqual(job["source"], "workday")

    def test_apply_detail_sharpens_date_and_sets_location(self):
        job = {"location": "2 Locations", "description_html": "", "employment_type": "",
               "posted_at": datetime(2026, 11, 7, tzinfo=timezone.utc), "url": "u"}
        w.apply_detail(job, {"jobPostingInfo": {
            "location": "London", "additionalLocations": ["Reading"], "jobDescription": "<p>Salary £120,000</p>",
            "timeType": "Full time", "startDate": "2026-11-06"}})
        self.assertEqual(job["location"], "London; Reading")
        self.assertEqual(job["employment_type"], "Full time")
        self.assertIn("£120,000", job["description_html"])
        self.assertEqual(job["posted_at"], datetime(2026, 11, 6, tzinfo=timezone.utc))

    def test_apply_detail_ignores_far_future_start_date(self):
        job = {"location": "", "description_html": "", "employment_type": "",
               "posted_at": datetime(2026, 11, 7, tzinfo=timezone.utc), "url": "u"}
        w.apply_detail(job, {"jobPostingInfo": {"startDate": "2027-03-01"}})
        self.assertEqual(job["posted_at"], datetime(2026, 11, 7, tzinfo=timezone.utc))


class FetchTests(unittest.TestCase):
    def test_fetch_filters_to_uk_pages_and_fetches_details_only_for_matching_titles(self):
        facets = [{"facetParameter": "locationCountry", "descriptor": "Country",
                   "values": [{"descriptor": "United Kingdom", "id": "uk"}]}]
        rows = [{"title": f"Role {i}", "externalPath": f"/job/L/Role_{i}", "locationsText": "London",
                 "postedOn": "Posted Today", "bulletFields": [str(i)]} for i in range(25)]
        rows[3]["title"] = "AI Governance Lead"
        posts = []

        class FakeSession:
            def post(self, url, body):
                posts.append(body)
                if body["limit"] == 1:
                    return {"total": 25, "jobPostings": [], "facets": facets}
                off = body["offset"]
                return {"total": 25 if off == 0 else 0, "jobPostings": rows[off:off + 20]}

            def get(self, url):
                return {"jobPostingInfo": {"jobDescription": "desc", "location": "London"}}

        gets = []
        fake = FakeSession()
        orig_get = fake.get
        fake.get = lambda url: (gets.append(url), orig_get(url))[1]
        company = {"name": "X", "workday_url": "https://x.wd5.myworkdayjobs.com/Site"}
        with mock.patch.object(w, "_Session", return_value=fake):
            jobs = w.fetch_workday(company, title_prefilter=lambda t: "Governance" in t)
        self.assertEqual(len(jobs), 25)                       # both list pages read
        self.assertEqual(posts[1]["appliedFacets"], {"locationCountry": ["uk"]})
        self.assertEqual(len(gets), 1)                        # one detail call only
        self.assertEqual([j["description_html"] for j in jobs if j["description_html"]], ["desc"])
        self.assertTrue(all("_detail_path" not in j for j in jobs))


if __name__ == "__main__":
    unittest.main()
