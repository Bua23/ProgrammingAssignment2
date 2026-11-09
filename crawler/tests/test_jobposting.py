import json
import sys
import unittest
from datetime import datetime, timezone
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parents[1]))
from jobposting import find_jobposting, jobposting_fields  # noqa: E402

GREENHOUSE_STYLE = json.dumps({
    "@context": "https://schema.org", "@type": "JobPosting", "title": "Head of AI Governance",
    "datePosted": "2026-11-04", "employmentType": "FULL_TIME", "description": "<p>Lead our AI governance.</p>",
    "jobLocation": {"@type": "Place", "address": {"@type": "PostalAddress", "addressLocality": "London",
                                                  "addressRegion": "England", "addressCountry": "GB"}},
    "baseSalary": {"@type": "MonetaryAmount", "currency": "GBP",
                   "value": {"@type": "QuantitativeValue", "minValue": 120000, "maxValue": 150000, "unitText": "YEAR"}},
})
GRAPH_STYLE = json.dumps({"@graph": [{"@type": "Organization"}, {
    "@type": ["JobPosting"], "datePosted": "2026-11-05T09:30:00Z", "employmentType": ["CONTRACTOR"],
    "jobLocationType": "TELECOMMUTE", "applicantLocationRequirements": {"@type": "Country", "name": "United Kingdom"}}]})


class JobPostingTest(unittest.TestCase):
    def test_full_posting(self):
        jp = find_jobposting(["not json", GREENHOUSE_STYLE])
        f = jobposting_fields(jp)
        self.assertEqual(f["posted_at"], datetime(2026, 11, 4, tzinfo=timezone.utc))
        self.assertEqual(f["employment_type"], "FULL_TIME")
        self.assertEqual(f["location"], "London, England, GB")
        self.assertTrue(f["description_html"].startswith("<p>Salary: £120,000 - £150,000</p>"))

    def test_graph_remote_contract(self):
        f = jobposting_fields(find_jobposting([GRAPH_STYLE]))
        self.assertEqual(f["posted_at"], datetime(2026, 11, 5, 9, 30, tzinfo=timezone.utc))
        self.assertEqual(f["employment_type"], "CONTRACTOR")
        self.assertEqual(f["location"], "Remote, United Kingdom")

    def test_none_when_absent(self):
        self.assertIsNone(find_jobposting([json.dumps({"@type": "Organization"}), "{bad"]))
        self.assertEqual(jobposting_fields({"@type": "JobPosting"}), {})

    def test_non_gbp_or_hourly_salary_ignored(self):
        usd = json.loads(GREENHOUSE_STYLE); usd["baseSalary"]["currency"] = "USD"
        self.assertNotIn("Salary", jobposting_fields(usd)["description_html"])
        hourly = json.loads(GREENHOUSE_STYLE); hourly["baseSalary"]["value"]["unitText"] = "HOUR"
        self.assertNotIn("Salary", jobposting_fields(hourly)["description_html"])


if __name__ == "__main__":
    unittest.main()
