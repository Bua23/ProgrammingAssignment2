import sys
import unittest
from datetime import datetime, timedelta, timezone
from pathlib import Path
from unittest.mock import MagicMock, patch

sys.path.insert(0, str(Path(__file__).resolve().parents[1]))
import crawl  # noqa: E402
from role_filter import RoleFilter  # noqa: E402

NOW = datetime.now(timezone.utc)
RF = RoleFilter.from_yaml(Path(__file__).resolve().parents[1] / "roles.yaml")


def job(title, location="London, UK", posted_at=None, ext="1", source="ashby", desc="", etype=""):
    return {"external_id": ext, "title": title, "location": location, "url": f"https://x/{ext}",
            "description_html": desc, "employment_type": etype, "posted_at": posted_at, "source": source}


FAKE = {
    "NetCo": ([
        job("AI Governance Lead", posted_at=NOW - timedelta(hours=2), ext="a"),
        job("Senior AI Engineer", posted_at=NOW - timedelta(hours=2), ext="b"),         # role filter drops
        job("AI Strategist", location="New York, US", posted_at=NOW - timedelta(hours=2), ext="c"),  # not London
        job("Head of AI", posted_at=NOW - timedelta(hours=72), ext="d"),                # too old
    ], "network"),
    "DomCo": ([
        job("AI Strategist (London)\n\nAI • London • Full time", location="London (unconfirmed)", ext="e", source="dom"),
        job("AI Strategist (New York)\n\nAI • New York • Full time", location="London (unconfirmed)", ext="f", source="dom"),
        job("Customer Success Architect (AI), EMEA", location="London (unconfirmed)", ext="g", source="dom"),
        job("Office Manager", location="London (unconfirmed)", ext="h", source="dom"),
    ], "dom_fallback"),
}
COMPANIES = [{"name": n, "token": n.lower(), "ats": "ashby", "careers_url": "https://x"} for n in FAKE]


def run(role_filter, fake=FAKE, hours=24, first_seen=None, min_salary=None):
    companies = [{"name": n, "token": n.lower(), "ats": "ashby", "careers_url": "https://x"} for n in fake]
    with patch("crawl.crawl_company", side_effect=lambda b, c, **kw: fake[c["name"]]), patch("crawl.sync_playwright") as sp:
        sp.return_value.__enter__.return_value.chromium.launch.return_value = MagicMock()
        return crawl.crawl(companies, NOW - timedelta(hours=hours), role_filter, first_seen, min_salary=min_salary)


class CrawlRoleFilterTest(unittest.TestCase):
    def test_with_filter(self):
        confirmed, unconfirmed, meta = run(RF)
        self.assertEqual([j["title"] for j in confirmed], ["AI Governance Lead"])
        self.assertEqual(confirmed[0]["matched_roles"], ["ai_management", "ai_governance"])
        self.assertEqual(sorted(j["title"] for j in unconfirmed),
                         ["AI Strategist (London)", "Customer Success Architect (AI), EMEA"])
        self.assertEqual(meta["dropped_wrong_city"], 1)
        self.assertEqual(meta["dropped_by_role_filter"], 2)   # Senior AI Engineer, Office Manager
        self.assertIn("ai_strategy", meta["role_filter"])

    def test_without_filter(self):
        confirmed, unconfirmed, meta = run(None)
        self.assertEqual(len(confirmed), 2)      # AI Governance Lead + Senior AI Engineer (both London, <24h)
        self.assertIsNone(meta["role_filter"])


class SevenDayWindowTest(unittest.TestCase):
    WEEK = 168

    def fake(self):
        return {"Co": ([
            job("AI Strategy Lead", posted_at=NOW - timedelta(hours=10), ext="new"),        # <2 days
            job("AI Governance Manager", posted_at=NOW - timedelta(days=5), ext="mid"),     # 2-7 days
            job("Head of AI", posted_at=NOW - timedelta(days=8), ext="old"),                # >7 days: dropped
            job("AI Strategist (London)", location="London (unconfirmed)", ext="seen-recently", source="dom"),
            job("AI Programme Manager", location="London (unconfirmed)", ext="seen-ages-ago", source="dom"),
        ], "dom_fallback")}

    def test_confirmed_window_is_seven_days(self):
        confirmed, _, meta = run(RF, self.fake(), hours=self.WEEK)
        self.assertEqual(sorted(j["title"] for j in confirmed), ["AI Governance Manager", "AI Strategy Lead"])
        self.assertEqual(meta["highlight_hours"], 48)

    def test_unconfirmed_aged_by_first_seen(self):
        prior = {
            "dom-co-seen-recently": (NOW - timedelta(days=3)).isoformat(),
            "dom-co-seen-ages-ago": (NOW - timedelta(days=9)).isoformat(),
        }
        _, unconfirmed, meta = run(RF, self.fake(), hours=self.WEEK, first_seen=prior)
        self.assertEqual([j["title"] for j in unconfirmed], ["AI Strategist (London)"])
        self.assertEqual(unconfirmed[0]["first_seen_at"], prior["dom-co-seen-recently"])  # carried over, not reset
        self.assertEqual(meta["dropped_stale_unconfirmed"], 1)

    def test_new_unconfirmed_get_first_seen_now(self):
        _, unconfirmed, _ = run(RF, self.fake(), hours=self.WEEK, first_seen={})
        self.assertEqual(len(unconfirmed), 2)
        for j in unconfirmed:
            self.assertLess((datetime.now(timezone.utc) - datetime.fromisoformat(j["first_seen_at"])).total_seconds(), 60)


class TargetRoleFiltersTest(unittest.TestCase):
    """Location (London / South East / UK-remote), permanent-only, senior/£110k+."""

    def fake(self):
        recent = NOW - timedelta(hours=2)
        return {"Co": ([
            job("Head of AI Governance", posted_at=recent, ext="keep-pay",
                desc="<p>Salary £130,000 - £160,000 plus bonus</p>"),
            job("Senior AI Strategy Manager", location="Reading, England, United Kingdom", posted_at=recent, ext="keep-se"),
            job("Director of AI Strategy", location="Remote - UK", posted_at=recent, ext="keep-remote"),
            job("AI Governance Manager", posted_at=recent, ext="low-pay", desc="<p>£80,000 - £95,000</p>"),
            job("AI Governance Manager", posted_at=recent, ext="no-pay-not-senior"),
            job("Head of AI (12 Month FTC)", posted_at=recent, ext="ftc"),
            job("Director of AI Strategy", location="Remote - UK", posted_at=recent, ext="contract", etype="Contract"),
            job("Director of AI", location="Remote - United States", posted_at=recent, ext="us-remote"),
            job("Principal AI Consultant", location="Manchester, UK", posted_at=recent, ext="manchester"),
            job("AI Lead", posted_at=recent, ext="lead-fixedterm-desc", desc="<p>This is a 12 month fixed term contract.</p>"),
        ], "network")}

    def test_filters(self):
        confirmed, _, meta = self._run()
        titles = {(j["title"], j["location_kind"]) for j in confirmed}
        self.assertEqual(titles, {("Head of AI Governance", "london"),
                                  ("Senior AI Strategy Manager", "south_east"),
                                  ("Director of AI Strategy", "remote_uk")})
        pay = next(j for j in confirmed if j["title"] == "Head of AI Governance")
        self.assertEqual((pay["salary_text"], pay["salary_max_gbp"]), ("£130k–£160k", 160000))
        self.assertEqual(meta["dropped_below_salary"], 1)
        self.assertEqual(meta["dropped_not_senior"], 1)
        self.assertEqual(meta["dropped_not_permanent"], 3)      # FTC title, Contract type, fixed-term description
        self.assertEqual(meta["filters"]["min_salary_gbp"], 110000)

    def _run(self):
        fake = self.fake()
        with patch("crawl.crawl_company", side_effect=lambda b, c, **kw: fake[c["name"]]), patch("crawl.sync_playwright") as sp:
            sp.return_value.__enter__.return_value.chromium.launch.return_value = MagicMock()
            return crawl.crawl([{"name": "Co", "token": "co", "ats": "ashby", "careers_url": "https://x"}],
                               NOW - timedelta(hours=168), RF, None, min_salary=110_000)

    def test_salary_filter_off(self):
        fake = self.fake()
        with patch("crawl.crawl_company", side_effect=lambda b, c, **kw: fake[c["name"]]), patch("crawl.sync_playwright") as sp:
            sp.return_value.__enter__.return_value.chromium.launch.return_value = MagicMock()
            confirmed, _, _ = crawl.crawl([{"name": "Co", "token": "co", "ats": "ashby", "careers_url": "https://x"}],
                                          NOW - timedelta(hours=168), RF, None, min_salary=None)
        self.assertIn("AI Governance Manager", {j["title"] for j in confirmed})   # junior-ish titles return


if __name__ == "__main__":
    unittest.main()
