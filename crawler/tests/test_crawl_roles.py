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


def job(title, location="London, UK", posted_at=None, ext="1", source="ashby"):
    return {"external_id": ext, "title": title, "location": location, "url": f"https://x/{ext}",
            "description_html": "", "posted_at": posted_at, "source": source}


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


def run(role_filter):
    with patch("crawl.crawl_company", side_effect=lambda b, c: FAKE[c["name"]]), patch("crawl.sync_playwright") as sp:
        sp.return_value.__enter__.return_value.chromium.launch.return_value = MagicMock()
        return crawl.crawl(COMPANIES, NOW - timedelta(hours=24), role_filter)


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


if __name__ == "__main__":
    unittest.main()
