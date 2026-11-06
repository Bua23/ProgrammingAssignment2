import sys
import unittest
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parents[1]))
from job_filters import (  # noqa: E402
    extract_gbp_salary, format_salary, is_permanent, is_senior_title, passes_salary_filter)
from locations import classify_location  # noqa: E402


class LocationTest(unittest.TestCase):
    def test_classification(self):
        cases = {
            "London, UK": "london", "London": "london", "Hybrid London; Hybrid Paris": "london",
            "United Kingdom - London": "london", "Zurich, Switzerland ⋅ London, UK": "london",
            "London, Ontario, Canada": None, "London, ON": None, "London, KY": None,
            "Reading, England, United Kingdom": "south_east", "United Kingdom - Reading": "south_east",
            "Guildford, Surrey": "south_east", "Oxford, UK": "south_east", "Brighton, England": "south_east",
            "Milton Keynes": "south_east", "South East England": "south_east",
            "Reading, PA": None, "Oxford, MS": None, "Windsor, ON, Canada": None, "Kent, WA": None,
            "Cambridge, UK": None, "Hertfordshire, UK": None, "Manchester, UK": None,
            "Remote - UK": "remote_uk", "United Kingdom - Remote": "remote_uk", "Remote (United Kingdom)": "remote_uk",
            "Remote, UK": "remote_uk", "Remote": None, "Remote - EMEA": None, "Remote, United States": None,
            "New York, NY": None, "": None, None: None,
        }
        bad = {k: (classify_location(k), v) for k, v in cases.items() if classify_location(k) != v}
        self.assertFalse(bad, bad)


class FilterTest(unittest.TestCase):
    def test_permanent(self):
        self.assertTrue(is_permanent("Head of AI"))
        self.assertTrue(is_permanent("Head of AI", "FullTime"))
        self.assertTrue(is_permanent("Head of AI", "Regular"))
        self.assertTrue(is_permanent("Head of AI", "Full-time"))
        for title in ["AI Lead (Contract)", "AI Lead - 12 Month FTC", "AI Governance Manager (Fixed Term)",
                      "Intern, AI", "Graduate AI Programme", "Interim Head of AI"]:
            self.assertFalse(is_permanent(title), title)
        for t in ["Contract", "Intern", "PartTime", "Temporary", "Fixed Term"]:
            self.assertFalse(is_permanent("Head of AI", t), t)
        self.assertFalse(is_permanent("Head of AI", "", "This is a 12 month fixed term contract."))
        self.assertTrue(is_permanent("Head of AI", "", "We also work with contractors and interns."))

    def test_senior_title(self):
        for t in ["Senior Manager, AI", "Sr. AI Lead", "Head of AI Governance", "Director of AI", "Principal AI Consultant",
                  "VP, AI Strategy", "Chief AI Officer", "Staff AI Product Manager"]:
            self.assertTrue(is_senior_title(t), t)
        for t in ["AI Manager", "AI Governance Analyst", "AI Customer Success Manager", "AI Strategist"]:
            self.assertFalse(is_senior_title(t), t)

    def test_salary_extraction(self):
        self.assertEqual(extract_gbp_salary("Salary £120,000 - £150,000 per year"), (120000, 150000))
        self.assertEqual(extract_gbp_salary("£110k to £140k + equity"), (110000, 140000))
        self.assertEqual(extract_gbp_salary("£95k–£105k"), (95000, 105000))
        self.assertIsNone(extract_gbp_salary("$150,000 - $200,000"))
        self.assertIsNone(extract_gbp_salary("£1,000 learning budget and £500 wellbeing; raised £50 million"))
        self.assertIsNone(extract_gbp_salary("Competitive salary"))
        self.assertEqual(format_salary(120000, 150000), "£120k–£150k")
        self.assertEqual(format_salary(130000, 130000), "£130k")

    def test_salary_decision(self):
        self.assertTrue(passes_salary_filter("AI Lead", (120000, 150000)))
        self.assertTrue(passes_salary_filter("AI Manager", (100000, 115000)))        # top of range counts
        self.assertFalse(passes_salary_filter("Head of AI", (70000, 90000)))          # stated pay beats title
        self.assertTrue(passes_salary_filter("Head of AI", None))                     # unstated: title decides
        self.assertFalse(passes_salary_filter("AI Manager", None))


if __name__ == "__main__":
    unittest.main()
