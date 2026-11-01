import sys
import unittest
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parents[1]))
from role_filter import RoleFilter  # noqa: E402

RF = RoleFilter.from_yaml(Path(__file__).resolve().parents[1] / "roles.yaml")

# (title, roles that must be matched; empty list = must be dropped)
CASES = [
    # --- 1. AI Manager / Lead / Senior Manager (+ adjacent seniority) ---
    ("AI Manager", ["ai_management"]),
    ("Senior Manager, AI", ["ai_management"]),
    ("AI Lead", ["ai_management"]),
    ("Head of AI", ["ai_management"]),
    ("Director, AI Platform", ["ai_management"]),
    ("Generative AI Programme Manager", ["ai_management", "ai_strategy"]),
    ("AI Engineering Manager", ["ai_management"]),
    # --- 2. AI Customer Success ---
    ("AI Customer Success Manager", ["ai_management", "ai_customer_success"]),
    ("Customer Success Architect (AI), EMEA", ["ai_customer_success"]),
    ("AI Value Partner, Customer Analytics", ["ai_customer_success"]),
    ("AI Adoption Specialist", ["ai_customer_success", "ai_strategy"]),
    # --- 3. AI Governance ---
    ("AI Governance Lead", ["ai_management", "ai_governance"]),
    ("Senior Manager, Responsible AI", ["ai_management", "ai_governance"]),
    ("AI Policy Analyst", ["ai_governance"]),
    ("Head of AI Risk and Compliance", ["ai_management", "ai_governance"]),
    ("Responsible AI Specialist", ["ai_governance"]),
    # --- 4. AI Strategy ---
    ("AI Strategist (London)", ["ai_strategy"]),
    ("Director of AI Strategy", ["ai_management", "ai_strategy"]),
    ("AI Transformation Lead", ["ai_management", "ai_strategy"]),
    ("Chief AI Officer", ["ai_management", "ai_strategy"]),
    # --- 5. AI Consulting ---
    ("AI Consulting Manager", ["ai_management", "ai_consulting"]),
    ("Principal AI Consultant", ["ai_management", "ai_consulting"]),
    ("AI Advisory Lead", ["ai_management", "ai_strategy", "ai_consulting"]),
    ("Senior Manager, AI Consulting", ["ai_management", "ai_consulting"]),
    # --- must be dropped ---
    ("Senior AI Engineer", []),
    ("Lead AI Engineer", []),
    ("Machine Learning Engineer", []),
    ("Member of Technical Staff, Agentic Environments", []),
    ("AI Research Scientist", []),
    ("Senior Product Marketing Manager, Agentic Identity Security", []),
    ("Director, Marketing Strategy and AI Transformation", []),
    ("Enterprise Account Manager, AI", []),
    ("Sales Director, AI Solutions", []),
    ("Customer Success Manager", []),          # no AI term
    ("Engineering Manager, Payments", []),     # no AI term
    ("Finance Manager", []),
    ("Paid Media Manager", []),                # "paid" must not match \bai\b
    ("Software Engineer Intern, Generative AI", []),
    ("", []),
    # multi-line DOM-fallback titles: only first line counts
    ("AI Strategist (London)\n\nAI • London • Full time", ["ai_strategy"]),
    ("Software Engineer\nAI Manager Team", []),
]


class RoleFilterTest(unittest.TestCase):
    def test_cases(self):
        failures = []
        for title, expected in CASES:
            got = RF.match(title)
            if sorted(got) != sorted(expected):
                failures.append(f"{title!r}: expected {sorted(expected)}, got {sorted(got)}")
        self.assertFalse(failures, "\n" + "\n".join(failures))


if __name__ == "__main__":
    unittest.main()
