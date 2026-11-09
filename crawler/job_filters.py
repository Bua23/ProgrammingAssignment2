"""Job-level filters beyond title/role family: permanent only, and senior roles
likely to pay at least a salary floor (default GBP 110k).

Salary logic (decide on stated pay first, title second):
  * If the posting states GBP pay, keep it only if the top of the stated range
    is at least the floor — whatever the title says.
  * If no GBP pay is stated, keep it only if the title reads as senior
    (senior / lead / principal / staff / head / director / VP / chief ...).
    A plain "Manager" with no stated pay is dropped: it usually pays less.
"""
from __future__ import annotations

import re

MIN_SALARY_GBP = 110_000

# Title words that mean contract / temporary / junior-entry rather than a
# permanent role.
NOT_PERMANENT_TITLE = re.compile(
    r"\b(contract|contractor|fixed[- ]term|ftc|temporary|temp|intern|internship|"
    r"graduate|apprentice(ship)?|placement|part[- ]time|secondment|freelance|"
    r"maternity cover|interim)\b",
    re.I,
)
# ATS employment-type values that are not permanent full-time.
NOT_PERMANENT_TYPE = re.compile(r"contract|temp|intern|fixed|part|freelance|seasonal|casual", re.I)
# Narrow phrases in the description that signal a fixed-term role.
NOT_PERMANENT_DESCRIPTION = re.compile(
    r"\b\d{1,2}[- ]months?\s+(?:fixed[- ]term|contract|ftc)\b|"
    r"\bfixed[- ]term contract\b|\bthis is a (?:fixed[- ]term|temporary|contract)\b",
    re.I,
)

SENIOR_TITLE = re.compile(
    r"\b(senior|sr\.?|lead|principal|staff|head|director|vp|svp|evp|vice president|"
    r"chief|partner|distinguished|fellow)\b",
    re.I,
)

_AMOUNT = re.compile(r"£\s*(\d{1,3}(?:,\d{3})+|\d+(?:\.\d+)?)\s*([kK])?")


def is_permanent(title: str, employment_type: str = "", description: str = "") -> bool:
    if NOT_PERMANENT_TITLE.search(title or ""):
        return False
    if employment_type and NOT_PERMANENT_TYPE.search(employment_type):
        return False
    if description and NOT_PERMANENT_DESCRIPTION.search(description):
        return False
    return True


def is_senior_title(title: str) -> bool:
    return bool(SENIOR_TITLE.search(title or ""))


def extract_gbp_salary(text: str) -> tuple[int, int] | None:
    """(min, max) of the plausible annual GBP amounts mentioned, or None.
    Amounts under 30k (benefits, budgets, hourly rates) and over 600k are
    ignored. Non-GBP amounts are ignored."""
    values = []
    for m in _AMOUNT.finditer(text or ""):
        num = float(m.group(1).replace(",", ""))
        if m.group(2):
            num *= 1000
        if 30_000 <= num <= 600_000:
            values.append(int(num))
    if not values:
        return None
    return min(values), max(values)


def format_salary(lo: int, hi: int) -> str:
    def k(v: int) -> str:
        return f"£{v // 1000}k"
    return k(lo) if lo == hi else f"{k(lo)}–{k(hi)}"


def passes_salary_filter(title: str, salary: tuple[int, int] | None, floor: int = MIN_SALARY_GBP) -> bool:
    if salary is not None:
        return salary[1] >= floor
    return is_senior_title(title)
