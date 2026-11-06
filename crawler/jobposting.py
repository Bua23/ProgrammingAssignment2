"""Read schema.org JobPosting data (the JSON-LD block Google Jobs reads) from a
job's own page. Greenhouse, Lever, Ashby, Workable, Workday, SmartRecruiters
and many custom career sites publish it, and it carries what list pages
usually don't: the posted date, employment type, location and often pay.

Pure functions so they can be unit-tested without a browser.
"""
from __future__ import annotations

import json
from datetime import datetime, timezone
from typing import Any


def _walk(node: Any):
    if isinstance(node, dict):
        yield node
        for v in node.values():
            if isinstance(v, (dict, list)):
                yield from _walk(v)
    elif isinstance(node, list):
        for v in node:
            yield from _walk(v)


def find_jobposting(blocks: list[str]) -> dict | None:
    """First JobPosting object in any of the JSON-LD script bodies."""
    for raw in blocks:
        try:
            data = json.loads(raw)
        except (ValueError, TypeError):
            continue
        for node in _walk(data):
            t = node.get("@type")
            types = t if isinstance(t, list) else [t]
            if "JobPosting" in types:
                return node
    return None


def _parse_date(value: Any) -> datetime | None:
    if not isinstance(value, str) or not value.strip():
        return None
    v = value.strip().replace("Z", "+00:00")
    try:
        dt = datetime.fromisoformat(v)
    except ValueError:
        try:
            dt = datetime.strptime(v[:10], "%Y-%m-%d")
        except ValueError:
            return None
    if dt.tzinfo is None:
        dt = dt.replace(tzinfo=timezone.utc)
    return dt.astimezone(timezone.utc)


def _name(value: Any) -> str:
    if isinstance(value, dict):
        return str(value.get("name") or value.get("addressCountry") or "")
    return str(value or "")


def _location_text(jp: dict) -> str:
    parts = []
    locs = jp.get("jobLocation")
    for loc in locs if isinstance(locs, list) else [locs]:
        if not isinstance(loc, dict):
            continue
        addr = loc.get("address", loc)
        if isinstance(addr, dict):
            bits = [_name(addr.get("addressLocality")), _name(addr.get("addressRegion")),
                    _name(addr.get("addressCountry"))]
            text = ", ".join(b for b in bits if b)
        else:
            text = str(addr or "")
        if text:
            parts.append(text)
    if str(jp.get("jobLocationType", "")).upper() == "TELECOMMUTE":
        reqs = jp.get("applicantLocationRequirements")
        countries = ", ".join(_name(r) for r in (reqs if isinstance(reqs, list) else [reqs]) if r)
        parts.append(f"Remote, {countries}" if countries else "Remote")
    return " | ".join(parts)


def _salary_text(jp: dict) -> str:
    sal = jp.get("baseSalary")
    if not isinstance(sal, dict) or str(sal.get("currency", "")).upper() != "GBP":
        return ""
    value = sal.get("value")
    if not isinstance(value, dict):
        return ""
    unit = str(value.get("unitText", "YEAR")).upper()
    if unit not in ("YEAR", "ANNUAL", ""):
        return ""
    lo, hi = value.get("minValue"), value.get("maxValue", value.get("value"))
    try:
        lo = float(lo if lo is not None else hi)
        hi = float(hi if hi is not None else lo)
    except (TypeError, ValueError):
        return ""
    return f"£{int(lo):,} - £{int(hi):,}"


def jobposting_fields(jp: dict) -> dict:
    """Fields to merge into a scraped job. Only keys we could actually read."""
    out: dict = {}
    posted = _parse_date(jp.get("datePosted"))
    if posted:
        out["posted_at"] = posted
    et = jp.get("employmentType")
    if et:
        out["employment_type"] = " ".join(et) if isinstance(et, list) else str(et)
    loc = _location_text(jp)
    if loc:
        out["location"] = loc
    description = str(jp.get("description") or "")
    salary = _salary_text(jp)
    if salary:
        description = f"<p>Salary: {salary}</p>\n{description}"
    if description:
        out["description_html"] = description
    return out
