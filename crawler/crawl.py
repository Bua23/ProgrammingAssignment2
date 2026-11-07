#!/usr/bin/env python3
"""Render each curated company's own careers page in a headless browser and
keep only the roles you care about:

  * title matches an AI role family (roles.yaml)
  * based in London, the South East of England, or remote within the UK
  * permanent (not contract / fixed-term / intern / part-time)
  * senior, likely to pay >= GBP 110k (stated pay if given, else title)
  * posted within the last N hours (default 168 = 7 days)

and write jobs.json / meta.json for the dashboard.

Usage:
    python crawl.py [--hours 168] [--highlight-hours 48] [--workers 4]
                    [--min-salary 110000] [--output ../frontend/public/data]

Exit code is 0 even if individual companies fail to load (logged and skipped)
so a single bad company doesn't take down a scheduled run.
"""
from __future__ import annotations

import argparse
import hashlib
import json
import logging
import re
import sys
import time
from concurrent.futures import ThreadPoolExecutor
from datetime import datetime, timedelta, timezone
from pathlib import Path

import yaml
from bs4 import BeautifulSoup
from playwright.sync_api import sync_playwright

sys.path.insert(0, str(Path(__file__).resolve().parent))
from ats_clients import FETCHERS, fetch_company_jobs  # noqa: E402
from browser_crawl import crawl_company, launch_chromium  # noqa: E402
from job_filters import (  # noqa: E402
    MIN_SALARY_GBP, extract_gbp_salary, format_salary, is_permanent, passes_salary_filter)
from locations import LOCATION_LABELS, classify_location  # noqa: E402
from role_filter import RoleFilter  # noqa: E402

logging.basicConfig(
    level=logging.INFO, format="%(asctime)s %(levelname)s %(message)s"
)
logger = logging.getLogger("crawl")


def within_window(posted_at: datetime | None, cutoff: datetime) -> bool:
    if posted_at is None:
        return False
    return posted_at >= cutoff


def html_to_text(html: str, max_len: int | None = 4000) -> str:
    if not html:
        return ""
    text = BeautifulSoup(html, "html.parser").get_text(separator="\n").strip()
    text = re.sub(r"\n{3,}", "\n\n", text)
    return text if max_len is None else text[:max_len]


def dom_location_ok(raw_title: str) -> bool:
    """DOM-fallback titles are raw link text: first line is the title, any
    later lines are usually location/team. If there ARE later lines and none
    is a target location (London / South East / UK-remote), the card is for
    another place — the page only mentioned a target location somewhere
    nearby — so drop it. A bare single-line title carries no location info,
    so keep it as unconfirmed."""
    lines = [ln.strip() for ln in (raw_title or "").splitlines() if ln.strip()]
    if len(lines) <= 1:
        return True
    return any(classify_location(ln) for ln in lines[1:])


def make_id(source: str, token: str, external_id: str) -> str:
    if len(external_id) > 60:
        external_id = hashlib.sha1(external_id.encode()).hexdigest()[:16]
    return f"{source}-{token}-{external_id}"


def load_previous_first_seen(output_dir: Path) -> dict[str, str]:
    """id -> first_seen_at from the previous crawl's output, so roles without a
    posted date can be aged by when we first saw them. Roles from data that
    predates first_seen_at fall back to the previous crawl's timestamp (they
    were on the site no later than then)."""
    seen: dict[str, str] = {}
    try:
        jobs = json.loads((output_dir / "jobs.json").read_text())
        prev_generated = json.loads((output_dir / "meta.json").read_text()).get("generated_at")
    except (OSError, ValueError):
        return seen
    for job in jobs.get("jobs", []) + jobs.get("unconfirmed_jobs", []):
        first_seen = job.get("first_seen_at") or prev_generated
        if first_seen and job.get("id"):
            seen[job["id"]] = first_seen
    return seen


def load_companies(path: Path) -> list[dict]:
    with open(path) as f:
        data = yaml.safe_load(f)
    return data.get("companies", [])


def _crawl_chunk(companies: list[dict], title_prefilter=None, ats_direct: bool = False) -> dict[str, tuple]:
    """One browser, a slice of companies. Returns name -> ("ok", jobs, method)
    or ("error", exception).

    ats_direct: for companies on Greenhouse / Lever / Ashby / Workable, read the
    ATS's public job-board feed instead of the company's own page. Complete and
    dated, but it is a call to the ATS, not to the company's website. If the
    feed fails the company falls back to the normal careers-page route."""
    results: dict[str, tuple] = {}
    if ats_direct:
        for company in list(companies):
            if company["ats"] in FETCHERS:
                try:
                    results[company["name"]] = ("ok", fetch_company_jobs(company), "ats_direct")
                except Exception as exc:  # noqa: BLE001
                    # Wrong/changed board name etc. -> fall back to the company's own page.
                    logger.info("ATS feed failed for %s (%s); falling back to its careers page", company["name"], exc)
        companies = [c for c in companies if c["name"] not in results]
        if not companies:
            return results
    with sync_playwright() as p:
        browser = launch_chromium(p)
        try:
            for company in companies:
                name = company["name"]
                try:
                    raw_jobs, method = crawl_company(browser, company, title_prefilter=title_prefilter)
                    results[name] = ("ok", raw_jobs, method)
                except Exception as exc:  # noqa: BLE001 - one bad company shouldn't kill the run
                    results[name] = ("error", exc)
        finally:
            browser.close()
    return results


def fetch_all(companies: list[dict], workers: int, title_prefilter=None, ats_direct: bool = False) -> dict[str, tuple]:
    workers = max(1, min(workers, len(companies) or 1))
    if workers == 1:
        return _crawl_chunk(companies, title_prefilter, ats_direct)
    chunks = [companies[i::workers] for i in range(workers)]  # round-robin keeps slow sites spread out
    merged: dict[str, tuple] = {}
    with ThreadPoolExecutor(max_workers=workers) as pool:
        for part in pool.map(lambda chunk: _crawl_chunk(chunk, title_prefilter, ats_direct), chunks):
            merged.update(part)
    return merged


def crawl(
    companies: list[dict],
    cutoff: datetime,
    role_filter: RoleFilter | None = None,
    previous_first_seen: dict[str, str] | None = None,
    highlight_hours: int = 48,
    workers: int = 1,
    min_salary: int | None = MIN_SALARY_GBP,
    permanent_only: bool = True,
    drop_log: list | None = None,
    ats_direct: bool = False,
) -> tuple[list[dict], list[dict], dict]:
    previous_first_seen = previous_first_seen or {}

    def dropped(reason: str, company_name: str, title: str) -> None:
        drops[reason] += 1
        if drop_log is not None:
            drop_log.append({"reason": reason, "company": company_name,
                             "title": (title.strip().splitlines() or [""])[0].strip()})

    now = datetime.now(timezone.utc)
    started = time.time()
    drops = {"role_filter": 0, "wrong_city": 0, "not_permanent": 0, "not_senior": 0,
             "below_salary": 0, "stale_unconfirmed": 0}
    confirmed_jobs: list[dict] = []
    unconfirmed_jobs: list[dict] = []
    companies_failed = []
    companies_dom_fallback = set()
    companies_with_matches = set()
    companies_with_unconfirmed = set()

    def title_prefilter(raw_title: str) -> bool:
        first = (raw_title.strip().splitlines() or [""])[0].strip()
        if permanent_only and not is_permanent(first):
            return False
        return role_filter is None or bool(role_filter.match(first))

    fetched = fetch_all(companies, workers, title_prefilter, ats_direct)

    for company in companies:
        name = company["name"]
        result = fetched.get(name)
        if result is None or result[0] == "error":
            logger.warning("Failed to crawl %s: %s", name, result[1] if result else "no result")
            companies_failed.append(name)
            continue
        _, raw_jobs, method = result
        if method == "failed":
            companies_failed.append(name)
            continue
        if method == "dom_fallback":
            companies_dom_fallback.add(name)

        for job in raw_jobs:
            kind = classify_location(job["location"])
            if kind is None:
                continue
            if job["posted_at"] is None and not dom_location_ok(job["title"]):
                dropped("wrong_city", name, job["title"])
                continue

            clean_title = (job["title"].strip().splitlines() or [""])[0].strip()
            matched_roles: list[str] = []
            if role_filter is not None:
                matched_roles = role_filter.match(clean_title)
                if not matched_roles:
                    dropped("role_filter", name, clean_title)
                    continue

            full_text = html_to_text(job["description_html"], None)
            employment_type = job.get("employment_type", "")
            if permanent_only and not is_permanent(clean_title, employment_type, full_text):
                dropped("not_permanent", name, clean_title)
                continue

            salary = extract_gbp_salary(f"{clean_title}\n{full_text}")
            if min_salary is not None and not passes_salary_filter(clean_title, salary, min_salary):
                dropped("below_salary" if salary else "not_senior", name, clean_title)
                continue

            description_text = full_text[:4000]
            entry = {
                "id": make_id(job["source"], company["token"], job["external_id"]),
                "company": name,
                "company_website": company.get("website", ""),
                "origin": company.get("origin", ""),
                "title": clean_title,
                "matched_roles": matched_roles,
                "location": job["location"],
                "location_kind": kind,
                "employment_type": employment_type,
                "salary_text": format_salary(*salary) if salary else None,
                "salary_max_gbp": salary[1] if salary else None,
                "description_text": description_text,
                "description_snippet": description_text[:280].rsplit(" ", 1)[0] + "…"
                if len(description_text) > 280
                else description_text,
                "url": job["url"],
                "source": job["source"],
            }
            entry["first_seen_at"] = previous_first_seen.get(entry["id"]) or now.isoformat()

            if job["posted_at"] is not None and within_window(job["posted_at"], cutoff):
                entry["posted_at"] = job["posted_at"].isoformat()
                confirmed_jobs.append(entry)
                companies_with_matches.add(name)
            elif job["posted_at"] is None:
                # Page-scraped: no posted date. Age it by when we first saw it:
                # still on the site after a full window means it was posted
                # before the window, so drop it.
                if datetime.fromisoformat(entry["first_seen_at"]) < cutoff:
                    dropped("stale_unconfirmed", name, clean_title)
                    continue
                entry["posted_at"] = None
                unconfirmed_jobs.append(entry)
                companies_with_unconfirmed.add(name)
            # else: has a real timestamp but it's outside the window — drop.

    confirmed_jobs.sort(key=lambda j: j["posted_at"], reverse=True)
    unconfirmed_jobs.sort(key=lambda j: (j["company"], j["title"]))

    meta = {
        "generated_at": now.isoformat(),
        "window_hours": None,  # filled in by caller
        "highlight_hours": highlight_hours,
        "companies_checked": len(companies),
        "companies_with_matches": sorted(companies_with_matches),
        "companies_with_unconfirmed": sorted(companies_with_unconfirmed),
        "companies_dom_fallback": sorted(companies_dom_fallback),
        "ats_direct": ats_direct,
        "companies_failed": companies_failed,
        "total_jobs_found": len(confirmed_jobs),
        "total_unconfirmed_found": len(unconfirmed_jobs),
        "role_filter": role_filter.labels if role_filter is not None else None,
        "filters": {
            "locations": [LOCATION_LABELS[k] for k in ("london", "south_east", "remote_uk")],
            "permanent_only": permanent_only,
            "min_salary_gbp": min_salary,
        },
        "dropped_by_role_filter": drops["role_filter"],
        "dropped_wrong_city": drops["wrong_city"],
        "dropped_not_permanent": drops["not_permanent"],
        "dropped_not_senior": drops["not_senior"],
        "dropped_below_salary": drops["below_salary"],
        "dropped_stale_unconfirmed": drops["stale_unconfirmed"],
        "crawl_seconds": round(time.time() - started),
    }
    return confirmed_jobs, unconfirmed_jobs, meta


def main() -> None:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument(
        "--hours", type=int, default=168,
        help="Keep roles posted within this many hours (default 168 = 7 days)",
    )
    parser.add_argument(
        "--highlight-hours", type=int, default=48,
        help="Roles posted within this many hours are highlighted as new in the UI (default 48)",
    )
    parser.add_argument("--workers", type=int, default=4, help="Parallel browsers (default 4)")
    parser.add_argument(
        "--min-salary", type=int, default=MIN_SALARY_GBP,
        help="GBP floor: stated pay must reach it, or (if unstated) the title must be senior. 0 turns the seniority/salary filter off.",
    )
    parser.add_argument(
        "--include-non-permanent", action="store_true",
        help="Keep contract / fixed-term / intern / part-time roles too",
    )
    parser.add_argument(
        "--ats-direct", action=argparse.BooleanOptionalAction, default=True,
        help="For companies on Greenhouse/Lever/Ashby/Workable, read the ATS's public job-board feed (complete, with posted dates), falling back to the company's own careers page if the feed fails. Default on; --no-ats-direct reads company pages only.",
    )
    parser.add_argument(
        "--debug-drops", action="store_true",
        help="Also write dropped.json (company, title, reason) next to the output, to check what the filters remove",
    )
    parser.add_argument(
        "--only", type=str, default="",
        help="Comma-separated company names to crawl (default: all) — for quick tests",
    )
    parser.add_argument(
        "--companies",
        type=Path,
        default=Path(__file__).resolve().parent / "companies.yaml",
    )
    parser.add_argument(
        "--output",
        type=Path,
        default=Path(__file__).resolve().parent.parent / "frontend" / "public" / "data",
        help="Directory to write jobs.json and meta.json into (also read for previous first-seen dates)",
    )
    parser.add_argument(
        "--roles",
        type=Path,
        default=Path(__file__).resolve().parent / "roles.yaml",
        help="Role-filter config; only jobs whose title matches a role family are kept",
    )
    parser.add_argument(
        "--no-role-filter",
        action="store_true",
        help="Keep every matching-location job regardless of title",
    )
    args = parser.parse_args()

    role_filter = None if args.no_role_filter else RoleFilter.from_yaml(args.roles)
    cutoff = datetime.now(timezone.utc) - timedelta(hours=args.hours)
    companies = load_companies(args.companies)
    if args.only:
        wanted = {n.strip() for n in args.only.split(",")}
        companies = [c for c in companies if c["name"] in wanted]

    logger.info(
        "Rendering careers pages for %d companies with %d workers; keeping roles posted since %s",
        len(companies), args.workers, cutoff.isoformat(),
    )
    previous_first_seen = load_previous_first_seen(args.output)
    drop_log: list | None = [] if args.debug_drops else None
    confirmed_jobs, unconfirmed_jobs, meta = crawl(
        companies, cutoff, role_filter, previous_first_seen, args.highlight_hours,
        workers=args.workers,
        min_salary=args.min_salary or None,
        permanent_only=not args.include_non_permanent,
        drop_log=drop_log,
        ats_direct=args.ats_direct,
    )
    meta["window_hours"] = args.hours

    args.output.mkdir(parents=True, exist_ok=True)
    (args.output / "jobs.json").write_text(
        json.dumps({"jobs": confirmed_jobs, "unconfirmed_jobs": unconfirmed_jobs}, indent=2)
    )
    (args.output / "meta.json").write_text(json.dumps(meta, indent=2))
    if drop_log is not None:
        (args.output / "dropped.json").write_text(json.dumps(drop_log, indent=1))

    logger.info(
        "Done in %ss. %d confirmed + %d unconfirmed roles; %d companies failed, %d used page-scraping.",
        meta["crawl_seconds"], len(confirmed_jobs), len(unconfirmed_jobs),
        len(meta["companies_failed"]), len(meta["companies_dom_fallback"]),
    )


if __name__ == "__main__":
    main()
