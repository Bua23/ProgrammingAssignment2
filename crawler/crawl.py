#!/usr/bin/env python3
"""Render each curated UK software product company's own careers page in a
headless browser, keep roles that are (a) based in London, UK and (b)
posted within the last N hours, and write the result to a JSON file the
frontend reads directly.

Usage:
    python crawl.py [--hours 24] [--output ../frontend/public/data]

Exit code is always 0 on a "successful run" even if individual companies
fail to load (those are logged as warnings and skipped) so a single bad
company doesn't take down the scheduled crawl.
"""
from __future__ import annotations

import argparse
import hashlib
import json
import logging
import re
import sys
from datetime import datetime, timedelta, timezone
from pathlib import Path

import yaml
from bs4 import BeautifulSoup
from playwright.sync_api import sync_playwright

sys.path.insert(0, str(Path(__file__).resolve().parent))
from browser_crawl import crawl_company  # noqa: E402

logging.basicConfig(
    level=logging.INFO, format="%(asctime)s %(levelname)s %(message)s"
)
logger = logging.getLogger("crawl")

NON_UK_LONDON_MARKERS = (
    "ontario", "canada", "ca,", ", on", "kentucky", " ky", "ohio", " oh",
    "south africa", "texas", " tx", "california", " ca ",
)


def is_london_uk(location: str) -> bool:
    if not location:
        return False
    loc = location.lower()
    if "london" not in loc:
        return False
    if any(marker in loc for marker in NON_UK_LONDON_MARKERS):
        return False
    return True


def within_window(posted_at: datetime | None, cutoff: datetime) -> bool:
    if posted_at is None:
        return False
    return posted_at >= cutoff


def html_to_text(html: str, max_len: int = 4000) -> str:
    if not html:
        return ""
    text = BeautifulSoup(html, "html.parser").get_text(separator="\n").strip()
    text = re.sub(r"\n{3,}", "\n\n", text)
    return text[:max_len]


def make_id(source: str, token: str, external_id: str) -> str:
    if len(external_id) > 60:
        external_id = hashlib.sha1(external_id.encode()).hexdigest()[:16]
    return f"{source}-{token}-{external_id}"


def load_companies(path: Path) -> list[dict]:
    with open(path) as f:
        data = yaml.safe_load(f)
    return data.get("companies", [])


def crawl(companies: list[dict], cutoff: datetime) -> tuple[list[dict], list[dict], dict]:
    confirmed_jobs: list[dict] = []
    unconfirmed_jobs: list[dict] = []
    companies_checked = 0
    companies_failed = []
    companies_dom_fallback = set()
    companies_with_matches = set()
    companies_with_unconfirmed = set()

    with sync_playwright() as p:
        browser = p.chromium.launch(args=["--no-sandbox"])
        try:
            for company in companies:
                companies_checked += 1
                name = company["name"]
                try:
                    raw_jobs, method = crawl_company(browser, company)
                except Exception as exc:  # noqa: BLE001 - one bad company shouldn't kill the run
                    logger.warning("Failed to crawl %s: %s", name, exc)
                    companies_failed.append(name)
                    continue

                if method == "failed":
                    companies_failed.append(name)
                    continue
                if method == "dom_fallback":
                    companies_dom_fallback.add(name)

                for job in raw_jobs:
                    if not is_london_uk(job["location"]):
                        continue

                    description_text = html_to_text(job["description_html"])
                    entry = {
                        "id": make_id(job["source"], company["token"], job["external_id"]),
                        "company": name,
                        "company_website": company.get("website", ""),
                        "origin": company.get("origin", ""),
                        "title": job["title"],
                        "location": job["location"],
                        "description_text": description_text,
                        "description_snippet": description_text[:280].rsplit(" ", 1)[0] + "…"
                        if len(description_text) > 280
                        else description_text,
                        "url": job["url"],
                        "source": job["source"],
                    }

                    if job["posted_at"] is not None and within_window(job["posted_at"], cutoff):
                        entry["posted_at"] = job["posted_at"].isoformat()
                        confirmed_jobs.append(entry)
                        companies_with_matches.add(name)
                    elif job["posted_at"] is None:
                        # DOM-fallback match: London-relevant but no reliable
                        # posted-date, so we can't confirm the 24h window.
                        entry["posted_at"] = None
                        unconfirmed_jobs.append(entry)
                        companies_with_unconfirmed.add(name)
                    # else: has a real timestamp but it's outside the window — drop.
        finally:
            browser.close()

    confirmed_jobs.sort(key=lambda j: j["posted_at"], reverse=True)
    unconfirmed_jobs.sort(key=lambda j: (j["company"], j["title"]))

    meta = {
        "generated_at": datetime.now(timezone.utc).isoformat(),
        "window_hours": None,  # filled in by caller
        "companies_checked": companies_checked,
        "companies_with_matches": sorted(companies_with_matches),
        "companies_with_unconfirmed": sorted(companies_with_unconfirmed),
        "companies_dom_fallback": sorted(companies_dom_fallback),
        "companies_failed": companies_failed,
        "total_jobs_found": len(confirmed_jobs),
        "total_unconfirmed_found": len(unconfirmed_jobs),
    }
    return confirmed_jobs, unconfirmed_jobs, meta


def main() -> None:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--hours", type=int, default=24, help="Lookback window in hours")
    parser.add_argument(
        "--companies",
        type=Path,
        default=Path(__file__).resolve().parent / "companies.yaml",
    )
    parser.add_argument(
        "--output",
        type=Path,
        default=Path(__file__).resolve().parent.parent / "frontend" / "public" / "data",
        help="Directory to write jobs.json and meta.json into",
    )
    args = parser.parse_args()

    cutoff = datetime.now(timezone.utc) - timedelta(hours=args.hours)
    companies = load_companies(args.companies)

    logger.info(
        "Rendering careers pages for %d companies, keeping London, UK roles posted since %s",
        len(companies), cutoff.isoformat(),
    )
    confirmed_jobs, unconfirmed_jobs, meta = crawl(companies, cutoff)
    meta["window_hours"] = args.hours

    args.output.mkdir(parents=True, exist_ok=True)
    (args.output / "jobs.json").write_text(
        json.dumps({"jobs": confirmed_jobs, "unconfirmed_jobs": unconfirmed_jobs}, indent=2)
    )
    (args.output / "meta.json").write_text(json.dumps(meta, indent=2))

    logger.info(
        "Done. %d confirmed + %d unconfirmed jobs from %d/%d companies (%d failed, %d via DOM fallback).",
        len(confirmed_jobs), len(unconfirmed_jobs), len(meta["companies_with_matches"]) + len(meta["companies_with_unconfirmed"]),
        meta["companies_checked"], len(meta["companies_failed"]), len(meta["companies_dom_fallback"]),
    )


if __name__ == "__main__":
    main()
