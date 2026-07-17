#!/usr/bin/env python3
"""Crawl a curated list of UK software product companies' own job boards,
keep roles that are (a) based in London, UK and (b) posted within the last
N hours, and write the result to a JSON file the frontend reads directly.

Usage:
    python crawl.py [--hours 24] [--output ../frontend/public/data]

Exit code is always 0 on a "successful run" even if individual companies
fail to fetch (those are logged as warnings and skipped) so a single bad
company doesn't take down the scheduled crawl.
"""
from __future__ import annotations

import argparse
import json
import logging
import re
import sys
import time
from datetime import datetime, timedelta, timezone
from pathlib import Path

import yaml
from bs4 import BeautifulSoup

sys.path.insert(0, str(Path(__file__).resolve().parent))
from ats_clients import fetch_company_jobs  # noqa: E402

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
        # No reliable timestamp from the source feed: don't guess, exclude.
        return False
    return posted_at >= cutoff


def html_to_text(html: str, max_len: int = 4000) -> str:
    if not html:
        return ""
    text = BeautifulSoup(html, "html.parser").get_text(separator="\n").strip()
    text = re.sub(r"\n{3,}", "\n\n", text)
    return text[:max_len]


def load_companies(path: Path) -> list[dict]:
    with open(path) as f:
        data = yaml.safe_load(f)
    return data.get("companies", [])


def crawl(companies: list[dict], cutoff: datetime) -> tuple[list[dict], dict]:
    all_jobs: list[dict] = []
    companies_checked = 0
    companies_failed = []
    companies_with_matches = set()

    for company in companies:
        companies_checked += 1
        name = company["name"]
        try:
            raw_jobs = fetch_company_jobs(company)
        except Exception as exc:  # noqa: BLE001 - one bad company shouldn't kill the run
            logger.warning("Failed to fetch jobs for %s (%s): %s", name, company["ats"], exc)
            companies_failed.append(name)
            time.sleep(0.5)
            continue

        for job in raw_jobs:
            if not is_london_uk(job["location"]):
                continue
            if not within_window(job["posted_at"], cutoff):
                continue

            companies_with_matches.add(name)
            description_text = html_to_text(job["description_html"])
            all_jobs.append(
                {
                    "id": f"{job['source']}-{company['token']}-{job['external_id']}",
                    "company": name,
                    "company_website": company.get("website", ""),
                    "title": job["title"],
                    "location": job["location"],
                    "description_text": description_text,
                    "description_snippet": description_text[:280].rsplit(" ", 1)[0] + "…"
                    if len(description_text) > 280
                    else description_text,
                    "url": job["url"],
                    "source": job["source"],
                    "posted_at": job["posted_at"].isoformat(),
                }
            )

        time.sleep(0.5)  # be polite between requests

    all_jobs.sort(key=lambda j: j["posted_at"], reverse=True)

    meta = {
        "generated_at": datetime.now(timezone.utc).isoformat(),
        "window_hours": None,  # filled in by caller
        "companies_checked": companies_checked,
        "companies_with_matches": sorted(companies_with_matches),
        "companies_failed": companies_failed,
        "total_jobs_found": len(all_jobs),
    }
    return all_jobs, meta


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

    logger.info("Crawling %d companies for London, UK roles posted since %s", len(companies), cutoff.isoformat())
    jobs, meta = crawl(companies, cutoff)
    meta["window_hours"] = args.hours

    args.output.mkdir(parents=True, exist_ok=True)
    (args.output / "jobs.json").write_text(json.dumps({"jobs": jobs}, indent=2))
    (args.output / "meta.json").write_text(json.dumps(meta, indent=2))

    logger.info(
        "Done. %d matching jobs from %d/%d companies (%d failed).",
        len(jobs), len(meta["companies_with_matches"]), meta["companies_checked"], len(meta["companies_failed"]),
    )


if __name__ == "__main__":
    main()
