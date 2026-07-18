"""Parsers + thin clients for the public job-board JSON feeds published by
Greenhouse, Lever, Ashby, and Workable.

Every `parse_*` function is a pure function: JSON body in, list of
NormalizedJob dicts out:
    {
        "external_id": str,
        "title": str,
        "location": str,
        "url": str,
        "description_html": str,
        "posted_at": datetime | None,   # tz-aware UTC
        "source": "greenhouse" | "lever" | "ashby" | "workable",
    }

`browser_crawl.py` calls these on JSON bodies captured from network responses
while rendering each company's own careers page (i.e. the ATS call is made
by the company's own page, exactly as it would be for a human visitor — we
just read the response). The `fetch_*`/`fetch_company_jobs` functions below
call the same ATS endpoints directly and are kept only for local testing/
debugging the parsers in isolation; the scheduled crawl does not use them.
"""
from __future__ import annotations

import logging
import re
from datetime import datetime, timezone
from typing import Any

import requests

logger = logging.getLogger(__name__)

USER_AGENT = (
    "UKSoftwareJobsCrawler/1.0 "
    "(+https://github.com/bua23/programmingassignment2; personal/educational use)"
)

REQUEST_TIMEOUT = 20

# URL patterns used by browser_crawl.py to recognize a captured network
# response as belonging to a given ATS.
API_URL_PATTERNS: dict[str, re.Pattern] = {
    "greenhouse": re.compile(r"boards-api(\.eu)?\.greenhouse\.io/v1/boards/[^/]+/jobs"),
    "lever": re.compile(r"api\.lever\.co/v0/postings/"),
    "ashby": re.compile(r"api\.ashbyhq\.com/posting-api/job-board/"),
    "workable": re.compile(r"apply\.workable\.com/api/v1/widget/accounts/"),
}

# Human-facing ATS board URL patterns. Many companies' own careers page is a
# marketing page with a "View open roles" button that links out to one of
# these rather than embedding the listing directly. When no API call is
# captured on the initial page, browser_crawl.py looks for a link matching
# the company's configured ATS here and follows it — one click-through, same
# as a real visitor clicking "View open roles" — before falling back to DOM
# scraping.
BOARD_URL_PATTERNS: dict[str, re.Pattern] = {
    "greenhouse": re.compile(r"(boards|job-boards)(\.eu)?\.greenhouse\.io/[^/\s\"']+", re.IGNORECASE),
    "lever": re.compile(r"jobs\.lever\.co/[^/\s\"']+", re.IGNORECASE),
    "ashby": re.compile(r"jobs\.ashbyhq\.com/[^/\s\"']+", re.IGNORECASE),
    "workable": re.compile(r"apply\.workable\.com/[^/\s\"']+", re.IGNORECASE),
}


def _get(url: str, params: dict | None = None) -> Any:
    resp = requests.get(
        url,
        params=params,
        headers={"User-Agent": USER_AGENT, "Accept": "application/json"},
        timeout=REQUEST_TIMEOUT,
    )
    resp.raise_for_status()
    return resp.json()


def _parse_iso(value: str | None) -> datetime | None:
    if not value:
        return None
    try:
        value = value.replace("Z", "+00:00")
        dt = datetime.fromisoformat(value)
        if dt.tzinfo is None:
            dt = dt.replace(tzinfo=timezone.utc)
        return dt.astimezone(timezone.utc)
    except ValueError:
        logger.warning("Could not parse timestamp: %r", value)
        return None


def parse_greenhouse(data: dict) -> list[dict]:
    """Parse a Greenhouse `/v1/boards/<token>/jobs?content=true` response body.

    NOTE: Greenhouse's public feed does not expose a true "created"
    timestamp, only `updated_at`. For newly posted roles this is normally
    the same moment, but a role that was edited later will look "recently
    updated" even if it was posted long ago — a known limitation of the
    public (unauthenticated) feed.
    """
    jobs = []
    for job in data.get("jobs", []):
        location = (job.get("location") or {}).get("name", "")
        jobs.append(
            {
                "external_id": str(job.get("id")),
                "title": job.get("title", "").strip(),
                "location": location,
                "url": job.get("absolute_url", ""),
                "description_html": job.get("content", "") or "",
                "posted_at": _parse_iso(job.get("updated_at")),
                "source": "greenhouse",
            }
        )
    return jobs


def parse_lever(data: list) -> list[dict]:
    """Parse a Lever `/v0/postings/<token>?mode=json` response body."""
    jobs = []
    for job in data:
        categories = job.get("categories", {}) or {}
        location = categories.get("location", "") or job.get("country", "") or ""
        created_ms = job.get("createdAt")
        posted_at = (
            datetime.fromtimestamp(created_ms / 1000, tz=timezone.utc)
            if created_ms
            else None
        )
        jobs.append(
            {
                "external_id": str(job.get("id")),
                "title": job.get("text", "").strip(),
                "location": location,
                "url": job.get("hostedUrl", ""),
                "description_html": job.get("descriptionPlain") or job.get("description", "") or "",
                "posted_at": posted_at,
                "source": "lever",
            }
        )
    return jobs


def parse_ashby(data: dict) -> list[dict]:
    """Parse an Ashby `/posting-api/job-board/<token>` response body."""
    jobs = []
    for job in data.get("jobs", []):
        location = job.get("location", "") or job.get("address", {}).get("postalAddress", {}).get("addressLocality", "")
        jobs.append(
            {
                "external_id": str(job.get("id")),
                "title": job.get("title", "").strip(),
                "location": location,
                "url": job.get("jobUrl") or job.get("applyUrl", ""),
                "description_html": job.get("descriptionHtml", "") or "",
                "posted_at": _parse_iso(job.get("publishedAt")),
                "source": "ashby",
            }
        )
    return jobs


def parse_workable(data: dict) -> list[dict]:
    """Parse a Workable `/api/v1/widget/accounts/<token>` response body."""
    jobs = []
    for job in data.get("jobs", []):
        loc = job.get("location", {}) or {}
        location = ", ".join(filter(None, [loc.get("city"), loc.get("country")]))
        jobs.append(
            {
                "external_id": str(job.get("shortcode") or job.get("id")),
                "title": job.get("title", "").strip(),
                "location": location,
                "url": job.get("url", ""),
                "description_html": job.get("description", "") or "",
                "posted_at": _parse_iso(job.get("published_on")),
                "source": "workable",
            }
        )
    return jobs


PARSERS = {
    "greenhouse": parse_greenhouse,
    "lever": parse_lever,
    "ashby": parse_ashby,
    "workable": parse_workable,
}


def parse_captured_response(ats: str, data: Any) -> list[dict]:
    parser = PARSERS.get(ats)
    if parser is None:
        raise ValueError(f"Unsupported ATS type: {ats}")
    return parser(data)


# --- Direct-fetch helpers: local testing / debugging only. The scheduled
#     crawl goes through browser_crawl.py + parse_captured_response above. ---

def fetch_greenhouse(token: str, **_ignored) -> list[dict]:
    url = f"https://boards-api.greenhouse.io/v1/boards/{token}/jobs"
    return parse_greenhouse(_get(url, params={"content": "true"}))


def fetch_lever(token: str) -> list[dict]:
    url = f"https://api.lever.co/v0/postings/{token}"
    return parse_lever(_get(url, params={"mode": "json"}))


def fetch_ashby(token: str) -> list[dict]:
    url = f"https://api.ashbyhq.com/posting-api/job-board/{token}"
    return parse_ashby(_get(url, params={"includeCompensation": "false"}))


def fetch_workable(token: str) -> list[dict]:
    url = f"https://apply.workable.com/api/v1/widget/accounts/{token}"
    return parse_workable(_get(url, params={"details": "true"}))


FETCHERS = {
    "greenhouse": fetch_greenhouse,
    "lever": fetch_lever,
    "ashby": fetch_ashby,
    "workable": fetch_workable,
}


def fetch_company_jobs(company: dict) -> list[dict]:
    fetcher = FETCHERS.get(company["ats"])
    if fetcher is None:
        raise ValueError(f"Unsupported ATS type: {company['ats']}")
    return fetcher(company["token"])
