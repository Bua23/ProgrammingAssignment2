"""Thin clients for the public job-board JSON feeds published by each ATS.

Every function returns a list of NormalizedJob dicts:
    {
        "external_id": str,
        "title": str,
        "location": str,
        "url": str,
        "description_html": str,
        "posted_at": datetime | None,   # tz-aware UTC
        "source": "greenhouse" | "lever" | "ashby",
    }

These are all public, unauthenticated endpoints that back the company's own
"Careers" page widget on their own website — no login, no credential
automation, no bypassing of access controls.
"""
from __future__ import annotations

import logging
from datetime import datetime, timezone
from typing import Any

import requests

logger = logging.getLogger(__name__)

USER_AGENT = (
    "UKSoftwareJobsCrawler/1.0 "
    "(+https://github.com/bua23/programmingassignment2; personal/educational use)"
)

REQUEST_TIMEOUT = 20


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


def fetch_greenhouse(token: str, **_ignored) -> list[dict]:
    """Greenhouse job-board API.

    The public API host (boards-api.greenhouse.io) is the same regardless of
    whether the company's front-end careers page is served from the legacy
    boards.greenhouse.io UI or the newer job-boards.greenhouse.io UI.

    NOTE: Greenhouse's public list endpoint does not expose a true "created"
    timestamp, only `updated_at`. For newly posted roles this is normally the
    same moment, but a role that was edited later will look "recently
    updated" even if it was posted long ago. This is a known limitation of
    the public feed (no auth = no access to the richer recruiting API).
    """
    url = f"https://boards-api.greenhouse.io/v1/boards/{token}/jobs"
    data = _get(url, params={"content": "true"})
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


def fetch_lever(token: str) -> list[dict]:
    url = f"https://api.lever.co/v0/postings/{token}"
    data = _get(url, params={"mode": "json"})
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


def fetch_ashby(token: str) -> list[dict]:
    url = f"https://api.ashbyhq.com/posting-api/job-board/{token}"
    data = _get(url, params={"includeCompensation": "false"})
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


def fetch_workable(token: str) -> list[dict]:
    url = f"https://apply.workable.com/api/v1/widget/accounts/{token}"
    data = _get(url, params={"details": "true"})
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


FETCHERS = {
    "greenhouse": fetch_greenhouse,
    "lever": fetch_lever,
    "ashby": fetch_ashby,
    "workable": fetch_workable,
}


def fetch_company_jobs(company: dict) -> list[dict]:
    ats = company["ats"]
    fetcher = FETCHERS.get(ats)
    if fetcher is None:
        raise ValueError(f"Unsupported ATS type: {ats}")
    return fetcher(company["token"])
