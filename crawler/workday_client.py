"""Reader for Workday-hosted career sites (`{tenant}.wd{N}.myworkdayjobs.com`).

Every Workday career site is a single-page app that loads its listings from a
public JSON endpoint, the same call a visitor's browser makes:

    POST https://{host}/wday/cxs/{tenant}/{site}/jobs
         {"appliedFacets": {...}, "limit": 20, "offset": 0, "searchText": ""}
    GET  https://{host}/wday/cxs/{tenant}/{site}{externalPath}      (one job)

The list has no descriptions and only a relative "Posted 3 Days Ago" date, so:
  * the list is narrowed to the United Kingdom with the site's own country facet;
  * only titles that pass `title_prefilter` get a detail call, which returns the
    full description, the exact location, and the employment type.

`parse_workday_jobs` and `parse_posted_on` are pure functions (unit-tested).
`fetch_workday` is the network part.
"""
from __future__ import annotations

import logging
import re
import time
from datetime import datetime, timedelta, timezone
from typing import Callable
from urllib.parse import urlparse

import requests

logger = logging.getLogger(__name__)

USER_AGENT = (
    "UKSoftwareJobsCrawler/1.0 "
    "(+https://github.com/bua23/programmingassignment2; personal/educational use)"
)
PAGE_SIZE = 20           # Workday rejects larger pages
MAX_LIST_JOBS = 1500     # safety cap if no UK facet can be applied
REQUEST_GAP_SECONDS = 0.4
REQUEST_TIMEOUT = 25

UK_NAMES = re.compile(r"^(united kingdom|uk|gbr|great britain|england)\b", re.IGNORECASE)
UK_LOCATION_VALUE = re.compile(r"\b(united kingdom|uk|gbr|england|london)\b", re.IGNORECASE)


def parse_workday_url(url: str) -> tuple[str, str, str]:
    """`https://adobe.wd5.myworkdayjobs.com/en-US/external_experienced/...` ->
    ("adobe.wd5.myworkdayjobs.com", "adobe", "external_experienced")."""
    parsed = urlparse(url)
    host = parsed.hostname or ""
    if "myworkdayjobs.com" not in host:
        raise ValueError(f"Not a Workday URL: {url}")
    tenant = host.split(".")[0]
    parts = [p for p in parsed.path.split("/") if p]
    if parts and re.fullmatch(r"[a-z]{2}-[A-Z]{2}", parts[0]):
        parts = parts[1:]
    if not parts:
        raise ValueError(f"No career-site name in Workday URL: {url}")
    return host, tenant, parts[0]


def parse_posted_on(text: str | None, now: datetime | None = None) -> datetime | None:
    """Workday's relative date ("Posted Today", "Posted 3 Days Ago", "Posted
    30+ Days Ago") -> a UTC datetime at the start of that day. Day-granular,
    which is all the 7-day window and 2-day highlight need. "30+" is returned
    as exactly 30 days ago (so it ages out)."""
    if not text:
        return None
    now = now or datetime.now(timezone.utc)
    today = now.replace(hour=0, minute=0, second=0, microsecond=0)
    lowered = text.lower()
    if "today" in lowered:
        return today
    if "yesterday" in lowered:
        return today - timedelta(days=1)
    m = re.search(r"(\d+)\+?\s*day", lowered)
    if m:
        return today - timedelta(days=int(m.group(1)))
    return None


def _parse_iso_date(value: str | None) -> datetime | None:
    if not value:
        return None
    try:
        dt = datetime.fromisoformat(value.replace("Z", "+00:00"))
    except ValueError:
        return None
    return dt.replace(tzinfo=timezone.utc) if dt.tzinfo is None else dt.astimezone(timezone.utc)


def _walk_facets(facets: list[dict]):
    for facet in facets or []:
        yield facet
        for value in facet.get("values", []) or []:
            if "facetParameter" in value:
                yield from _walk_facets([value])


def uk_applied_facets(facets: list[dict]) -> dict[str, list[str]]:
    """Pick the site's country facet value(s) for the UK, e.g.
    {"locationCountry": ["29247e57..."]}. Empty dict when there's no country
    facet (or no UK value in it)."""
    all_facets = list(_walk_facets(facets))
    # Preferred: a country facet with a United Kingdom value.
    for facet in all_facets:
        param = facet.get("facetParameter") or ""
        label = (facet.get("descriptor") or "").lower()
        if "country" not in param.lower() and "country" not in label:
            continue
        ids = [
            v["id"] for v in facet.get("values", []) or []
            if v.get("id") and UK_NAMES.match((v.get("descriptor") or "").strip())
        ]
        if ids:
            return {param: ids}
    # Otherwise: every UK-looking value of the site's location facet
    # ("London, United Kingdom", "UK - London", "Remote - United Kingdom"...).
    for facet in all_facets:
        param = facet.get("facetParameter") or ""
        if "location" not in param.lower() or param == "locationMainGroup":
            continue
        ids = [
            v["id"] for v in facet.get("values", []) or []
            if v.get("id") and "facetParameter" not in v and UK_LOCATION_VALUE.search(v.get("descriptor") or "")
        ]
        if ids:
            return {param: ids}
    return {}


def parse_workday_jobs(postings: list[dict], host: str, tenant: str, site: str,
                       now: datetime | None = None) -> list[dict]:
    """List response `jobPostings` -> normalised jobs (no description yet)."""
    jobs = []
    for p in postings:
        path = p.get("externalPath") or ""
        if not path:
            continue
        bullets = p.get("bulletFields") or []
        jobs.append(
            {
                "external_id": str(bullets[0]) if bullets else path.rsplit("_", 1)[-1],
                "title": (p.get("title") or "").strip(),
                "location": p.get("locationsText") or "",
                "url": f"https://{host}/en-US/{site}{path}",
                "description_html": "",
                "employment_type": "",
                "posted_at": parse_posted_on(p.get("postedOn"), now),
                "source": "workday",
                "_detail_path": path,
            }
        )
    return jobs


def apply_detail(job: dict, detail: dict) -> dict:
    """Merge a job-detail response (`jobPostingInfo`) into a listing entry."""
    info = detail.get("jobPostingInfo") or {}
    locations = [info.get("location")] + list(info.get("additionalLocations") or [])
    country = (info.get("country") or {}).get("descriptor") if isinstance(info.get("country"), dict) else ""
    loc_text = "; ".join(l for l in locations if l)
    if loc_text:
        job["location"] = f"{loc_text}, {country}" if country and country not in loc_text else loc_text
    if info.get("jobDescription"):
        job["description_html"] = info["jobDescription"]
    time_type = (info.get("timeType") or "").strip()
    if time_type:
        job["employment_type"] = time_type          # "Full time"
    # `startDate` is the posting date on the tenants checked, but could be a
    # future start date elsewhere: use it only when it agrees (within 2 days)
    # with the relative "Posted N Days Ago" text, to sharpen it to the real day.
    posted = _parse_iso_date(info.get("startDate"))
    if posted and job["posted_at"] and abs(posted - job["posted_at"]) <= timedelta(days=2):
        job["posted_at"] = posted
    elif posted and job["posted_at"] is None and posted <= datetime.now(timezone.utc):
        job["posted_at"] = posted
    if info.get("externalUrl"):
        job["url"] = info["externalUrl"]
    return job


class _Session:
    def __init__(self) -> None:
        self.s = requests.Session()
        self.s.headers.update({"User-Agent": USER_AGENT, "Accept": "application/json",
                               "Content-Type": "application/json"})
        self._last = 0.0

    def _wait(self) -> None:
        gap = self._last + REQUEST_GAP_SECONDS - time.monotonic()
        if gap > 0:
            time.sleep(gap)

    def _send(self, method: str, url: str, **kw):
        self._wait()
        try:
            resp = self.s.request(method, url, timeout=REQUEST_TIMEOUT, **kw)
        finally:
            self._last = time.monotonic()
        if resp.status_code == 429:
            raise RuntimeError(f"{urlparse(url).hostname} returned HTTP 429")
        resp.raise_for_status()
        return resp.json()

    def post(self, url: str, body: dict):
        return self._send("POST", url, json=body)

    def get(self, url: str):
        return self._send("GET", url)


def fetch_workday(company: dict, title_prefilter: Callable[[str], bool] | None = None) -> list[dict]:
    """All UK postings of one Workday career site; descriptions fetched only for
    titles passing `title_prefilter` (all titles if None, which is slow)."""
    host, tenant, site = parse_workday_url(company["workday_url"])
    base = f"https://{host}/wday/cxs/{tenant}/{site}"
    http = _Session()

    # 1) A one-row probe returns the facet list, so the UK country id can be found.
    probe = http.post(f"{base}/jobs", {"appliedFacets": {}, "limit": 1, "offset": 0, "searchText": ""})
    applied = uk_applied_facets(probe.get("facets") or [])
    if not applied:
        logger.info("%s: no UK country facet on the Workday site; reading the unfiltered list", company["name"])

    # 2) Page through the (UK-filtered) list.
    postings: list[dict] = []
    offset = 0
    total = None
    while True:
        page = http.post(f"{base}/jobs", {"appliedFacets": applied, "limit": PAGE_SIZE,
                                          "offset": offset, "searchText": ""})
        rows = page.get("jobPostings") or []
        if total is None:
            total = page.get("total") or 0   # later pages report total=0
        postings.extend(rows)
        offset += PAGE_SIZE
        if not rows or offset >= total or len(postings) >= MAX_LIST_JOBS:
            break

    now = datetime.now(timezone.utc)
    jobs = parse_workday_jobs(postings, host, tenant, site, now)

    # 3) Detail call for plausible titles only.
    for job in jobs:
        path = job.pop("_detail_path")
        if title_prefilter is not None and not title_prefilter(job["title"]):
            continue
        try:
            apply_detail(job, http.get(f"{base}{path}"))
        except Exception as exc:  # noqa: BLE001 - keep the listing entry if detail fails
            logger.info("%s: detail fetch failed for %s (%s)", company["name"], path, exc)
    return jobs
