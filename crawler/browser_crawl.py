"""Render each company's own public careers page in a headless browser.

This is the actual "visit the company's own website" crawl path: for each
company we navigate a real (headless) browser to the careers URL on their
own domain and watch what the page itself does while it loads.

- If the page's own JavaScript calls a known ATS API (Greenhouse/Lever/
  Ashby/Workable) to populate the listing — which is exactly what these
  embedded "careers" widgets normally do — we capture that response (the
  same request a human visitor's browser would make) and parse it with the
  shared normalizers in ats_clients.py. We never call the ATS ourselves.
- If no such call is observed on the initial page — common for companies
  whose own careers page is a marketing page with a "View open roles"
  button that links out to the ATS-hosted board rather than embedding it —
  we look for a link matching the company's configured ATS and follow it
  once (the same click a real visitor would make), then check again.
- If still nothing, we fall back to best-effort scraping of the rendered
  DOM. That fallback has no reliable "posted date" field, so callers should
  treat those results as unconfirmed / lower-confidence.
"""
from __future__ import annotations

import logging
import re

from playwright.sync_api import Browser, Response
from playwright.sync_api import TimeoutError as PlaywrightTimeoutError

from ats_clients import API_URL_PATTERNS, BOARD_URL_PATTERNS, parse_captured_response
from jobposting import find_jobposting, jobposting_fields
from locations import LOCATION_LABELS, classify_location

logger = logging.getLogger(__name__)

PAGE_TIMEOUT_MS = 25_000
POST_LOAD_WAIT_MS = 4_000

DESKTOP_USER_AGENT = (
    "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 "
    "(KHTML, like Gecko) Chrome/124.0.0.0 Safari/537.36 "
    "UKSoftwareJobsCrawler/1.0 (+https://github.com/bua23/programmingassignment2)"
)

NAV_WORDS = {
    "home", "about", "about us", "contact", "contact us", "privacy",
    "privacy policy", "terms", "terms of service", "cookie", "cookies",
    "cookie policy", "login", "log in", "sign in", "sign up", "blog",
    "press", "news", "careers", "jobs", "open roles", "view all jobs",
    "see all jobs", "apply", "apply now", "learn more", "read more",
    "get started", "contact sales", "book a demo",
}


def launch_chromium(playwright):
    """Launch headless Chromium. Prefers an explicit CRAWLER_CHROMIUM path, then
    a browser pre-installed under PLAYWRIGHT_BROWSERS_PATH / /opt/pw-browsers
    (whose revision may not match the pip playwright version), else Playwright's
    own download (what GitHub Actions gets from `playwright install`)."""
    import glob
    import os

    exe = os.environ.get("CRAWLER_CHROMIUM")
    if not exe:
        root = os.environ.get("PLAYWRIGHT_BROWSERS_PATH", "/opt/pw-browsers")
        found = sorted(glob.glob(os.path.join(root, "chromium-*", "chrome-linux*", "chrome")))
        exe = found[-1] if found else None
    kwargs = {"args": ["--no-sandbox"]}
    if exe:
        kwargs["executable_path"] = exe
    return playwright.chromium.launch(**kwargs)


def crawl_company(browser: Browser, company: dict, title_prefilter=None) -> tuple[list[dict], str]:
    """Returns (jobs, capture_method); capture_method is one of
    'network' | 'dom_fallback' | 'failed'.

    title_prefilter: optional callable(title) -> bool. When given, page-scraped
    jobs whose title fails it are dropped, and the survivors are opened one by
    one to read the posted date, employment type, location and pay from the
    job page's own JobPosting data (see jobposting.py)."""
    ats = company["ats"]
    pattern = API_URL_PATTERNS.get(ats)
    careers_url = company.get("careers_url")
    if not careers_url:
        logger.warning("No careers_url configured for %s", company["name"])
        return [], "failed"

    captured: dict[str, object] = {}
    page = browser.new_page(user_agent=DESKTOP_USER_AGENT)

    def on_response(response: Response) -> None:
        if "body" in captured:
            return
        if pattern and pattern.search(response.url):
            try:
                captured["body"] = response.json()
            except Exception:  # noqa: BLE001 - matched URL wasn't a clean JSON body
                pass

    page.on("response", on_response)

    try:
        page.goto(careers_url, timeout=PAGE_TIMEOUT_MS, wait_until="load")
        page.wait_for_timeout(POST_LOAD_WAIT_MS)  # let client-side XHRs fire
    except PlaywrightTimeoutError:
        logger.warning("Timed out loading %s (%s)", company["name"], careers_url)
    except Exception as exc:  # noqa: BLE001
        logger.warning("Failed to load %s (%s): %s", company["name"], careers_url, exc)
        page.close()
        return [], "failed"

    if "body" not in captured:
        board_url = _find_board_link(page, ats)
        if board_url:
            try:
                page.goto(board_url, timeout=PAGE_TIMEOUT_MS, wait_until="load")
                page.wait_for_timeout(POST_LOAD_WAIT_MS)
            except Exception as exc:  # noqa: BLE001
                logger.info(
                    "Click-through to %s ATS board failed for %s (%s): %s",
                    ats, company["name"], board_url, exc,
                )

    if "body" in captured:
        try:
            jobs = parse_captured_response(ats, captured["body"])
            page.close()
            return jobs, "network"
        except Exception as exc:  # noqa: BLE001
            logger.warning(
                "Captured a %s response for %s but couldn't parse it: %s",
                ats, company["name"], exc,
            )

    jobs = _dom_fallback(page, company)
    page.close()
    if title_prefilter is not None:
        jobs = [j for j in jobs if title_prefilter(j["title"])]
        _enrich_from_job_pages(browser, jobs)
    return jobs, "dom_fallback"


def _enrich_from_job_pages(browser: Browser, jobs: list[dict], limit: int = 40) -> None:
    """Open each job's own page and merge in whatever its JobPosting JSON-LD
    says (posted date, employment type, location, pay, description)."""
    for job in jobs[:limit]:
        page = browser.new_page(user_agent=DESKTOP_USER_AGENT)
        try:
            page.goto(job["url"], timeout=20_000, wait_until="domcontentloaded")
            page.wait_for_timeout(1_500)
            blocks = page.eval_on_selector_all(
                'script[type="application/ld+json"]', "els => els.map(e => e.textContent)"
            )
            posting = find_jobposting(blocks)
            if posting:
                job.update(jobposting_fields(posting))
                job["source"] = "jobposting-data"
        except Exception as exc:  # noqa: BLE001 - enrichment is best-effort
            logger.info("Could not read job page %s: %s", job.get("url"), exc)
        finally:
            page.close()


def _find_board_link(page, ats: str) -> str | None:
    """Look for an on-page link pointing at the company's configured ATS
    board (e.g. a "View open roles" button) and return its href, if any."""
    pattern = BOARD_URL_PATTERNS.get(ats)
    if not pattern:
        return None
    try:
        hrefs = page.eval_on_selector_all("a", "els => els.map(e => e.href)")
    except Exception:  # noqa: BLE001
        return None
    for href in hrefs:
        m = pattern.search(href)
        if not m:
            continue
        host_and_token = m.group(0)
        if host_and_token.rstrip("/").endswith("/embed"):
            # boards.greenhouse.io/embed/job_board?for=<token>
            q = re.search(r"[?&]for=([^&#]+)", href)
            if not q:
                continue
            host_and_token = host_and_token.rsplit("/", 1)[0] + "/" + q.group(1)
        # Go to the board itself, not the single job the link pointed at.
        return "https://" + host_and_token
    return None


def _dom_fallback(page, company: dict, max_results: int = 300) -> list[dict]:
    """Best-effort: find links on the page whose surrounding card/row
    mentions London, the South East or UK-remote. No reliable posted-date is available this way."""
    try:
        body_text = page.inner_text("body").lower()
    except Exception:  # noqa: BLE001
        return []
    if not classify_location(body_text):
        return []

    try:
        candidates = page.eval_on_selector_all(
            "a",
            """els => els.slice(0, 800).map(e => {
                const card = e.closest('li,tr,article,div') || e;
                return {
                    text: (e.innerText || '').trim(),
                    href: e.href,
                    context: (card.innerText || '').slice(0, 300),
                };
            })""",
        )
    except Exception:  # noqa: BLE001
        return []

    jobs = []
    seen_urls = set()
    for c in candidates:
        text, href, context = c.get("text", ""), c.get("href", ""), c.get("context", "")
        if not (4 <= len(text) <= 120):
            continue
        if text.strip().lower() in NAV_WORDS:
            continue
        if not href.startswith("http"):
            continue
        kind = classify_location(context)
        if not kind:
            continue
        if href in seen_urls:
            continue
        seen_urls.add(href)
        jobs.append(
            {
                "external_id": href,
                "title": text,
                "location": f"{LOCATION_LABELS[kind]} (unconfirmed)",
                "url": href,
                "description_html": "",
                "employment_type": "",
                "posted_at": None,
                "source": f"dom-fallback-{company['ats']}",
            }
        )
        if len(jobs) >= max_results:
            break
    return jobs
