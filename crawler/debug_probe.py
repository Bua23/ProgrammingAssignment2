#!/usr/bin/env python3
"""Diagnostic tool: render a handful of companies' careers pages with heavy
logging, to figure out why browser_crawl.py's network-capture / click-through
path isn't working for a given company. Not part of the scheduled crawl —
run manually (locally or via the debug-crawl.yml workflow_dispatch) when
investigating a specific company or a systemic capture problem.

Usage:
    python debug_probe.py --companies "DeepMind,GoodNotes,Palantir Technologies"
    python debug_probe.py --all-dom-fallback   # re-probe everyone currently
                                                 # in meta.json's companies_dom_fallback
"""
from __future__ import annotations

import argparse
import json
import logging
import sys
from pathlib import Path

import yaml
from playwright.sync_api import sync_playwright

sys.path.insert(0, str(Path(__file__).resolve().parent))
from ats_clients import API_URL_PATTERNS, BOARD_URL_PATTERNS  # noqa: E402
from browser_crawl import DESKTOP_USER_AGENT, PAGE_TIMEOUT_MS, POST_LOAD_WAIT_MS  # noqa: E402

logging.basicConfig(level=logging.INFO, format="%(asctime)s %(levelname)s %(message)s")
logger = logging.getLogger("debug_probe")

BOT_BLOCK_MARKERS = (
    "just a moment", "checking your browser", "enable javascript and cookies",
    "attention required", "access denied", "are you a robot", "verify you are human",
    "unusual traffic", "captcha", "cf-browser-verification", "please wait while we verify",
)


def probe(browser, company: dict, screenshot_dir: Path) -> None:
    name = company["name"]
    ats = company["ats"]
    careers_url = company["careers_url"]
    api_pattern = API_URL_PATTERNS.get(ats)
    board_pattern = BOARD_URL_PATTERNS.get(ats)

    print(f"\n{'=' * 70}\n{name}  (ats={ats}, careers_url={careers_url})\n{'=' * 70}")

    page = browser.new_page(user_agent=DESKTOP_USER_AGENT)
    console_msgs = []
    page.on("console", lambda msg: console_msgs.append(f"[{msg.type}] {msg.text}"))
    page.on("pageerror", lambda err: console_msgs.append(f"[pageerror] {err}"))

    responses_seen = []
    captured = {}

    def on_response(response):
        responses_seen.append((response.status, response.url))
        if "body" in captured:
            return
        if api_pattern and api_pattern.search(response.url):
            try:
                captured["body"] = response.json()
                captured["from_url"] = response.url
            except Exception as exc:  # noqa: BLE001
                print(f"  ! matched API pattern but couldn't parse JSON: {exc}")

    page.on("response", on_response)

    try:
        resp = page.goto(careers_url, timeout=PAGE_TIMEOUT_MS, wait_until="load")
        page.wait_for_timeout(POST_LOAD_WAIT_MS)
        print(f"  goto() response status: {resp.status if resp else 'None'}")
    except Exception as exc:  # noqa: BLE001
        print(f"  ! goto() FAILED: {exc}")
        page.close()
        return

    print(f"  final URL after load: {page.url}")
    print(f"  page title: {page.title()!r}")

    try:
        body_text = page.inner_text("body")
    except Exception as exc:  # noqa: BLE001
        body_text = ""
        print(f"  ! could not read body text: {exc}")

    lowered = body_text.lower()
    blocked_markers_found = [m for m in BOT_BLOCK_MARKERS if m in lowered]
    print(f"  body text length: {len(body_text)} chars")
    print(f"  bot-block markers found: {blocked_markers_found or 'none'}")
    print(f"  body text snippet: {body_text[:300]!r}")

    print(f"  total network responses seen: {len(responses_seen)}")
    non_2xx = [(s, u) for s, u in responses_seen if s is not None and not (200 <= s < 300)]
    if non_2xx:
        print(f"  non-2xx responses ({len(non_2xx)}): {non_2xx[:10]}")

    if "body" in captured:
        n_jobs = len(captured["body"].get("jobs", captured["body"])) if isinstance(captured["body"], (dict, list)) else "?"
        print(f"  >>> CAPTURED matching API response from {captured['from_url']} on initial load ({n_jobs} raw entries)")
    else:
        print("  no matching API response captured on initial load")

        if board_pattern:
            try:
                hrefs = page.eval_on_selector_all("a", "els => els.map(e => e.href)")
            except Exception as exc:  # noqa: BLE001
                hrefs = []
                print(f"  ! could not enumerate links: {exc}")
            print(f"  total <a> links on page: {len(hrefs)}")
            matching = [h for h in hrefs if board_pattern.search(h)]
            print(f"  links matching board pattern for '{ats}': {matching[:5] or 'NONE FOUND'}")

            if matching:
                board_url = matching[0]
                print(f"  following click-through to: {board_url}")
                try:
                    page.goto(board_url, timeout=PAGE_TIMEOUT_MS, wait_until="load")
                    page.wait_for_timeout(POST_LOAD_WAIT_MS)
                    print(f"  board page title: {page.title()!r}")
                    board_text = page.inner_text("body")
                    board_lowered = board_text.lower()
                    board_blocked = [m for m in BOT_BLOCK_MARKERS if m in board_lowered]
                    print(f"  board page bot-block markers: {board_blocked or 'none'}")
                    print(f"  board page text snippet: {board_text[:300]!r}")
                except Exception as exc:  # noqa: BLE001
                    print(f"  ! click-through goto() FAILED: {exc}")

                if "body" in captured:
                    print(f"  >>> CAPTURED matching API response after click-through from {captured['from_url']}")
                else:
                    print("  still no matching API response captured after click-through")
        else:
            print(f"  (no BOARD_URL_PATTERN configured for ats={ats!r}, skipping click-through)")

    if console_msgs:
        print(f"  console/page errors ({len(console_msgs)}):")
        for m in console_msgs[:15]:
            print(f"    {m}")

    screenshot_dir.mkdir(parents=True, exist_ok=True)
    safe_name = "".join(c if c.isalnum() else "_" for c in name)
    screenshot_path = screenshot_dir / f"{safe_name}.png"
    try:
        page.screenshot(path=str(screenshot_path), full_page=False)
        print(f"  screenshot saved: {screenshot_path}")
    except Exception as exc:  # noqa: BLE001
        print(f"  ! screenshot failed: {exc}")

    page.close()


def main() -> None:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--companies", type=str, default="", help="Comma-separated company names to probe")
    parser.add_argument(
        "--companies-file",
        type=Path,
        default=Path(__file__).resolve().parent / "companies.yaml",
    )
    parser.add_argument(
        "--all-dom-fallback",
        action="store_true",
        help="Probe every company currently listed in meta.json's companies_dom_fallback",
    )
    parser.add_argument(
        "--meta-file",
        type=Path,
        default=Path(__file__).resolve().parent.parent / "frontend" / "public" / "data" / "meta.json",
    )
    parser.add_argument(
        "--screenshot-dir",
        type=Path,
        default=Path(__file__).resolve().parent.parent / "debug-screenshots",
    )
    args = parser.parse_args()

    with open(args.companies_file) as f:
        all_companies = {c["name"]: c for c in yaml.safe_load(f)["companies"]}

    if args.all_dom_fallback:
        meta = json.loads(args.meta_file.read_text())
        wanted = meta.get("companies_dom_fallback", [])
    elif args.companies:
        wanted = [n.strip() for n in args.companies.split(",")]
    else:
        parser.error("Pass --companies or --all-dom-fallback")
        return

    companies = []
    for name in wanted:
        if name not in all_companies:
            print(f"! Unknown company (not in companies.yaml): {name}")
            continue
        companies.append(all_companies[name])

    print(f"Probing {len(companies)} companies...")

    with sync_playwright() as p:
        browser = p.chromium.launch(args=["--no-sandbox"])
        try:
            for company in companies:
                probe(browser, company, args.screenshot_dir)
        finally:
            browser.close()


if __name__ == "__main__":
    main()
