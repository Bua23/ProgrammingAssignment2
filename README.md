# London Software Jobs Dashboard

A dashboard that surfaces software-engineering roles based in **London, UK**
that were posted in the **last 24 hours**, at a curated list of ~144
companies: large employers already established in the UK (Google, Meta,
Amazon, ...), UK-native scaleups and unicorns (Monzo, Revolut, Darktrace,
...), and smaller US/international companies actively expanding into
London (Cresta, Ramp, Harvey AI, ...) — consultancies excluded throughout.
Crawls run automatically **twice on every weekday**.

> This repository started life as a Coursera R programming assignment. That
> original content is preserved at
> [`docs/original-coursera-assignment.md`](docs/original-coursera-assignment.md)
> and [`cachematrix.R`](cachematrix.R); everything below is a separate project
> built on top of it.

## How it works

```
crawler/companies.yaml   curated list of companies: name, website,
        │                 careers_url (their OWN public careers page),
        │                 and the ATS provider that page is expected to use
        ▼
crawler/browser_crawl.py  renders careers_url in a headless browser (Playwright)
        │                 and watches what the page itself does while loading:
        │                   • if the page's own JS calls a known ATS API
        │                     (Greenhouse/Lever/Ashby/Workable) to populate the
        │                     listing — exactly as it would for a human visitor —
        │                     we capture that response. We never call the ATS
        │                     ourselves.
        │                   • if not, but the page links out to an ATS-hosted
        │                     board (a "View open roles" button), we follow
        │                     that one link — same as a real visitor clicking
        │                     it — and check again.
        │                   • otherwise, best-effort DOM scraping of the
        │                     rendered page (no reliable posted-date this way)
        ▼
crawler/crawl.py          keeps roles located in London, UK; splits results into
        │                 "confirmed" (real posted-date within the last 24h)
        │                 and "unconfirmed" (DOM fallback, date unknown)
        ▼
frontend/public/data/     jobs.json ({jobs, unconfirmed_jobs}) + meta.json
        │                 (committed to the repo)
        ▼
frontend/ (React + Vite)  reads that JSON and renders the dashboard: stat
                           tiles, a jobs-by-company chart, filters, a card
                           per confirmed role, and a separate lower-confidence
                           section for unconfirmed ones — each linking to the
                           original posting on the company's own site
```

`.github/workflows/crawl.yml` runs the crawler on a schedule
(`0 7,13 * * 1-5` UTC — twice every weekday) and commits the refreshed
`jobs.json`/`meta.json`. `.github/workflows/deploy.yml` then rebuilds and
publishes the frontend to GitHub Pages. You can also trigger either workflow
manually from the Actions tab (`workflow_dispatch`).

### Why render each company's own site instead of calling Greenhouse/Lever/Ashby directly

The crawler's data source is still each ATS's public, unauthenticated JSON
feed — reliable and structured (title, location, posted date, full
description, canonical apply URL). But rather than calling
`boards-api.greenhouse.io` etc. ourselves, `crawl.py` opens each company's own
`careers_url` in headless Chromium and only reads the network response *if
the company's own page makes that call itself* while rendering — the same
thing that happens in a normal visitor's browser. No login, no credential
automation, no bypassing of access controls, and the visible "source" for
each job is genuinely each company's own website.

Two companies (Stacklok, Deductive AI) don't have a separate branded careers
page at all — their own site routes straight to the ATS-hosted board — so for
those `careers_url` points directly at that board page, which is still
rendered and captured the same way.

### The DOM-fallback path, and why some jobs are "unconfirmed"

Not every company's careers page makes a client-side call to one of the four
supported ATS APIs (e.g. FIXR runs on SmartRecruiters, which isn't
implemented; Mercor and Profound appear to be fully custom-built with jobs
hosted directly on their own domain). For those, the crawler falls back to
best-effort scraping of the rendered page: it looks for links whose
surrounding card/row mentions "London".

This fallback has no reliable posted-date field, so **we don't guess** — those
results go into a separate `unconfirmed_jobs` list and render in their own
dashboard section ("Also spotted on company career pages") rather than being
claimed as posted-within-24h. `meta.json`'s `companies_dom_fallback` field
lists which companies used this path on the most recent run.

### A known limitation of the network-capture path

Greenhouse's public feed only exposes `updated_at`, not a true "posted"
timestamp — a role that was edited stays looking "fresh" even if it was
originally posted earlier. Lever and Ashby both expose true creation/publish
timestamps, so this only affects Greenhouse-listed companies.

## Repository layout

```
crawler/
  companies.yaml        curated company list (edit this to add/remove companies)
  ats_clients.py          pure JSON parsers for Greenhouse/Lever/Ashby/Workable
                           responses, plus direct-fetch helpers used only for
                           local testing (the scheduled crawl never calls these)
  browser_crawl.py        renders each company's careers_url, captures the ATS
                           response if the page makes one (or one click-through
                           away), else DOM fallback
  crawl.py                 orchestrates crawl → filter → write JSON
  requirements.txt
frontend/
  src/                     React + TypeScript dashboard
  public/data/             jobs.json / meta.json (crawler output, committed)
.github/workflows/
  crawl.yml                scheduled crawl (twice/weekday) + manual trigger
  deploy.yml                build + publish frontend to GitHub Pages
```

## Running the crawler locally

```bash
cd crawler
python3 -m venv .venv && source .venv/bin/activate
pip install -r requirements.txt
playwright install --with-deps chromium
python crawl.py --hours 24
# writes frontend/public/data/jobs.json and meta.json
```

## Running the dashboard locally

```bash
cd frontend
npm install
npm run dev       # http://localhost:5173, reads public/data/*.json
```

`npm run build` produces a static `frontend/dist/` that can be hosted
anywhere (GitHub Pages, Netlify, S3, etc.) — the app is pure static
files plus two JSON fetches, no backend server required.

## Enabling GitHub Pages (one-time)

In the repo's **Settings → Pages**, set **Source** to **GitHub Actions**.
The `deploy.yml` workflow will then publish the site on every push to
`main`/`master` that touches `frontend/`, and after every scheduled crawl
(since the crawl commits into `frontend/public/data/`, which triggers the
path filter).

## Important notes

- **Sandboxed build environment**: I could not reach the public internet from
  the session that built this (its outbound network is restricted to a small
  allowlist — package registries, not company websites), so I could not
  render real pages or capture real data while developing. The crawl logic
  was verified with mocked network responses instead (see the test approach
  in the PR history). GitHub's own Actions runners have normal internet
  access, so the scheduled/manual crawl is where this actually runs for
  real — check `meta.json` after a run for `companies_failed` and
  `companies_dom_fallback` to see how each company actually behaved.
- **Company list is a curated, researched snapshot (~144 companies)**, not a
  live search — verified via web research on 2026-07-17 across three
  categories: large employers already established in the UK, UK-native
  scaleups/unicorns, and smaller US/international companies expanding into
  London, with consultancies explicitly excluded throughout. This caught a
  couple of wrong assumptions along the way (e.g. `edra.com` is an unrelated
  furniture company; the real one is `edra.ai`). Sites redesign and migrate
  ATS providers over time; re-verify periodically and prune/extend
  `companies.yaml` as needed.
- **`ats: other` means no confirmed-timestamp parser exists yet** for that
  company's real backend (Workday, SmartRecruiters, Avature, Eightfold, or a
  fully custom system are all common among the larger companies) — those
  always land in the unconfirmed bucket rather than the confirmed last-24h
  list. The `note` field on each entry says what the real backend actually
  is, for whoever adds a parser for it later.
- **Making company discovery itself run automatically before every crawl is
  not yet implemented.** This list was researched manually (via web search)
  rather than by a script the scheduled workflow can invoke — GitHub Actions
  has no built-in web-search capability. Doing that for real needs either a
  jobs-aggregator API (e.g. Adzuna/Reed, free tier, used only to discover
  *which companies* are currently hiring, never as the job-data source
  itself) or an LLM API call with web search (recurring cost). Neither is
  wired up yet; this file is a snapshot to be refreshed periodically until
  that's built.
- **Attribution**: the dashboard only stores title/location/description/link
  for roles found via each company's own public page, and links back to the
  original posting for the actual application. It does not scrape or mirror
  LinkedIn.
