# London Software Jobs Dashboard

A dashboard that surfaces software-engineering roles based in **London, UK**
that were posted in the **last 24 hours**, at a curated set of UK software
product companies. Crawls run automatically **twice on every weekday**.

> This repository started life as a Coursera R programming assignment. That
> original content is preserved at
> [`docs/original-coursera-assignment.md`](docs/original-coursera-assignment.md)
> and [`cachematrix.R`](cachematrix.R); everything below is a separate project
> built on top of it.

## How it works

```
crawler/companies.yaml   curated list of UK software product companies
        │                 (name, website, ATS provider + board token)
        ▼
crawler/crawl.py          for each company, calls that company's own public
        │                 job-board API (Greenhouse / Lever / Ashby) — the
        │                 same feed that powers their own "Careers" page —
        │                 keeps roles located in London, UK and posted
        │                 within the last 24 hours
        ▼
frontend/public/data/     jobs.json + meta.json (committed to the repo)
        │
        ▼
frontend/ (React + Vite)  reads that JSON and renders the dashboard:
                           stat tiles, a jobs-by-company chart, filters,
                           and a card per role linking to the original
                           posting on the company's own site
```

`.github/workflows/crawl.yml` runs the crawler on a schedule
(`0 7,13 * * 1-5` UTC — twice every weekday) and commits the refreshed
`jobs.json`/`meta.json`. `.github/workflows/deploy.yml` then rebuilds and
publishes the frontend to GitHub Pages. You can also trigger either workflow
manually from the Actions tab (`workflow_dispatch`).

### Why company ATS feeds instead of LinkedIn / a scraper aggregator

Each of Greenhouse, Lever, and Ashby exposes a public, unauthenticated JSON
API that is the actual data source for the "Careers" page embedded on the
company's own website — no login, no credential automation, no bypassing of
access controls, and no violation of LinkedIn's anti-scraping terms. It's
also far more reliable than HTML scraping: structured fields for title,
location, posted date, full description, and a canonical application URL.

The tradeoff: coverage is limited to the companies listed in
[`crawler/companies.yaml`](crawler/companies.yaml), and only to companies
using one of the supported ATS providers (Greenhouse, Lever, Ashby, plus a
Workable client that's implemented but unused by default). Add a company by
finding its board token (the slug in `boards.greenhouse.io/<token>`,
`jobs.lever.co/<token>`, or `jobs.ashbyhq.com/<token>`) and adding an entry to
`companies.yaml`.

### A known limitation, by design

Greenhouse's public feed only exposes `updated_at`, not a true "posted"
timestamp — a role that was edited stays looking "fresh" even if it was
originally posted earlier. Lever and Ashby both expose true creation/publish
timestamps, so this only affects Greenhouse-listed companies. This is a
limitation of the public (unauthenticated) feed, not of the crawler.

## Repository layout

```
crawler/
  companies.yaml       curated company list (edit this to add/remove companies)
  ats_clients.py        Greenhouse / Lever / Ashby / Workable API clients
  crawl.py               orchestrates fetch → filter → write JSON
  requirements.txt
frontend/
  src/                   React + TypeScript dashboard
  public/data/           jobs.json / meta.json (crawler output, committed)
.github/workflows/
  crawl.yml              scheduled crawl (twice/weekday) + manual trigger
  deploy.yml              build + publish frontend to GitHub Pages
```

## Running the crawler locally

```bash
cd crawler
python3 -m venv .venv && source .venv/bin/activate
pip install -r requirements.txt
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

- **First run**: I could not reach the internet from the sandboxed session
  that built this (its outbound network is restricted to a small allowlist —
  package registries, not job-board APIs), so `frontend/public/data/jobs.json`
  starts out empty. Trigger `crawl.yml` manually from the Actions tab (or wait
  for the next scheduled run) once this is pushed to populate real data —
  GitHub's own runners have normal internet access.
- **Company list is a curated seed**, verified via web search on 2026-07-17
  against each company's real public job-board URL. ATS providers and board
  tokens can change; re-verify periodically and prune/extend
  `companies.yaml` as needed.
- **Attribution**: the dashboard only stores title/location/description/link
  for roles found via each company's own public feed, and links back to the
  original posting for the actual application. It does not scrape or mirror
  LinkedIn.
