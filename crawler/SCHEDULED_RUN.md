# Scheduled run (Claude session, no GitHub Actions)

A Claude scheduled task runs the crawl Tuesday, Wednesday and Thursday at 1pm
UK time and republishes the page. Everything the session needs is fetched
from the published page itself: the crawler code lives under `code/` and the
previous crawl's data under `data/`. Nothing is read from or written to GitHub.

## What the session does

1. **Fetch** `index.html`, `data/*` and `code/*` from the published page.
2. **Install** (`python3 -m venv`, `pip install -r code/requirements.txt`).
   Chromium is already installed in the environment; `launch_chromium()` finds it.
3. **Crawl**, in the background, because it takes a few minutes:
   `python code/crawl.py --workers 4 --output <dir holding the previous data>`.
   Companies on Greenhouse / Lever / Ashby / Workable / Workday are read from the
   job board's public feed (complete, with posted dates); if a feed fails, or the
   company uses another system, its own careers page is rendered instead.
   Workday sites (`ats: workday` + `workday_url` in companies.yaml) are filtered
   to the UK with the site's own facet, then only plausible titles get a detail
   request (description, exact location, posting date).
   The previous `jobs.json` / `meta.json` are in that directory so roles with
   no posted date keep their "first seen" date between runs.
4. **Sanity check.** If more than half the companies failed to load, or the
   crawl wrote nothing, do NOT publish (it would wipe good data). Report why.
5. **Publish** the same `index.html` with the new `data/jobs.json` and
   `data/meta.json` to the same page URL.
6. **Report** roles posted in the last 2 days, then anything else new since
   the previous run.

## Updating the code or the page

The scheduled session only refreshes data. To change the crawler, filters,
company list or UI, edit the repo and run
`python crawler/build_artifact.py --dist frontend/dist --data <data dir> --out <dir>`,
then publish the printed `file_path` and `files` to the page. That also
refreshes the `code/` copy the scheduled session downloads.

## Needs

* A cloud environment with unrestricted network access (the crawler visits
  ~144 company sites and their job boards).
* The page is private to its owner; the scheduled session runs as that user.
