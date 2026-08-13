# Home energy tracker

A small local dashboard for your Octopus Energy electricity and gas
consumption: daily kWh charts, summary stats, and a table view — no
account/cloud service involved, everything runs on your own machine.

```
fetch_consumption.py   pulls daily consumption from the Octopus Energy
        │                REST API for your electricity and gas meters
        ▼
data/electricity.json   committed nowhere — .gitignore'd by default,
data/gas.json            since this is your real household usage data
        ▼
index.html + app.js     a static page (no build step) that reads those
                          JSON files and renders the charts/stats/table
```

## Setup

```bash
cd energy-tracker
python3 -m venv .venv
source .venv/bin/activate
pip install -r requirements.txt

cp .env.example .env
# edit .env and fill in your Octopus API key + meter details
```

Your `.env` holds your live API key — it is git-ignored and must never be
committed. Get your API key from
https://octopus.energy/dashboard/developer/ (starts with `sk_live_`).

## Fetch your data

```bash
python fetch_consumption.py
```

This writes `data/electricity.json` and `data/gas.json` (daily kWh totals).
Re-run it any time to pull the latest days — it always re-fetches your full
history, so the files stay in sync with your Octopus account.

## Run the dashboard

Browsers block `fetch()` against local files opened directly (`file://`),
so serve the folder over HTTP:

```bash
python3 -m http.server 8000
```

Then open http://localhost:8000 in your browser.

## Notes

- **Gas units:** most Octopus smart (SMETS2) meters report gas consumption
  in m³; the fetch script converts to kWh using the standard formula
  (volume correction factor × calorific value ÷ 3.6). If your meter already
  reports in kWh, set `GAS_UNITS=kwh` in `.env`.
- **Automating the fetch:** you can put `fetch_consumption.py` in a cron job
  to refresh `data/*.json` on a schedule — just make sure whatever runs it
  has the `.env` values available as environment variables.
- **Cost estimates** aren't included yet — Octopus's tariff/rates endpoints
  need your product & tariff code, which isn't in the consumption API. Ask
  if you'd like that added.
