#!/usr/bin/env python3
"""Fetch daily electricity and gas consumption from the Octopus Energy API
and write it to data/electricity.json and data/gas.json for the dashboard.

Credentials are read from environment variables (see .env.example) — never
hardcode the API key here.
"""

import json
import os
import sys
from pathlib import Path

import requests
from dotenv import load_dotenv

BASE_URL = "https://api.octopus.energy/v1"
DATA_DIR = Path(__file__).parent / "data"

# Standard SMETS2 gas conversion constants (Ofgem), used because Octopus's
# gas consumption API reports volume in m3 for most smart meters.
VOLUME_CORRECTION_FACTOR = 1.02264
CALORIFIC_VALUE_MJ_PER_M3 = 40.0  # a reasonable UK national-average estimate
KWH_PER_M3 = VOLUME_CORRECTION_FACTOR * CALORIFIC_VALUE_MJ_PER_M3 / 3.6


def fetch_all_pages(url, api_key):
    results = []
    while url:
        resp = requests.get(url, auth=(api_key, ""), timeout=30)
        if resp.status_code == 401:
            sys.exit("Octopus API rejected the key (401 Unauthorized) — check OCTOPUS_API_KEY.")
        if resp.status_code == 404:
            sys.exit(f"404 from Octopus API for {url} — check your MPAN/MPRN/serial numbers.")
        resp.raise_for_status()
        payload = resp.json()
        results.extend(payload["results"])
        url = payload.get("next")
    return results


def fetch_electricity(api_key, mpan, serial):
    url = (
        f"{BASE_URL}/electricity-meter-points/{mpan}/meters/{serial}/consumption/"
        "?page_size=25000&order_by=period&group_by=day"
    )
    rows = fetch_all_pages(url, api_key)
    rows.sort(key=lambda r: r["interval_start"])
    return [
        {
            "date": r["interval_start"][:10],
            "interval_start": r["interval_start"],
            "interval_end": r["interval_end"],
            "kwh": round(r["consumption"], 3),
        }
        for r in rows
    ]


def fetch_gas(api_key, mprn, serial):
    url = (
        f"{BASE_URL}/gas-meter-points/{mprn}/meters/{serial}/consumption/"
        "?page_size=25000&order_by=period&group_by=day"
    )
    rows = fetch_all_pages(url, api_key)
    rows.sort(key=lambda r: r["interval_start"])
    out = []
    for r in rows:
        raw = r["consumption"]
        # Most SMETS2 gas meters report volume in m3; convert to kWh.
        # If your meter already reports in kWh, set GAS_UNITS=kwh in .env.
        kwh = raw if os.environ.get("GAS_UNITS", "m3").lower() == "kwh" else raw * KWH_PER_M3
        out.append(
            {
                "date": r["interval_start"][:10],
                "interval_start": r["interval_start"],
                "interval_end": r["interval_end"],
                "raw": raw,
                "kwh": round(kwh, 3),
            }
        )
    return out


def main():
    load_dotenv()

    api_key = os.environ.get("OCTOPUS_API_KEY")
    mpan = os.environ.get("ELECTRICITY_MPAN")
    elec_serial = os.environ.get("ELECTRICITY_SERIAL")
    mprn = os.environ.get("GAS_MPRN")
    gas_serial = os.environ.get("GAS_SERIAL")

    missing = [
        name
        for name, val in [
            ("OCTOPUS_API_KEY", api_key),
            ("ELECTRICITY_MPAN", mpan),
            ("ELECTRICITY_SERIAL", elec_serial),
            ("GAS_MPRN", mprn),
            ("GAS_SERIAL", gas_serial),
        ]
        if not val
    ]
    if missing:
        sys.exit(
            "Missing required environment variables: "
            + ", ".join(missing)
            + "\nCopy .env.example to .env and fill in your details."
        )

    DATA_DIR.mkdir(exist_ok=True)

    print("Fetching electricity consumption...")
    electricity = fetch_electricity(api_key, mpan, elec_serial)
    (DATA_DIR / "electricity.json").write_text(json.dumps(electricity, indent=2))
    print(f"  wrote {len(electricity)} daily readings to data/electricity.json")

    print("Fetching gas consumption...")
    gas = fetch_gas(api_key, mprn, gas_serial)
    (DATA_DIR / "gas.json").write_text(json.dumps(gas, indent=2))
    print(f"  wrote {len(gas)} daily readings to data/gas.json")


if __name__ == "__main__":
    main()
