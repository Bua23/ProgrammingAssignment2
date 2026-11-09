#!/usr/bin/env python3
"""Assemble the publishable page: one self-contained index.html (JS and CSS
inlined from the Vite build), the crawl data under data/, and a copy of the
crawler code under code/ so a scheduled session can fetch everything it needs
from the published page alone.

    python build_artifact.py --dist ../frontend/dist --data <dir with jobs.json + meta.json> --out <empty dir>

Prints the `files` mapping to pass to the Artifact tool.
"""
from __future__ import annotations

import argparse
import glob
import json
import shutil
from pathlib import Path

HERE = Path(__file__).resolve().parent
CODE_FILES = [
    "crawl.py", "browser_crawl.py", "ats_clients.py", "workday_client.py", "role_filter.py", "job_filters.py",
    "locations.py", "jobposting.py", "roles.yaml", "companies.yaml", "requirements.txt",
]
TITLE = "London AI Roles"


def main() -> None:
    ap = argparse.ArgumentParser(description=__doc__)
    ap.add_argument("--dist", type=Path, required=True)
    ap.add_argument("--data", type=Path, required=True)
    ap.add_argument("--out", type=Path, required=True)
    args = ap.parse_args()

    js = Path(glob.glob(str(args.dist / "assets" / "*.js"))[0]).read_text().replace("</script", "<\\/script")
    css = Path(glob.glob(str(args.dist / "assets" / "*.css"))[0]).read_text()

    args.out.mkdir(parents=True, exist_ok=True)
    (args.out / "index.html").write_text(
        f'<title>{TITLE}</title>\n<style>{css}</style>\n<div id="root"></div>\n<script type="module">{js}</script>\n'
    )
    files = {}
    (args.out / "data").mkdir(exist_ok=True)
    for name in ("jobs.json", "meta.json"):
        shutil.copy(args.data / name, args.out / "data" / name)
        files[f"data/{name}"] = str(args.out / "data" / name)
    (args.out / "code").mkdir(exist_ok=True)
    for name in CODE_FILES:
        shutil.copy(HERE / name, args.out / "code" / name)
        # .py/.yaml are not served types; publish them as plain text.
        files[f"code/{name}"] = {"from": str(args.out / "code" / name), "contentType": "text/plain"}
    print(json.dumps({"file_path": str(args.out / "index.html"), "files": files}, indent=2))


if __name__ == "__main__":
    main()
