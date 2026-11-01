"""Title-based role filter driven by roles.yaml.

    rf = RoleFilter.from_yaml(Path("roles.yaml"))
    rf.match("Senior Manager, AI Governance")   # -> ["ai_governance", "ai_management"]
    rf.match("Senior AI Engineer")               # -> []

Only the first line of the title is considered: DOM-fallback titles are the
raw text of a link and often carry the location / team on later lines.
"""
from __future__ import annotations

import re
from dataclasses import dataclass, field
from pathlib import Path

import yaml


def _compile_any(patterns: list[str]) -> re.Pattern:
    return re.compile("|".join(f"(?:{p})" for p in patterns), re.IGNORECASE)


@dataclass
class Role:
    key: str
    label: str
    all_of: list[re.Pattern] = field(default_factory=list)
    standalone: re.Pattern | None = None

    def matches(self, title: str, ai: re.Pattern, mgmt: re.Pattern) -> bool:
        if self.standalone and self.standalone.search(title):
            return True
        if not self.all_of:
            return False
        # Sentinel patterns stand in for the shared "ai"/"management" groups.
        for group in self.all_of:
            if group.pattern == "__ai__":
                group = ai
            elif group.pattern == "__management__":
                group = mgmt
            if not group.search(title):
                return False
        return True


class RoleFilter:
    def __init__(self, config: dict):
        self.ai = _compile_any(config["ai_terms"])
        self.mgmt = _compile_any(config["management_terms"])
        self.strong_mgmt = _compile_any(config["strong_management_terms"])
        self.ic_exclude = _compile_any(config["ic_exclude"])
        self.exclude_titles = _compile_any(config["exclude_titles"])
        self.roles: list[Role] = []
        for r in config["roles"]:
            all_of = []
            for group in r.get("all_of", []):
                if group == "ai":
                    all_of.append(re.compile("__ai__"))
                elif group == "management":
                    all_of.append(re.compile("__management__"))
                else:
                    all_of.append(_compile_any(group))
            standalone = _compile_any(r["standalone"]) if r.get("standalone") else None
            self.roles.append(Role(r["key"], r["label"], all_of, standalone))

    @classmethod
    def from_yaml(cls, path: Path) -> "RoleFilter":
        with open(path) as f:
            return cls(yaml.safe_load(f))

    @property
    def labels(self) -> dict[str, str]:
        return {r.key: r.label for r in self.roles}

    def match(self, title: str) -> list[str]:
        first_line = (title or "").strip().splitlines()[0] if (title or "").strip() else ""
        if not first_line:
            return []
        if self.exclude_titles.search(first_line):
            return []
        if self.ic_exclude.search(first_line) and not self.strong_mgmt.search(first_line):
            return []
        return [r.key for r in self.roles if r.matches(first_line, self.ai, self.mgmt)]
