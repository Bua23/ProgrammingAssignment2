"""Location targeting: London, South East England, or UK-based remote.

classify_location("Reading, England, United Kingdom") -> "south_east"
classify_location("Remote - UK")                      -> "remote_uk"
classify_location("London, UK")                       -> "london"
classify_location("New York, NY")                     -> None

Priority when several apply: london > south_east > remote_uk.

"South East" is the official English region: Berkshire, Buckinghamshire,
East Sussex, Hampshire, Isle of Wight, Kent, Oxfordshire, Surrey, West
Sussex (Hertfordshire, Essex and Cambridgeshire are East of England and
are NOT included). "Remote" only counts when the text also names the UK;
a bare "Remote" or "Remote - EMEA" is ambiguous and is not matched.
Edit the lists below to change any of this.
"""
from __future__ import annotations

import re

NON_UK_LONDON = re.compile(
    r"ontario|canada|,\s*on\b|kentucky|\bky\b|ohio|\boh\b|south africa|texas|\btx\b|california", re.I
)

UK_SIGNAL = re.compile(r"\b(uk|u\.k\.|united kingdom|england|great britain|britain|gbr)\b", re.I)

# Names that only ever mean the English county / region.
SOUTH_EAST_UNAMBIGUOUS = re.compile(
    r"\b(surrey|sussex|hampshire|berkshire|buckinghamshire|oxfordshire|isle of wight|"
    r"south[- ]east england|south[- ]east uk|south east, (?:uk|england)|"
    r"guildford|woking|crawley|horsham|chichester|basingstoke|bracknell|maidenhead|"
    r"farnborough|camberley|high wycombe|newbury|tunbridge wells|sevenoaks|farnham|"
    r"godalming|aldershot|eastleigh|fareham|worthing|eastbourne|egham|weybridge|"
    r"leatherhead|redhill|reigate|dorking|gatwick|aylesbury|bicester|abingdon|didcot|"
    r"harwell|milton keynes|maidstone|canterbury|winchester|southampton|portsmouth|"
    r"epsom|staines)\b",
    re.I,
)

# Names shared with towns elsewhere (Reading PA, Oxford MS, Windsor ON, ...):
# only counted when the text also carries a UK signal.
SOUTH_EAST_AMBIGUOUS = re.compile(
    r"\b(reading|oxford|windsor|kent|brighton|hove|slough|ashford|thatcham)\b", re.I
)

REMOTE = re.compile(r"\bremote\b", re.I)


def classify_location(text: str | None) -> str | None:
    if not text:
        return None
    t = text.strip()
    if re.search(r"\blondon\b", t, re.I) and not NON_UK_LONDON.search(t):
        return "london"
    uk = bool(UK_SIGNAL.search(t))
    if SOUTH_EAST_UNAMBIGUOUS.search(t) or (uk and SOUTH_EAST_AMBIGUOUS.search(t)):
        return "south_east"
    if uk and REMOTE.search(t):
        return "remote_uk"
    return None


LOCATION_LABELS = {"london": "London", "south_east": "South East", "remote_uk": "Remote (UK)"}
