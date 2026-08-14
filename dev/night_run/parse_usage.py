#!/usr/bin/env python3
"""Parse `claude -p "/usage"` (JSON on stdin) into machine-readable window state.

Prints one line:

    <session_pct> <week_pct> <session_reset_epoch> <week_reset_epoch> <reset_human>

`/usage` is handled locally by the CLI — zero tokens, zero turns — so the driver
can call it before every phase for free. It is the only real "should I start
another phase now" signal: the 5-hour session window and the weekly cap.

Fails loudly (non-zero) rather than guessing. A driver that cannot read the
window must not silently assume there is room.
"""
import json
import re
import sys
from datetime import datetime

# "Current session: 3% used · resets Aug 14, 11:50am (Europe/Paris)"
SESSION = re.compile(r"Current session:\s*(\d+)%\s*used(?:\s*·\s*resets\s*([^(\n]+))?", re.I)
WEEK = re.compile(r"Current week \(all models\):\s*(\d+)%\s*used(?:\s*·\s*resets\s*([^(\n]+))?", re.I)


def to_epoch(s: str) -> int:
    """'Aug 14, 11:50am' -> epoch. 0 if unparseable (caller treats as unknown).

    WARNING: strptime defaults a yearless date to 1900, so "does this format carry
    a date?" cannot be answered by inspecting the result — a parsed "Aug 18" and a
    parsed bare "12pm" both come back as year 1900. Testing caught this returning
    Aug 14 for a week that resets Aug 18. The format string is the only reliable
    discriminator, so it is carried alongside.
    """
    if not s:
        return 0
    s = s.strip().rstrip("·").strip()
    now = datetime.now()
    # (format, carries a month/day)
    for fmt, dated in (("%b %d, %I:%M%p", True), ("%b %d, %I%p", True),
                       ("%b %d %I:%M%p", True), ("%I:%M%p", False), ("%I%p", False)):
        try:
            # Supply the year explicitly: a yearless date is ambiguous and Python
            # 3.15 will stop accepting it.
            t = (datetime.strptime(f"{s} {now.year}", f"{fmt} %Y") if dated
                 else datetime.strptime(s, fmt).replace(
                     year=now.year, month=now.month, day=now.day))
        except ValueError:
            continue
        ts = t.timestamp()
        if ts < now.timestamp():
            # Already past: a time-only reset means tomorrow; a dated one means
            # the string wrapped into next year.
            t = (t.replace(year=now.year + 1) if dated
                 else datetime.fromtimestamp(ts + 86400))
            ts = t.timestamp()
        return int(ts)
    return 0


def main() -> int:
    raw = sys.stdin.read().strip()
    if not raw:
        print("ERR empty /usage output", file=sys.stderr)
        return 3
    text = raw
    if raw.startswith("{"):
        try:
            text = json.loads(raw).get("result") or ""
        except json.JSONDecodeError as e:
            print(f"ERR malformed /usage envelope: {e}", file=sys.stderr)
            return 3

    ms, mw = SESSION.search(text), WEEK.search(text)
    if not ms:
        print("ERR no 'Current session' line in /usage output", file=sys.stderr)
        return 3

    s_pct, s_reset = int(ms.group(1)), (ms.group(2) or "").strip()
    w_pct = int(mw.group(1)) if mw else 0
    w_reset = (mw.group(2) or "").strip() if mw else ""
    print(s_pct, w_pct, to_epoch(s_reset), to_epoch(w_reset), s_reset or "unknown")
    return 0


if __name__ == "__main__":
    sys.exit(main())
