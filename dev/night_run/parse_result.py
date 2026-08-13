#!/usr/bin/env python3
"""Parse one Claude Code `--output-format stream-json` result envelope.

Prints four space-separated fields for the driver to read:

    <cost> <session_id> <cache_1h_tokens> <cache_5m_tokens>

The last two back the driver's billing tripwire, so this script must FAIL LOUDLY
rather than return zeros: a parse error that printed "0 0" would look exactly like
"this phase stayed on the subscription", which is the one wrong answer that
matters. On any failure it prints an ERR line and exits non-zero.
"""
import json
import sys


def main() -> int:
    if len(sys.argv) != 2:
        print("ERR usage: parse_result.py <result.json>", file=sys.stderr)
        return 2
    try:
        with open(sys.argv[1], encoding="utf-8") as fh:
            text = fh.read().strip()
        if not text:
            print("ERR empty result envelope", file=sys.stderr)
            return 3
        d = json.loads(text)
    except FileNotFoundError:
        print("ERR no result envelope written", file=sys.stderr)
        return 3
    except json.JSONDecodeError as e:
        print(f"ERR malformed result envelope: {e}", file=sys.stderr)
        return 3

    usage = d.get("usage") or {}
    cache = usage.get("cache_creation") or {}
    cost = d.get("total_cost_usd")
    print(
        f"{cost:.2f}" if isinstance(cost, (int, float)) else "?",
        d.get("session_id") or "?",
        int(cache.get("ephemeral_1h_input_tokens") or 0),
        int(cache.get("ephemeral_5m_input_tokens") or 0),
    )
    return 0


if __name__ == "__main__":
    sys.exit(main())
