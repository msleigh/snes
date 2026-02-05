#!/usr/bin/env python3
"""Compare numerical values in files with a relative tolerance."""

import sys


def parse_numbers(filepath, column=None):
    """Extract numbers from a file, optionally from a specific column."""
    numbers = []
    with open(filepath) as f:
        for line in f:
            line = line.strip()
            if not line or line.startswith("#"):
                continue
            parts = line.split()
            try:
                if column is not None:
                    val = float(parts[column])
                else:
                    val = float(parts[0]) if len(parts) == 1 else None
                    if val is None:
                        continue
                numbers.append(val)
            except (ValueError, IndexError):
                continue
    return numbers


def compare_numbers(actual, expected, rel_tol):
    """Compare two lists of numbers with relative tolerance."""
    if len(actual) != len(expected):
        return False, f"Length mismatch: {len(actual)} vs {len(expected)}"

    failures = []
    for i, (a, e) in enumerate(zip(actual, expected)):
        if e == 0:
            rel_diff = abs(a) if a != 0 else 0
        else:
            rel_diff = abs(a - e) / abs(e)

        if rel_diff > rel_tol:
            failures.append(f"  Line {i + 1}: {a} vs {e} (rel diff: {rel_diff:.2e})")

    if failures:
        return False, "Values differ beyond tolerance:\n" + "\n".join(failures)
    return True, None


def main():
    if len(sys.argv) < 4:
        print(f"Usage: {sys.argv[0]} <actual> <expected> <rel_tol> [column]")
        sys.exit(1)

    actual_file = sys.argv[1]
    expected_file = sys.argv[2]
    rel_tol = float(sys.argv[3])
    column = int(sys.argv[4]) if len(sys.argv) > 4 else None

    actual = parse_numbers(actual_file, column)
    expected = parse_numbers(expected_file, column)

    ok, msg = compare_numbers(actual, expected, rel_tol)
    if not ok:
        print(f"FAIL: {actual_file} vs {expected_file}")
        print(msg)
        sys.exit(1)


if __name__ == "__main__":
    main()
