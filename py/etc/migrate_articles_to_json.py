"""
Convert article .json files from Python-dict-repr format to real JSON.

Historically scrape_releases.py wrote articles with `file.write(str(article))`,
producing files that use single quotes and Python escape sequences. This script
rewrites those files as proper JSON, in place.

It is idempotent: files that already parse as JSON are skipped. Safe to re-run.
"""

import argparse
import ast
import json
import os
import sys
from multiprocessing import Pool
from pathlib import Path


def migrate_file(path: str) -> tuple[str, str]:
    """Return (path, status) where status is 'skipped', 'migrated', or 'failed: <reason>'."""
    try:
        with open(path, "r", encoding="utf-8") as f:
            content = f.read()
    except Exception as e:
        return path, f"failed: read error: {e}"

    if not content.strip():
        return path, "failed: empty file"

    try:
        json.loads(content)
        return path, "skipped"
    except json.JSONDecodeError:
        pass

    try:
        data = ast.literal_eval(content)
    except (ValueError, SyntaxError, MemoryError) as e:
        return path, f"failed: literal_eval: {e.__class__.__name__}"

    if not isinstance(data, dict):
        return path, f"failed: parsed type {type(data).__name__}, expected dict"

    tmp_path = path + ".tmp"
    try:
        with open(tmp_path, "w", encoding="utf-8") as f:
            json.dump(data, f, ensure_ascii=False)
        os.replace(tmp_path, path)
    except Exception as e:
        if os.path.exists(tmp_path):
            try:
                os.remove(tmp_path)
            except OSError:
                pass
        return path, f"failed: write error: {e}"

    return path, "migrated"


def iter_files(root: str):
    for dirpath, _, files in os.walk(root):
        for name in files:
            if name.endswith(".json"):
                yield os.path.join(dirpath, name)


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument(
        "--root",
        default="data/articles",
        help="Root directory to walk (default: data/articles)",
    )
    parser.add_argument(
        "--workers",
        type=int,
        default=max(1, (os.cpu_count() or 1) - 1),
        help="Number of parallel worker processes",
    )
    parser.add_argument(
        "--dry-run",
        action="store_true",
        help="Scan and classify files but do not write anything",
    )
    parser.add_argument(
        "--limit",
        type=int,
        default=None,
        help="Process only the first N files (for pilot runs)",
    )
    parser.add_argument(
        "--fail-log",
        default="migrate_articles_failures.log",
        help="Path to write failures (one line per failed file)",
    )
    parser.add_argument(
        "--progress-every",
        type=int,
        default=5000,
        help="Print a progress line every N files",
    )
    args = parser.parse_args()

    root = Path(args.root)
    if not root.is_dir():
        print(f"error: {root} is not a directory", file=sys.stderr)
        sys.exit(2)

    files_iter = iter_files(str(root))
    if args.limit is not None:
        files_iter = (p for i, p in enumerate(files_iter) if i < args.limit)

    counts = {"skipped": 0, "migrated": 0, "failed": 0}
    failures: list[tuple[str, str]] = []

    def classify(status: str) -> str:
        if status == "skipped":
            return "skipped"
        if status == "migrated":
            return "migrated"
        return "failed"

    worker = _dry_run_file if args.dry_run else migrate_file

    processed = 0
    if args.workers <= 1:
        for path in files_iter:
            p, status = worker(path)
            counts[classify(status)] += 1
            if status.startswith("failed"):
                failures.append((p, status))
            processed += 1
            if processed % args.progress_every == 0:
                print(f"  processed {processed:,} files ({counts})", flush=True)
    else:
        with Pool(args.workers) as pool:
            for p, status in pool.imap_unordered(worker, files_iter, chunksize=64):
                counts[classify(status)] += 1
                if status.startswith("failed"):
                    failures.append((p, status))
                processed += 1
                if processed % args.progress_every == 0:
                    print(f"  processed {processed:,} files ({counts})", flush=True)

    print(f"done. processed {processed:,} files. {counts}")

    if failures:
        with open(args.fail_log, "w", encoding="utf-8") as f:
            for p, status in failures:
                f.write(f"{p}\t{status}\n")
        print(f"wrote {len(failures):,} failures to {args.fail_log}")


def _dry_run_file(path: str) -> tuple[str, str]:
    """Classify without writing. Mirrors migrate_file's decision logic."""
    try:
        with open(path, "r", encoding="utf-8") as f:
            content = f.read()
    except Exception as e:
        return path, f"failed: read error: {e}"

    if not content.strip():
        return path, "failed: empty file"

    try:
        json.loads(content)
        return path, "skipped"
    except json.JSONDecodeError:
        pass

    try:
        data = ast.literal_eval(content)
    except (ValueError, SyntaxError, MemoryError) as e:
        return path, f"failed: literal_eval: {e.__class__.__name__}"

    if not isinstance(data, dict):
        return path, f"failed: parsed type {type(data).__name__}, expected dict"

    return path, "migrated"


if __name__ == "__main__":
    main()
