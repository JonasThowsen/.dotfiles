# Token usage collector for the "tokens" mod. Run with python3 -I -c.
# Prints JSON: {"rows": [[day, tool, model, project, input, output, cache_read, cache_write], ...]}
# Days are local dates (YYYY-MM-DD). Reads Claude Code, Codex and opencode logs.
import glob, json, os, sqlite3
from collections import defaultdict
from datetime import datetime, timezone

HOME = os.path.expanduser("~")
agg = defaultdict(lambda: [0, 0, 0, 0])


def day_of_iso(ts):
    try:
        return datetime.fromisoformat(ts.replace("Z", "+00:00")).astimezone().strftime("%Y-%m-%d")
    except Exception:
        return None


def day_of_ms(ms):
    return datetime.fromtimestamp(ms / 1000, timezone.utc).astimezone().strftime("%Y-%m-%d")


def project_of(cwd):
    if not cwd:
        return "?"
    cwd = cwd.rstrip("/")
    return "~" if cwd == HOME else os.path.basename(cwd)


def add(day, tool, model, project, i, o, cr, cw):
    if not day or not (i or o or cr or cw):
        return
    a = agg[(day, tool, model or "?", project)]
    a[0] += i; a[1] += o; a[2] += cr; a[3] += cw


def claude():
    seen = set()
    roots = [os.path.join(HOME, ".claude", "projects"), os.path.join(HOME, ".config", "claude", "projects")]
    for root in roots:
        for path in glob.glob(os.path.join(root, "**", "*.jsonl"), recursive=True):
            try:
                f = open(path, encoding="utf-8", errors="replace")
            except OSError:
                continue
            with f:
                for line in f:
                    if '"usage"' not in line:
                        continue
                    try:
                        e = json.loads(line)
                    except ValueError:
                        continue
                    m = e.get("message") or {}
                    u = m.get("usage")
                    if not isinstance(u, dict):
                        continue
                    key = (m.get("id"), e.get("requestId"))
                    if key[0] and key in seen:
                        continue
                    seen.add(key)
                    add(day_of_iso(e.get("timestamp", "")), "Claude Code", m.get("model"), project_of(e.get("cwd")),
                        u.get("input_tokens", 0) or 0, u.get("output_tokens", 0) or 0,
                        u.get("cache_read_input_tokens", 0) or 0, u.get("cache_creation_input_tokens", 0) or 0)


def codex():
    base = os.environ.get("CODEX_HOME") or os.path.join(HOME, ".codex")
    for path in glob.glob(os.path.join(base, "sessions", "**", "*.jsonl"), recursive=True) + \
            glob.glob(os.path.join(base, "archived_sessions", "*.jsonl")):
        cwd, model, prev = None, None, None
        try:
            f = open(path, encoding="utf-8", errors="replace")
        except OSError:
            continue
        with f:
            for line in f:
                if '"session_meta"' in line or '"turn_context"' in line:
                    try:
                        p = json.loads(line).get("payload") or {}
                    except ValueError:
                        continue
                    cwd = p.get("cwd") or cwd
                    model = p.get("model") or model
                    continue
                if '"token_count"' not in line or '"total_token_usage"' not in line:
                    continue
                try:
                    e = json.loads(line)
                except ValueError:
                    continue
                t = ((e.get("payload") or {}).get("info") or {}).get("total_token_usage")
                if not t:
                    continue
                cur = (t.get("input_tokens", 0), t.get("cached_input_tokens", 0), t.get("output_tokens", 0))
                if prev is None:
                    d = cur
                else:
                    d = tuple(max(0, c - p) for c, p in zip(cur, prev))
                prev = cur
                inp, cached, out = d
                add(day_of_iso(e.get("timestamp", "")), "Codex", model, project_of(cwd),
                    max(0, inp - cached), out, cached, 0)


def opencode():
    db = os.path.join(os.environ.get("XDG_DATA_HOME") or os.path.join(HOME, ".local", "share"), "opencode", "opencode.db")
    if not os.path.exists(db):
        return
    try:
        c = sqlite3.connect("file:" + db + "?mode=ro", uri=True, timeout=2)
        rows = c.execute("select time_created, data from message where data like '%\"tokens\"%'")
        for created, data in rows:
            try:
                m = json.loads(data)
            except ValueError:
                continue
            t = m.get("tokens") or {}
            cache = t.get("cache") or {}
            add(day_of_ms(created), "opencode", m.get("modelID"), project_of((m.get("path") or {}).get("cwd")),
                t.get("input", 0) or 0, (t.get("output", 0) or 0) + (t.get("reasoning", 0) or 0),
                cache.get("read", 0) or 0, cache.get("write", 0) or 0)
    except sqlite3.Error:
        pass


for fn in (claude, codex, opencode):
    try:
        fn()
    except Exception:
        pass

print(json.dumps({"rows": [[*k, *v] for k, v in agg.items()]}))
