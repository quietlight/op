#!/usr/bin/env python3
"""Strip base64 images out of op session logs (~/.op_global/sessions/**/*.jsonl).

Each image block {"type":"image","mimeType":..,"data":<base64>} becomes a text block
"[image deleted YYYY-MM-DD: image/png, 18 KB, sha256 <hex of the image bytes>]".
It becomes a text block, not an image with dummy data, so resumed or branched sessions still replay.
Session files never store an image's file name; the sha256 is what matches an image back to a file on disk.
Lines without images are kept byte for byte; each file is rewritten atomically and keeps its mtime.
Already-stripped images are skipped, so a rerun is a no-op. Dry run unless --apply.
Do not run while an op session is writing.

    python3 -I skills/strip-session-images.py [DIR] [--apply]     # DIR defaults to ~/.op_global/sessions
"""
import base64, datetime, hashlib, json, os, sys

args = [a for a in sys.argv[1:] if not a.startswith("--")]
root = os.path.expanduser(args[0] if args else "~/.op_global/sessions")
apply = "--apply" in sys.argv
today = datetime.date.today().isoformat()
n_files = n_imgs = before = after = 0

def size_str(n):
    return f"{n // 2**20} MB" if n >= 2**20 else f"{n // 1024} KB" if n >= 1024 else f"{n} B"

for d, _, fs in os.walk(root):
    for f in fs:
        if not f.endswith(".jsonl"):
            continue
        p = os.path.join(d, f)
        raw = open(p, "rb").read()
        out, hits = [], 0
        for line in raw.splitlines(keepends=True):
            if b'"image"' not in line:
                out.append(line)
                continue
            o = json.loads(line)
            c = o.get("message", {}).get("content") if isinstance(o, dict) else None
            changed = False
            if isinstance(c, list):
                for i, b in enumerate(c):
                    if isinstance(b, dict) and b.get("type") == "image" and isinstance(b.get("data"), str):
                        try:
                            img = base64.b64decode(b["data"], validate=False)
                            size, h = len(img), hashlib.sha256(img).hexdigest()
                        except Exception:
                            size, h = len(b["data"]), "undecodable"
                        c[i] = {"type": "text",
                                "text": f"[image deleted {today}: {b.get('mimeType', '?')}, {size_str(size)}, sha256 {h}]"}
                        changed, hits = True, hits + 1
            if changed:
                nl = b"\n" if line.endswith(b"\n") else b""
                out.append(json.dumps(o, ensure_ascii=False, separators=(",", ":")).encode() + nl)
            else:
                out.append(line)
        if not hits:
            continue
        new = b"".join(out)
        for l in new.splitlines():
            json.loads(l)  # every line must still parse
        n_files, n_imgs, before, after = n_files + 1, n_imgs + hits, before + len(raw), after + len(new)
        if apply:
            st = os.stat(p)
            tmp = p + ".strip-tmp"
            with open(tmp, "wb") as fh:
                fh.write(new)
            os.chmod(tmp, st.st_mode)
            os.replace(tmp, p)
            os.utime(p, (st.st_atime, st.st_mtime))

print(f"{'APPLIED' if apply else 'DRY RUN (add --apply)'}: {n_files} files, {n_imgs} images, "
      f"{before / 2**20:.0f} MB -> {after / 2**20:.0f} MB")
