---
description: Reclaim disk by replacing base64 images in op session logs with a short text placeholder
---

# Strip session images

op saves every image it sends in the session log as base64, so image-heavy runs (spectrogram
or trail-cam queries) grow `~/.op_global/sessions` by GBs. op itself stays as it is: a fresh
session keeps its images, so you can branch off an example image. Strip images from old sessions
once they're no longer needed.

```
python3 -I skills/strip-session-images.py              # dry run: files, images, MB before -> after
python3 -I skills/strip-session-images.py --apply      # rewrite in place
python3 -I skills/strip-session-images.py DIR --apply  # another session tree
```

What it does:
- Each image block becomes a text block `[image deleted YYYY-MM-DD: image/png, 18 KB, sha256 <hex>]`.
  Sessions still load, list (`op --list messages`) and can be resumed or branched. The model just
  sees the placeholder text where the image used to be.
- Session logs never store the image's file name. The sha256 of the decoded bytes is
  what matches an image back to a file on disk (`sha256sum`).
- Lines without images are left byte for byte. Each file is rewritten atomically and keeps its mtime.
  A rerun does nothing.
- Don't run it while op is writing a session (`pgrep -x op`).

First run, 2026-11-07: 94 files, 14,207 images, 949 MB -> 46 MB.
