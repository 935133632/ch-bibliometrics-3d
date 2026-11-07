"""Rebuild WoS plain-text files that contain only the core records (TI/AB/AK scope).

Reads the nine raw exports, keeps records whose UT is in data/clean/core_ut.txt and
rewrites them as <=500-record files for CiteSpace (download_XX.txt) and VOSviewer.
"""
import glob
import os
import re

root = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
core = set(open(os.path.join(root, "data/clean/core_ut.txt"), encoding="utf-8").read().split())

header = None
records = []
for f in sorted(glob.glob(os.path.join(root, "data/raw/wos_*.txt"))):
    txt = open(f, encoding="utf-8-sig").read().replace("\r\n", "\n")
    parts = txt.split("\nER\n")
    head, body = parts[0].split("\nPT ", 1)
    header = header or head + "\n"
    chunks = ["PT " + body] + ["PT " + p.split("\nPT ", 1)[1] if "\nPT " in p else p
                               for p in parts[1:-1]]
    # the split above re-attaches the PT tag; keep the first chunk as-is
    for c in [c for c in chunks if c.strip().startswith("PT ")]:
        m = re.search(r"^UT (\S+)", c, re.M)
        if m and m.group(1) in core:
            records.append(c.strip("\n"))

print("core records written:", len(records), "of", len(core))
out_dir = os.path.join(root, "citespace", "input")
for old in glob.glob(os.path.join(out_dir, "download_*.txt")):
    os.remove(old)
for i in range(0, len(records), 500):
    name = os.path.join(out_dir, "download_%02d.txt" % (i // 500 + 1))
    with open(name, "w", encoding="utf-8", newline="\n") as fh:
        fh.write(header + "\n".join(r + "\nER\n" for r in records[i:i + 500]) + "\nEF\n")
print(sorted(os.listdir(out_dir)))
