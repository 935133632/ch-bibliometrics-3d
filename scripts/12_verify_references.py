"""Check every manuscript reference against Crossref (public API, no personal data sent).

For each reference we send the citation text, take the best match, and compare year,
volume and first page. Output: manuscript/reference_check.csv
"""
import csv
import json
import re
import urllib.parse
import urllib.request

REFS = [
    ("Aria 2017 bibliometrix", "Aria M, Cuccurullo C. bibliometrix: An R-tool for comprehensive science mapping analysis. Journal of Informetrics", 2017, "11", "959"),
    ("Bastian 2009 Gephi", "Bastian M, Heymann S, Jacomy M. Gephi: an open source software for exploring and manipulating networks. ICWSM", 2009, "3", "361"),
    ("Bick 2020", "Bick AG Inherited causes of clonal haematopoiesis in 97,691 whole genomes. Nature", 2020, "586", "763"),
    ("Blondel 2008", "Blondel Guillaume Lambiotte Lefebvre Fast unfolding of communities in large networks. Journal of Statistical Mechanics", 2008, "2008", "P10008"),
    ("Bolton 2020", "Bolton KL Cancer therapy shapes the fitness landscape of clonal hematopoiesis. Nature Genetics", 2020, "52", "1219"),
    ("Coombs 2017", "Coombs CC Therapy-related clonal hematopoiesis in patients with non-hematologic cancers is common and associated with adverse clinical outcomes. Cell Stem Cell", 2017, "21", "374"),
    ("Fuster 2017", "Fuster JJ Clonal hematopoiesis associated with TET2 deficiency accelerates atherosclerosis development in mice. Science", 2017, "355", "842"),
    ("Genovese 2014", "Genovese G Clonal hematopoiesis and blood-cancer risk inferred from blood DNA sequence. New England Journal of Medicine", 2014, "371", "2477"),
    ("Jaiswal & Ebert 2019", "Jaiswal S, Ebert BL. Clonal hematopoiesis in human aging and disease. Science", 2019, "366", "eaan4673"),
    ("Jaiswal 2014", "Jaiswal S Age-related clonal hematopoiesis associated with adverse outcomes. New England Journal of Medicine", 2014, "371", "2488"),
    ("Jaiswal 2017", "Jaiswal S Clonal hematopoiesis and risk of atherosclerotic cardiovascular disease. New England Journal of Medicine", 2017, "377", "111"),
    ("Kleinberg 2003", "Kleinberg J. Bursty and hierarchical structure in streams. Data Mining and Knowledge Discovery", 2003, "7", "373"),
    ("Sano 2018", "Sano S Tet2-mediated clonal hematopoiesis accelerates heart failure through a mechanism involving the IL-1 beta/NLRP3 inflammasome. Journal of the American College of Cardiology", 2018, "71", "875"),
    ("Steensma 2015", "Steensma DP Clonal hematopoiesis of indeterminate potential and its distinction from myelodysplastic syndromes. Blood", 2015, "126", "9"),
    ("van Eck 2010", "van Eck NJ, Waltman L. Software survey: VOSviewer, a computer program for bibliometric mapping. Scientometrics", 2010, "84", "523"),
    ("Xie 2014", "Xie M Age-related mutations associated with clonal hematopoietic expansion and malignancies. Nature Medicine", 2014, "20", "1472"),
]


import time


def tokens(t):
    return set(re.findall(r"[a-z0-9]{3,}", t.lower()))


def query(text):
    """Return the journal-article among the top 8 hits whose title best overlaps the query."""
    url = ("https://api.crossref.org/works?rows=8&filter=type:journal-article,type:proceedings-article"
           "&query.bibliographic=" + urllib.parse.quote(text))
    req = urllib.request.Request(url, headers={"User-Agent": "reference-check/1.0"})
    for attempt in range(4):
        try:
            with urllib.request.urlopen(req, timeout=30) as r:
                items = json.load(r)["message"]["items"]
            break
        except Exception as e:
            if "429" in str(e) and attempt < 3:
                time.sleep(6 * (attempt + 1)); continue
            raise
    q = tokens(text)
    best = max(items, key=lambda it: len(q & tokens((it.get("title") or [""])[0])) /
               (len(tokens((it.get("title") or [""])[0])) or 1))
    time.sleep(2)
    return best


rows = []
for label, text, year, vol, page in REFS:
    try:
        it = query(text)
        y = (it.get("issued", {}).get("date-parts") or [[None]])[0][0]
        v = str(it.get("volume", ""))
        p = str(it.get("page", it.get("article-number", "")))
        title = (it.get("title") or [""])[0]
        ok_year = y == year
        ok_vol = v == vol
        ok_page = p.split("-")[0].lstrip("0") == page.lstrip("0") or page.lower() in p.lower()
        rows.append([label, title[:90], it.get("container-title", [""])[0][:40], y, v, p, it.get("DOI", ""),
                     "Y" if ok_year else "N", "Y" if ok_vol else "N", "Y" if ok_page else "N"])
    except Exception as e:  # network or API error
        rows.append([label, "ERROR " + str(e)[:60], "", "", "", "", "", "?", "?", "?"])

with open("manuscript/reference_check.csv", "w", newline="", encoding="utf-8") as f:
    w = csv.writer(f)
    w.writerow(["ref", "crossref_title", "journal", "year", "volume", "page", "doi", "year_ok", "vol_ok", "page_ok"])
    w.writerows(rows)
for r in rows:
    print(f"{r[0]:22s} {r[7]}{r[8]}{r[9]}  {r[1][:70]} | {r[2][:22]} {r[3]} v{r[4]} p{r[5]} | {r[6]}")
