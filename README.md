# Clonal hematopoiesis bibliometric analysis

Analysis code and interactive 3D figures for the bibliometric study of clonal hematopoiesis (CH)
research, 2014 to 2026.

## Interactive 3D figures

Site: https://935133632.github.io/ch-bibliometrics-3d/ (files `index.html`, `keyword.html`,
`cocitation.html`, `globe.html` in this repository; built by scripts 34 to 37 of the project)

- [Keyword Star Map](https://935133632.github.io/ch-bibliometrics-3d/keyword.html): 146 author keywords (Figures 4 and 5)
- [Co-citation Star Map](https://935133632.github.io/ch-bibliometrics-3d/cocitation.html): the 40 most co-cited references (Figure 3)
- [Collaboration Globe](https://935133632.github.io/ch-bibliometrics-3d/globe.html): co-authorship between 76 countries (Figure 2A)

## Search strategy

Web of Science Core Collection, searched on 16 October 2026 (4,284 records):

```
TS=("clonal hematopoiesis" OR "clonal haematopoiesis" OR "clonal hematopoiesis of indeterminate potential"
    OR "age-related clonal hematopoiesis" OR "clonal cytopenia of undetermined significance")
```

Scope rule (`02_descriptive.R`): a record is kept when a CH phrase appears in the title, abstract or
author keywords. This leaves 2,293 records; their accession numbers are in
`data/S0_accession_numbers_2293_records.txt`.

## Data

The Web of Science exports are subject to the Clarivate license and are not included.
To rerun the analysis, export the 2,293 records (plain text, full record with cited references) and save
them as `data/raw/wos_*.txt`. All scripts run from the repository root.

## Run order

| Script | Purpose |
|---|---|
| `01_import.R` |
| `02_descriptive.R` |
| `03_thesaurus.R` |
| `04_networks.R` |
| `06_figures.R` |
| `07_citespace_like.R` |
| `08_burst_vosviewer.R` |
| `09_export_core_wos.py` |
| `09_gephi_gexf.py` |
| `10_search_precision.R` |
| `11_paper_numbers.R` |
| `12_verify_references.py` |

- `01_import.R` reads the exports; `02_descriptive.R` applies the scope rule and writes descriptive tables.
- `03_thesaurus.R` applies the keyword and institution thesauri in `thesaurus/`.
- `04_networks.R` and `06_figures.R` build the networks and the manuscript figures.
- `07_citespace_like.R` reproduces the CiteSpace procedure (g-index, Pathfinder, Louvain, silhouette, timeline).
- `08_burst_vosviewer.R` runs Kleinberg burst detection on author keywords.
- `09_export_core_wos.py` writes the core records for VOSviewer; `09_gephi_gexf.py` writes the Gephi file.
- `10_search_precision.R` checks search precision; `11_paper_numbers.R` writes every number quoted in the paper.
- `12_verify_references.py` checks the reference list against the public Crossref API.

VOSviewer maps use author keywords, full counting, at least 5 occurrences, default clustering
(resolution 1.00) and `vosviewer/thesaurus_keywords.txt`.

## Software

R 4.4.2 (bibliometrix 5.5.0, igraph 2.3.1, ggplot2 4.0.3, ggrepel 0.9.8, cluster 2.1.8.2, openxlsx 4.2.8.1);
Python 3.13 (pandas, networkx); VOSviewer 1.6.21; Gephi 0.11.3.
