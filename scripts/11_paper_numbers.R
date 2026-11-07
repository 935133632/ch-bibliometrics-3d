# 11_paper_numbers.R: every number quoted in the manuscript, computed in one place
suppressPackageStartupMessages({ library(bibliometrix); library(openxlsx) })
M <- readRDS("data/clean/M_wos_final.rds")
A <- readRDS("data/clean/M_wos_all_clean.rds")
out <- character(); say <- function(...) out <<- c(out, sprintf(...))
uniq_split <- function(v) { v <- v[!is.na(v)]; trimws(unlist(lapply(strsplit(v, ";"), function(z) unique(trimws(z))))) }
tt <- function(x, n = 10) head(sort(table(x), decreasing = TRUE), n)

say("## Dataset")
say("retrieved (TS, 2014-2026, Article+Review, English, all WoS editions): %d", nrow(A))
say("core records (phrase in title/abstract/author keywords): %d (%.1f%%)", nrow(M), 100 * nrow(M) / nrow(A))
say("articles: %d ; reviews: %d", sum(grepl("^ARTICLE", M$DT)), sum(grepl("^REVIEW", M$DT)))
say("records with author keywords: %d (%.1f%%)", sum(!is.na(M$DE)), 100 * mean(!is.na(M$DE)))
say("total citations (WoS TC): %d ; mean per paper: %.1f ; median: %d", sum(M$TC), mean(M$TC), median(M$TC))
say("sources (journals): %d ; authors: %d", length(unique(M$SO)), length(unique(uniq_split(M$AU))))

say("\n## Annual production")
yr <- table(M$PY); for (y in names(yr)) say("%s: %d", y, yr[[y]])
y1 <- as.integer(yr["2015"]); y2 <- as.integer(yr["2025"])
say("CAGR 2015-2025 (full years): %.1f%%", 100 * ((y2 / y1)^(1 / 10) - 1))
fit <- lm(log(as.integer(yr[as.character(2015:2025)])) ~ I(2015:2025))
y18 <- as.integer(yr["2018"]); say("CAGR 2018-2025: %.1f%%", 100 * ((y2 / y18)^(1 / 7) - 1))
say("log-linear fit 2015-2025: growth %.1f%%/yr, R2 = %.3f", 100 * (exp(coef(fit)[2]) - 1), summary(fit)$r.squared)
say("share of 2014-2025 publications in 2021-2025: %.1f%%", 100 * sum(yr[as.character(2021:2025)]) / sum(yr[as.character(2014:2025)]))

say("\n## Countries (each country once per paper)")
M2 <- metaTagExtraction(M, Field = "AU_CO", sep = ";")
ct <- tt(uniq_split(M2$AU_CO), 10); for (i in seq_along(ct)) say("%s: %d", names(ct)[i], ct[[i]])
mc <- vapply(strsplit(M2$AU_CO, ";"), function(z) length(unique(trimws(z[nzchar(z)]))) > 1, TRUE)
say("internationally co-authored papers: %d (%.1f%%)", sum(mc, na.rm = TRUE), 100 * mean(mc, na.rm = TRUE))

say("\n## Institutions (thesaurus-merged)")
it <- tt(uniq_split(M$C3_clean), 10); for (i in seq_along(it)) say("%s: %d", names(it)[i], it[[i]])

say("\n## Journals")
jt <- tt(M$SO, 10); for (i in seq_along(jt)) say("%s: %d", names(jt)[i], jt[[i]])
say("top-10 journals share of papers: %.1f%%", 100 * sum(jt) / nrow(M))

say("\n## Authors")
at <- tt(uniq_split(M$AU), 10); for (i in seq_along(at)) say("%s: %d", names(at)[i], at[[i]])

say("\n## Most cited core papers")
o <- order(-M$TC)[1:10]
for (i in o) say("%d | %s | %s | %s | TC=%d", M$PY[i], substr(M$AU[i], 1, 30), substr(M$TI[i], 1, 90), M$SO[i], M$TC[i])

say("\n## Author keywords after thesaurus (top 20)")
kt <- tt(uniq_split(M$DE_clean), 20); for (i in seq_along(kt)) say("%s: %d", names(kt)[i], kt[[i]])

say("\n## Bursts (author keywords, g-index k=25, Kleinberg s=2 gamma=1)")
b <- read.csv("tables/citespace_like/keyword_bursts_ak.csv")
say("bursts: %d ; 2026-only bursts: %d", nrow(b), sum(b$begin == 2026 & b$end == 2026))
for (i in seq_len(nrow(b))) say("%s | %s-%s | strength %.2f | n=%d", b$keyword[i], b$begin[i], b$end[i], b$strength[i], b$total[i])

say("\n## CiteSpace-style network")
say("%s", readLines("tables/citespace_like/network_summary.txt"))
cl <- read.csv("tables/citespace_like/clusters.csv")
for (i in seq_len(nrow(cl))) say("cluster %d | size %d | S=%.3f | median year %.1f | %s", cl$cluster[i], cl$size[i], cl$silhouette[i], cl$mean_year[i], cl$label_LLR[i])

say("\n## VOSviewer (author keywords, >=5 occurrences, full counting)")
vm <- read.delim("vosviewer/ak_map.txt", check.names = FALSE, fileEncoding = "UTF-8-BOM")
vn <- read.delim("vosviewer/ak_network.txt", header = FALSE)
say("nodes: %d ; links: %d ; total link strength: %d ; clusters: %d", nrow(vm), nrow(vn), sum(vn[[3]]), length(unique(vm$cluster)))
cs <- aggregate(vm[["weight<Occurrences>"]], list(cluster = vm$cluster), sum)
for (k in sort(unique(vm$cluster))) {
  d <- vm[vm$cluster == k, ]; d <- d[order(-d[["weight<Occurrences>"]]), ]
  say("VOS cluster %d | %d items | mean pub year %.2f | top: %s", k, nrow(d), mean(d[["score<Avg. pub. year>"]]),
      paste(head(d$label, 5), collapse = "; "))
}

say("\n## Thematic map clusters (bibliometrix)")
tm <- read.csv("tables/network/thematic_clusters.csv")
for (i in seq_len(nrow(tm))) say("%s | centrality %.3f | density %.3f | n=%d", tm$name[i], tm$centrality[i], tm$density[i], tm$freq[i])

say("\n## Co-citation: top 10 references")
refs <- lapply(strsplit(M$CR[!is.na(M$CR)], ";"), function(z) unique(trimws(vapply(strsplit(trimws(z), ","), function(f) paste(trimws(head(f, 5)), collapse = ", "), ""))))
rt <- tt(unlist(refs), 10); for (i in seq_along(rt)) say("%s: %d", names(rt)[i], rt[[i]])


say("
## Strict set (phrase in title or author keywords) for sensitivity analysis")
core_pat <- "CLONAL HAEMATOPOIESIS|CLONAL HEMATOPOIESIS|CLONAL HAEMOPOIESIS|CLONAL HEMOPOIESIS|CLONAL CYTOPENIA"
upc <- function(x) toupper(ifelse(is.na(x), "", x))
in_ti <- grepl(core_pat, upc(M$TI)); in_ab <- grepl(core_pat, upc(M$AB)); in_de <- grepl(core_pat, upc(M$DE))
strict <- in_ti | in_de
say("strict records (title or author keywords): %d (%.1f%% of core)", sum(strict), 100 * mean(strict))
say("abstract-only records: %d (%.1f%% of core)", sum(!strict), 100 * mean(!strict))
say("top-10 most cited in core that are abstract-only: %d", sum(!strict[order(-M$TC)[1:10]]))
S <- M[strict, ]
say("strict set by year: %s", paste(names(table(S$PY)), table(S$PY), sep = "=", collapse = ", "))
so <- order(-S$TC)[1:10]
for (i in so) say("STRICT %d | %s | %s | %s | TC=%d", S$PY[i], substr(S$AU[i], 1, 25), substr(S$TI[i], 1, 85), S$SO[i], S$TC[i])
sct <- tt(uniq_split(metaTagExtraction(S, Field = "AU_CO", sep = ";")$AU_CO), 6)
say("strict top countries: %s", paste(names(sct), sct, sep = "=", collapse = ", "))
sjt <- tt(S$SO, 5); say("strict top journals: %s", paste(names(sjt), sjt, sep = "=", collapse = ", "))
say("strict records with author keywords: %d (%.1f%%)", sum(!is.na(S$DE)), 100 * mean(!is.na(S$DE)))


say("
## Comparison with Luo and Yu 2025 (851 original articles, 2014 to 2024)")
art <- grepl("^ARTICLE", M$DT) & !grepl("REVIEW", M$DT)
say("core set, original articles, 2014-2024: %d", sum(art & M$PY <= 2024))
say("strict set (title or author keywords), original articles, 2014-2024: %d", sum(art & strict & M$PY <= 2024))
say("core set, all document types, 2014-2024: %d", sum(M$PY <= 2024))


say("
## Geographic shift and citation impact")
hidx <- function(tc) { tc <- sort(tc, decreasing = TRUE); sum(tc >= seq_along(tc)) }
cl_c <- lapply(strsplit(M2$AU_CO, ";"), function(z) unique(trimws(z[nzchar(z)])))
has <- function(country) vapply(cl_c, function(z) country %in% z, TRUE)
usa <- has("USA"); chn <- has("CHINA")
yrs <- 2014:2026
share <- function(f, idx = rep(TRUE, nrow(M))) vapply(yrs, function(y) 100 * mean(f[idx & M$PY == y]), 0)
tab_share <- data.frame(year = yrs, n_core = as.integer(table(factor(M$PY, levels = yrs))),
                        usa_core = share(usa), china_core = share(chn),
                        n_strict = vapply(yrs, function(y) sum(strict & M$PY == y), 0L),
                        usa_strict = share(usa, strict), china_strict = share(chn, strict))
write.csv(tab_share, "tables/country_share_by_year.csv", row.names = FALSE)
for (i in 8:13) say("%d | n=%d | USA %.1f%% | China %.1f%% | strict n=%d USA %.1f%% China %.1f%%", tab_share$year[i], tab_share$n_core[i],
                    tab_share$usa_core[i], tab_share$china_core[i], tab_share$n_strict[i], tab_share$usa_strict[i], tab_share$china_strict[i])
say("China papers 2024 / 2025 / 2026 (core): %d / %d / %d ; strict: %d / %d / %d",
    sum(chn & M$PY == 2024), sum(chn & M$PY == 2025), sum(chn & M$PY == 2026),
    sum(chn & strict & M$PY == 2024), sum(chn & strict & M$PY == 2025), sum(chn & strict & M$PY == 2026))
tc2526 <- sort(table(unlist(cl_c[M$PY >= 2025])), decreasing = TRUE)[1:5]
say("top countries 2025-2026 (core): %s", paste(names(tc2526), tc2526, collapse = ", "))
tc2526s <- sort(table(unlist(cl_c[strict & M$PY >= 2025])), decreasing = TRUE)[1:5]
say("top countries 2025-2026 (strict): %s", paste(names(tc2526s), tc2526s, collapse = ", "))
art_i <- grepl("^ARTICLE", M$DT) & !grepl("REVIEW", M$DT)
tcA <- sort(table(unlist(cl_c[art_i & M$PY <= 2024])), decreasing = TRUE)[1:6]
say("top countries, original articles 2014-2024 (core): %s", paste(names(tcA), tcA, collapse = ", "))
say("China: %d of %d papers (%.1f%%) are from 2025-2026 (core)", sum(chn & M$PY >= 2025), sum(chn), 100 * sum(chn & M$PY >= 2025) / sum(chn))
say("share of papers in 2025-2026: %.1f%% (%d of %d)", 100 * mean(M$PY >= 2025), sum(M$PY >= 2025), nrow(M))

nc <- readRDS("data/clean/normalised_citations.rds")           # written by 24_extra_analyses.R
M$NCS <- nc$NCS[match(M$UT, nc$UT)]; M$TOP10 <- nc$TOP10[match(M$UT, nc$UT)]
impact <- function(group, sets) {
  do.call(rbind, lapply(names(sets), function(n) { idx <- sets[[n]]
    data.frame(group = group, name = n, papers = sum(idx), citations = sum(M$TC[idx]), h_index = hidx(M$TC[idx]),
               citations_per_paper = round(sum(M$TC[idx]) / sum(idx), 1),
               mncs = round(mean(M$NCS[idx], na.rm = TRUE), 2), top10_pct = round(100 * mean(M$TOP10[idx & !is.na(M$TOP10)]), 1)) }))
}
top_c <- names(sort(table(unlist(cl_c)), decreasing = TRUE))[1:10]
imp_c <- impact("Country", setNames(lapply(top_c, has), top_c))
inst_l <- lapply(strsplit(M$C3_clean, ";"), function(z) unique(trimws(z)))
top_i <- names(sort(table(unlist(inst_l)), decreasing = TRUE))[1:10]
imp_i <- impact("Institution", setNames(lapply(top_i, function(n) vapply(inst_l, function(z) n %in% z, TRUE)), top_i))
top_j <- names(sort(table(M$SO), decreasing = TRUE))[1:10]
imp_j <- impact("Journal", setNames(lapply(top_j, function(n) M$SO == n), top_j))
imp <- rbind(imp_c, imp_i, imp_j)
write.csv(imp, "tables/impact_metrics.csv", row.names = FALSE)
for (i in seq_len(nrow(imp))) say("%s | %s | papers %d | citations %d | H %d | per paper %.1f | MNCS %.2f | top10 %.1f%%", imp$group[i], imp$name[i], imp$papers[i], imp$citations[i], imp$h_index[i], imp$citations_per_paper[i], imp$mncs[i], imp$top10_pct[i])
say("H-index of the whole analysis set: %d", hidx(M$TC))


say("
## Additional analyses (tables written by scripts/24_extra_analyses.R and 25_topic_model.py)")
rd <- function(f) read.csv(f, stringsAsFactors = FALSE, check.names = FALSE)
cs <- rd("tables/china_shift_characteristics.csv")
for (i in seq_len(nrow(cs))) say("CHINA-TABLE | %s", paste(names(cs), unlist(cs[i, ]), sep = "=", collapse = "; "))
for (i in seq_len(nrow(rd("tables/china_cardiovascular_share_by_year.csv")))) { r <- rd("tables/china_cardiovascular_share_by_year.csv")[i, ]; say("China share of cardiovascular-term papers %d: %.1f%% (n=%d)", r$year, r[[2]], r[[3]]) }
ci <- rd("tables/china_impact_by_collaboration.csv"); for (i in seq_len(nrow(ci))) say("MNCS-GROUP | %s | n=%d | MNCS %.2f | top10 %.1f%%", ci$group[i], ci$papers[i], ci$mncs[i], ci$top10_pct[i])
et <- rd("tables/entity_trends.csv")
for (i in seq_len(nrow(et))) say("ENTITY | %s | %s | n=%d | %.1f / %.1f / %.1f / %.1f | fold %s | adj p %.4g", et$panel[i], et$term[i], et$n_total[i], et$p1[i], et$p2[i], et$p3[i], et$p4[i], et$fold_p4_vs_p1[i], et$p_adj_BH[i])
nt <- rd("tables/nmf_topics.csv"); ks <- rd("tables/nmf_k_selection.csv")
say("NMF k selection: %s", paste(sprintf("k=%d ARI %.2f", ks$k, ks$bootstrap_ARI_mean), collapse = "; "))
for (i in seq_len(nrow(nt))) say("NMF topic %d | n=%d (%.1f%%) | %.1f / %.1f / %.1f / %.1f | %s", nt$topic[i], nt$docs[i], nt$share_pct[i], nt$p_2014_2019[i], nt$p_2020_2022[i], nt$p_2023_2024[i], nt$p_2025_2026[i], nt$top_words[i])
dv <- rd("tables/subject_diversity.csv")
for (i in seq_len(nrow(dv))) say("DIVERSITY | %s | papers %d | categories %d | effective %.1f | rarefied %.1f | %s", dv$period[i], dv$papers[i], dv$categories[i], dv$effective_n[i], dv$effective_n_300[i], dv$top5[i])
say("OPEN ACCESS by year: %s", paste(sprintf("%d any %.1f gold %.1f", rd("tables/open_access.csv")$year, rd("tables/open_access.csv")$any_oa_pct, rd("tables/open_access.csv")$gold_pct), collapse = "; "))
tm <- rd("tables/team_size.csv"); for (i in seq_len(nrow(tm))) say("TEAM | %s | median %.0f | mean %.1f | >=50 authors %.1f%% | multi-country %.1f%%", tm$period[i], tm$median_authors[i], tm$mean_authors[i], tm$over_50_authors_pct[i], tm$multi_country_pct[i])
nw <- rd("tables/nowcast_2026.csv"); for (i in seq_len(nrow(nw))) say("NOWCAST | %s | share %.3f | full-year 2026 %d", nw$basis[i], nw$share_jan_oct[i], nw$full_year_2026[i])
mc <- rd("tables/monthly_counts_2025_2026.csv"); say("Jan-Sep papers: 2025 %d, 2026 %d; months where 2026 exceeds 2025 (Jan-Sep): %d of 9", sum(mc$y2025[1:9]), sum(mc$y2026[1:9]), sum(mc$y2026[1:9] > mc$y2025[1:9]))
oc <- rd("tables/online_year_counts.csv"); say("By online year: 2025 %d, 2026 %d (by publication year 2025 %d, 2026 %d)", oc$by_online_year[oc$year == 2025], oc$by_online_year[oc$year == 2026], oc$by_publication_year[oc$year == 2025], oc$by_publication_year[oc$year == 2026])
ab <- rd("tables/authors_by_scope.csv"); for (sc in unique(ab$scope)) say("AUTHORS | %s | %s", sc, paste(sprintf("%s %d", ab$author[ab$scope == sc][1:6], ab$papers[ab$scope == sc][1:6]), collapse = "; "))

writeLines(out, "tables/paper_numbers.md")
cat(out, sep = "\n")
