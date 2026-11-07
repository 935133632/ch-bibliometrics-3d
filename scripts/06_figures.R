# 06_figures.R: manuscript figures (ggplot2 only), vector PDF
suppressPackageStartupMessages({
  library(ggplot2); library(igraph); library(ggrepel); library(scales)
})

# ============================ PARAMETER BLOCK ============================
text_size_pt <- 7                 # ONE size for all text (axes, ticks, legend, labels)
font_family  <- "Times"
line_width   <- 0.3               # axes / grid / thin lines (mm-based stroke units)
edge_width   <- c(0.15, 1.0)      # network edge width range (stroke units)
point_size   <- c(1.2, 7)         # network node size range (mm)
width_single <- 85                # mm, single column
width_double <- 180               # mm, double column
height_flat  <- 62                # mm, default flat height
n_kw_nodes   <- 50                # keyword network nodes
kw_edge_min  <- 6                 # minimum co-occurrence count drawn
n_ref_nodes  <- 40                # co-citation network nodes
ref_edge_min <- 30                # minimum co-citation count drawn
n_country    <- 15                # countries in collaboration heatmap
wong <- c("#E69F00", "#56B4E9", "#009E73", "#CC79A7",
          "#D55E00", "#F0E442", "#0072B2", "#000000")   # fixed order
# Continuous colour: viridis family ("viridis"); colour bands are equal-count
# quantile bins of the data (see quantile_bins()), mapped through the full ramp.
viridis_option <- "viridis"
# =========================================================================

pt_mm <- text_size_pt / ggplot2::.pt          # geom_text size (mm) for text_size_pt

theme_pub <- function() {
  theme_bw(base_family = font_family, base_size = text_size_pt) +
    theme(
      text             = element_text(family = font_family, size = text_size_pt),
      axis.text        = element_text(size = text_size_pt, colour = "black"),
      axis.title       = element_text(size = text_size_pt),
      legend.text      = element_text(size = text_size_pt),
      legend.title     = element_text(size = text_size_pt),
      strip.text       = element_text(size = text_size_pt),
      plot.title       = element_blank(), plot.subtitle = element_blank(),
      panel.grid.major = element_line(colour = "grey90", linewidth = line_width),
      panel.grid.minor = element_blank(),
      panel.border     = element_rect(colour = "black", linewidth = line_width),
      axis.ticks       = element_line(linewidth = line_width),
      plot.background  = element_rect(fill = NA, colour = NA),
      legend.background = element_rect(fill = NA, colour = NA),
      legend.key.size  = unit(3, "mm")
    )
}
theme_net <- function() theme_pub() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        axis.text = element_blank(), axis.title = element_blank(),
        axis.ticks = element_blank())

save_pdf <- function(p, file, w, h) {
  ggsave(file, p, width = w, height = h, units = "mm", device = "pdf")
  stopifnot(file.exists(file))
}
country_label <- function(x) {
  x <- tools::toTitleCase(tolower(x))
  x[x == "Usa"] <- "USA"; x[x == "Korea"] <- "South Korea"
  x
}
quantile_bins <- function(x, k = 5) {
  br <- unique(quantile(x[is.finite(x)], probs = seq(0, 1, length.out = k + 1), names = FALSE))
  cut(x, breaks = br, include.lowest = TRUE, dig.lab = 4)
}

M <- readRDS("data/clean/M_wos_final.rds")
dir.create("figures", showWarnings = FALSE)

# ---------- network helper --------------------------------------------------
draw_network <- function(cooc, freq, edge_min, file, w, h, seed = 1, lower = TRUE) {
  diag(cooc) <- 0
  cooc[cooc < edge_min] <- 0
  keep <- rowSums(cooc) > 0
  cooc <- cooc[keep, keep]; freq <- freq[rownames(cooc)]
  g <- graph_from_adjacency_matrix(cooc, mode = "undirected", weighted = TRUE)
  set.seed(seed)
  cl <- cluster_louvain(g)$membership
  big <- order(-table(cl))[seq_len(min(length(unique(cl)), length(wong) - 1))]
  cl_f <- ifelse(cl %in% big, match(cl, big), length(big) + 1)
  lay <- layout_with_fr(g, weights = log1p(E(g)$weight), niter = 2000)
  nd <- data.frame(name = V(g)$name, name_label = if (lower) tolower(V(g)$name) else V(g)$name, x = lay[, 1], y = lay[, 2],
                   cluster = factor(cl_f), freq = freq[V(g)$name])
  ed <- as.data.frame(as_edgelist(g)); names(ed) <- c("a", "b"); ed$w <- E(g)$weight
  ed <- cbind(ed, setNames(nd[match(ed$a, nd$name), c("x", "y")], c("x1", "y1")),
                  setNames(nd[match(ed$b, nd$name), c("x", "y")], c("x2", "y2")))
  p <- ggplot() +
    geom_segment(data = ed, aes(x1, y1, xend = x2, yend = y2, linewidth = w),
                 colour = "grey60", alpha = 0.45) +
    geom_point(data = nd, aes(x, y, size = freq, fill = cluster),
               shape = 21, colour = "black", stroke = line_width) +
    geom_text_repel(data = nd, aes(x, y, label = name_label),
                    family = font_family, size = pt_mm, max.overlaps = Inf,
                    box.padding = 0.15, point.padding = 0.1, segment.size = 0.15,
                    min.segment.length = 0.2, seed = seed) +
    scale_linewidth(range = edge_width, guide = "none") +
    scale_size(range = point_size, name = "Frequency") +
    scale_fill_manual(values = wong[seq_along(levels(nd$cluster))], guide = "none") +
    coord_equal() + theme_net() + theme(legend.position = "right")
  save_pdf(p, file, w, h)
  invisible(nd)
}

# ---------- Fig 1: annual production ----------------------------------------
yr <- as.data.frame(table(Year = M$PY), stringsAsFactors = FALSE)
yr$Year <- as.integer(yr$Year)
yr$Partial <- ifelse(yr$Year == 2026, "partial", "full")
p1 <- ggplot(yr, aes(Year, Freq, fill = Partial)) +
  geom_col(colour = "black", linewidth = line_width, width = 0.75) +
  geom_text(aes(label = Freq), vjust = -0.4, family = font_family, size = pt_mm) +
  scale_fill_manual(values = c(full = wong[7], partial = wong[2]), guide = "none") +
  scale_x_continuous(breaks = 2014:2026) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
  labs(x = "Year", y = "Publications") + theme_pub() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
save_pdf(p1, "figures/fig1_annual_production.pdf", width_single, height_flat)

# ---------- Fig 2: top countries --------------------------------------------
suppressPackageStartupMessages(library(bibliometrix))
M2 <- metaTagExtraction(M, Field = "AU_CO", sep = ";")
cl_list <- lapply(strsplit(M2$AU_CO, ";"), function(z) unique(trimws(z[nzchar(z)])))
ct <- sort(table(unlist(cl_list)), decreasing = TRUE)
ctd <- data.frame(Country = country_label(names(ct)[1:n_country]),
                  N = as.integer(ct[1:n_country]))
ctd$Country <- factor(ctd$Country, levels = rev(ctd$Country))
p2 <- ggplot(ctd, aes(N, Country)) +
  geom_col(fill = wong[7], colour = "black", linewidth = line_width, width = 0.75) +
  geom_text(aes(label = N), hjust = -0.15, family = font_family, size = pt_mm) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.12))) +
  labs(x = "Publications", y = NULL) + theme_pub()
save_pdf(p2, "figures/fig2_top_countries.pdf", width_single, height_flat + 20)

# ---------- Fig 2C: share of papers with US and Chinese authors by year ----------
has_c <- function(country) vapply(cl_list, function(z) country %in% z, TRUE)
yrs_c <- 2016:2026                                       # earlier years have fewer than 30 papers
sh <- data.frame(Year = rep(yrs_c, 2),
                 Country = rep(c("USA", "China"), each = length(yrs_c)),
                 Share = c(vapply(yrs_c, function(y) 100 * mean(has_c("USA")[M$PY == y]), 0),
                           vapply(yrs_c, function(y) 100 * mean(has_c("CHINA")[M$PY == y]), 0)))
sh$Country <- factor(sh$Country, levels = c("USA", "China"))
p2c <- ggplot(sh, aes(Year, Share, colour = Country, shape = Country)) +
  geom_line(linewidth = 0.5) + geom_point(size = 1.6) +
  scale_colour_manual(values = c(USA = wong[7], China = wong[5]), name = NULL) +
  scale_shape_manual(values = c(USA = 16, China = 17), name = NULL) +
  scale_x_continuous(breaks = yrs_c) +
  scale_y_continuous(limits = c(0, 75), expand = expansion(mult = c(0, 0.02))) +
  labs(x = "Year", y = "Papers with authors from the country (%)") + theme_pub() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1), legend.position = "bottom")
save_pdf(p2c, "figures/fig2c_country_share_by_year.pdf", 90, height_flat + 20)

# ---------- Fig 3: keyword co-occurrence ------------------------------------
kw_list <- lapply(strsplit(M$DE_clean[!is.na(M$DE_clean)], ";"), unique)
kw_top  <- names(sort(table(unlist(kw_list)), decreasing = TRUE))[1:n_kw_nodes]
inc <- do.call(rbind, lapply(kw_list, function(z) as.integer(kw_top %in% z)))
colnames(inc) <- kw_top
kw_nodes <- draw_network(crossprod(inc), setNames(colSums(inc), kw_top), kw_edge_min,
                         "figures/fig3_keyword_cooccurrence.pdf", width_double, 130)
write.csv(kw_nodes, "tables/network/fig3_keyword_nodes.csv", row.names = FALSE)

# ---------- Fig 4: country collaboration heatmap ----------------------------
top_c <- names(ct)[1:n_country]
inc_c <- do.call(rbind, lapply(cl_list, function(z) as.integer(top_c %in% z)))
colnames(inc_c) <- top_c
co <- crossprod(inc_c); diag(co) <- NA
lab <- country_label(top_c)
hd <- expand.grid(a = seq_along(top_c), b = seq_along(top_c))
hd$n <- co[cbind(hd$a, hd$b)]
hd$bin <- quantile_bins(hd$n, 5)
hd$a <- factor(lab[hd$a], levels = lab); hd$b <- factor(lab[hd$b], levels = rev(lab))
p4 <- ggplot(hd, aes(a, b, fill = bin)) +
  geom_tile(colour = "white", linewidth = line_width) +
  geom_text(aes(label = n, colour = as.integer(bin) <= 2),
            family = font_family, size = pt_mm, na.rm = TRUE) +
  scale_colour_manual(values = c(`TRUE` = "white", `FALSE` = "black"), guide = "none") +
  scale_fill_viridis_d(option = viridis_option, na.translate = FALSE,
                       name = "Co-authored\npublications") +
  labs(x = NULL, y = NULL) + coord_equal() + theme_pub() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), panel.grid = element_blank())
save_pdf(p4, "figures/fig4_country_collaboration.pdf", width_double, 110)

# ---------- Fig 5: reference co-citation ------------------------------------
refs <- lapply(strsplit(M$CR[!is.na(M$CR)], ";"), function(z) {
  z <- trimws(z)
  key <- vapply(strsplit(z, ","), function(f) paste(trimws(head(f, 5)), collapse = ", "), "")
  unique(key[nzchar(key)])
})
rt <- sort(table(unlist(refs)), decreasing = TRUE)
top_r <- names(rt)[1:n_ref_nodes]
inc_r <- do.call(rbind, lapply(refs, function(z) as.integer(top_r %in% z)))
colnames(inc_r) <- top_r
short <- vapply(strsplit(top_r, ", "), function(f) {
  tk <- strsplit(f[1], " ")[[1]]                      # "STEENSMA DP" -> "Steensma DP"
  au <- paste(c(tools::toTitleCase(tolower(head(tk, -1))), toupper(tail(tk, 1))), collapse = " ")
  paste0(au, " ", f[2])
}, "")
src <- vapply(strsplit(top_r, ", "), function(f) tools::toTitleCase(tolower(f[3])), "")
dup <- short %in% short[duplicated(short)]
short[dup] <- paste0(short[dup], " (", src[dup], ")")
short <- make.unique(short, sep = "-")
co_r <- crossprod(inc_r); dimnames(co_r) <- list(short, short)
fr <- setNames(colSums(inc_r), short)
ref_nodes <- draw_network(co_r, fr, ref_edge_min, "figures/fig5_cocitation.pdf",
                          width_double, 130, lower = FALSE)
ref_nodes$reference <- top_r[match(ref_nodes$name, short)]
write.csv(ref_nodes, "tables/network/fig5_cocitation_nodes.csv", row.names = FALSE)

# ---------- Fig 6: thematic map ---------------------------------------------
Tm_cl <- read.csv("tables/network/thematic_clusters.csv", stringsAsFactors = FALSE)
Tm_cl$label <- tolower(Tm_cl$name)
Tm_cl$cluster <- factor(seq_len(nrow(Tm_cl)))
xm <- mean(Tm_cl$centrality); ym <- mean(Tm_cl$density)   # mean, as in bibliometrix; median sits on a point when n is small
p6 <- ggplot(Tm_cl, aes(centrality, density)) +
  geom_vline(xintercept = xm, linetype = "dashed", linewidth = line_width) +
  geom_hline(yintercept = ym, linetype = "dashed", linewidth = line_width) +
  geom_point(aes(size = freq, fill = cluster), shape = 21, colour = "black",
             stroke = line_width, alpha = 0.85) +
  geom_text_repel(aes(label = label), family = font_family, size = pt_mm,
                  max.overlaps = Inf, box.padding = 1, point.padding = 0.6, force = 3, seed = 1) +
  scale_size(range = c(2.5, 9), name = "Occurrences") +
  scale_fill_manual(values = wong[seq_len(nrow(Tm_cl))], guide = "none") +
  labs(x = "Centrality (relevance degree)", y = "Density (development degree)") +
  theme_pub()
save_pdf(p6, "figures/fig6_thematic_map.pdf", width_double, 90)

# ---------- Fig 7: trend topics ---------------------------------------------
tt <- read.csv("tables/network/trend_topics.csv", stringsAsFactors = FALSE)
tt <- tt[tt$freq >= 8, ]                       # same filters as bibliometrix fieldByYear
tt <- do.call(rbind, lapply(split(tt, tt$year_med), function(d) head(d[order(-d$freq), ], 4)))
tt <- tt[order(tt$year_med, -tt$freq), ]
tt$item <- factor(tolower(tt$item), levels = rev(unique(tolower(tt$item))))
p7 <- ggplot(tt, aes(y = item)) +
  geom_linerange(aes(xmin = year_q1, xmax = year_q3), colour = "grey50",
                 linewidth = 0.8) +
  geom_point(aes(x = year_med, size = freq), shape = 21, fill = wong[7],
             colour = "black", stroke = line_width) +
  scale_size(range = c(1.5, 5), name = "Frequency") +
  scale_x_continuous(breaks = seq(2018, 2026, 2)) +
  labs(x = "Year (Q1 to Q3, median)", y = NULL) + theme_pub()
save_pdf(p7, "figures/fig7_trend_topics.pdf", width_single + 40, 100)

# Fig 8 (keyword bursts) is produced by scripts/08_burst_vosviewer.R -> figures/fig8b_keyword_bursts.pdf


# ---------- Fig 9: genes, conditions and methods named in titles, abstracts and keywords, by period ----------
et <- read.csv("tables/entity_trends.csv", stringsAsFactors = FALSE)
per_lab <- c("2014-2019", "2020-2022", "2023-2024", "2025-2026")
period_m <- cut(M$PY, c(2013, 2019, 2022, 2024, 2026), labels = per_lab)
has_abs <- !is.na(M$AB) & nchar(M$AB) > 0
n_per <- as.integer(table(period_m[has_abs]))
long <- do.call(rbind, lapply(seq_len(nrow(et)), function(i) data.frame(
  panel = et$panel[i], term = paste0(et$term[i], ifelse(et$p_adj_BH[i] < 0.05, "*", "")), p4 = et$p4[i],
  period = factor(per_lab, levels = per_lab), share = unlist(et[i, c("p1", "p2", "p3", "p4")]))))
long$panel <- factor(long$panel, levels = c("Genes", "Diseases", "Methods"), labels = c("Genes and chromosomal changes", "Conditions", "Methods and data"))
ord <- unique(long[order(long$panel, -long$p4), c("panel", "term")])
long$term <- factor(long$term, levels = rev(ord$term))
long$bin <- quantile_bins(long$share, 5)
p9 <- ggplot(long, aes(period, term, fill = bin)) +
  geom_tile(colour = "white", linewidth = line_width) +
  geom_text(aes(label = sprintf("%.1f", share), colour = as.integer(bin) <= 2), family = font_family, size = pt_mm) +
  scale_colour_manual(values = c(`TRUE` = "white", `FALSE` = "black"), guide = "none") +
  scale_fill_viridis_d(option = viridis_option, name = "Papers (%)") +
  scale_x_discrete(labels = sprintf("%s
(n = %d)", per_lab, n_per), position = "top") +
  facet_grid(panel ~ ., scales = "free_y", space = "free_y", switch = "y") +
  labs(x = NULL, y = NULL) + theme_pub() +
  theme(panel.grid = element_blank(), strip.placement = "outside", strip.text.y.left = element_text(angle = 0, hjust = 1),
        legend.position = "right")
save_pdf(p9, "figures/fig9_entity_trends.pdf", width_double, 205)

cat("Figures written:\n"); print(list.files("figures", pattern = "\\.pdf$"))
