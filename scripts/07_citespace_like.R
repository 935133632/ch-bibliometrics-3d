# 07_citespace_like.R: CiteSpace-style keyword network, reproduced in R
#   time slicing -> g-index node selection -> merged co-occurrence network ->
#   Pathfinder (PFNET, r = Inf, q = n-1) -> Louvain clusters (Q, silhouette S) ->
#   LLR cluster labels -> timeline view; exports for Gephi and VOSviewer.
suppressPackageStartupMessages({
  library(igraph); library(ggplot2); library(ggrepel); library(cluster)
})

# ============================ PARAMETER BLOCK ============================
years_all   <- 2014:2026
g_k         <- 25          # g-index scale factor k (CiteSpace default 25)
max_per_slice <- 60        # safety cap on nodes per slice
min_node_freq <- 5         # drop nodes with fewer total occurrences
n_cluster_show <- 8        # clusters drawn in the timeline (<= 8, Wong palette)
label_per_cluster <- 4     # labelled nodes per timeline row
text_size_pt <- 7; font_family <- "Times"; line_width <- 0.3
width_double <- 180
wong <- c("#E69F00", "#56B4E9", "#009E73", "#CC79A7",
          "#D55E00", "#F0E442", "#0072B2", "#000000")
# =========================================================================
pt_mm <- text_size_pt / ggplot2::.pt
set.seed(42)

M <- readRDS("data/clean/M_wos_final.rds")
M <- M[!is.na(M$PY) & M$PY %in% years_all, ]
M$DE_raw_or <- M$DE   # raw author keywords, re-normalised below
norm_term <- function(x) trimws(gsub("[[:space:]]+", " ", gsub("[-–]", " ", toupper(gsub("<[^>]+>", "", x)))))
th <- read.csv("thesaurus/keyword_thesaurus.csv", stringsAsFactors = FALSE)
th_map <- setNames(norm_term(th$to), norm_term(th$from))
stops  <- norm_term(read.csv("thesaurus/keyword_stoplist.csv", stringsAsFactors = FALSE)$term)
terms <- lapply(seq_len(nrow(M)), function(i) {
  z <- strsplit(M$DE_raw_or[i], ";")[[1]]   # author keywords only
  z <- norm_term(z[!is.na(z)]); z <- z[nzchar(z)]
  hit <- z %in% names(th_map); z[hit] <- th_map[z[hit]]
  unique(z[!z %in% stops])
})
yr <- M$PY

# ---- 1. g-index selection per slice ---------------------------------------
g_select <- function(freq, k) {
  f <- sort(freq, decreasing = TRUE)
  cs <- cumsum(f); g <- which(seq_along(f)^2 <= k * cs)
  g <- if (length(g)) max(g) else 0
  names(f)[seq_len(min(g, max_per_slice))]
}
sel <- lapply(years_all, function(y) {
  tb <- table(unlist(terms[yr == y])); g_select(setNames(as.integer(tb), names(tb)), g_k)
})
names(sel) <- years_all
nodes <- unique(unlist(sel))
tot <- table(unlist(terms)); nodes <- nodes[tot[nodes] >= min_node_freq]
cat("Selected nodes:", length(nodes), "\n")

# ---- 2. merged co-occurrence network (edge counted in a slice only if both ends selected) ----
n <- length(nodes); W <- matrix(0, n, n, dimnames = list(nodes, nodes))
for (y in years_all) {
  keep <- intersect(sel[[as.character(y)]], nodes)
  for (tt in terms[yr == y]) {
    z <- intersect(tt, keep)
    if (length(z) > 1) { W[z, z] <- W[z, z] + 1 }
  }
}
diag(W) <- 0
freq <- as.integer(tot[nodes]); names(freq) <- nodes
yrs_of <- lapply(nodes, function(k) yr[vapply(terms, function(z) k %in% z, TRUE)])
first_year  <- vapply(yrs_of, min, 0);    names(first_year)  <- nodes
median_year <- vapply(yrs_of, median, 0); names(median_year) <- nodes

# cosine-normalised link strength (CiteSpace default)
C <- W / sqrt(outer(freq, freq)); C[!is.finite(C)] <- 0

# ---- 3. Pathfinder PFNET(r = Inf, q = n-1) --------------------------------
D <- ifelse(C > 0, 1 / C, Inf)
B <- D
for (k in seq_len(n)) B <- pmin(B, pmax(matrix(B[, k], n, n), matrix(B[k, ], n, n, byrow = TRUE)))
keep_edge <- (D <= B + 1e-12) & is.finite(D)
Wp <- W * keep_edge; Wp[lower.tri(Wp)] <- t(Wp)[lower.tri(Wp)]   # symmetric
g_full <- graph_from_adjacency_matrix(W,  mode = "undirected", weighted = TRUE)
g      <- graph_from_adjacency_matrix(Wp, mode = "undirected", weighted = TRUE)
g <- delete_vertices(g, which(degree(g) == 0))
cat("Edges full / pruned:", ecount(g_full), "/", ecount(g), "\n")

# ---- 4. clustering, modularity Q, silhouette S -----------------------------
cl <- cluster_louvain(g, weights = E(g)$weight)
memb <- membership(cl); Q <- modularity(cl)
ord <- order(-table(memb)); remap <- setNames(seq_along(ord), names(table(memb))[ord])
memb <- setNames(remap[as.character(memb)], names(memb))
# silhouette uses shortest-path distances on the UNPRUNED cosine network (pruned net is a tree)
Cg <- graph_from_adjacency_matrix(C[names(memb), names(memb)], mode = "undirected", weighted = TRUE)
dist_m <- distances(Cg, weights = 1 / E(Cg)$weight)
dist_m[!is.finite(dist_m)] <- max(dist_m[is.finite(dist_m)]) * 1.5
sil <- silhouette(as.integer(memb), as.dist(dist_m))
S_by <- tapply(sil[, "sil_width"], memb, mean)
cat(sprintf("Modularity Q = %.3f; mean silhouette S = %.3f; clusters = %d\n",
            Q, mean(sil[, "sil_width"]), length(unique(memb))))

# ---- 5. LLR cluster labels (Dunning G2 on keyword terms) -------------------
paper_cluster <- vapply(terms, function(z) {
  m <- memb[intersect(z, names(memb))]
  if (!length(m)) NA_integer_ else as.integer(names(sort(-table(m)))[1])
}, 0L)
llr_label <- function(cid) {
  inn <- terms[which(paper_cluster == cid)]; out <- terms[which(paper_cluster != cid)]
  ti <- table(unlist(inn)); to <- table(unlist(out))
  cand <- names(memb)[memb == cid]
  N1 <- length(inn); N2 <- length(out)
  g2 <- vapply(cand, function(w) {
    a <- if (w %in% names(ti)) ti[[w]] else 0; b <- if (w %in% names(to)) to[[w]] else 0
    e1 <- (a + b) * N1 / (N1 + N2); e2 <- (a + b) * N2 / (N1 + N2)
    2 * (ifelse(a > 0, a * log(a / e1), 0) + ifelse(b > 0, b * log(b / e2), 0)) *
      ifelse(a / N1 > b / N2, 1, -1)
  }, 0)
  names(sort(g2, decreasing = TRUE))[1:min(3, length(g2))]
}
cids <- sort(unique(memb))
lab <- vapply(cids, function(c) paste(llr_label(c), collapse = "; "), "")
cl_tab <- data.frame(cluster = cids, size = as.integer(table(memb)[as.character(cids)]),
                     silhouette = round(as.numeric(S_by[as.character(cids)]), 3),
                     mean_year = round(as.numeric(tapply(median_year[names(memb)], memb, mean)[as.character(cids)]), 1),
                     label_LLR = lab)
print(cl_tab)

# ---- 6. node table with Kleinberg burst flag ------------------------------
bu <- read.csv("tables/citespace_like/keyword_bursts_ak.csv", stringsAsFactors = FALSE)
nt <- data.frame(id = names(memb), freq = freq[names(memb)], first_year = first_year[names(memb)], median_year = median_year[names(memb)],
                 cluster = as.integer(memb), stringsAsFactors = FALSE)
nt$burst_strength <- bu$strength[match(nt$id, norm_term(bu$keyword))]
dir.create("tables/citespace_like", showWarnings = FALSE, recursive = TRUE)
write.csv(nt, "tables/citespace_like/nodes.csv", row.names = FALSE)
write.csv(cl_tab, "tables/citespace_like/clusters.csv", row.names = FALSE)
writeLines(sprintf("Q=%.4f; S=%.4f; nodes=%d; edges=%d", Q, mean(sil[, "sil_width"]), vcount(g), ecount(g)),
           "tables/citespace_like/network_summary.txt")

# ---- 7. exports for Gephi and VOSviewer -----------------------------------
dir.create("gephi", showWarnings = FALSE); dir.create("vosviewer", showWarnings = FALSE)
ed <- as_data_frame(g, "edges")
write.csv(data.frame(Id = nt$id, Label = nt$id, Frequency = nt$freq, FirstYear = nt$first_year, MedianYear = nt$median_year,
                     Cluster = nt$cluster), "gephi/nodes.csv", row.names = FALSE)
write.csv(data.frame(Source = ed$from, Target = ed$to, Weight = ed$weight, Type = "Undirected"),
          "gephi/edges.csv", row.names = FALSE)
write_graph(set_vertex_attr(set_vertex_attr(g, "cluster", value = memb[V(g)$name]),
            "freq", value = freq[V(g)$name]), "gephi/keyword_network.graphml", "graphml")
idx <- setNames(seq_len(vcount(g)), V(g)$name)
write.table(data.frame(id = idx, label = V(g)$name, cluster = memb[V(g)$name],
                       weight = freq[V(g)$name], score_year = median_year[V(g)$name]),
            "vosviewer/map.txt", sep = "\t", quote = FALSE, row.names = FALSE)
write.table(data.frame(idx[ed$from], idx[ed$to], ed$weight),
            "vosviewer/network.txt", sep = "\t", quote = FALSE, row.names = FALSE, col.names = FALSE)

# ---- 8. timeline view --------------------------------------------------------
top_cl <- head(cl_tab[order(-cl_tab$size), ], n_cluster_show)
nt2 <- nt[nt$cluster %in% top_cl$cluster, ]
nt2$row <- match(nt2$cluster, top_cl$cluster)
nt2$cl_f <- factor(nt2$cluster, levels = top_cl$cluster)
nt2 <- nt2[order(nt2$cluster, -nt2$freq), ]
nt2$lab <- ave(nt2$freq, nt2$cluster, FUN = function(v) rank(-v, ties.method = "first")) <= label_per_cluster
# jitter within a row to separate nodes sharing a year
set.seed(1); nt2$yj <- nt2$row + runif(nrow(nt2), -0.28, 0.28)
pos <- setNames(seq_len(nrow(nt2)), nt2$id)
ed2 <- ed[ed$from %in% nt2$id & ed$to %in% nt2$id, ]
e <- data.frame(x = nt2$median_year[match(ed2$from, nt2$id)], y = nt2$yj[match(ed2$from, nt2$id)],
                xe = nt2$median_year[match(ed2$to, nt2$id)], ye = nt2$yj[match(ed2$to, nt2$id)])
ylab <- paste0("#", top_cl$cluster, " ", vapply(strsplit(top_cl$label_LLR, "; "), `[`, "", 1))
p <- ggplot() +
  geom_segment(data = e, aes(x = x, y = y, xend = xe, yend = ye), colour = "grey70",
               linewidth = 0.15, alpha = 0.6) +
  geom_point(data = nt2, aes(median_year, yj, size = freq, fill = cl_f), shape = 21,
             colour = "black", stroke = line_width, alpha = 0.9) +
  geom_text_repel(data = subset(nt2, lab), aes(median_year, yj, label = tolower(id)),
                  family = font_family, size = pt_mm, max.overlaps = Inf, seed = 1,
                  box.padding = 0.2, segment.size = 0.15) +
  scale_fill_manual(values = wong[seq_len(nrow(top_cl))], guide = "none") +
  scale_size(range = c(0.8, 6), name = "Frequency") +
  scale_x_continuous(breaks = 2014:2026, limits = c(floor(min(nt2$median_year)) - 0.5, 2026.5)) +
  scale_y_reverse(breaks = seq_len(nrow(top_cl)), labels = ylab) +
  labs(x = "Median year of occurrence", y = NULL) +
  theme_bw(base_family = font_family, base_size = text_size_pt) +
  theme(text = element_text(size = text_size_pt), axis.text = element_text(size = text_size_pt, colour = "black"),
        axis.title = element_text(size = text_size_pt), legend.text = element_text(size = text_size_pt),
        legend.title = element_text(size = text_size_pt),
        panel.grid.major.x = element_line(colour = "grey90", linewidth = line_width),
        panel.grid.major.y = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_rect(colour = "black", linewidth = line_width),
        plot.title = element_blank(), plot.subtitle = element_blank(),
        plot.background = element_rect(fill = NA, colour = NA),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
ggsave("figures/fig9_timeline.pdf", p, width = width_double, height = 120, units = "mm", device = "pdf")
cat("Timeline written.\n")
