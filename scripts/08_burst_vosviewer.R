# 08_burst_vosviewer.R: CiteSpace-equivalent keyword bursts + VOSviewer overlay inputs
#   Replaces the CiteSpace run (Basic edition caps networks at 300 nodes; g-index k = 25 gives 691).
#   1. keywords = author keywords (DE) only, thesaurus + stoplist (same as 07_citespace_like.R)
#      (fig8_keyword_bursts_k25 was an earlier DE + ID run; fig8b is the author-keyword version)
#   2. burst candidates = union of g-index (k = 25) selections per 1-year slice, uncapped
#      (the node set CiteSpace would have built)
#   3. Kleinberg two-state burst detection, s = 2, gamma = 1, minimum duration = 1 slice
#   4. optional: VOSviewer map/network inputs (off; fig10b/fig12b are built in VOSviewer from
#      author keywords with vosviewer/thesaurus_keywords.txt)
suppressPackageStartupMessages({ library(ggplot2) })

# ============================ PARAMETER BLOCK ============================
years_all <- 2014:2026
g_k       <- 25            # g-index scale factor k
burst_s   <- 2             # Kleinberg rate ratio between burst and base state
burst_gamma <- 1           # transition cost multiplier (CiteSpace gamma)
min_duration <- 1          # minimum burst length in slices
n_burst_show <- 25         # rows in the burst figure
vos_min_freq <- 10         # VOSviewer map: minimum occurrences of a keyword
export_vos   <- FALSE      # write vosviewer/overlay_map.txt + overlay_network.txt
text_size_pt <- 7; font_family <- "Times"; line_width <- 0.3
width_double <- 180
# =========================================================================

M <- readRDS("data/clean/M_wos_final.rds")
M <- M[!is.na(M$PY) & M$PY %in% years_all, ]
norm_term <- function(x) trimws(gsub("[[:space:]]+", " ", gsub("[-–]", " ", toupper(gsub("<[^>]+>", "", x)))))
th <- read.csv("thesaurus/keyword_thesaurus.csv", stringsAsFactors = FALSE)
th_map <- setNames(norm_term(th$to), norm_term(th$from))
stops  <- norm_term(read.csv("thesaurus/keyword_stoplist.csv", stringsAsFactors = FALSE)$term)
terms <- lapply(seq_len(nrow(M)), function(i) {
  z <- strsplit(M$DE[i], ";")[[1]]   # author keywords only
  z <- norm_term(z[!is.na(z)]); z <- z[nzchar(z)]
  hit <- z %in% names(th_map); z[hit] <- th_map[z[hit]]
  unique(z[!z %in% stops])
})
yr <- M$PY
cat("Records:", length(terms), "\n")

# ---- 1. g-index candidates per slice (uncapped) ---------------------------
g_select <- function(tb, k) {
  f <- sort(setNames(as.integer(tb), names(tb)), decreasing = TRUE)
  g <- which(seq_along(f)^2 <= k * cumsum(f))
  names(f)[seq_len(if (length(g)) max(g) else 0)]
}
sel <- lapply(years_all, function(y) g_select(table(unlist(terms[yr == y])), g_k))
cand <- unique(unlist(sel))
cat("g-index candidate keywords (k = 25):", length(cand), "\n")

# ---- 2. yearly document counts per candidate ------------------------------
d <- as.integer(table(factor(yr, levels = years_all)))
R <- matrix(0L, length(cand), length(years_all), dimnames = list(cand, years_all))
for (i in seq_along(terms)) {
  z <- intersect(terms[[i]], cand)
  if (length(z)) { j <- yr[i] - years_all[1] + 1; R[z, j] <- R[z, j] + 1L }
}

# ---- 3. Kleinberg two-state automaton -------------------------------------
trans_cost <- burst_gamma * log(length(years_all))
kleinberg <- function(r) {
  p0 <- sum(r) / sum(d); p1 <- min(burst_s * p0, 0.9999)
  c0 <- -dbinom(r, d, p0, log = TRUE); c1 <- -dbinom(r, d, p1, log = TRUE)
  n <- length(r); cost <- matrix(Inf, n, 2); from <- matrix(0L, n, 2)
  cost[1, ] <- c(c0[1], c1[1] + trans_cost)
  for (t in 2:n) for (j in 1:2) {
    stay <- cost[t - 1, j]; move <- cost[t - 1, 3 - j] + (j == 2) * trans_cost
    from[t, j] <- if (stay <= move) j else 3L - j
    cost[t, j] <- min(stay, move) + c(c0[t], c1[t])[j]
  }
  st <- integer(n); st[n] <- which.min(cost[n, ])
  for (t in n:2) st[t - 1] <- from[t, st[t]]
  list(state = st, gain = c0 - c1)
}
res <- list()
for (kw in cand) {
  r <- R[kw, ]; b <- kleinberg(r)
  if (!any(b$state == 2)) next
  runs <- rle(b$state == 2); end <- cumsum(runs$lengths); start <- end - runs$lengths + 1
  for (i in which(runs$values & runs$lengths >= min_duration)) {
    res[[length(res) + 1]] <- data.frame(
      keyword = tolower(kw), year_first = years_all[which(r > 0)[1]], total = sum(r),
      strength = round(sum(b$gain[start[i]:end[i]]), 2),
      begin = years_all[start[i]], end = years_all[end[i]])
  }
}
bursts <- do.call(rbind, res)
bursts <- bursts[order(-bursts$strength), ]
dir.create("tables/citespace_like", showWarnings = FALSE, recursive = TRUE)
write.csv(bursts, "tables/citespace_like/keyword_bursts_ak.csv", row.names = FALSE)
cat("Bursts detected:", nrow(bursts), "\n"); print(head(bursts, n_burst_show), row.names = FALSE)

# ---- 4. CiteSpace-style burst figure --------------------------------------
top <- head(bursts[!duplicated(bursts$keyword), ], n_burst_show)   # strongest burst per keyword
top <- top[order(top$begin, -top$strength), ]
top$row <- rev(seq_len(nrow(top)))
cells <- do.call(rbind, lapply(seq_len(nrow(top)), function(i)
  data.frame(row = top$row[i], year = years_all,
             state = ifelse(years_all >= top$begin[i] & years_all <= top$end[i], "burst",
                     ifelse(years_all >= top$year_first[i], "present", "absent")))))
x_kw <- 2008.5; x_st <- 2028.2
p <- ggplot() +
  geom_tile(data = cells, aes(year, row, fill = state), height = 0.55, width = 1) +
  scale_fill_manual(values = c(absent = "#E3EAF2", present = "#7DA7D9", burst = "#D55E00"),
                    guide = "none") +
  geom_text(data = top, aes(x_kw, row, label = keyword), hjust = 0, family = font_family,
            size = text_size_pt / .pt) +
  geom_text(data = top, aes(x_st, row, label = sprintf("%.2f", strength)), hjust = 1,
            family = font_family, size = text_size_pt / .pt) +
  geom_text(data = top, aes(x_st + 0.4, row, label = paste0(begin, "–", end)), hjust = 0,
            family = font_family, size = text_size_pt / .pt) +
  annotate("text", x = c(x_kw, x_st, x_st + 0.4), y = nrow(top) + 1,
           label = c("Keyword", "Strength", "Burst"), hjust = c(0, 1, 0),
           family = font_family, fontface = "bold", size = text_size_pt / .pt) +
  scale_x_continuous(breaks = years_all, limits = c(x_kw, 2030.4), expand = c(0, 0)) +
  scale_y_continuous(limits = c(0.4, nrow(top) + 1.5), expand = c(0, 0)) +
  labs(x = NULL, y = NULL) +
  theme_void(base_family = font_family, base_size = text_size_pt) +
  theme(axis.text.x = element_text(size = text_size_pt, angle = 90, vjust = 0.5, colour = "black"),
        plot.background = element_rect(fill = "white", colour = NA))
ggsave("figures/fig8b_keyword_bursts.pdf", p, width = width_double, height = 125, units = "mm", device = "pdf")
ggsave("figures/fig8b_keyword_bursts.png", p, width = width_double, height = 125, units = "mm", dpi = 400)

# ---- 5. VOSviewer overlay map (average publication year) ------------------
if (export_vos) {
tot <- table(unlist(terms))
vk <- names(tot)[tot >= vos_min_freq]
mean_year <- vapply(vk, function(k) mean(yr[vapply(terms, function(z) k %in% z, TRUE)]), 0)
idx <- setNames(seq_along(vk), vk)
ed <- list()
for (z in terms) {
  z <- sort(intersect(z, vk)); if (length(z) < 2) next
  pr <- utils::combn(z, 2); ed[[length(ed) + 1]] <- paste(idx[pr[1, ]], idx[pr[2, ]])
}
ec <- table(unlist(ed)); ij <- do.call(rbind, strsplit(names(ec), " "))
dir.create("vosviewer", showWarnings = FALSE)
map <- data.frame(id = idx, label = tolower(vk), `weight<Occurrences>` = as.integer(tot[vk]),
                  `score<Avg. pub. year>` = round(mean_year, 2), check.names = FALSE)
write.table(map, "vosviewer/overlay_map.txt", sep = "\t", quote = FALSE, row.names = FALSE,
            fileEncoding = "UTF-8")
write.table(data.frame(as.integer(ij[, 1]), as.integer(ij[, 2]), as.integer(ec)),
            "vosviewer/overlay_network.txt", sep = "\t", quote = FALSE, row.names = FALSE,
            col.names = FALSE)
cat(sprintf("VOSviewer overlay: %d keywords, %d links\n", length(vk), length(ec)))
}
