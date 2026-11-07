# 04_networks.R: co-occurrence / collaboration / co-citation networks, thematic maps
suppressPackageStartupMessages({ library(bibliometrix); library(openxlsx) })

M <- readRDS("data/clean/M_wos_final.rds")
M$DE_raw <- M$DE
M$DE <- M$DE_clean          # bibliometrix reads DE / ID / C3
M$ID <- M$ID_clean

dir.create("figures", showWarnings = FALSE)
dir.create("tables/network", showWarnings = FALSE, recursive = TRUE)
dir.create("vosviewer", showWarnings = FALSE)

W <- 180 / 25.4                      # 180 mm in inches
pdf_dev <- function(f, h = 7) pdf(f, width = W, height = h, family = "Times")

node_table <- function(NP, file) {
  df <- data.frame(NP$cluster_res)
  write.csv(df, file, row.names = FALSE)
}

# ---- 1. author-keyword co-occurrence ----------------------------------------
NET <- biblioNetwork(M, analysis = "co-occurrences", network = "author_keywords", sep = ";")
pdf_dev("figures/fig3_keyword_cooccurrence.pdf")
NP <- networkPlot(NET, n = 50, edges.min = 15, Title = NULL, type = "fruchterman", size = TRUE,
                  size.cex = TRUE, remove.multiple = FALSE, labelsize = 0.6,
                  edgesize = 3, cluster = "louvain", curved = FALSE)
dev.off()
node_table(NP, "tables/network/keyword_clusters.csv")
igraph::write_graph(NP$graph, "vosviewer/keyword_network.net", format = "pajek")

# ---- 2. country collaboration ------------------------------------------------
M2 <- metaTagExtraction(M, Field = "AU_CO", sep = ";")
NETc <- biblioNetwork(M2, analysis = "collaboration", network = "countries", sep = ";")
pdf_dev("figures/fig4_country_collaboration.pdf")
NPc <- networkPlot(NETc, n = 30, Title = NULL, type = "circle", size = TRUE,
                   remove.multiple = FALSE, labelsize = 0.8, cluster = "none")
dev.off()
node_table(NPc, "tables/network/country_nodes.csv")

# ---- 3. reference co-citation (top 50 cited references) ----------------------
NETr <- biblioNetwork(M, analysis = "co-citation", network = "references", sep = ";")
pdf_dev("figures/fig5_cocitation.pdf")
NPr <- networkPlot(NETr, n = 50, Title = NULL, type = "fruchterman", size = TRUE,
                   remove.multiple = FALSE, labelsize = 0.5, edgesize = 3,
                   cluster = "louvain")
dev.off()
node_table(NPr, "tables/network/cocitation_clusters.csv")

# ---- 4. thematic map and trend topics ----------------------------------------
Tm <- thematicMap(M, field = "DE", n = 250, minfreq = 5, stemming = FALSE,
                  size = 0.5, n.labels = 3, repel = TRUE)
pdf_dev("figures/fig6_thematic_map.pdf")
print(Tm$map)
dev.off()
write.csv(Tm$clusters, "tables/network/thematic_clusters.csv", row.names = FALSE)

Tt <- fieldByYear(M, field = "DE", timespan = c(2014, 2026), min.freq = 8,
                  n.items = 4, graph = FALSE)
pdf_dev("figures/fig7_trend_topics.pdf", h = 6)
print(Tt$graph)
dev.off()
write.csv(Tt$df, "tables/network/trend_topics.csv", row.names = FALSE)

cat("Done. Keyword clusters:\n")
print(table(NP$cluster_res$cluster))
