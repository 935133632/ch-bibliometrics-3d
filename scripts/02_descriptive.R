# 02_descriptive.R: descriptive bibliometric analysis (bibliometrix)
suppressPackageStartupMessages({
  library(bibliometrix); library(ggplot2); library(openxlsx)
})

M <- readRDS("data/clean/M_wos.rds")
M$PY[M$PY == 2027] <- 2026          # one early-access record dated 2027
M$PY <- as.integer(M$PY)

# ---- scope: the core phrase must appear in title, abstract or author keywords ----
# WoS TS= also matches Keywords Plus (derived from cited-reference titles), which
# retrieved 1,991 records that never mention the topic. Full set kept for reference.
core_pat <- "CLONAL HAEMATOPOIESIS|CLONAL HEMATOPOIESIS|CLONAL HAEMOPOIESIS|CLONAL HEMOPOIESIS|CLONAL CYTOPENIA"
up <- function(x) toupper(ifelse(is.na(x), "", x))
is_core <- grepl(core_pat, up(M$TI)) | grepl(core_pat, up(M$AB)) | grepl(core_pat, up(M$DE))
saveRDS(M, "data/clean/M_wos_all_clean.rds")
cat("All retrieved:", nrow(M), " core (TI/AB/AK):", sum(is_core), "\n")
M <- M[is_core, ]

dir.create("tables",  showWarnings = FALSE)
dir.create("figures", showWarnings = FALSE)

res <- biblioAnalysis(M, sep = ";")
S   <- summary(res, k = 20, pause = FALSE)

# ---- annual production ----------------------------------------------------
yr <- as.data.frame(table(Year = M$PY), stringsAsFactors = FALSE)
yr$Year <- as.integer(yr$Year)
yr$Cumulative <- cumsum(yr$Freq)
cit <- aggregate(TC ~ PY, data = M, FUN = sum)
yr$TotalCitations <- cit$TC[match(yr$Year, cit$PY)]

p <- ggplot(yr, aes(Year, Freq)) +
  geom_col(fill = "#4C78A8") +
  geom_col(data = subset(yr, Year == 2026), fill = "#BBBBBB") +
  geom_text(aes(label = Freq), vjust = -0.4, size = 3) +
  scale_x_continuous(breaks = 2014:2026) +
  labs(x = NULL, y = "Publications",
       caption = "2026 is a partial year (search date 2026-10-16)") +
  theme_classic(base_family = "serif", base_size = 10)
ggsave("figures/fig1_annual_production.pdf", p, width = 180, height = 90, units = "mm")

# ---- top lists --------------------------------------------------------------
top <- function(x, n = 20) head(sort(table(x), decreasing = TRUE), n)

split_field <- function(v) {
  v <- v[!is.na(v)]
  trimws(unlist(strsplit(v, ";")))
}

# count each country / institution once per paper
split_unique <- function(v) {
  v <- v[!is.na(v)]
  trimws(unlist(lapply(strsplit(v, ";"), function(z) unique(trimws(z)))))
}

countries <- metaTagExtraction(M, Field = "AU_CO", sep = ";")
country_tab <- as.data.frame(top(split_unique(countries$AU_CO), 25))

au_tab  <- as.data.frame(top(split_field(M$AU), 25))
so_tab  <- as.data.frame(top(M$SO, 20))
kw_tab  <- as.data.frame(top(split_field(M$DE), 40))

# institutions from affiliation field
af <- split_unique(M$C3)
inst_tab <- as.data.frame(top(af, 25))

most_cited <- M[order(-M$TC), c("AU", "TI", "SO", "PY", "TC", "DI")]
most_cited <- head(most_cited, 20)

# ---- export -----------------------------------------------------------------
wb <- createWorkbook()
add <- function(name, df) { addWorksheet(wb, name); writeData(wb, name, df) }
add("annual",        yr)
add("countries",     country_tab)
add("institutions",  inst_tab)
add("authors",       au_tab)
add("journals",      so_tab)
add("author_kw",     kw_tab)
add("most_cited",    most_cited)
saveWorkbook(wb, "tables/descriptive_tables.xlsx", overwrite = TRUE)

saveRDS(M, "data/clean/M_wos_clean.rds")
cat("Records:", nrow(M), " Years:", range(M$PY), "\n")
print(head(yr, 14)); print(head(country_tab, 10)); print(head(so_tab, 10))
