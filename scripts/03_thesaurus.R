# 03_thesaurus.R: apply keyword / institution thesauri, rebuild top-lists
suppressPackageStartupMessages({ library(openxlsx) })

M <- readRDS("data/clean/M_wos_clean.rds")

th_kw   <- read.csv("thesaurus/keyword_thesaurus.csv",     stringsAsFactors = FALSE)
stop_kw <- read.csv("thesaurus/keyword_stoplist.csv",      stringsAsFactors = FALSE)$term
th_inst <- read.csv("thesaurus/institution_thesaurus.csv", stringsAsFactors = FALSE)
ex_inst <- read.csv("thesaurus/institution_exclude.csv",   stringsAsFactors = FALSE)$term

# map each ';'-separated item, drop stop terms, keep each term once per paper
clean_field <- function(v, th, drop = character(0)) {
  map <- setNames(th$to, th$from)
  vapply(v, function(s) {
    if (is.na(s)) return(NA_character_)
    z <- toupper(trimws(gsub("<[^>]+>", "", strsplit(s, ";")[[1]])))   # drop <italic> etc.
    z <- z[nzchar(z)]
    hit <- z %in% names(map)
    z[hit] <- map[z[hit]]
    z <- unique(z[!z %in% drop])
    if (length(z) == 0) NA_character_ else paste(z, collapse = ";")
  }, character(1), USE.NAMES = FALSE)
}

M$DE_clean <- clean_field(M$DE, th_kw, stop_kw)
M$ID_clean <- clean_field(M$ID, th_kw, stop_kw)
M$C3_clean <- clean_field(M$C3, th_inst, ex_inst)

top_tab <- function(v, n = 30) {
  z <- trimws(unlist(strsplit(v[!is.na(v)], ";")))
  as.data.frame(head(sort(table(z), decreasing = TRUE), n), stringsAsFactors = FALSE)
}

wb <- createWorkbook()
add <- function(name, df) { addWorksheet(wb, name); writeData(wb, name, df) }
add("institutions_clean",  top_tab(M$C3_clean, 30))
add("author_kw_clean",     top_tab(M$DE_clean, 60))
add("keywords_plus_clean", top_tab(M$ID_clean, 60))
saveWorkbook(wb, "tables/descriptive_tables_clean.xlsx", overwrite = TRUE)

saveRDS(M, "data/clean/M_wos_final.rds")
cat("Institutions (top 12):\n"); print(head(top_tab(M$C3_clean), 12))
cat("Author keywords (top 20):\n"); print(head(top_tab(M$DE_clean), 20))
