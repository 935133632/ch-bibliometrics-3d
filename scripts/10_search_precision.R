# 08_search_precision.R: how many retrieved records mention clonal hematopoiesis in
# title / abstract / author keywords (WoS TS also matches Keywords Plus, which can add noise)
M <- readRDS("data/clean/M_wos_final.rds")
pat <- "CLONAL HAEMATOPOIESIS|CLONAL HEMATOPOIESIS|CLONAL HAEMOPOIESIS|CLONAL HEMOPOIESIS|CLONAL CYTOPENIA|CLONAL HEMATOPOIETIC|CHIP|CCUS|ARCH"
up <- function(x) toupper(ifelse(is.na(x), "", x))
core <- "CLONAL HAEMATOPOIESIS|CLONAL HEMATOPOIESIS|CLONAL HAEMOPOIESIS|CLONAL HEMOPOIESIS|CLONAL CYTOPENIA"
in_ti <- grepl(core, up(M$TI)); in_ab <- grepl(core, up(M$AB)); in_de <- grepl(core, up(M$DE))
any_core <- in_ti | in_ab | in_de
cat(sprintf("Records: %d\n", nrow(M)))
cat(sprintf("Core phrase in title: %d (%.1f%%)\n", sum(in_ti), 100 * mean(in_ti)))
cat(sprintf("Core phrase in title/abstract/author keywords: %d (%.1f%%)\n", sum(any_core), 100 * mean(any_core)))
only_plus <- !any_core
cat(sprintf("Matched only through other fields (possible noise): %d (%.1f%%)\n", sum(only_plus), 100 * mean(only_plus)))
set.seed(7)
smp <- M[only_plus, c("PY", "SO", "TI")]
smp <- smp[sample(nrow(smp), min(25, nrow(smp))), ]
write.csv(smp, "tables/search_noise_sample.csv", row.names = FALSE)
print(smp[, c("PY", "TI")], right = FALSE)
