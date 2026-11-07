# 01_import.R: merge WoS plain-text exports into one bibliometrix data frame
suppressPackageStartupMessages(library(bibliometrix))

raw_dir   <- "data/raw"
clean_dir <- "data/clean"
files <- list.files(raw_dir, pattern = "^wos_.*\\.txt$", full.names = TRUE)
stopifnot(length(files) > 0)

M <- convert2df(file = files, dbsource = "wos", format = "plaintext")
cat("Imported records:", nrow(M), "\n")

# de-duplicate on WoS accession number
M <- M[!duplicated(M$UT), ]
cat("After de-duplication:", nrow(M), "\n")

# 2026 is a partial year (search date 2026-10-16); kept per study design
cat("Year range:", range(M$PY, na.rm = TRUE), "\n")
print(table(M$DT))

saveRDS(M, file.path(clean_dir, "M_wos.rds"))
