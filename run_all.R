# run_all.R
# Reproduces every table and figure.
#   Rscript run_all.R            (uses data/wdi_raw.csv if present)
#   Rscript run_all.R --refresh  (re-downloads from the World Bank first)

args <- commandArgs(trailingOnly = TRUE)
if ("--refresh" %in% args || !file.exists("data/wdi_raw.csv")) source("R/01_get_data.R")

for (s in c("R/02_clean.R", "R/03_corruption_hurdle.R", "R/04_aid_need.R",
            "R/05_aid_growth.R", "R/06_allocation_optimizer.R")) {
  message("\n==== ", s, " ====")
  source(s, local = new.env())
}
message("\nDone. Tables in output/tables, figures in output/figures.")
