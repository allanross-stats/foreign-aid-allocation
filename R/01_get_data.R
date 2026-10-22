# 01_get_data.R
# Downloads every indicator for every country from the World Bank and saves
# data/wdi_raw.csv, so the rest of the analysis runs offline.
#
# Most indicators come from the World Development Indicators (WDI package).
# The governance scores (Worldwide Governance Indicators) now live in a separate
# World Bank database (source 3) under codes that have changed over time, so
# they are requested directly from the World Bank API, trying old and new codes.

source("R/00_setup.R")
for (p in c("WDI", "jsonlite")) if (!requireNamespace(p, quietly = TRUE)) install.packages(p)

wdi_codes <- INDICATORS[!names(INDICATORS) %in% c("oda_gni", "cc_est", "cc_se", "ge_est", "rl_est", "va_est")]
raw <- WDI::WDI(country = "all", indicator = wdi_codes, start = START_YEAR, end = END_YEAR, extra = TRUE)

get_wb <- function(code, source = NULL) {
  url <- paste0("https://api.worldbank.org/v2/country/all/indicator/", code,
                "?format=json&per_page=30000&date=", START_YEAR, ":", END_YEAR,
                if (!is.null(source)) paste0("&source=", source) else "")
  j <- tryCatch(jsonlite::fromJSON(url), error = function(e) NULL)
  if (is.null(j) || length(j) < 2 || is.null(j[[2]]) || length(j[[2]]) == 0) return(NULL)
  d <- j[[2]]
  out <- data.frame(iso3c = d$countryiso3code, year = as.integer(d$date), value = d$value)
  out[out$iso3c != "", ]
}
add_series <- function(raw, name, codes, source) {
  for (cd in codes) {
    d <- get_wb(cd, source)
    if (!is.null(d) && sum(!is.na(d$value)) > 0) {
      message(name, ": found as ", cd)
      names(d)[3] <- name
      return(merge(raw, d, by = c("iso3c", "year"), all.x = TRUE))
    }
  }
  warning(name, " not found"); raw
}

wgi <- list(cc_est = c("CC.EST", "GOV_WGI_CC.EST"), ge_est = c("GE.EST", "GOV_WGI_GE.EST"),
            rl_est = c("RL.EST", "GOV_WGI_RL.EST"), va_est = c("VA.EST", "GOV_WGI_VA.EST"))
for (nm in names(wgi)) raw <- add_series(raw, nm, wgi[[nm]], source = 3)

# Control of Corruption standard error: look up its current code by name
lst <- jsonlite::fromJSON("https://api.worldbank.org/v2/sources/3/indicators?format=json&per_page=1000")[[2]]
se_code <- lst$id[grepl("corruption", lst$name, ignore.case = TRUE) &
                  grepl("standard error|std", lst$name, ignore.case = TRUE)][1]
raw <- add_series(raw, "cc_se", c(se_code, "CC.STD.ERR"), source = 3)

raw <- add_series(raw, "oda_gni", "DT.ODA.ODAT.GN.ZS", source = 2)

write.csv(raw, RAW_FILE, row.names = FALSE)
message("Saved ", RAW_FILE, ": ", nrow(raw), " rows")
