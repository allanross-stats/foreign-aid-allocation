# 02_clean.R
# Keeps individual countries (drops regional aggregates), builds per-capita
# aid and the analysis variables, and saves a country-year panel.

source("R/00_setup.R")
raw <- read.csv(RAW_FILE, stringsAsFactors = FALSE)

panel <- raw |>
  filter(region != "Aggregates", !is.na(region), region != "") |>
  transmute(
    country, iso3c, year, region, income,
    us_aid, oda_gni, gdp_pc_ppp, gdp_pc, gdp_pc_grow, gni_pc_atlas, pop,
    infant_mort, inflation, trade, gov_cons, cc_est, cc_se, ge_est, rl_est, va_est,
    us_aid_pc = us_aid / pop,                       # US aid per person, current US$
    developing = income %in% c("Low income", "Lower middle income", "Upper middle income")
  ) |>
  arrange(iso3c, year)

saveRDS(panel, PANEL_FILE)
message("Countries: ", n_distinct(panel$iso3c), "; country-years: ", nrow(panel))
message("Developing countries with any US aid record: ",
        n_distinct(panel$iso3c[panel$developing & !is.na(panel$us_aid)]))
