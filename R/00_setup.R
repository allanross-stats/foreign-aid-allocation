# 00_setup.R
# Packages, indicator codes, paths and plot style shared by every script.

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(scales)
  library(sandwich)   # cluster-robust standard errors
  library(lmtest)
})
set.seed(2025)

# World Bank indicator codes (World Development Indicators and the Worldwide
# Governance Indicators, both available through the WDI package)
INDICATORS <- c(
  us_aid       = "DC.DAC.USAL.CD",     # net bilateral aid from the United States, current US$
  oda_gni      = "DT.ODA.ODAT.GN.ZS",  # net ODA received, % of GNI (all donors)
  gdp_pc_ppp   = "NY.GDP.PCAP.PP.KD",  # GDP per capita, PPP, constant international $
  gdp_pc       = "NY.GDP.PCAP.KD",     # GDP per capita, constant US$
  gdp_pc_grow  = "NY.GDP.PCAP.KD.ZG",  # GDP per capita growth, annual %
  gni_pc_atlas = "NY.GNP.PCAP.CD",     # GNI per capita, Atlas method (income classification)
  pop          = "SP.POP.TOTL",        # population
  infant_mort  = "SP.DYN.IMRT.IN",     # infant mortality per 1,000 live births
  inflation    = "FP.CPI.TOTL.ZG",     # CPI inflation, annual %
  trade        = "NE.TRD.GNFS.ZS",     # trade, % of GDP (openness)
  gov_cons     = "NE.CON.GOVT.ZS",     # government consumption, % of GDP
  cc_est       = "CC.EST",             # Control of Corruption, estimate
  cc_se        = "CC.STD.ERR",         # Control of Corruption, standard error
  ge_est       = "GE.EST",             # Government Effectiveness, estimate
  rl_est       = "RL.EST",             # Rule of Law, estimate
  va_est       = "VA.EST"              # Voice and Accountability, estimate
)
START_YEAR <- 1996
END_YEAR <- 2024

RAW_FILE <- "data/wdi_raw.csv"
PANEL_FILE <- "data/panel.rds"
FIG_DIR <- "output/figures"
TAB_DIR <- "output/tables"
dir.create(FIG_DIR, recursive = TRUE, showWarnings = FALSE)
dir.create(TAB_DIR, recursive = TRUE, showWarnings = FALSE)

theme_set(theme_minimal(base_size = 11) +
            theme(plot.title = element_text(face = "bold"),
                  panel.grid.minor = element_blank(),
                  legend.position = "bottom"))
save_plot <- function(p, name, w = 8, h = 4.5) {
  ggsave(file.path(FIG_DIR, paste0(name, ".png")), p, width = w, height = h, dpi = 150, bg = "white")
}
save_table <- function(df, name) {
  write.csv(df, file.path(TAB_DIR, paste0(name, ".csv")), row.names = FALSE)
}
r4 <- function(df) df |> mutate(across(where(is.numeric), ~ round(.x, 4)))

# Coefficient table with standard errors clustered by country
clustered <- function(model, cluster) {
  ct <- coeftest(model, vcov = vcovCL(model, cluster = cluster))
  data.frame(Term = rownames(ct), Estimate = ct[, 1], SE = ct[, 2], t = ct[, 3], p = ct[, 4],
             row.names = NULL)
}
