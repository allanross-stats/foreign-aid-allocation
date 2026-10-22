# 05_aid_growth.R
# Does aid raise economic growth, and only in countries with good policies?
# Burnside & Dollar (2000) found that aid boosts growth when policies are good,
# which became the basis for "allocate aid to good-policy countries" rules.
# Easterly, Levine & Roodman (2004) re-ran it with more data and the result
# disappeared. This script re-estimates the model on 1996-2023 data and then
# checks how fragile the answer is by re-running it under 48 reasonable
# variations (a specification curve).
#
#   growth = log initial income + aid/GNI + policy + aid x policy + period effects
#
# Data are averaged over 4-year periods to smooth out business cycles.

source("R/00_setup.R")
panel <- readRDS(PANEL_FILE)

periods <- data.frame(year = 1996:2023, period = paste0(1996 + 4 * ((1996:2023 - 1996) %/% 4), "-",
                                                         1999 + 4 * ((1996:2023 - 1996) %/% 4)))
pp <- panel |>
  filter(developing) |>
  inner_join(periods, by = "year") |>
  group_by(iso3c, country, region, income, period) |>
  summarise(growth = mean(gdp_pc_grow, na.rm = TRUE),
            init_gdp = first(gdp_pc[!is.na(gdp_pc)]),
            aid_gni = mean(oda_gni, na.rm = TRUE),
            inflation = mean(inflation, na.rm = TRUE),
            trade = mean(trade, na.rm = TRUE),
            gov_cons = mean(gov_cons, na.rm = TRUE),
            n_years = sum(!is.na(gdp_pc_grow)), .groups = "drop") |>
  filter(n_years >= 3, is.finite(growth), !is.na(init_gdp), is.finite(aid_gni),
         is.finite(inflation), is.finite(trade), is.finite(gov_cons), inflation > -50) |>
  mutate(log_init = log(init_gdp))

zs <- function(x) (x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE)
pp <- pp |>
  mutate(infl_bad = log1p(pmax(inflation, 0) / 100),
         policy_2 = zs(-infl_bad) + zs(trade),                          # low inflation + openness
         policy_3 = zs(-infl_bad) + zs(trade) + zs(-gov_cons))          # + smaller government consumption
message("Country-periods: ", nrow(pp), "; countries: ", n_distinct(pp$iso3c))

fit_spec <- function(policy, sample, outliers, controls, aid_sq) {
  d <- pp
  if (sample == "Low & lower-middle income") d <- d |> filter(income %in% c("Low income", "Lower middle income"))
  if (outliers == "Drop aid > 25% of GNI") d <- d |> filter(aid_gni <= 25)
  d$policy <- d[[policy]]
  rhs <- "log_init + aid_gni * policy + factor(period)"
  if (controls == "Region effects") rhs <- paste(rhs, "+ region")
  if (aid_sq == "Aid squared") rhs <- paste(rhs, "+ I(aid_gni^2)")
  m <- lm(as.formula(paste("growth ~", rhs)), data = d)
  ct <- clustered(m, d$iso3c)
  get <- function(term) ct[ct$Term == term, ]
  inter <- get("aid_gni:policy"); aid <- get("aid_gni")
  data.frame(policy = policy, sample = sample, outliers = outliers, controls = controls, aid_sq = aid_sq,
             n = nrow(d), aid_coef = aid$Estimate, aid_p = aid$p,
             inter_coef = inter$Estimate, inter_se = inter$SE, inter_p = inter$p)
}

grid <- expand.grid(policy = c("policy_2", "policy_3"),
                    sample = c("All developing", "Low & lower-middle income"),
                    outliers = c("Keep all", "Drop aid > 25% of GNI"),
                    controls = c("Period effects only", "Region effects"),
                    aid_sq = c("Linear aid", "Aid squared"),
                    stringsAsFactors = FALSE)
# 3 policy-index variants would make 48; we add a third: inflation only
grid <- rbind(grid, transform(grid[grid$policy == "policy_2", ], policy = "policy_infl"))
pp$policy_infl <- zs(-pp$infl_bad)

specs <- bind_rows(lapply(seq_len(nrow(grid)), function(i) do.call(fit_spec, as.list(grid[i, ]))))
specs <- specs |> arrange(inter_coef) |> mutate(rank = row_number(),
                                                sig = ifelse(inter_p < 0.05, ifelse(inter_coef > 0, "Positive, p < 0.05", "Negative, p < 0.05"), "Not significant"))
save_table(r4(specs), "aid_growth_specification_curve")

base <- specs |> filter(policy == "policy_2", sample == "All developing", outliers == "Keep all",
                        controls == "Period effects only", aid_sq == "Linear aid")
spec_summary <- data.frame(
  Specifications = nrow(specs),
  Positive_significant = sum(specs$sig == "Positive, p < 0.05"),
  Negative_significant = sum(specs$sig == "Negative, p < 0.05"),
  Not_significant = sum(specs$sig == "Not significant"),
  Baseline_interaction = base$inter_coef, Baseline_p = base$inter_p,
  Baseline_aid_coef = base$aid_coef, Baseline_aid_p = base$aid_p, Baseline_n = base$n)
print(r4(spec_summary))
save_table(r4(spec_summary), "aid_growth_summary")

p <- ggplot(specs, aes(rank, inter_coef, color = sig)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey40") +
  geom_errorbar(aes(ymin = inter_coef - 1.96 * inter_se, ymax = inter_coef + 1.96 * inter_se), width = 0, alpha = 0.6) +
  geom_point(size = 2) +
  scale_color_manual(values = c("Positive, p < 0.05" = "#2166AC", "Negative, p < 0.05" = "#B2182B",
                                "Not significant" = "grey60"), name = NULL) +
  labs(title = "Does aid work better with good policies? It depends on the specification",
       subtitle = sprintf("Aid x policy coefficient across %d reasonable model variations, sorted (95%% CIs, clustered by country)", nrow(specs)),
       x = "Specification (sorted by estimate)", y = "Aid x policy interaction")
save_plot(p, "05_aid_growth_specification_curve", w = 9, h = 4.8)
