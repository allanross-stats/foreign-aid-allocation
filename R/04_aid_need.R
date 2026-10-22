# 04_aid_need.R
# Where does U.S. aid actually go? A country-year panel of developing
# countries, 2002-2023, regressing U.S. aid per person on measures of need,
# governance and size (in the spirit of Alesina & Dollar, 2000).
#
#   log(US aid per capita) = need + governance + log population + region + year
#
# Two parts, because many country-years receive little or nothing:
#   Part 1 (logit): does a country receive meaningful U.S. aid (> $1 per person)?
#   Part 2 (OLS):   among recipients, how much per person?
# Standard errors are clustered by country (each country appears many years).

source("R/00_setup.R")
panel <- readRDS(PANEL_FILE)

d <- panel |>
  filter(developing, year >= 2002, year <= 2023,
         !is.na(us_aid_pc), !is.na(gdp_pc_ppp), !is.na(infant_mort), !is.na(cc_est), !is.na(pop)) |>
  mutate(recipient = as.integer(us_aid_pc > 1),
         log_aid_pc = ifelse(us_aid_pc > 1, log(pmax(us_aid_pc, 1)), NA),
         log_gdp_pc = log(gdp_pc_ppp),
         log_imr = log(infant_mort),
         log_pop = log(pop),
         year_f = factor(year), region = factor(region))
message("Panel: ", n_distinct(d$iso3c), " countries, ", nrow(d), " country-years")

f_rhs <- "log_gdp_pc + log_imr + cc_est + log_pop + region + year_f"
m1 <- glm(as.formula(paste("recipient ~", f_rhs)), data = d, family = binomial)
d2 <- d |> filter(recipient == 1)
m2 <- lm(as.formula(paste("log_aid_pc ~", f_rhs)), data = d2)

keep <- c("log_gdp_pc", "log_imr", "cc_est", "log_pop")
labels <- c(log_gdp_pc = "Income (log GDP per capita)", log_imr = "Need (log infant mortality)",
            cc_est = "Control of corruption", log_pop = "Size (log population)")
t1 <- clustered(m1, d$iso3c) |> filter(Term %in% keep) |> mutate(Part = "1: Receives aid (logit)", Odds_ratio = exp(Estimate))
t2 <- clustered(m2, d2$iso3c) |> filter(Term %in% keep) |> mutate(Part = "2: Amount per person (log OLS)", Odds_ratio = NA)
need_tab <- bind_rows(t1, t2) |> mutate(Variable = labels[Term])
print(r4(need_tab))
save_table(r4(need_tab), "aid_need_regression")

# Elasticity interpretation for part 2: a 1% rise in X changes aid per person by b%
fit_info <- data.frame(Part2_n = nrow(d2), Part2_R2 = summary(m2)$r.squared,
                       Part1_n = nrow(d), Countries = n_distinct(d$iso3c))
save_table(r4(fit_info), "aid_need_fit")

# Small-country bias: the population coefficient. A value near -1 means
# aid per person falls almost one-for-one with size (each country gets a
# similar total regardless of population).

# ---- Figure: coefficient plot ---------------------------------------------
pd <- need_tab |> mutate(lo = Estimate - 1.96 * SE, hi = Estimate + 1.96 * SE,
                         Variable = factor(Variable, levels = rev(labels)))
p <- ggplot(pd, aes(Estimate, Variable)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey40") +
  geom_errorbarh(aes(xmin = lo, xmax = hi), height = 0.2, color = "grey45") +
  geom_point(aes(color = p < 0.05), size = 2.8) +
  facet_wrap(~Part, scales = "free_x") +
  scale_color_manual(values = c(`TRUE` = "#2166AC", `FALSE` = "grey60"),
                     labels = c(`TRUE` = "p < 0.05", `FALSE` = "not significant"), name = NULL) +
  labs(title = "What predicts U.S. aid to developing countries, 2002-2023?",
       subtitle = "Coefficients with 95% CIs (clustered by country); region and year effects included",
       x = "Coefficient", y = NULL)
save_plot(p, "03_aid_need_coefficients", w = 9.5, h = 4.2)

# ---- Figure: aid per person vs infant mortality (latest common year) -------
yr <- max(d$year)
sc <- d |> filter(year == yr, us_aid_pc > 0)
p <- ggplot(sc, aes(infant_mort, us_aid_pc, size = pop / 1e6)) +
  geom_point(alpha = 0.55, color = "#2166AC") +
  ggrepel::geom_text_repel(data = sc |> slice_max(us_aid_pc, n = 8), aes(label = country),
                           size = 2.8, seed = 1, show.legend = FALSE) +
  scale_y_log10(labels = dollar) +
  scale_size_area(max_size = 12, name = "Population (millions)") +
  labs(title = sprintf("U.S. aid per person vs. infant mortality, %d", yr),
       subtitle = "Each point is a country; larger points have larger populations",
       x = "Infant mortality (per 1,000 births)", y = "U.S. aid per person (log scale)")
save_plot(p, "04_aid_vs_need_scatter", w = 8.5, h = 5)
