# 03_corruption_hurdle.R
# The Millennium Challenge Corporation (MCC) only funds countries that score
# above the median of their income peer group on Control of Corruption (a
# "hard hurdle"). The World Bank's Control of Corruption score is itself an
# estimate from a statistical model that combines many surveys and expert
# ratings (an unobserved-components model), and the World Bank publishes a
# standard error for every country.
#
# Question: how many countries' pass/fail result is decided by measurement
# noise rather than by real differences in corruption?
#
# Method: treat each country's true score as Normal(estimate, SE), which is the
# World Bank model's own uncertainty, draw 10,000 possible "true" worlds, and
# recompute the peer-group median and every country's pass/fail in each draw.

source("R/00_setup.R")
panel <- readRDS(PANEL_FILE)
N_SIM <- 10000

# Candidate pool: low-income and lower-middle-income countries (MCC scores the
# two groups separately against their own medians)
latest <- max(panel$year[!is.na(panel$cc_est)])
cand <- panel |>
  filter(year == latest, income %in% c("Low income", "Lower middle income"),
         !is.na(cc_est), !is.na(cc_se)) |>
  mutate(group = ifelse(income == "Low income", "Low income", "Lower middle income"))
message("Year ", latest, ": ", nrow(cand), " candidate countries")

simulate_pass <- function(d) {
  est <- d$cc_est; se <- d$cc_se; n <- nrow(d)
  draws <- matrix(rnorm(N_SIM * n, mean = rep(est, each = N_SIM), sd = rep(se, each = N_SIM)), N_SIM, n)
  med <- apply(draws, 1, median)
  passes <- draws > med                                  # N_SIM x n
  data.frame(iso3c = d$iso3c, p_pass = colMeans(passes),
             official_pass = est > median(est), median_est = median(est),
             median_sd = sd(med))
}
res <- bind_rows(lapply(split(cand, cand$group), function(d) {
  cbind(d[, c("country", "iso3c", "group", "region", "cc_est", "cc_se")],
        simulate_pass(d)[, -1])
})) |>
  mutate(status = case_when(p_pass >= 0.9 ~ "Clear pass (90%+)",
                            p_pass <= 0.1 ~ "Clear fail (10% or less)",
                            TRUE ~ "Uncertain (10-90%)"),
         coin_flip = p_pass > 0.3 & p_pass < 0.7)

summary_tab <- res |>
  group_by(group) |>
  summarise(Countries = n(),
            Official_passes = sum(official_pass),
            Uncertain_10_90 = sum(status == "Uncertain (10-90%)"),
            Coin_flip_30_70 = sum(coin_flip),
            Median_SE = median(cc_se),
            Median_estimate = first(median_est)) |>
  mutate(Share_uncertain = Uncertain_10_90 / Countries)
print(summary_tab)
save_table(r4(summary_tab), "hurdle_summary")
save_table(r4(res |> arrange(group, desc(p_pass))), "hurdle_country_probabilities")

# ---- Year-to-year flips ----------------------------------------------------
# How often does a country's official pass/fail change from one year to the
# next, and how often is that change smaller than the noise in the scores?
flips <- panel |>
  filter(income %in% c("Low income", "Lower middle income"), !is.na(cc_est), !is.na(cc_se), year >= 2010) |>
  group_by(year, income) |>
  mutate(pass = cc_est > median(cc_est)) |>
  ungroup() |>
  arrange(iso3c, year) |>
  group_by(iso3c) |>
  mutate(prev_pass = lag(pass), prev_est = lag(cc_est), prev_se = lag(cc_se), prev_year = lag(year)) |>
  ungroup() |>
  filter(!is.na(prev_pass), year - prev_year == 1) |>
  mutate(flipped = pass != prev_pass,
         change = cc_est - prev_est,
         change_se = sqrt(cc_se^2 + prev_se^2),
         within_noise = abs(change) < 1.96 * change_se)
flip_tab <- flips |>
  summarise(Country_years = n(), Flips = sum(flipped), Flip_rate = mean(flipped),
            Flips_within_noise = sum(flipped & within_noise),
            Share_of_flips_within_noise = mean(within_noise[flipped]))
print(flip_tab)
save_table(r4(flip_tab), "hurdle_year_to_year_flips")

# ---- Figure: estimates with 90% intervals vs the median line --------------
plot_df <- res |>
  group_by(group) |>
  mutate(rank = rank(cc_est, ties.method = "first")) |>
  ungroup()
lab_df <- plot_df |> group_by(group) |> filter(abs(p_pass - 0.5) < 0.2) |> ungroup()
p <- ggplot(plot_df, aes(cc_est, rank, color = status)) +
  geom_vline(aes(xintercept = median_est), linetype = "dashed", color = "grey30") +
  geom_errorbarh(aes(xmin = cc_est - 1.645 * cc_se, xmax = cc_est + 1.645 * cc_se), height = 0, alpha = 0.6) +
  geom_point(size = 1.4) +
  ggrepel::geom_text_repel(data = lab_df, aes(label = country), size = 2.7, color = "grey20",
                           max.overlaps = 30, seed = 1, min.segment.length = 0) +
  facet_wrap(~group, scales = "free_y") +
  scale_color_manual(values = c("Clear pass (90%+)" = "#2166AC", "Uncertain (10-90%)" = "#F4A300",
                                "Clear fail (10% or less)" = "#B2182B"), name = NULL) +
  labs(title = "Who clears MCC's corruption hurdle depends on measurement noise",
       subtitle = sprintf("Control of Corruption estimates (%d) with 90%% intervals; dashed line = peer-group median", latest),
       x = "Control of Corruption estimate (World Bank WGI)", y = NULL) +
  theme(axis.text.y = element_blank(), panel.grid.major.y = element_blank())
save_plot(p, "01_corruption_hurdle", w = 10, h = 7)

p <- ggplot(res, aes(p_pass, fill = group)) +
  geom_histogram(breaks = seq(0, 1, 0.05), color = "white") +
  facet_wrap(~group) +
  scale_fill_manual(values = c("#4393C3", "#2166AC"), guide = "none") +
  scale_x_continuous(labels = percent) +
  labs(title = "Probability of passing the corruption hurdle",
       subtitle = "Share of 10,000 simulated 'true score' draws in which each country beats its group median",
       x = "Probability of passing", y = "Countries")
save_plot(p, "02_pass_probability_distribution", w = 8, h = 4)
