# 06_allocation_optimizer.R
# How much does the choice of objective change who gets aid?
#
# A planner splits a fixed $1 billion across MCC-style candidate countries.
# Each country's benefit from aid has diminishing returns:
#     benefit_i(a_i) = w_i * log(1 + a_i / s_i)
# where s_i is the country's size (population) and w_i is how much the planner
# values a dollar there under each objective:
#   1. Need:        w = population x infant mortality (where children are dying)
#   2. Growth:      w = population x expected growth return, which the
#                   Burnside-Dollar logic says is higher with better policy
#                   (here: Control of Corruption, rescaled to be positive)
#   3. Governance:  need, but only for countries that pass the corruption
#                   hurdle (MCC-style rule)
#
# Maximizing total benefit subject to the budget has a closed-form "water-filling"
# solution: a_i = max(0, w_i / lambda - s_i), with lambda set so spending = budget.

source("R/00_setup.R")
panel <- readRDS(PANEL_FILE)
BUDGET <- 1e9

latest <- max(panel$year[!is.na(panel$cc_est)])
cand <- panel |>
  filter(income %in% c("Low income", "Lower middle income")) |>
  group_by(iso3c) |>
  arrange(desc(year)) |>
  summarise(country = first(country), region = first(region), income = first(income),
            pop = first(pop[!is.na(pop)]), infant_mort = first(infant_mort[!is.na(infant_mort)]),
            cc_est = first(cc_est[year == latest]), .groups = "drop") |>
  filter(!is.na(pop), !is.na(infant_mort), !is.na(cc_est)) |>
  group_by(income) |>
  mutate(passes_hurdle = cc_est > median(cc_est)) |>
  ungroup()

water_fill <- function(w, s, budget) {
  spend <- function(lambda) sum(pmax(0, w / lambda - s))
  lambda <- uniroot(function(l) spend(l) - budget, c(1e-12, max(w / s)), tol = 1e-14)$root
  pmax(0, w / lambda - s)
}

pop_m <- cand$pop / 1e6
policy_mult <- exp(0.5 * cand$cc_est)          # better governance -> higher assumed return
w <- list(
  `Need` = pop_m * cand$infant_mort,
  `Growth (policy-weighted)` = pop_m * policy_mult,
  `Governance hurdle + need` = pop_m * cand$infant_mort * cand$passes_hurdle
)
alloc <- bind_rows(lapply(names(w), function(obj) {
  a <- water_fill(w[[obj]], pop_m, BUDGET / 1e6)       # in $ millions
  cbind(cand[, c("country", "iso3c", "region", "income", "infant_mort", "cc_est", "passes_hurdle")],
        Objective = obj, Allocation_musd = a)
}))
save_table(r4(alloc |> arrange(Objective, desc(Allocation_musd))), "allocation_by_objective")

summ <- alloc |>
  group_by(Objective) |>
  summarise(Countries_funded = sum(Allocation_musd > 0.01),
            Share_to_low_income = sum(Allocation_musd[income == "Low income"]) / sum(Allocation_musd),
            Share_to_SSA = sum(Allocation_musd[region == "Sub-Saharan Africa"]) / sum(Allocation_musd),
            Aid_weighted_infant_mort = weighted.mean(infant_mort, Allocation_musd),
            Aid_weighted_corruption_score = weighted.mean(cc_est, Allocation_musd))
print(r4(summ))
save_table(r4(summ), "allocation_summary")

# Overlap: how similar are the allocations? (share of money that would stay
# with the same countries if you switched objectives)
wide <- alloc |> dplyr::select(iso3c, Objective, Allocation_musd) |>
  pivot_wider(names_from = Objective, values_from = Allocation_musd)
overlap <- function(a, b) sum(pmin(a / sum(a), b / sum(b)))
ov <- expand.grid(From = names(w), To = names(w), stringsAsFactors = FALSE) |>
  filter(From < To) |>
  rowwise() |>
  mutate(Overlap = overlap(wide[[From]], wide[[To]])) |>
  ungroup()
print(r4(ov))
save_table(r4(ov), "allocation_overlap")

reg <- alloc |>
  group_by(Objective, region) |>
  summarise(Share = sum(Allocation_musd), .groups = "drop_last") |>
  mutate(Share = Share / sum(Share)) |>
  ungroup()
p <- ggplot(reg, aes(Objective, Share, fill = region)) +
  geom_col(width = 0.65) +
  scale_y_continuous(labels = percent) +
  scale_fill_brewer(palette = "Set2", name = NULL) +
  labs(title = "Same $1 billion, three objectives, three different maps",
       subtitle = "Share of an optimal allocation going to each region, by planner objective",
       x = NULL, y = NULL) +
  guides(fill = guide_legend(nrow = 2))
save_plot(p, "06_allocation_by_region", w = 8.5, h = 5)

top <- alloc |> group_by(Objective) |> slice_max(Allocation_musd, n = 8) |> ungroup()
p <- ggplot(top, aes(Allocation_musd, reorder(country, Allocation_musd))) +
  geom_col(fill = "#2166AC") +
  facet_wrap(~Objective, scales = "free_y") +
  scale_x_continuous(labels = label_dollar(suffix = "M")) +
  labs(title = "Top recipients under each objective", x = "Allocation ($ millions)", y = NULL)
save_plot(p, "07_allocation_top_recipients", w = 10, h = 4.5)
