# Mafia intensity and sector productivity — within-province (FE) and
# absolute (cross-province) effects in one pass. FE slopes are relative to
# Manufacturing; absolute slopes are the raw cross-province gradients, and
# fe_slope_s = abs_slope_s - abs_slope_Manufacturing  by construction.

library(tidyverse)
library(fixest)
library(broom)
source("Thesis/Code/target indicators.R")   # val_add, emp, mafia_index, val_add_raw

# NUTS-1 macro-areas
# This can be done by pulling it from the ISTAT database directly, but it comes out very ugly if you try to include multiple levels.
# Monza e della Brianza (IT108), Fermo (IT109) and Barletta-Andria-Trani (IT110)
# were established later, so they have to manually be assigned to their respective macro areas.
# Sud Sardegna (IT111) is folded into Cagliari upstream (VA/emp/pop reads) to match the
# crime data's pre-2016 boundaries; only its now-redundant raw row is excluded here.
macro_map <- val_add_raw |>
  distinct(REF_AREA, province = Territory) |>
  filter(REF_AREA != "IT111") |>          # Sud Sardegna already merged into Cagliari
  mutate(macro_area = case_when(
    REF_AREA == "IT108" ~ "Nord-Ovest",
    REF_AREA == "IT109" ~ "Centro",
    REF_AREA == "IT110" ~ "Sud",
    str_starts(REF_AREA, "ITC") ~ "Nord-Ovest",
    str_starts(REF_AREA, "ITD") ~ "Nord-Est",
    str_starts(REF_AREA, "ITE") ~ "Centro",
    str_starts(REF_AREA, "ITF") ~ "Sud",
    str_starts(REF_AREA, "ITG") ~ "Isole",
    TRUE ~ substr(REF_AREA, 1, 3)
  )) |>
  # North/South split: South = the "Sud" macro-area plus Sicily (ITG1); everything
  # else is North, which places Sardinia (ITG2) in the North.
  mutate(north_south = if_else(macro_area == "Sud" | str_starts(REF_AREA, "ITG1"),
                               "South", "North")) |>
  distinct(province, macro_area, north_south)

panel <- val_add |>
  inner_join(emp, by = c("province", "year", "industry")) |>
  inner_join(mafia_index |> select(province, mafia_index), by = "province") |>
  left_join(macro_map, by = "province") |>
  mutate(
    `VA/emp`  = val_add / emp,           # Million euro VA per thousand workers
    prod_metric      = ifelse(is.na(log(`VA/emp`)), 0, log(`VA/emp`)),
    industry      = relevel(factor(industry), ref = "Manufacturing")
  )

sd_index <- sd(distinct(panel, province, mafia_index)$mafia_index)

# m_sector: The full FE model, relative to Manufacturing
# m_level: no province FE -> cross-province level effect (to find the overarching negative effect)
m_sector <- feols(
  prod_metric ~ i(industry, mafia_index, ref = "Manufacturing") |
    province + year + industry,
  data = panel, cluster = ~ province
)
m_level <- feols(prod_metric ~ mafia_index | year + industry,
                 data = panel, cluster = ~ province)

# m_northsouth: sector FE model with the province dummy replaced by a North/South
# dummy (South = Sud macro-area + Sicily; North = the rest, incl. Sardinia).
m_northsouth <- feols(
  prod_metric ~ i(industry, mafia_index, ref = "Manufacturing") |
    north_south + year + industry,
  data = panel, cluster = ~ province
)

# Only needed for table 3 (tab:ns_sec_rep): Share of employment in North/South in the interesting sectors
ns_emp <- panel |> summarise(emp = sum(emp), .by = c(industry, north_south))
ns_shares <- bind_rows(
    ns_emp |> summarise(emp = sum(emp), .by = north_south) |> mutate(industry = "All sectors (Benchmark)"),
    ns_emp
  ) |>
  mutate(value = 100 * emp / sum(emp), .by = industry) |>
  filter(industry %in% c("All sectors (Benchmark)", "Com/Info", "Public admin/health/edu", "Real estate")) |>
  select(industry, north_south, value) |>
  mutate(value = round(value, 1))
ns_index <- panel |> distinct(province, north_south, mafia_index) |>
  summarise(value = round(mean(mafia_index), 2), .by = north_south) |>
  mutate(industry = "Mean mafia index")
ns_sec_rep <- bind_rows(ns_shares, ns_index) |>
  pivot_wider(names_from = north_south, values_from = value) |>
  select(industry, North, South)
write_csv(ns_sec_rep, "Thesis/Data/output/ns_sectoral_representation.csv")


etable(m_sector, m_northsouth,
       file    = "Thesis/Data/output/productivity_mafia_models.tex",
       replace = TRUE,
       title   = "Sector productivity and organised-crime intensity",
       digits  = 3)

ci <- as.data.frame(confint(m_sector))
cf <- data.frame(
  term      = rownames(ci),
  estimate  = coef(m_sector),
  lo        = ci[, 1],
  hi        = ci[, 2],
  row.names = NULL
)
cf <- cf[grepl("mafia_index", cf$term), ]
cf$sector <- strip(cf$term)

p_slopes <- ggplot(cf, aes(x = estimate, y = reorder(sector, estimate))) +
  geom_vline(xintercept = 0, linetype = "dashed", colour = "gray") +
  geom_pointrange(aes(xmin = lo, xmax = hi)) +
  labs(
    x = "Slope estimate", y = NULL
  ) +
  theme_minimal(base_size = 12)
ggsave("Thesis/Data/output/mafia_sector_slopes.png", p_slopes, width = 9, height = 6, dpi = 120)

sector_effects <- as.data.frame(coeftable(m_sector))
sector_effects$term <- rownames(sector_effects)
write_csv(sector_effects, "Thesis/Data/output/mafia_sector_effects.csv")

# Compare the province-FE and North/South-FE sector slopes
cmp_ci <- function(m, model) {
  ci <- as.data.frame(confint(m))
  data.frame(term = rownames(ci), estimate = coef(m),
             lo = ci[, 1], hi = ci[, 2], model = model, row.names = NULL) |>
    filter(grepl("mafia_index", term)) |>
    mutate(sector = strip(term))
}
cf_cmp <- bind_rows(cmp_ci(m_sector, "Province FE"),
                    cmp_ci(m_northsouth, "North/South FE"))
p_compare <- ggplot(cf_cmp, aes(x = estimate, y = reorder(sector, estimate), colour = model)) +
  geom_vline(xintercept = 0, linetype = "dashed", colour = "gray") +
  geom_pointrange(aes(xmin = lo, xmax = hi), position = position_dodge(width = 0.5)) +
  scale_colour_manual(values = c("Province FE" = "#2166ac", "North/South FE" = "#b2182b")) +
  labs(x = "Slope estimate", y = NULL, colour = NULL) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "top")
ggsave("Thesis/Data/output/mafia_fe_vs_northsouth.png", p_compare, width = 9, height = 6, dpi = 120)

# ---- absolute cross-province + FE/absolute comparison ----------------

# m_abs   : absolute by-sector slopes (no province FE, no reference sector)
# m_macro : same, within macro-area x year (nets out the North/South divide)
m_abs   <- feols(prod_metric ~ i(industry, mafia_index) | year + industry,
                 data = panel, cluster = ~ province)
m_macro <- feols(prod_metric ~ i(industry, mafia_index) | macro_area^year + industry,
                 data = panel, cluster = ~ province)

etable(m_sector, m_northsouth, m_abs,
       file    = "Thesis/Data/output/model_etables_combined.tex",
       replace = TRUE,
       title   = "Absolute (cross-province) vs within-province mafia effects by sector",
       digits  = 3)

slopes <- function(m) {
  cf <- coef(m); kp <- grep("mafia_index", names(cf), value = TRUE)
  tibble(industry = strip(kp), slope = unname(cf[kp]))
}

int_terms_abs <- grep("mafia_index", names(coef(m_abs)), value = TRUE)
abs_ci        <- as.data.frame(confint(m_abs))[int_terms_abs, ]
beta_manuf    <- coef(m_abs)[grep("Manufacturing", names(coef(m_abs)))]

comparison <- tibble(
    industry  = strip(int_terms_abs),
    abs_slope = unname(coef(m_abs)[int_terms_abs]),
    abs_se    = unname(se(m_abs)[int_terms_abs]),
    abs_p     = unname(pvalue(m_abs)[int_terms_abs]),
    abs_lo    = abs_ci[, 1],
    abs_hi    = abs_ci[, 2]
  ) |>
  left_join(slopes(m_sector) |> rename(fe_slope = slope), by = "industry") |>
  left_join(slopes(m_macro)  |> rename(macro_slope = slope), by = "industry") |>
  mutate(
    fe_slope         = ifelse(is.na(fe_slope), 0, fe_slope),  # Manufacturing = FE reference
    abs_pct_per_sd   = (exp(abs_slope   * sd_index) - 1) * 100,
    fe_pct_per_sd    = (exp(fe_slope    * sd_index) - 1) * 100,
    macro_pct_per_sd = (exp(macro_slope * sd_index) - 1) * 100,
    abs_minus_manuf  = abs_slope - beta_manuf                 # equals fe_slope by construction
  ) |>
  arrange(abs_slope)

write_csv(comparison, "Thesis/Data/output/absolute_vs_fe_effects.csv")

lv <- comparison$industry        # already ordered by abs_slope
plot_df <- comparison |>
  mutate(industry = factor(industry, levels = lv),
         lo_pct = (exp(abs_lo * sd_index) - 1) * 100,
         hi_pct = (exp(abs_hi * sd_index) - 1) * 100)

p_cmp <- ggplot(plot_df, aes(y = industry)) +
  geom_vline(xintercept = 0, linetype = "dashed", colour = "gray") +
  geom_errorbar(aes(xmin = lo_pct, xmax = hi_pct), orientation = "y", width = 0, colour = "red3") +
  geom_point(aes(x = abs_pct_per_sd, color = "Absolute model"), size = 2.6) +
  geom_point(aes(x = fe_pct_per_sd, colour = "Full model"),
             shape = 18, size = 3.2) +
  scale_colour_manual(values = c("Absolute model" = "red3",
                                 "Full model" = "blue2"),
                      name = NULL) +
  labs(x = "Productivity differential (%)", y = NULL) +
  theme_minimal() +
  theme(legend.position = "top")
ggsave("Thesis/Data/output/absolute_vs_fe.png", p_cmp, width = 10, height = 6.5, dpi = 120)
