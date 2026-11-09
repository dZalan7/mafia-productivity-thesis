# Mean log labour productivity across provinces, by industry

source("Thesis/Code/target indicators.R")

lp_prov <- val_add |>
  inner_join(emp, by = c("province", "year", "industry")) |>
  mutate(lp = log(val_add / emp)) |>
  filter(is.finite(lp)) |>
  summarise(lp = mean(lp), .by = c(province, industry))

summary_tab <- lp_prov |>
  summarise(
    min = min(lp),
    max = max(lp),
    sd  = sd(lp),
    mad = mean(abs(lp - mean(lp))),
    .by = industry
  )

write_csv(summary_tab, "Thesis/Data/output/prod_summary.csv")
print(summary_tab, n = Inf)
