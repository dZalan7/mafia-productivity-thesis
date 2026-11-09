# Cleaning the data, calculating the target indicators which the mafia index will be regressed upon
source("Thesis/Code/mafia index.R")

# Italian provinces saved in the ISTAT database have single quotes (') in their name.
# This poses a problem as cells are also separated by single quotes in the .csv file.
# They solve this by adding a single apostrophe (") before the quotation mark.
# I replace these with another quotation mark to avoid parsing issues.
val_add_raw <- read_csv(
  I(gsub('"', "'", read_file("Thesis/Data/ISTAT/Value added by industry (IT1,93_1227_DF_DCCN_TNA1_1,1.0).csv"), fixed = TRUE)),
  quote = "'",
  show_col_types = FALSE
)
emp_raw <- read_csv(
  I(gsub('"', "'", read_file("Thesis/Data/ISTAT/Employment - provinces (IT1,93_379_DF_DCCN_OCCTSEC2010_2,1.0).csv"), fixed = TRUE)),
  quote = "'",
  show_col_types = FALSE
)

# Shortening the names of sectors for ease of use
industry_short <- c(
  "Agriculture"                 = "Agriculture, forestry and fishing",
  "Mining/utilities/waste"      = "Mining and quarrying, manufacturing, electricity, gas, steam and air conditioning supply, water supply, sewerage, waste management and remediation activities",
  "Manufacturing"               = "Manufacturing",
  "Construction"                = "Construction",
  "Trade/transport/hospitality" = "Wholesale and retail trade, repair of motor vehicles and motorcycles, transportation and storage, accommodation and food service activities",
  "Com/Info"                    = "Information and communication",
  "Finance/admin"               = "Financial and insurance activities, real estate activities, real estate activities, administrative and support service activities",
  "Professional/admin"          = "Professional, scientific and technical activities, administrative and support service activities",
  "Public admin/health/edu"     = "Public administration and defence, compulsory social security, education, human health and social work activities",
  "Arts/other services"         = "Arts, entertainment and recreation, repair of household goods and other services",
  "Real estate"                 = "Real estate activities"
)

#
val_add <- val_add_raw |>
  mutate(
    province = Territory,
    industry = `Breakdown by industry (NACE Rev.2)`,
    year     = as.integer(TIME_PERIOD),
    val_add  = as.numeric(Observation),
    .keep    = "none"
  ) |>
  drop_na(year) |>
  pivot_wider(names_from = industry, values_from = val_add) |>
  rename(!!!industry_short) |>
  mutate(
    `Mining/utilities/waste` = `Mining/utilities/waste` - Manufacturing,
    `Finance/admin`          = `Finance/admin` - `Real estate`
  ) |>
  pivot_longer(
    cols      = -c(province, year),
    names_to  = "industry",
    values_to = "val_add"
  ) |>
  mutate(province = fold_sud_sardegna(province)) |>
  group_by(province, year, industry) |>
  summarise(val_add = sum(val_add), .groups = "drop")

emp <- emp_raw |>
  mutate(
    province = Territory,
    industry = `Breakdown by industry (NACE Rev.2)`,
    year     = as.integer(TIME_PERIOD),
    emp      = as.numeric(Observation),
    .keep    = "none"
  ) |>
  drop_na(year) |>
  pivot_wider(names_from = industry, values_from = emp) |>
  rename(!!!industry_short) |>
  mutate(
    `Mining/utilities/waste` = `Mining/utilities/waste` - Manufacturing,
    `Finance/admin`          = `Finance/admin` - `Real estate`
  ) |>
  pivot_longer(
    cols      = -c(province, year),
    names_to  = "industry",
    values_to = "emp"
  ) |>
  mutate(province = fold_sud_sardegna(province)) |>
  group_by(province, year, industry) |>
  summarise(emp = sum(emp), .groups = "drop")


lp_prov <- val_add |>
  inner_join(emp, by = c("province", "year", "industry")) |>
  mutate(lp = log(val_add / emp)) |>
  filter(is.finite(lp)) |>
  summarise(lp = mean(lp), .by = c(province, industry))

prod_met_summary <- lp_prov |>
  summarise(
    min = min(lp),
    max = max(lp),
    sd  = sd(lp),
    mad = mean(abs(lp - mean(lp))),
    .by = industry
  )

