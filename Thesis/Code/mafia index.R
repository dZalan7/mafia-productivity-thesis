# Calculating the dependent variable, the mafia index

library(readr)
library(tidyverse)

source("Thesis/Code/helpers.R")

# The exported csv files from the ISTAT database contain commas within some of their fields (per 100,000 population).
# To fix this, they elected to use single quotation marks (') to separate the values. This is a problem, as
# some Italian provinces' names also contain single quotation marks. Any actual single quotation marks therefore
# are put after a quotaion mark to avoid this error.
prov_crime_raw <- read_csv(
  I(gsub('"', "'", read_file("Thesis/Data/ISTAT/Crime rate - provinces (IT1,73_67_DF_DCCV_DELITTIPS_9,1.0).csv"), fixed = TRUE)),
  quote = "'",
  na = c("", "..", "NA"),
  show_col_types = FALSE
)

prov_crime <- prov_crime_raw |>
  mutate(
    province = Territory,
    crime    = `Type of crime`,
    year     = as.integer(TIME_PERIOD),
    rate     = as.numeric(Observation),       # per 100,000 population
    .keep    = "none"
  )
prov_crime <- prov_crime |> mutate(rate = ifelse(is.na(rate), 0, rate)) 


# Table describing avg crime rates per province
crime_avg <- prov_crime |>
  group_by(province, crime) |>
  summarise(
    mean_rate = mean(rate, na.rm = TRUE),
    n_years   = sum(!is.na(rate)),
    .groups   = "drop"
  ) |>
  mutate(mean_rate = if_else(is.nan(mean_rate), NA_real_, mean_rate))

# Wide format: one column per crime type, one row per province.
crime_avg_wide <- crime_avg |>
  select(province, crime, mean_rate) |>
  pivot_wider(names_from = crime, values_from = mean_rate) |>
  arrange(province)

# ANBSC confiscation data is added

# Names are normalized
namefix <- c(
  "Aosta" = "Valle d'Aosta / Vallée d'Aoste",
  "Bolzano" = "Bolzano / Bozen",
  "Monza e Brianza" = "Monza e della Brianza",
  "Reggio Calabria" = "Reggio di Calabria",
  "Reggio Emilia" = "Reggio nell'Emilia",
  "Carbonia-Iglesias" = "Cagliari",
  "Olbia-Tempio" = "Sassari",
  "Ogliastra" = "Nuoro"
)

clean_prov <- function(x) {
  x <- str_squish(x)
  coalesce(unname(namefix[x]), x)
}

# Aziende means firms
aziende <- read_csv(
  "Thesis/Data/ANBSC/totale__aziende-in-amministrazione.csv",
  show_col_types = FALSE
) |>
  mutate(province = clean_prov(Provincia),
         sector   = str_squish(`Settore attivita`),
         .keep    = "none") |>
  filter(province != "Estera", province != "")

firms_by_prov   <- aziende |> count(province, name = "confiscated_firms")
firms_by_sector <- aziende |> mutate(sector = na_if(sector, "")) |>
  count(sector, name = "n_firms", sort = TRUE)

# Immobili means real estate
immobili <- read_csv(
  "Thesis/Data/ANBSC/totale__immobili-in-amministrazione.csv",
  show_col_types = FALSE
) |>
  mutate(province = clean_prov(Provincia),
         surface  = suppressWarnings(as.numeric(Superficie)),
         .keep    = "none") |>
  filter(province != "Estera", province != "")

re_by_prov <- immobili |>
  group_by(province) |>
  summarise(confiscated_realestate    = n(),
            confiscated_re_surface_m2 = sum(surface, na.rm = TRUE),
            .groups = "drop")

istat_provinces <- read_csv(
  I(gsub('"', "'", read_file(
    "Thesis/Data/ISTAT/Value added by industry (IT1,93_1227_DF_DCCN_TNA1_1,1.0).csv"
  ), fixed = TRUE)),
  quote = "'", show_col_types = FALSE
) |>
  distinct(province = Territory) |>
  mutate(province = ifelse(province == "Sud Sardegna", "Cagliari", province)) |>
  distinct(province)


norm_prov <- function(x) gsub("\\s*/\\s*", "/", stringi::stri_trans_general(str_squish(x), "Latin-ASCII"))
pop <- read_csv("Thesis/Data/ISTAT/Population estimates 2002-2019.csv",
                show_col_types = FALSE) |>
  filter(Sex == "Total") |>
  mutate(Province = ifelse(Province == "Sud Sardegna", "Cagliari", Province)) |>
  group_by(prov_key = norm_prov(Province)) |>
  summarise(pop_thousands = round(sum(Population, na.rm = TRUE) / 1000), .groups = "drop")

anbsc_province <- istat_provinces |>
  mutate(prov_key = norm_prov(province)) |>
  left_join(firms_by_prov |> mutate(prov_key = norm_prov(province)) |> select(-province), by = "prov_key") |>
  left_join(re_by_prov    |> mutate(prov_key = norm_prov(province)) |> select(-province), by = "prov_key") |>
  left_join(pop, by = "prov_key") |>
  select(-prov_key) |>
  mutate(across(c(confiscated_firms, confiscated_realestate, confiscated_re_surface_m2),
                ~ replace_na(., 0))) |>
  mutate(confiscated_firms_pc      = confiscated_firms / pop_thousands,
         confiscated_realestate_pc = confiscated_realestate / pop_thousands) |>
  arrange(desc(confiscated_realestate))

write_csv(anbsc_province,  "Thesis/Data/ANBSC/confiscated_assets_by_province.csv")
write_csv(firms_by_sector, "Thesis/Data/ANBSC/confiscated_firms_by_sector.csv")


index_wide <- crime_avg_wide |>
  left_join(anbsc_province |> select(province, confiscated_firms_pc, confiscated_realestate_pc),
            by = "province") |>
  mutate(across(c(confiscated_firms_pc, confiscated_realestate_pc), ~ replace_na(., 0)))


coef_matrix <- index_wide |>
  mutate(across(-province, ~ log1p(.x))) |>
  mutate(across(-province, ~ (.x - min(.x, na.rm = TRUE)) /
                  (max(.x, na.rm = TRUE) - min(.x, na.rm = TRUE))))


# Vector describing the weight each crime is associated with. 
# Crimes more blatantly related to mafia activities are given higher weights
# The two indicators especially related to the presence of money laundering activities
# are given higher weight (Money laundering and confiscated_firms)
weights <- c(
  `Mafia criminal association` = 2,
  `Homicides of mafia` = 2,
  `Money laundering` = 2,
   confiscated_firms_pc = 2,          # ANBSC confiscated firms per 1,000 inhabitants
  `Smuggling` = 1.5,
  `Exploitation and abetting prostitution` = 1.25,
  `Usury` = 1.25,
  `Trafficking and drugs possession` = 1,
  `Kidnappings` = 1,
  `Damage followed by arson` = 1,
   confiscated_realestate_pc = 1,     # ANBSC confiscated real estate per 1,000 inhabitants
  `Counterfeiting of goods and industrial products` = 1,
  `Extortions` = 1,
  `Attacks` = 0.5,
  `Menaces` = 0.5
)

# A matrix is constructed with the components multiplied by the weights
comp_mat    <- as.matrix(coef_matrix[, names(weights)])
contrib_mat <- sweep(comp_mat, 2, weights, `*`)

# The matrix is converted into a tibble and gets cleaned
mafia_index <- tibble(province = coef_matrix$province) |>
  bind_cols(as_tibble(contrib_mat)) |>
  mutate(mafia_index = rowSums(contrib_mat)) |>
  relocate(mafia_index, .after = province) |>
  drop_na(mafia_index)

# Descriptive tables
crime_summary <- prov_crime |>
  group_by(crime) |>
  summarise(
    mean     = mean(rate,   na.rm = TRUE),
    median   = median(rate, na.rm = TRUE),
    sd       = sd(rate,     na.rm = TRUE),
    max      = max(rate,    na.rm = TRUE),
    pct_zero = mean(rate == 0, na.rm = TRUE),
    pct_na   = mean(is.na(rate)),
    .groups  = "drop"
  ) |>
  arrange(desc(mean))

top_by_crime <- crime_avg |>
  group_by(crime) |>
  slice_max(mean_rate, n = 5, with_ties = FALSE) |>
  arrange(crime, desc(mean_rate))

crime_cor_mat <- crime_avg_wide |>
  select(-province) |>
  cor(use = "pairwise.complete.obs")

crime_cor_df <- crime_cor_mat |>
  as.data.frame() |>
  tibble::rownames_to_column("crime")

# Correlation Matrix
correlation_matrix_vis <- crime_cor_df |>
  mutate(crime = substr(crime, 1, 3)) |>
  pivot_longer(-crime, names_to = "crime2", values_to = "corr") |>
  mutate(crime2 = substr(crime2, 1, 3)) |>
  ggplot(aes(crime, crime2, fill = corr)) +
  geom_tile() +
  geom_text(aes(label = sprintf("%.2f", corr)), size = 3) +
  scale_fill_distiller(palette = "RdBu", limits = c(-1, 1)) +
  theme_minimal() +
  theme(legend.position = "none", axis.title = element_blank())

ggsave("Thesis/Data/output/corr_mat_vis.png", correlation_matrix_vis, width = 6, height = 6, dpi = 120)

write_csv(coef_matrix, "Thesis/Data/output/mafia_index/coef_matrix.csv")
write.csv(mafia_index, "Thesis/Data/output/mafia_index/mafia_index_breakdown.csv")
