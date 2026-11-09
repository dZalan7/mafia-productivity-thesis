# Two province maps from the mafia index: the index itself, and the real-estate
# productivity residual's ratio to the index.

# giscoR provides a simple interface to the Eurostat GISCO geodata distribution.
library(giscoR)
# giscoR returns sf objects, so sf package is needed
library(sf)

source("Thesis/Code/target indicators.R")   # does everything we need

# Function, that fixes the problem where ISTAT uses the official names,
# and GISCO uses the latin ASCII names by transforming all of them into the latin form
norm_name <- function(names) {
  names |>
    str_to_lower() |>
    stringi::stri_trans_general("Latin-ASCII") |>   # é -> e, ò -> o, ...
    str_replace_all("[/\\-]", " ") |>
    str_squish()
}

# Creates an sf object - acts like a dataframe, except it has a column designating geometrical shape
prov_shapes <- gisco_get_nuts(country = "IT", nuts_level = 3, year = "2021") |>
  st_transform(3035) |>                       # ETRS89-LAEA, the standard EU map CRS
  # Sud sardegna is removed for being a ridiculous, and fake province
  mutate(key = norm_name(NAME_LATN),
         key = if_else(key == "sud sardegna", "cagliari", key)) |>
  group_by(key) |>
  summarise(.groups = "drop")

mafia_index_latin <- mafia_index |>
  mutate(key = norm_name(province)) |>
  select(province, mafia_index, key)

map_index <- prov_shapes |> left_join(mafia_index_latin, by = "key")

# The colored map showing the different intensity of mafia presence
p_index <- ggplot(map_index) +
  geom_sf(aes(fill = mafia_index), colour = "black", linewidth = 0.1) +
  scale_fill_viridis_c(option = "plasma", direction = -1, na.value = "grey") +
  theme_void(base_size = 12) +
  theme(legend.position = "none")

ggsave("Thesis/Data/output/mafia_index_map.png", p_index, width = 7, height = 8, dpi = 150)

# Per-province Real-Estate-minus-Manufacturing gap regressed on the mafia index; the
# residual is mapped (red = real estate more productive than the index predicts, blue = less).
# Mostly reflects the North-South Manufacturing gradient, not a clean laundering signal.
re_gap <- val_add |>
  inner_join(emp, by = c("province", "year", "industry")) |>
  filter(industry %in% c("Real estate", "Manufacturing")) |>
  mutate(lp = log(val_add / emp)) |>
  filter(is.finite(lp)) |>
  summarise(lp = mean(lp), .by = c(province, industry)) |>
  pivot_wider(names_from = industry, values_from = lp) |>
  mutate(gap = `Real estate` - Manufacturing) |>
  inner_join(mafia_index |> select(province, mafia_index), by = "province") |>
  drop_na(gap, mafia_index)

re_fit <- lm(gap ~ mafia_index, data = re_gap)
re_gap <- re_gap |> mutate(fitted = fitted(re_fit), resid = resid(re_fit))
write_csv(re_gap, "Thesis/Data/output/mafia_re_residuals.csv")

map_resid <- prov_shapes |> left_join(re_gap |> mutate(key = norm_name(province)), by = "key")

lim <- max(abs(map_resid$resid), na.rm = TRUE)
p_resid <- ggplot(map_resid) +
  geom_sf(aes(fill = resid), colour = "white", linewidth = 0.1) +
  scale_fill_gradient2(low = "#2166ac", mid = "grey95", high = "#b2182b",
                       midpoint = 0, limits = c(-lim, lim), na.value = "grey90",
                       name = "Real estate\nresidual") +
  theme_void(base_size = 12) +
  theme(plot.title = element_text(face = "bold"), legend.position = "right")

ggsave("Thesis/Data/output/re_residual_map.png", p_resid, width = 7, height = 8, dpi = 150)
