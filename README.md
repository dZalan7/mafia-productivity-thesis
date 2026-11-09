# Mafia Intensity and Sector Productivity in Italian Provinces

Replication package for a master's thesis on organised crime and labour productivity.
It builds a province-level index of mafia intensity from Italian administrative data,
then estimates how sector labour productivity varies with that index across
106 provinces and 11 sectors.

The motivating puzzle: organised crime is consistently found to depress regional
productivity *in aggregate*, but that average hides considerable variation across
sectors. Some sectors plausibly benefit from criminal presence — real estate above
all, the classic laundering channel. These models separate the overall cross-province
level effect from the sector-specific gradients.

## Data

| Source | Contents | Coverage |
| --- | --- | --- |
| ISTAT | Reported crime rates by province and crime type (per 100,000) | 2006–2024 |
| ISTAT | Gross value added by industry (NACE Rev. 2), current prices | 2006–2023 |
| ISTAT | Employment by province and industry | 2006–2023 |
| ISTAT | Resident population estimates | 2002–2019 |
| ANBSC | Confiscated firms and real estate under administration | cumulative |

ANBSC is the Italian agency administering assets seized from organised crime.
Sud Sardegna is folded into Cagliari throughout, since it has no separate crime
data from before its 2016 boundary change; that leaves 106 provinces.

ISTAT's CSV exports quote fields with single quotes while several province names
(L'Aquila, Valle d'Aosta, Reggio nell'Emilia) contain apostrophes of their own.
The read helpers in `mafia index.R` and `target indicators.R` repair this before parsing.

## The mafia index

Fifteen components: thirteen ISTAT crime rates plus two ANBSC confiscation
measures expressed per thousand inhabitants. Each is `log1p`-transformed, min–max
normalised to [0, 1], then combined as a weighted row sum. Weights rise with how
specifically an indicator signals mafia activity, and the two laundering-linked
measures are weighted up deliberately:

| Weight | Components |
| --- | --- |
| 2.0 | Mafia criminal association, mafia homicides, money laundering, confiscated firms p.c. |
| 1.5 | Smuggling |
| 1.25 | Usury, exploitation and abetting of prostitution |
| 1.0 | Drug trafficking and possession, kidnappings, arson, extortion, counterfeiting, confiscated real estate p.c. |
| 0.5 | Attacks, menaces |

The index averages 4.29 in the North and 7.17 in the South.
Full per-province breakdown: `Thesis/Data/output/mafia_index/mafia_index_breakdown.csv`.

## Models

All specifications are estimated with `fixest::feols`, standard errors clustered
by province. Productivity is log value added per worker.

| Model | Specification | Identifies |
| --- | --- | --- |
| `m_sector` | sector × index, province + year + industry FE | within-province sector gradients, relative to Manufacturing |
| `m_level` | index, year + industry FE | the overall cross-province level effect |
| `m_abs` | sector × index, year + industry FE, no reference sector | absolute by-sector gradients |
| `m_macro` | sector × index, macro-area × year FE | gradients net of the North–South divide |
| `m_northsouth` | sector × index, North/South + year + industry FE | the same, with a coarse two-region dummy |

The absolute and fixed-effects columns have to be read together:
`fe_slope = abs_slope − abs_slope(Manufacturing)` holds by construction, so the FE
estimates are differences against a sector that is itself strongly negative.

## Results

Productivity differential per one standard deviation of the mafia index (%):

| Sector | Absolute | Relative to Manufacturing |
| --- | --- | --- |
| Agriculture | −17.3 | −7.2 |
| Manufacturing | −10.9 | 0.0 |
| Finance/admin | −7.6 | +3.8 |
| Mining/utilities/waste | −6.9 | +4.5 |
| Professional/admin | −5.5 | +6.1 |
| Construction | −4.7 | +6.9 |
| Trade/transport/hospitality | −4.0 | +7.7 |
| Arts/other services | −2.3 | +9.7 |
| Public admin/health/edu | +1.2 | +13.6 |
| Com/Info | +1.7 | +14.2 |
| Real estate | +14.5 | +28.5 |

The absolute gradient is negative in eight of eleven sectors and steepest in
Agriculture and Manufacturing. Real estate is the clear outlier in the other
direction. That said, the residual map in `mafia_maps.R` suggests the real-estate
gap tracks the North–South manufacturing gradient more than it isolates a clean
laundering signal — which is what `m_macro` and `m_northsouth` are there to probe.

Full estimates: `Thesis/Data/output/absolute_vs_fe_effects.csv`, with LaTeX
versions of the regression tables in the same directory.

## Layout

```
Thesis/
  Code/
    helpers.R             shared helper functions
    mafia index.R         crime + confiscation data -> province mafia index
    target indicators.R   value added and employment, cleaned and sector-mapped
    model_combined.R      the FE, absolute, macro-area and North/South models
    mafia_maps.R          index and real-estate-residual choropleths
    prod_summary.R        descriptive productivity table
  Data/
    ISTAT/                crime, value added, employment, population
    ANBSC/                confiscated firms and real estate
    output/               every figure, table and estimate the scripts produce
```

## Running it

R 4.1 or newer (the scripts use the native `|>` pipe), plus:

```r
install.packages(c("tidyverse", "fixest", "broom", "giscoR", "sf", "stringi"))
```

Paths are written relative to the repository root, so set the working directory
there — not into `Thesis/` — before sourcing anything:

```r
setwd("/path/to/mafia-productivity-thesis")
source("Thesis/Code/model_combined.R")
```

Each script sources the ones upstream of it, so `model_combined.R` alone rebuilds
the index, the panel and all model outputs. `mafia_maps.R` additionally downloads
province boundaries from Eurostat GISCO at runtime and needs an internet connection.

## Licence

Code is MIT-licensed; see `LICENSE`.

The ISTAT and ANBSC data under `Thesis/Data/` are redistributed here for
replication only and remain the property of their producers, both released as
Italian open data. Attribute them directly rather than this repository.
