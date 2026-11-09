# Mafia Intensity and Sector Productivity in Italian Provinces

Replication package for a bachelor's thesis on organised crime and labour productivity.
It builds a province-level index of mafia intensity from Italian administrative data,
then estimates how sector labour productivity varies with that index across
106 provinces and 11 sectors.

## Data

| Source | Contents | Coverage |
| --- | --- | --- |
| ISTAT | Reported crime rates by province and crime type (per 100,000) | 2006–2024 |
| ISTAT | Gross value added by industry (NACE Rev. 2), current prices | 2006–2023 |
| ISTAT | Employment by province and industry | 2006–2023 |
| ISTAT | Resident population estimates | 2002–2019 |
| ANBSC | Confiscated firms and real estate under administration | cumulative |

ANBSC (Agenzia Nazionale
per l'amministrazione e la destinazione dei beni
sequestrati e confiscati alla criminalità organizzata) is the Italian agency administering assets seized from organised crime.
ISTAT (Istituto Nazionale di Statistica) is the Italian Statistics Agency.

## The mafia index

Fifteen measures pooled into a single composite index; thirteen ISTAT crime rates plus two ANBSC confiscation
measures expressed per thousand inhabitants. Each is `log1p`-transformed, min–max
normalised to [0, 1], then combined as a weighted row sum. Weights set based on how
closely related an indicator is to mafia activity, and the two laundering-linked
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

## Requirements for running

R 4.1 or newer (the scripts use the native `|>` pipe)

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
