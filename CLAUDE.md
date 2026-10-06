# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project overview

This repo supports a research paper (IDB, "Paper CCS") mapping population exposed to
extreme-heat risk in four Latin American cities: **Montería** (Colombia), **Mérida**
(Mexico — note: a Mexican Mérida/Yucatán, not the Venezuelan one; INEGI-sourced data),
**Daule** (Ecuador), and **Neuquén/Confluencia** (Argentina). It is a collection of
standalone R scripts, not an R package — there is no DESCRIPTION, no renv/packrat lockfile,
and no automated test suite. Scripts are run interactively/top-to-bottom in RStudio.

## Running the code

There is no build, lint, or test command. Workflow per city is always two stages, run in order:

1. **`scripts/MAIN_SCRIPT_<CITY>.R`** — loads raw census/cartography inputs (shapefiles +
   census CSV/Excel extracts), joins indicators onto geographic units, runs spatial
   imputation for missing values, generates choropleth maps, and writes the cleaned
   base layer (`.gpkg`/`.csv`/`.shp`) into `data/<city>/`.
2. **`scripts/RIESGO_<CITY>.R`** (or `RIESGO_NQN.R` for Neuquén) — consumes the output of
   step 1 plus a land-surface-temperature (LST/dLST) raster, computes a social
   vulnerability index (IVS) and a composite heat risk index, and writes risk maps/tables.

`scripts/mapeo PNA monteria.R` is a separate, unrelated analysis (walking isochrones to
first-level health facilities in Montería using OSM + `dodgr`); it does not feed into the
MAIN/RIESGO pipeline.

**Required packages** (no single install script — install as needed per script):
`sf`, `dplyr`, `ggplot2`, `stringr`, `tidyr`, `terra`, `tidyterra`, `readxl`, `scales`, and
for the isochrone script only: `stringi`, `osmdata`, `dodgr`, `concaveman`, `leaflet`,
`htmlwidgets`, `ggspatial`.

### Hardcoded paths — read before running or editing any script

Every script hardcodes absolute, machine-specific paths at the top (no CLI args, no env
vars, no project-relative paths via `here()`):

- `MAIN_SCRIPT_*.R` files point `carpeta` at this user's OneDrive sync folder
  (`.../Paper CCS/<City>/...`) for raw inputs, and write outputs into `data/<city>/`
  inside this repo (relative to the OneDrive path, not the repo root).
- `RIESGO_*.R` files instead `setwd("~/Documents/BID-HWandCITIES")` and read/write under
  `./Datos_nico/...` and `./Outputs/...` / `./Riesgo/...` — a completely different local
  directory tree than the MAIN scripts use, and not part of this repo at all.
- `MAIN_SCRIPT_MERIDA.R` also writes a timestamped copy to `C:/temp/merida_salida` before
  copying the final file back onto OneDrive — a workaround for OneDrive file locks.

When adapting a script for a new machine/user, these paths must be edited by hand; don't
assume they are portable or try to infer a shared config.

## Architecture / pipeline conventions

All four cities follow the same shape, so understanding one `MAIN_SCRIPT` + `RIESGO` pair
makes the others easy to read. Key shared patterns:

- **Unit of analysis** is the census block (`manzana`) for Montería, Mérida, and Daule, and
  the census tract (`radio censal`) for Neuquén (Neuquén uses Redatam/INDEC exports instead
  of a block-level census CSV — see the multi-sheet Excel join in `MAIN_SCRIPT_NEUQUEN.R`).
- **Join keys are strings, re-derived per city**: e.g. `CVEGEO` (INEGI, Mérida), `cod_manz`
  (Montería), `man`/`codigo_manzana` (INEC, Daule), `LINK`/`codigo` (INDEC radio code,
  Neuquén). Before joining, scripts rename any field in the geometry layer that
  case-insensitively collides with a census column (suffix `_shp`) — GeoPackage/SQLite
  backends are case-insensitive, so this matters when writing `.gpkg` output too.
- **Spatial imputation for missing values**: `imputar_por_vecinos()` (defined inline in each
  `MAIN_SCRIPT_*.R`, not shared as a package function) fills NA values by iterative
  averaging over polygon neighbors (found via a 15m buffer + `st_intersects`), falling back
  to the nearest feature with data (`st_nearest_feature`) for isolated polygons with no
  neighbor data after convergence.
- **Social Vulnerability Index (IVS) and Risk**, computed in `RIESGO_*.R`:
  - Variables are min-max normalized (`normalizar()`), optionally log-transformed for
    skewed variables (e.g. population density via `usar_log_densidad`).
  - Sub-dimensions are combined with `promedio_ponderado_na()`, a weighted average that
    re-normalizes weights per-row to ignore NA dimensions rather than propagating NA.
  - Weights live in a single clearly marked `pesos_ivs` / `pesos_riesgo` block per script
    ("ZONA DE CONTROL DE PESOS") — edit weights there only, not inline in the pipeline.
  - Final risk combines exposure (normalized LST/dLST, extracted from a raster per polygon
    via `terra::extract`) and vulnerability (IVS) either as a weighted product
    (`exposicion_norm^(2w) * ivs^(2w)`, which reduces to the classic IPCC
    exposure × vulnerability formula at equal weights) or a weighted sum, controlled by
    `metodo_riesgo`.
- **NBI (Necesidades Básicas Insatisfechas)**: `nbi_aproximado` is deliberately restricted
  to the 2 dimensions (housing + services) available in *both* Montería and Mérida data, so
  it's comparable cross-city; Mérida computes extra NBI dimensions (crowding, school
  non-attendance, economic dependency) as additional, non-comparable columns/maps — don't
  fold those into `nbi_aproximado` when extending this analysis.
- **Mapping helpers**: each `MAIN_SCRIPT_*.R` defines local `mapa_variable()` (quantile
  classification, `GnBu` palette, used for population/rate variables) and `mapa_carencia()`
  (continuous `viridis`/sqrt-transformed scale, used for rare-and-zero-concentrated count
  variables like "households without electricity"). These are copy-pasted per script, not
  imported from a shared file — if you fix a bug in one city's map function, check whether
  the same bug exists in the other cities' copies.
- **Output conventions**: PNGs are numbered by generation order (`01_...png`, `02_...png`,
  ...) into `data/<city>/`; final tabular/geo outputs are written as both `.gpkg` (full
  geometry) and `.csv` (`st_drop_geometry()`) for downstream use without a GIS tool.

## Notes on data sources (for interpreting indicators correctly)

- Montería: DANE block-level `.shp`/`.dbf` — NBI limited to 2 of 5 classic dimensions.
- Mérida: INEGI "AGEB y manzana urbana" Census 2020 (RESAGEBURB) — supports fuller NBI.
  Census CSV exports from this source have a known malformed-quoting issue around
  accented column values; scripts strip stray quote characters before parsing rather than
  relying on a standard CSV parser.
- Daule: INEC Ecuador CPV 2022, aggregated from anonymized MANLOC microdata via a separate
  browser-based tool (`agregador_manloc_daule.html`, not in this repo) — see comments in
  `MAIN_SCRIPT_DAULE.R` for exactly how each variable is derived.
- Neuquén: INDEC Census 2022, processed through Redatam 7 (CEPAL/CELADE) and exported as
  per-indicator Excel sheets with a fixed 9-row header, joined by 9-digit `radio` code.
  Analysis is clipped to the urban Neuquén–Plottier–Centenario–Vista Alegre conglomerate
  (excludes rural tracts and a bounding-box crop), not the full Confluencia department.
