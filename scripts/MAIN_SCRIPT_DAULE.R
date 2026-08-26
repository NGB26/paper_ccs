# ==============================================================================
# DAULE — Indicadores censales por manzana (CPV 2022, INEC)
# ==============================================================================
# Estructura análoga a MAIN_SCRIPT_MERIDA.R: (1) shapefile de manzanas,
# (2) CSV de indicadores censales por manzana, (3) unión espacial-tabular,
# (4) mapas, (5) guardado robusto de la base final (shp + gpkg + csv).
#
# Fuentes:
#  - Cartografía: INEC, Marco Geoestadístico Cantonal 2021 (cód. 0906 DAULE),
#    ya descargado y convertido a shapefile en 01_Cartografia_Manzanas/.
#  - Indicadores: microdatos anonimizados MANLOC del VIII Censo de Población y
#    VII de Vivienda 2022 (INEC), agregados a nivel de manzana/localidad con
#    la herramienta agregador_manloc_daule.html (corre en el navegador, sobre
#    los 3 CSV nacionales de Vivienda/Hogar/Población — ver esa herramienta
#    para el detalle exacto de cómo se calcula cada variable).
# ==============================================================================

library(sf)
library(dplyr)
library(tidyr)     # replace_na
library(ggplot2)
library(stringr)

carpeta <- "C:/Users/NICOLASGA/OneDrive - Inter-American Development Bank Group/General - SCL_SPH_SPH_CAR/Productos de conocimiento/Paper CCS/Daule"
ruta_shp <- file.path(carpeta, "01_Cartografia_Manzanas", "daule_manzanas.shp")
ruta_csv <- file.path(carpeta, "daule_manzanas_indicadores_censo2022.csv")
carpeta_salida <- file.path(carpeta, "salidas")
dir.create(carpeta_salida, showWarnings = FALSE)

# ------------------------------------------------------------------------------
# PASO 1. Shapefile de manzanas (INEC, Marco Geoestadístico Cantonal 2021)
# ------------------------------------------------------------------------------
manzanas <- st_read(ruta_shp, quiet = TRUE)

# El campo `man` es el código de 15 dígitos I01+I02+I03+I04+I05+I06 (provincia+
# cantón+parroquia+zona+sector+manzana), idéntico a como se arma la llave
# `codigo_manzana` en el CSV de indicadores -> se usa directo como clave de
# unión, sin necesidad de recodificar nada.

# ------------------------------------------------------------------------------
# PASO 2. Leer el CSV de indicadores (generado con agregador_manloc_daule.html
# a partir de los microdatos MANLOC del CPV2022)
# ------------------------------------------------------------------------------
censo <- read.csv(ruta_csv, colClasses = "character", stringsAsFactors = FALSE)

cols_numericas <- c("poblacion_total", "poblacion_65_mas", "total_viviendas",
                     "viviendas_sin_electricidad", "viviendas_sin_agua_entubada",
                     "total_hogares", "hogares_hacinamiento", "hogares_nbi")

censo <- censo |>
  mutate(across(all_of(cols_numericas), ~ as.numeric(str_trim(.x))))

# ------------------------------------------------------------------------------
# PASO 2.5. Evitar colisión de nombres case-insensitive antes del join
# (GeoPackage/SQLite no distingue mayúsculas de minúsculas en nombres de campo)
# ------------------------------------------------------------------------------
dup_en_manzanas <- toupper(names(manzanas)) %in% toupper(names(censo))
names(manzanas)[dup_en_manzanas] <- paste0(names(manzanas)[dup_en_manzanas], "_shp")

manzanas_daule <- manzanas |>
  left_join(censo, by = c("man" = "codigo_manzana"))

nombres_dup_check <- names(manzanas_daule)[
  duplicated(toupper(names(manzanas_daule))) | duplicated(toupper(names(manzanas_daule)), fromLast = TRUE)
]
if (length(nombres_dup_check) > 0) {
  stop("Todavía hay nombres duplicados: ", paste(nombres_dup_check, collapse = ", "))
}
cat("OK — sin duplicados de nombres.\n")

# ------------------------------------------------------------------------------
# PASO 3. Manzanas sin ningún registro en el censo = 0, no NA
# ------------------------------------------------------------------------------
# A diferencia del censo mexicano (INEGI), que suprime/redacta conteos chicos
# por confidencialidad y genera NA reales, los microdatos anonimizados MANLOC
# del INEC no suprimen conteos: una manzana ausente del CSV de indicadores
# significa que, tras filtrar a Daule, no hubo ningún registro de vivienda,
# hogar o persona con ese código — es decir, la manzana está genuinamente
# deshabitada o es un polígono no residencial (parque, vía, equipamiento,
# etc.), no un dato faltante.
#
# Por eso, a propósito, NO se aplica aquí la imputación espacial por vecinos
# que usa MAIN_SCRIPT_MERIDA.R: esa imputación resuelve un problema de
# censura estadística (NA por privacidad) que no existe en esta fuente. Basta
# con completar en 0.
n_sin_match <- sum(is.na(manzanas_daule$poblacion_total))
cat("Manzanas sin ningún registro censal (se completan en 0):", n_sin_match,
    "de", nrow(manzanas_daule), "\n")

manzanas_daule <- manzanas_daule |>
  mutate(across(all_of(cols_numericas), ~ replace_na(., 0)))

# ------------------------------------------------------------------------------
# PASO 3.5. Variables derivadas (porcentajes, para los mapas de carencia)
# ------------------------------------------------------------------------------
manzanas_daule <- manzanas_daule |>
  mutate(
    pct_pob_65       = ifelse(poblacion_total > 0, poblacion_65_mas / poblacion_total * 100, NA_real_),
    pct_sin_elec     = ifelse(total_viviendas > 0, viviendas_sin_electricidad / total_viviendas * 100, NA_real_),
    pct_sin_agua     = ifelse(total_viviendas > 0, viviendas_sin_agua_entubada / total_viviendas * 100, NA_real_),
    pct_hacinamiento = ifelse(total_hogares  > 0, hogares_hacinamiento / total_hogares * 100, NA_real_),
    pct_nbi          = ifelse(total_hogares  > 0, hogares_nbi / total_hogares * 100, NA_real_)
  )

# ------------------------------------------------------------------------------
# PASO 4. Función de mapa por cuantiles (variables de nivel: población)
# ------------------------------------------------------------------------------
mapa_variable <- function(datos, var, titulo, leyenda, archivo) {

  vals_pos <- datos[[var]][datos[[var]] > 0]
  cortes <- unique(quantile(vals_pos, probs = seq(0, 1, length.out = 7), na.rm = TRUE))

  datos$clas <- case_when(
    datos[[var]] == 0 ~ NA_character_,
    TRUE ~ as.character(cut(datos[[var]], breaks = c(-Inf, cortes, Inf),
                            include.lowest = TRUE, dig.lab = 5))
  )
  datos$clas <- factor(datos$clas,
                       levels = levels(cut(cortes, breaks = c(-Inf, cortes, Inf),
                                           include.lowest = TRUE, dig.lab = 5)))

  mapa <- ggplot(datos) +
    geom_sf(aes(fill = clas), colour = NA) +
    scale_fill_brewer(palette = "GnBu", direction = 1, name = leyenda,
                      na.value = "white", na.translate = TRUE) +
    labs(
      title    = titulo,
      subtitle = "Censo de Población y Vivienda 2022, INEC",
      caption  = "Fuente: INEC, microdatos MANLOC (CPV2022) + Marco Geoestadístico Cantonal."
    ) +
    theme_void() +
    theme(legend.title = element_text(size = 9), legend.text = element_text(size = 8))

  print(mapa)
  ggsave(file.path(carpeta_salida, archivo), mapa, width = 8, height = 8, dpi = 300, bg = "white")
  invisible(mapa)
}

mapa_variable(manzanas_daule, "poblacion_total",
              "Población total por manzana — Daule", "Personas",
              "02_poblacion_total.png")

mapa_variable(manzanas_daule, "poblacion_65_mas",
              "Población de 65 años y más por manzana — Daule", "Personas 65+",
              "03_poblacion_65_mas.png")

# ------------------------------------------------------------------------------
# PASO 4.5. Mapa de carencias (escala continua; variables muy concentradas en 0)
# ------------------------------------------------------------------------------
mapa_carencia <- function(datos, var, titulo, leyenda, archivo) {

  datos[[paste0(var, "_mapa")]] <- ifelse(datos[[var]] == 0, NA, datos[[var]])

  mapa <- ggplot(datos) +
    geom_sf(aes(fill = .data[[paste0(var, "_mapa")]]), colour = "grey80", linewidth = 0.01) +
    scale_fill_viridis_c(
      option = "rocket", direction = -1, name = leyenda,
      trans = "sqrt", na.value = "white"
    ) +
    labs(
      title    = titulo,
      subtitle = "Censo de Población y Vivienda 2022, INEC",
      caption  = "Fuente: INEC, microdatos MANLOC (CPV2022) + Marco Geoestadístico Cantonal."
    ) +
    theme_void()

  print(mapa)
  ggsave(file.path(carpeta_salida, archivo), mapa, width = 8, height = 8, dpi = 300, bg = "white")
  invisible(mapa)
}

mapa_carencia(manzanas_daule, "viviendas_sin_electricidad",
              "Viviendas sin electricidad por manzana — Daule",
              "Viviendas", "04_sin_electricidad.png")

mapa_carencia(manzanas_daule, "viviendas_sin_agua_entubada",
              "Viviendas sin agua entubada por manzana — Daule",
              "Viviendas", "05_sin_agua.png")

mapa_carencia(manzanas_daule, "hogares_hacinamiento",
              "Hogares con hacinamiento por manzana — Daule",
              "Hogares", "06_hacinamiento.png")

mapa_carencia(manzanas_daule, "hogares_nbi",
              "Hogares con NBI por manzana — Daule",
              "Hogares", "07_nbi.png")

# ------------------------------------------------------------------------------
# PASO 5. Guardar base final (robusto frente a locks de OneDrive/SharePoint)
# ------------------------------------------------------------------------------
# Se escribe primero a una carpeta local (fuera de OneDrive) y recién después
# se copia al destino final, igual que en MAIN_SCRIPT_MERIDA.R: evita fallos
# por bloqueo de archivo cuando OneDrive está sincronizando en el momento en
# que R intenta escribir el gpkg/shp.

carpeta_local <- "C:/temp/daule_salida"
dir.create(carpeta_local, showWarnings = FALSE, recursive = TRUE)

timestamp   <- format(Sys.time(), "%Y%m%d_%H%M%S")
nombre_gpkg <- paste0("daule_manzanas_indicadores_", timestamp, ".gpkg")
nombre_csv  <- paste0("daule_manzanas_indicadores_", timestamp, ".csv")

ruta_gpkg_local <- file.path(carpeta_local, nombre_gpkg)
ruta_csv_local  <- file.path(carpeta_local, nombre_csv)

sf::st_write(manzanas_daule, ruta_gpkg_local, quiet = TRUE)
write.csv(st_drop_geometry(manzanas_daule), ruta_csv_local, row.names = FALSE)

file.copy(ruta_gpkg_local, file.path(carpeta_salida, "daule_manzanas_indicadores.gpkg"), overwrite = TRUE)
file.copy(ruta_csv_local,  file.path(carpeta_salida, "daule_manzanas_indicadores.csv"),  overwrite = TRUE)

# --- 5.1 Además, shapefile final (.shp/.shx/.dbf/.prj) para que Daule quede
#     en el mismo formato que Montería. DBF limita los nombres de campo a 10
#     caracteres, así que se renombran a mano ANTES de escribir para evitar
#     que GDAL trunque y choque nombres automáticamente. ---
manzanas_shp_out <- manzanas_daule |>
  rename(
    POB_TOT   = poblacion_total,
    POB_65    = poblacion_65_mas,
    VIV_TOT   = total_viviendas,
    VIV_SELEC = viviendas_sin_electricidad,
    VIV_SAGUA = viviendas_sin_agua_entubada,
    HOG_TOT   = total_hogares,
    HOG_HAC   = hogares_hacinamiento,
    HOG_NBI   = hogares_nbi,
    PCT_65    = pct_pob_65,
    PCT_SELEC = pct_sin_elec,
    PCT_SAGUA = pct_sin_agua,
    PCT_HAC   = pct_hacinamiento,
    PCT_NBI   = pct_nbi
  )

ruta_shp_local <- file.path(carpeta_local, paste0("daule_manzanas_indicadores_", timestamp, ".shp"))
sf::st_write(manzanas_shp_out, ruta_shp_local, driver = "ESRI Shapefile", delete_layer = TRUE, quiet = TRUE)

for (ext in c(".shp", ".shx", ".dbf", ".prj")) {
  origen  <- sub("\\.shp$", ext, ruta_shp_local)
  destino <- file.path(carpeta_salida, paste0("daule_manzanas_indicadores", ext))
  if (file.exists(origen)) file.copy(origen, destino, overwrite = TRUE)
}

message("Listo. Archivos locales en: ", carpeta_local)
message("Copiados a: ", carpeta_salida)
