# ==============================================================================
# MÉRIDA — Imputación espacial de población (total y 65+) por manzana
# ==============================================================================

library(sf)
library(dplyr)
library(ggplot2)
library(stringr)

carpeta <- "C:/Users/NICOLASGA/OneDrive - Inter-American Development Bank Group/General - SCL_SPH_SPH_CAR/Productos de conocimiento/Paper CCS/Mérida/Manzanas"
ruta_shp <- file.path(carpeta, "2025_31050_M29062026_0700.shp")
ruta_csv <- file.path(carpeta, "merida_manzanas_censo2020.csv")
ruta_csv_nbi <- file.path(carpeta, "merida_manzanas_nbi_extra.csv")   # ver PASO 3.8
carpeta_salida <-"C:/Users/NICOLASGA/OneDrive - Inter-American Development Bank Group/General - SCL_SPH_SPH_CAR/Productos de conocimiento/Paper CCS/paper_ccs/data/merida"
dir.create(carpeta_salida, showWarnings = FALSE)

# ------------------------------------------------------------------------------
# PASO 1. Shapefile de manzanas (INEGI, Marco Geoestadístico 2020)
# ------------------------------------------------------------------------------
manzanas <- st_read(ruta_shp, options = "ENCODING=LATIN1", quiet = TRUE)

# ------------------------------------------------------------------------------
# PASO 2. Reparar y leer el CSV censal (ver explicación en mensajes anteriores:
# el exportador dejó comillas sueltas en campos con tilde, lo que desalinea
# columnas si se lee con un parser de CSV estándar)
# ------------------------------------------------------------------------------
lineas <- readLines(ruta_csv, encoding = "UTF-8", warn = FALSE)
lineas_reparadas <- gsub('"', "", lineas)

censo <- read.csv(text = paste(lineas_reparadas, collapse = "\n"),
                  quote = "", colClasses = "character", stringsAsFactors = FALSE)
stopifnot(ncol(censo) == 19)

cols_numericas <- c("pobtot", "tvivparhab", "vph_s_elec", "vph_aguafv",
                    "vph_ndeaed", "pct_sin_elec", "pct_sin_agua",
                    "pct_sin_servs", "pob65", "pob02")
censo <- censo |>
  mutate(across(all_of(cols_numericas), ~ as.numeric(na_if(str_trim(.x), ""))))

# ------------------------------------------------------------------------------
# PASO 2.5. Evitar colisión de nombres case-insensitive antes del join
# (GeoPackage/SQLite no distingue mayúsculas de minúsculas en nombres de campo)
# ------------------------------------------------------------------------------
dup_en_manzanas <- toupper(names(manzanas)) %in% toupper(names(censo))
names(manzanas)[dup_en_manzanas] <- paste0(names(manzanas)[dup_en_manzanas], "_shp")

# Chequeo: no debería quedar ningún duplicado tras el join
manzanas_ciudad <- manzanas |>
  filter(AMBITO_shp == "Urbana") |>
  left_join(censo, by = c("CVEGEO_shp" = "cvegeo"))

nombres_dup_check <- names(manzanas_ciudad)[
  duplicated(toupper(names(manzanas_ciudad))) | duplicated(toupper(names(manzanas_ciudad)), fromLast = TRUE)
]
if (length(nombres_dup_check) > 0) {
  stop("Todavía hay nombres duplicados: ", paste(nombres_dup_check, collapse = ", "))
}
cat("OK — sin duplicados de nombres./n")



# ------------------------------------------------------------------------------
# PASO 3.5. Vecindario espacial con tolerancia (buffer de 15 m)
# ------------------------------------------------------------------------------
manzanas_utm <- st_transform(manzanas_ciudad, 32616)   # UTM 16N, en metros
buffer_manzanas <- st_buffer(manzanas_utm, dist = 15)

vecinos <- st_intersects(buffer_manzanas)
vecinos <- lapply(seq_along(vecinos), function(i) setdiff(vecinos[[i]], i))

n_prom <- mean(lengths(vecinos))
n_islas <- sum(lengths(vecinos) == 0)
cat("Vecinos promedio:", round(n_prom, 1), "| islas sin vecinos:", n_islas, "/n")

# ------------------------------------------------------------------------------
# PASO 3.6. Función de imputación (promedio iterativo + respaldo por distancia)
# ------------------------------------------------------------------------------
imputar_por_vecinos <- function(valores, vecinos, geom_utm, max_iter = 15) {
  
  valores_imp <- valores
  era_na <- is.na(valores)
  
  # --- Imputación iterativa por vecinos colindantes ---
  for (i in seq_len(max_iter)) {
    na_actual <- which(is.na(valores_imp))
    if (length(na_actual) == 0) break
    
    hubo_cambio <- FALSE
    for (idx in na_actual) {
      vec_vals <- valores_imp[vecinos[[idx]]]
      vec_vals <- vec_vals[!is.na(vec_vals)]
      if (length(vec_vals) > 0) {
        valores_imp[idx] <- mean(vec_vals)
        hubo_cambio <- TRUE
      }
    }
    if (!hubo_cambio) break
  }
  
  # --- Respaldo: manzanas sin ningún vecino con dato -> vecino espacial
  #     más cercano por distancia de centroide ---
  idx_falta <- which(is.na(valores_imp))
  if (length(idx_falta) > 0) {
    idx_con_dato <- which(!is.na(valores))
    vecino_cercano <- st_nearest_feature(geom_utm[idx_falta, ],
                                         geom_utm[idx_con_dato, ])
    valores_imp[idx_falta] <- valores[idx_con_dato[vecino_cercano]]
  }
  
  list(valores = valores_imp, imputado = era_na & !is.na(valores_imp))
}

# --- Población total ---
res_pobtot <- imputar_por_vecinos(manzanas_ciudad$pobtot, vecinos, manzanas_utm)
manzanas_ciudad$pobtot_imp      <- round(res_pobtot$valores)
manzanas_ciudad$pobtot_imputado <- res_pobtot$imputado
cat("Pobtot — imputadas:", sum(res_pobtot$imputado), "/n")

# --- Población 65+ ---
res_pob65 <- imputar_por_vecinos(manzanas_ciudad$pob65, vecinos, manzanas_utm)
manzanas_ciudad$pob65_imp      <- round(res_pob65$valores)
manzanas_ciudad$pob65_imputado <- res_pob65$imputado
cat("Pob65 — imputadas:", sum(res_pob65$imputado), "/n")

# ------------------------------------------------------------------------------
# PASO 4. Función de mapa (reutilizable para ambas variables)
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
      subtitle = "Censo de Población y Vivienda 2020, INEGI (con imputación espacial)",
      caption  = "Fuente: INEGI. Manzanas sin dato imputadas por promedio de vecinos colindantes (buffer 15 m)."
    ) +
    theme_void() +
    theme(legend.title = element_text(size = 9), legend.text = element_text(size = 8))
  
  print(mapa)
  ggsave(file.path(carpeta_salida, archivo), mapa, width = 8, height = 8, dpi = 300, bg = "white")
  invisible(mapa)
}

mapa_variable(manzanas_ciudad, "pobtot_imp",
              "Población total por manzana — Mérida", "Personas",
              "02_poblacion_total_imputado.png")

mapa_variable(manzanas_ciudad, "pob65_imp",
              "Población de 65 años y más por manzana — Mérida", "Personas 65+",
              "03_poblacion_65_mas_imputado.png")




# ------------------------------------------------------------------------------
# PASO 3.7. Imputación espacial — electricidad y agua
# (usa el mismo objeto `vecinos` del Paso 3.5, calculado una sola vez)
# ------------------------------------------------------------------------------
res_elec <- imputar_por_vecinos(manzanas_ciudad$vph_s_elec, vecinos, manzanas_utm)
manzanas_ciudad$vph_s_elec_imp      <- round(res_elec$valores)
manzanas_ciudad$vph_s_elec_imputado <- res_elec$imputado
cat("Sin electricidad — imputadas:", sum(res_elec$imputado), "/n")

res_agua <- imputar_por_vecinos(manzanas_ciudad$vph_aguafv, vecinos, manzanas_utm)
manzanas_ciudad$vph_aguafv_imp      <- round(res_agua$valores)
manzanas_ciudad$vph_aguafv_imputado <- res_agua$imputado
cat("Sin agua entubada — imputadas:", sum(res_agua$imputado), "/n")

# ------------------------------------------------------------------------------
# PASO 4.5. Mapa de variables de carencia (escala continua, no cuantiles)
# ------------------------------------------------------------------------------
# A diferencia de población, estas variables son conteos muy raros y
# concentrados en 0 (>99% de las manzanas no tiene carencia), así que una
# clasificación por cuantiles colapsaría en una sola categoría. Se usa una
# escala continua (viridis) con transformación raíz cuadrada para no saturar
# el mapa con las pocas manzanas de valor alto.

mapa_carencia <- function(datos, var, titulo, leyenda, archivo) {
  
  datos[[paste0(var, "_mapa")]] <- ifelse(datos[[var]] == 0, NA, datos[[var]])
  
  mapa <- ggplot(datos) +
    geom_sf(aes(fill = .data[[paste0(var, "_mapa")]]), colour = "grey80", linewidth=0.01) +
    scale_fill_viridis_c(
      option = "rocket", direction = -1, name = leyenda,
      trans = "sqrt", na.value = "white"
    ) +
    labs(
      title    = titulo,
      subtitle = "Censo de Población y Vivienda 2020, INEGI (con imputación espacial)",
      caption  = "Fuente: INEGI. Manzanas sin dato imputadas por promedio de vecinos colindantes (buffer 15 m)."
    ) +
    theme_void()
  
  print(mapa)
  ggsave(file.path(carpeta_salida, archivo), mapa, width = 8, height = 8, dpi = 300, bg = "white")
  invisible(mapa)
}

mapa_carencia(manzanas_ciudad, "vph_s_elec_imp",
              "Viviendas sin energía eléctrica por manzana — Mérida",
              "Viviendas", "04_sin_electricidad_imputado.png")

mapa_carencia(manzanas_ciudad, "vph_aguafv_imp",
              "Viviendas sin agua entubada por manzana — Mérida",
              "Viviendas", "05_sin_agua_imputado.png")


# ------------------------------------------------------------------------------
# PASO 3.8. NBI (Necesidades Básicas Insatisfechas) — 4 de 5 dimensiones
# ------------------------------------------------------------------------------
# A diferencia de Montería (donde el .dbf de manzana del DANE solo permite
# aproximar 2 de las 5 dimensiones clásicas de NBI), el producto RESAGEBURB
# de INEGI (Censo 2020) SÍ releva a nivel manzana casi todo lo necesario:
#
#   NBI1 Vivienda inadecuada    -> VPH_PISOTI (viviendas con piso de tierra)
#   NBI2 Servicios inadecuados  -> TVIVPARHAB - VPH_C_SERV (viviendas que NO
#                                  tienen electricidad + agua entubada +
#                                  drenaje LOS TRES a la vez). A diferencia de
#                                  Montería, acá no hace falta aproximar la
#                                  superposición entre carencias: INEGI ya
#                                  publica el conteo conjunto (VPH_C_SERV), así
#                                  que "TVIVPARHAB - VPH_C_SERV" es exacto, no
#                                  una cota inferior.
#   NBI3 Hacinamiento crítico   -> PRO_OCUP_C (promedio de ocupantes por
#                                  cuarto). Es una aproximación PARCIAL: INEGI
#                                  da el promedio por manzana, no el % de
#                                  hogares que superan el umbral de INDEC/DANE
#                                  (más de 3 personas por cuarto), porque ese
#                                  conteo no está en el agregado por manzana.
#   NBI4 Inasistencia escolar   -> (P6A11_NOA + P12A14NOA) / (P_6A11 + P_12A14)
#                                  Niños de 6 a 14 años que no asisten a la
#                                  escuela, sobre el total de niños de esa
#                                  edad. Muy cercano al criterio oficial de
#                                  INDEC/DANE (niños 6-12/7-11 años sin
#                                  asistir), aunque el corte de edad exacto
#                                  difiere levemente (6-14 vs 6-12).
#   NBI5 Dependencia económica  -> P_12YMAS / POCUPADA (personas de 12+ años
#                                  por cada persona ocupada). Aproximación
#                                  PARCIAL: releja la idea de dependencia
#                                  económica de INDEC/DANE (4+ personas por
#                                  miembro ocupado), pero no incorpora el
#                                  segundo criterio (escolaridad del jefe de
#                                  hogar), que no está disponible a nivel
#                                  manzana.
#
# Fuente: INEGI, "Principales resultados por AGEB y manzana urbana 2020"
# (descriptor de variables):
# https://www.inegi.org.mx/app/scitel/doc/descriptor/fd_agebmza_urbana_cpv2020.pdf
#
# COMPARABILIDAD ENTRE CIUDADES DEL PAPER: para que el NBI de Mérida sea
# comparable con el de Montería (que solo cubre vivienda + servicios), el
# indicador "nbi_aproximado" de más abajo combina SOLO esas 2 dimensiones
# (mismo criterio: máximo entre ambos porcentajes). Hacinamiento, inasistencia
# escolar y dependencia económica se guardan como columnas y mapas ADICIONALES
# — no se mezclan en nbi_aproximado porque están en otras unidades (personas,
# no viviendas) y no hay manera de saber su superposición con las otras 2
# carencias a nivel manzana (no hay microdato de hogar).

nbi_extra <- read.csv(ruta_csv_nbi, colClasses = c(cvegeo = "character"), stringsAsFactors = FALSE)

manzanas_ciudad <- manzanas_ciudad |>
  left_join(nbi_extra, by = c("CVEGEO_shp" = "cvegeo"))

# --- Imputación espacial (mismo criterio y mismo objeto `vecinos` de 3.5) -----
res_pisoti   <- imputar_por_vecinos(manzanas_ciudad$vph_pisoti, vecinos, manzanas_utm)
res_tviv_nbi <- imputar_por_vecinos(manzanas_ciudad$tviv_nbi,   vecinos, manzanas_utm)
manzanas_ciudad$vph_pisoti_imp   <- round(res_pisoti$valores)
manzanas_ciudad$tviv_nbi_imp     <- round(res_tviv_nbi$valores)

# Déficit de servicios (conteo exacto, no aproximado) calculado ANTES de
# imputar, para no mezclar imputaciones de dos variables distintas.
manzanas_ciudad$servicios_deficit <- manzanas_ciudad$tviv_nbi - manzanas_ciudad$vph_c_serv
res_serv_def <- imputar_por_vecinos(manzanas_ciudad$servicios_deficit, vecinos, manzanas_utm)
manzanas_ciudad$servicios_deficit_imp <- round(res_serv_def$valores)

res_ocup_c <- imputar_por_vecinos(manzanas_ciudad$pro_ocup_c, vecinos, manzanas_utm)
manzanas_ciudad$pro_ocup_c_imp <- res_ocup_c$valores   # es un promedio, no se redondea

res_noa   <- imputar_por_vecinos(manzanas_ciudad$p6a11_noa + manzanas_ciudad$p12a14noa, vecinos, manzanas_utm)
res_base6_14 <- imputar_por_vecinos(manzanas_ciudad$p_6a11 + manzanas_ciudad$p_12a14, vecinos, manzanas_utm)
manzanas_ciudad$escolar_noa_imp  <- round(res_noa$valores)
manzanas_ciudad$escolar_base_imp <- round(res_base6_14$valores)

res_p12ymas  <- imputar_por_vecinos(manzanas_ciudad$p_12ymas, vecinos, manzanas_utm)
res_pocupada <- imputar_por_vecinos(manzanas_ciudad$pocupada, vecinos, manzanas_utm)
manzanas_ciudad$p_12ymas_imp <- round(res_p12ymas$valores)
manzanas_ciudad$pocupada_imp <- round(res_pocupada$valores)

# --- Indicadores finales (a partir de los valores ya imputados) ---------------
manzanas_ciudad <- manzanas_ciudad |>
  mutate(
    pct_vivienda_inadecuada = case_when(
      is.na(tviv_nbi_imp) | tviv_nbi_imp == 0 ~ NA_real_,
      TRUE ~ 100 * vph_pisoti_imp / tviv_nbi_imp
    ),
    pct_servicios_inadecuados = case_when(
      is.na(tviv_nbi_imp) | tviv_nbi_imp == 0 ~ NA_real_,
      TRUE ~ 100 * servicios_deficit_imp / tviv_nbi_imp
    ),
    pct_inasistencia_escolar = case_when(
      is.na(escolar_base_imp) | escolar_base_imp == 0 ~ NA_real_,
      TRUE ~ 100 * escolar_noa_imp / escolar_base_imp
    ),
    razon_dependencia = case_when(
      is.na(pocupada_imp) | pocupada_imp == 0 ~ NA_real_,
      TRUE ~ p_12ymas_imp / pocupada_imp
    ),
    # NBI aproximado comparable con Montería: máximo entre vivienda y
    # servicios inadecuados (las 2 dimensiones que ambas ciudades comparten).
    nbi_aproximado = pmax(pct_vivienda_inadecuada, pct_servicios_inadecuados, na.rm = FALSE)
  )

cat("/n--- NBI aproximado — Mérida ---/n")
cat("NBI aproximado (vivienda + servicios), promedio ciudad:",
    round(mean(manzanas_ciudad$nbi_aproximado, na.rm = TRUE), 2), "%/n")
cat("Hacinamiento (promedio ocupantes/cuarto), promedio ciudad:",
    round(mean(manzanas_ciudad$pro_ocup_c_imp, na.rm = TRUE), 2), "/n")
cat("Inasistencia escolar 6-14 años, promedio ciudad:",
    round(mean(manzanas_ciudad$pct_inasistencia_escolar, na.rm = TRUE), 2), "%/n")
cat("Dependencia económica (personas 12+/ocupado), promedio ciudad:",
    round(mean(manzanas_ciudad$razon_dependencia, na.rm = TRUE), 2), "/n")

# --- Mapa 1: NBI aproximado (vivienda + servicios) — cuantiles, GnBu ----------
# Mismo estilo que Montería (comparable entre ambas ciudades).
mapa_variable(manzanas_ciudad, "nbi_aproximado",
              "NBI aproximado por manzana — Mérida", "NBI aprox. (%)",
              "06_nbi_aproximado.png")

# --- Mapa 2 y 3: vivienda inadecuada y servicios inadecuados por separado ----
# Estilo "carencia" (conteo, escala continua) igual que agua/electricidad,
# porque son variables raras y concentradas en 0.
mapa_carencia(manzanas_ciudad, "vph_pisoti_imp",
              "Viviendas con piso de tierra por manzana — Mérida",
              "Viviendas", "07_vivienda_inadecuada_imputado.png")

mapa_carencia(manzanas_ciudad, "servicios_deficit_imp",
              "Viviendas sin electricidad, agua o drenaje por manzana — Mérida",
              "Viviendas", "08_servicios_inadecuados_imputado.png")

# --- Mapa 4: hacinamiento (variable continua, no concentrada en 0) -----------
mapa_variable(manzanas_ciudad, "pro_ocup_c_imp",
              "Hacinamiento por manzana — Mérida", "Ocupantes/cuarto",
              "09_hacinamiento_imputado.png")

# --- Mapa 5: inasistencia escolar (6-14 años) ---------------------------------
mapa_variable(manzanas_ciudad, "pct_inasistencia_escolar",
              "Inasistencia escolar (6-14 años) por manzana — Mérida", "% no asiste",
              "10_inasistencia_escolar_imputado.png")

# --- Mapa 6: dependencia económica --------------------------------------------
mapa_variable(manzanas_ciudad, "razon_dependencia",
              "Dependencia económica por manzana — Mérida", "Pers. 12+/ocupado",
              "11_dependencia_economica_imputado.png")


# ------------------------------------------------------------------------------
# PASO 5. Guardar base final (robusto frente a locks de OneDrive/SharePoint)
# ------------------------------------------------------------------------------

# Evitar colisión de nombres case-insensitive con las columnas del censo
names(manzanas) <- ifelse(
  toupper(names(manzanas)) %in% toupper(cols_numericas),
  paste0(names(manzanas), "_shp"),
  names(manzanas)
)
# --- 5.1 Escribir con nombre único por corrida (evita conflictos de lock) ---
carpeta_local <- "C:/temp/merida_salida"
dir.create(carpeta_local, showWarnings = FALSE, recursive = TRUE)

timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
nombre_gpkg <- paste0("merida_manzanas_poblacion_", timestamp, ".gpkg")
nombre_csv  <- paste0("merida_manzanas_poblacion_", timestamp, ".csv")

ruta_gpkg_local <- file.path(carpeta_local, nombre_gpkg)
ruta_csv_local  <- file.path(carpeta_local, nombre_csv)

sf::st_write(manzanas_ciudad, ruta_gpkg_local, quiet = TRUE)  # ya no hace falta delete_dsn
write.csv(st_drop_geometry(manzanas_ciudad), ruta_csv_local, row.names = FALSE)

# --- 5.2 Copiar a OneDrive con nombre final fijo (sin el timestamp) ---
file.copy(ruta_gpkg_local, file.path(carpeta_salida, "merida_manzanas_poblacion2.gpkg"), overwrite = TRUE)
file.copy(ruta_csv_local,  file.path(carpeta_salida, "merida_manzanas_poblacion.csv"),  overwrite = TRUE)

message("Listo. Archivo local: ", ruta_gpkg_local)
message("Copiado a: ", carpeta_salida)