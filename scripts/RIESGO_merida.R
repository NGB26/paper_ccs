################################################################################
# RIESGO CLIMATICO POR CALOR EXTREMO — MANZANAS, MERIDA
# Riesgo = Exposicion (dLST) x Vulnerabilidad social (IVS: densidad + pob65_imp)
################################################################################
library(sf)
library(dplyr)
library(ggplot2)
library(terra)
library(tidyterra)   # geom_spatraster

setwd("~/Documents/BID-HWandCITIES")

# ---- 0. Cargar datos ---------------------------------------------------------
# La geometria de manzana + poblacion ya viene unida en este gpkg (Marco
# Geoestadistico INEGI + datos censales); no hace falta una capa base aparte.
p1 <- st_read("./Datos_nico/salida_merida/merida_manzanas_poblacion.gpkg")

# ---- 1. Funcion de normalizacion min-max (0-1) -------------------------------
normalizar <- function(x) {
  (x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE))
}

# ---- 1b. Promedio ponderado que ignora NA fila a fila ------------------------
# Si a una manzana le falta UNA dimension (p.ej. no tiene dato de vivienda
# para energia/agua), esta funcion re-normaliza los pesos con las dimensiones
# que SI tiene esa manzana, en vez de devolver NA para todo el indice.
# Si TODAS las dimensiones de una manzana son NA, el resultado es NA.
promedio_ponderado_na <- function(valores, pesos) {
  # valores: data.frame/matrix, una columna por dimension, mismo orden que pesos
  valores <- as.matrix(valores)
  pesos_mat <- matrix(rep(pesos, each = nrow(valores)), nrow = nrow(valores))
  pesos_mat[is.na(valores)] <- NA
  numerador   <- rowSums(valores * pesos_mat, na.rm = TRUE)
  denominador <- rowSums(pesos_mat, na.rm = TRUE)
  ifelse(denominador > 0, numerador / denominador, NA_real_)
}

################################################################################
## PARTE A: EXPOSICION — dLST por manzana
################################################################################

# ---- 2. Cargar el raster de dLST ---------------------------------------------
lst <- rast("./Outputs/dLST/merida_dLST_media.tif")

plot(lst, main = "dLST Merida")

# ---- 3. Alinear CRS y extraer la media de dLST por manzana -------------------
manzanas <- st_transform(p1, crs(lst))

lst_medias <- terra::extract(lst, vect(manzanas), fun = mean, na.rm = TRUE)
manzanas$lst_media <- round(lst_medias[[2]], 2)  # 2a columna = valores extraidos

# ---- 4. ID simplificado de manzana -------------------------------------------
# CVEGEO_shp es un codigo largo, poco practico para leer en mapa -> se
# reemplaza por un ID secuencial y se guarda la tabla de equivalencia.
manzanas <- manzanas %>%
  mutate(id_manzana = row_number()) %>%
  relocate(id_manzana, .before = CVEGEO_shp)

tabla_equivalencias <- manzanas %>%
  st_drop_geometry() %>%
  select(id_manzana, CVEGEO_shp)

# ---- 5. Mapa: raster recortado al bbox + borde de ciudad ---------------------
paleta_lst <- c(
  '#026dff', '#8fbeff',
  '#fff705', '#ffb613', '#ff0000', '#911003'
)

lst_bbox <- crop(lst, vect(manzanas), mask = FALSE)

buffer_dist <- 30  # metros - subir si quedan lineas internas sin fusionar

borde_ciudad <- manzanas %>%
  st_buffer(buffer_dist) %>%
  st_union() %>%
  st_buffer(-buffer_dist) %>%
  st_sf(geometry = .) %>%
  st_cast("POLYGON")

borde_ciudad$area <- st_area(borde_ciudad)
borde_ciudad <- borde_ciudad[which.max(borde_ciudad$area), ]

anillo_exterior <- st_geometry(borde_ciudad)[[1]][1]
borde_ciudad <- st_sf(geometry = st_sfc(st_polygon(anillo_exterior), crs = st_crs(manzanas)))

mapa_lst <- ggplot() +
  geom_spatraster(data = lst_bbox) +
  scale_fill_gradientn(
    colors = paleta_lst,
    limits = c(25, 50),
    breaks = seq(25, 50, 5),
    name = "dLST\n(°C)",
    na.value = NA,
    oob = scales::squish
  ) +
  geom_sf(data = borde_ciudad, fill = NA, color = "black", linewidth = 0.5) +
  theme_minimal() +
  theme(axis.text = element_blank(), axis.ticks = element_blank(), panel.grid = element_blank()) +
  labs(title = "Temperatura superficial (dLST)", subtitle = "Merida", x = NULL, y = NULL)

mapa_lst

# ---- 6. Mapa de manzanas coloreadas por su dLST media ------------------------
mapa_lst_manzanas <- ggplot(manzanas) +
  geom_sf(aes(fill = lst_media), color = NA) +
  scale_fill_gradientn(
    colors = paleta_lst,
    limits = c(25, 45),
    breaks = seq(25, 45, 5),
    name = "dLST\n(°C)",
    na.value = NA,
    oob = scales::squish
  ) +
  theme_minimal() +
  theme(axis.text = element_blank(), axis.ticks = element_blank(), panel.grid = element_blank()) +
  labs(title = "Temperatura de superficie media (por manzanas)", subtitle = "Merida", x = NULL, y = NULL)

mapa_lst_manzanas

################################################################################
## PARTE B: VULNERABILIDAD SOCIAL — densidad + sensibilidad + energia + agua
################################################################################

# ---- 7. Densidad poblacional --------------------------------------------------
# Reproyectar a un CRS metrico para Mexico (INEGI LCC) antes de calcular area
manzanas <- st_transform(manzanas, 6372)

manzanas <- manzanas %>%
  mutate(
    area_km2 = as.numeric(st_area(geom)) / 1e6,
    dens_pob = ifelse(area_km2 > 0, pobtot_imp / area_km2, NA_real_)  # hab/km2
  )

# ---- 8. Variables derivadas y normalizacion -----------------------------------
# Poblacion: se usan las variables *_imp (0% de NA) en vez de pobtot/pob65,
# que tenian 26.4% y 43.2% de NA respectivamente.
#
# Energia / agua: las columnas ya calculadas pct_sin_elec / pct_sin_agua
# tienen MAS NA (39.5% / 42.0%) que los numeradores imputados solos, porque
# dependen ademas de tvivparhab (30.9% NA) y tvivparhab no tiene version
# imputada. Por eso se recalculan aqui con el numerador imputado sobre
# tvivparhab crudo: el NA resultante baja al minimo posible (~30.9%, el de
# tvivparhab), en vez de arrastrar NA de dos variables a la vez.
usar_log_densidad <- TRUE  # activar si la densidad esta muy sesgada (recomendado)

manzanas <- manzanas %>%
  mutate(
    pct_pob_65 = ifelse(pobtot_imp > 0, (pob65_imp / pobtot_imp) * 100, NA_real_),
    
    dens_pob_calc = if (usar_log_densidad) log1p(dens_pob) else dens_pob,
    n_dens_pob    = normalizar(dens_pob_calc),
    
    n_pct_pob_65  = normalizar(pct_pob_65),
    
    # vph_s_elec_imp / vph_aguafv_imp ya representan viviendas SIN el
    # servicio -> mas alto = mas vulnerable, no hace falta invertir.
    pct_sin_elec_recalc = ifelse(tvivparhab > 0, (vph_s_elec_imp / tvivparhab) * 100, NA_real_),
    pct_sin_agua_recalc = ifelse(tvivparhab > 0, (vph_aguafv_imp / tvivparhab) * 100, NA_real_),
    
    n_pct_sin_elec = normalizar(pct_sin_elec_recalc),
    n_pct_sin_agua = normalizar(pct_sin_agua_recalc)
  )

# ---- 9. Sub-indices por dimension ---------------------------------------------
manzanas <- manzanas %>%
  mutate(
    dim_densidad     = n_dens_pob,
    dim_sensibilidad = n_pct_pob_65,
    dim_energia      = n_pct_sin_elec,
    dim_agua         = n_pct_sin_agua
  )

# ---- 10. Indice compuesto de vulnerabilidad social ----------------------------
# =============================================================================
# ZONA DE CONTROL DE PESOS - IVS (editar solo aqui)
# Pesos RELATIVOS, no hace falta que sumen 1 (se normalizan solos). El calculo
# usa promedio_ponderado_na(): si a una manzana le falta una dimension (p.ej.
# sin dato de tvivparhab), el IVS de esa manzana se calcula solo con las
# dimensiones disponibles, re-normalizando los pesos para esa fila.
# =============================================================================
pesos_ivs <- c(
  densidad     = 0,
  sensibilidad = 1,
  energia      = 0,
  agua         = 0
)
pesos_ivs <- pesos_ivs / sum(pesos_ivs)  # normalizacion automatica (suman 1)

manzanas <- manzanas %>%
  mutate(
    ivs = promedio_ponderado_na(
      cbind(dim_densidad, dim_sensibilidad, dim_energia, dim_agua),
      pesos_ivs
    )
  )

# ---- 11. Mapa de vulnerabilidad social -----------------------------------------
a_quintil <- function(x) {
  factor(ntile(x, 5), levels = 1:5,
         labels = c("Muy baja", "Baja", "Media", "Alta", "Muy alta"))
}

manzanas <- manzanas %>%
  mutate(ivs_quintil = a_quintil(ivs))

mapa_ivs_manzanas <- ggplot(manzanas) +
  geom_sf(aes(fill = ivs_quintil), color = NA) +
  scale_fill_brewer(palette = "OrRd", name = "Vulnerabilidad\nsocial", drop = FALSE) +
  theme_minimal() +
  theme(axis.text = element_blank(), axis.ticks = element_blank(), panel.grid = element_blank()) +
  labs(
    title = "Vulnerabilidad social al calor extremo — Manzanas, Merida",
    subtitle = "Densidad poblacional + poblacion 65+ + acceso a energia + acceso a agua",
    caption = "Fuente: INEGI (imputado) | Elaboracion propia"
  )

mapa_ivs_manzanas

################################################################################
## PARTE C: RIESGO = EXPOSICION x VULNERABILIDAD
################################################################################

# ---- 12. Normalizar dLST y calcular riesgo -------------------------------------
# =============================================================================
# ZONA DE CONTROL DE PESOS - RIESGO (editar solo aqui)
# metodo_riesgo:
#   "producto" -> riesgo = exposicion_norm ^ (2*w_exp) * ivs ^ (2*w_vuln)
#                 Con pesos iguales (0.5/0.5) reproduce EXACTAMENTE la formula
#                 clasica exposicion_norm * ivs. Subir el peso de exposicion
#                 sube su exponente por encima de 1 (pesa mas) y baja el de
#                 vulnerabilidad por debajo de 1 (pesa menos), y viceversa.
#   "suma"     -> riesgo = w_exp*exposicion_norm + w_vuln*ivs (promedio
#                 ponderado lineal, mas facil de interpretar pero se aleja
#                 de la formula IPCC clasica).
# =============================================================================
metodo_riesgo <- "producto"   # "producto" o "suma"

pesos_riesgo <- c(
  exposicion     = 1,
  vulnerabilidad = 1
)
pesos_riesgo <- pesos_riesgo / sum(pesos_riesgo)  # normalizacion automatica

manzanas <- manzanas %>%
  mutate(
    exposicion_norm = normalizar(lst_media),
    riesgo = if (metodo_riesgo == "producto") {
      exposicion_norm ^ (2 * pesos_riesgo["exposicion"]) *
        ivs ^ (2 * pesos_riesgo["vulnerabilidad"])
    } else {
      pesos_riesgo["exposicion"] * exposicion_norm +
        pesos_riesgo["vulnerabilidad"] * ivs
    },
    riesgo_quintil = a_quintil(riesgo)
  )

# ---- 13. Mapa de riesgo ---------------------------------------------------------
mapa_riesgo <- ggplot(manzanas) +
  geom_sf(aes(fill = riesgo_quintil), color = NA) +
  scale_fill_brewer(palette = "OrRd", name = "Riesgo\nclimatico", drop = FALSE) +
  theme_minimal() +
  theme(axis.text = element_blank(), axis.ticks = element_blank(), panel.grid = element_blank()) +
  labs(
    title = "Riesgo climatico por calor extremo — Manzanas, Merida",
    subtitle = "Exposicion (dLST) x Vulnerabilidad social",
    caption = "Fuente: INEGI + dLST | Elaboracion propia"
  )

mapa_riesgo

# ---- 14. Tabla final + guardar resultados ---------------------------------------
tabla_riesgo <- manzanas %>%
  st_drop_geometry() %>%
  select(
    id_manzana, CVEGEO_shp, pobtot_imp, dens_pob, pob65_imp, pct_pob_65,
    tvivparhab, pct_sin_elec_recalc, pct_sin_agua_recalc,
    lst_media, exposicion_norm,
    dim_densidad, dim_sensibilidad, dim_energia, dim_agua, ivs, ivs_quintil,
    riesgo, riesgo_quintil
  ) %>%
  arrange(desc(riesgo))

print(head(tabla_riesgo, 20))

# Cuantas manzanas quedan con IVS/riesgo NA por falta total de datos de vivienda
cat("Manzanas con IVS = NA:", sum(is.na(manzanas$ivs)), "de", nrow(manzanas), "\n")

dir.create("./Riesgo", showWarnings = FALSE)
write.csv(tabla_riesgo, "./Riesgo/merida_riesgo_manzanas.csv", row.names = FALSE)
st_write(manzanas, "./Riesgo/merida_riesgo_manzanas.gpkg", delete_dsn = TRUE)

ggsave("./Riesgo/mapa_lst_merida.png",    mapa_lst_manzanas, width = 10, height = 10, dpi = 300, bg = "white")
ggsave("./Riesgo/mapa_ivs_merida.png",    mapa_ivs_manzanas, width = 10, height = 10, dpi = 300, bg = "white")
ggsave("./Riesgo/mapa_riesgo_merida.png", mapa_riesgo,       width = 10, height = 10, dpi = 300, bg = "white")
