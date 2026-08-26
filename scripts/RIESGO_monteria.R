################################################################################
# INDICE DE VULNERABILIDAD SOCIAL AL CALOR EXTREMO — MANZANAS MONTERIA
# Opcion A: solo vulnerabilidad social (sensibilidad + capacidad adaptativa)
# La exposicion climatica (LST) se integra despues en un paso posterior
# (riesgo = exposicion_norm * vulnerabilidad_norm)
#
# NOTA: a diferencia de Neuquen (radios censales), aca la unidad SI es
# manzana (cod_manz), asi que el resultado responde directamente a
# "que manzana es mas vulnerable al calor".
################################################################################
library(sf)
library(dplyr)
library(ggplot2)

# ---- 0. Cargar datos --------------------------------------------------------
setwd("~/Documents/BID-HWandCITIES")
p1 <- st_read("./Datos_nico/salida_monteria/monteria_base_manzanas.shp")

# ---- 1. Funcion de normalizacion min-max (0-1) ------------------------------
normalizar <- function(x) {
  (x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE))
}

# ---- 2. Densidad poblacional -------------------------------------------------
# Reproyectar a un CRS en metros para calcular area correctamente
# (MAGNA-SIRGAS / Origen-Nacional; cambiar si se prefiere otro EPSG)
p1 <- st_transform(p1, 9377)

p1 <- p1 %>%
  mutate(
    area_km2 = as.numeric(st_area(geometry)) / 1e6,
    dens_pob = ifelse(area_km2 > 0, pob_tot / area_km2, NA_real_)  # hab/km2
  )

# ---- 3. Variables derivadas y normalizacion ---------------------------------
usar_log_densidad <- TRUE  # activar si la densidad esta muy sesgada (recomendado)

p1 <- p1 %>%
  mutate(
    # pob_65 es un conteo -> convertir a porcentaje sobre poblacion total
    pct_pob_65 = ifelse(pob_tot > 0, (pob_65 / pob_tot) * 100, NA_real_),
    
    # --- Densidad (mas alto = mas vulnerable al calor) ---
    dens_pob_calc = if (usar_log_densidad) log1p(dens_pob) else dens_pob,
    n_dens_pob    = normalizar(dens_pob_calc),
    
    # --- Sensibilidad biologica (mas alto = mas vulnerable) ---
    n_pct_pob_65  = normalizar(pct_pob_65),
    
    # --- Energia (mas ALTO pct_ener = MENOS vulnerable, porque hay acceso
    #     a ventiladores/aires acondicionados/refrigeracion). Por eso se
    #     invierte antes de normalizar: a menor acceso, mayor vulnerabilidad ---
    falta_energia = 100 - pct_ener,
    n_falta_energia = normalizar(falta_energia)
  )

# ---- 4. Sub-indices por dimension --------------------------------------------
p1 <- p1 %>%
  mutate(
    dim_densidad     = n_dens_pob,
    dim_sensibilidad = n_pct_pob_65,
    dim_energia      = n_falta_energia
  )

# ---- 5. Indice compuesto de vulnerabilidad social ---------------------------
# =============================================================================
# ZONA DE CONTROL DE PESOS - IVS (editar solo aqui)
# Los pesos son RELATIVOS: no es necesario que sumen 1, el script los
# normaliza automaticamente. Por ejemplo, poner energia = 2 significa que
# pesa el doble que las otras dos dimensiones, sin importar los demas valores.
# =============================================================================
pesos_ivs <- c(
  densidad     = 1,
  sensibilidad = 1,
  energia      = 0
)
pesos_ivs <- pesos_ivs / sum(pesos_ivs)  # normalizacion automatica (suman 1)

p1 <- p1 %>%
  mutate(
    ivs = dim_densidad     * pesos_ivs["densidad"] +
      dim_sensibilidad * pesos_ivs["sensibilidad"] +
      dim_energia      * pesos_ivs["energia"]
  )

# ---- 6. Clasificacion en quintiles -------------------------------------------
p1 <- p1 %>%
  mutate(
    ivs_quintil = ntile(ivs, 5),
    ivs_quintil = factor(ivs_quintil,
                         levels = 1:5,
                         labels = c("Muy baja", "Baja", "Media",
                                    "Alta", "Muy alta"))
  )

# ---- 7. Manzanas mas vulnerables ---------------------------------------------
top_vulnerables <- p1 %>%
  st_drop_geometry() %>%
  select(cod_manz, pob_tot, dens_pob, pct_pob_65, pct_ener, ivs, ivs_quintil) %>%
  arrange(desc(ivs)) %>%
  slice_head(n = 20)

print(top_vulnerables)

# ---- 8. Mapa ------------------------------------------------------------------
ggplot(p1) +
  geom_sf(aes(fill = ivs_quintil), color = NA) +
  scale_fill_brewer(palette = "OrRd", name = "Vulnerabilidad\nsocial al calor") +
  labs(
    title = "Vulnerabilidad social al calor extremo — Manzanas, Montería",
    subtitle = "Densidad poblacional + poblacion mayor de 65 anos + acceso a energia",
    caption = "Fuente: DANE (manzanas) | Elaboracion propia"
  ) +
  theme_minimal() +
  theme(axis.text = element_blank(), axis.ticks = element_blank())

# ---- 9. Guardar resultados -----------------------------------------------------
dir.create("./Riesgo", showWarnings = FALSE)
st_write(p1, "./Riesgo/monteria_ivs_manzanas.gpkg", delete_dsn = TRUE)
write.csv(top_vulnerables, "./Riesgo/monteria_top20_vulnerables.csv", row.names = FALSE)

################################################################################
################################################################################
# RIESGO = EXPOSICION (LST) x VULNERABILIDAD SOCIAL (IVS)
################################################################################
library(terra)

# ---- 10. Cargar el raster de LST ---------------------------------------------
lst <- rast("./Outputs/dLST/Montería/resultados/monteria_LST_media.tif")

# ---- 11. Exposicion: LST media por manzana -----------------------------------
# p1 quedo en EPSG:9377 (paso 2, para calcular area/densidad). Para extraer
# el raster hay que pasar la geometria al CRS del raster.
p1_lst <- st_transform(p1, crs(lst))

lst_medias <- terra::extract(lst, vect(p1_lst), fun = mean, na.rm = TRUE)
p1$lst_media <- round(lst_medias[[2]], 2)  # 2a columna = valores extraidos

# ---- 12. Normalizar LST y calcular riesgo ------------------------------------
# =============================================================================
# ZONA DE CONTROL DE PESOS - RIESGO (editar solo aqui)
# Igual que arriba, los pesos son RELATIVOS y se normalizan automaticamente.
# metodo_riesgo:
#   "producto" -> riesgo = exposicion_norm ^ (2*w_exp) * ivs ^ (2*w_vuln)
#                 Con pesos iguales (0.5/0.5) esto reproduce EXACTAMENTE el
#                 calculo original (exposicion_norm * ivs). Si subes el peso
#                 de exposicion, su exponente sube por encima de 1 (pesa mas
#                 fuerte) y el de vulnerabilidad baja por debajo de 1 (pesa
#                 menos), y viceversa.
#   "suma"     -> riesgo = w_exp*exposicion_norm + w_vuln*ivs (promedio
#                 ponderado, mas facil de interpretar linealmente pero ya
#                 no es la formula clasica de riesgo climatico).
# =============================================================================
metodo_riesgo <- "producto"   # "producto" o "suma"

pesos_riesgo <- c(
  exposicion    = 1,
  vulnerabilidad = 1
)
pesos_riesgo <- pesos_riesgo / sum(pesos_riesgo)  # normalizacion automatica

p1 <- p1 %>%
  mutate(
    exposicion_norm = normalizar(lst_media),
    riesgo = if (metodo_riesgo == "producto") {
      exposicion_norm ^ (2 * pesos_riesgo["exposicion"]) *
        ivs ^ (2 * pesos_riesgo["vulnerabilidad"])
    } else {
      pesos_riesgo["exposicion"] * exposicion_norm +
        pesos_riesgo["vulnerabilidad"] * ivs
    }
  )

# ---- 13. Clasificacion en quintiles -------------------------------------------
p1 <- p1 %>%
  mutate(
    riesgo_quintil = ntile(riesgo, 5),
    riesgo_quintil = factor(riesgo_quintil,
                            levels = 1:5,
                            labels = c("Muy baja", "Baja", "Media",
                                       "Alta", "Muy alta"))
  )

# ---- 14. Manzanas de mayor riesgo ---------------------------------------------
top_riesgo <- p1 %>%
  st_drop_geometry() %>%
  select(cod_manz, pob_tot, dens_pob, pct_pob_65, pct_ener, ivs, ivs_quintil,
         lst_media, exposicion_norm, riesgo, riesgo_quintil) %>%
  arrange(desc(riesgo)) %>%
  slice_head(n = 20)

print(top_riesgo)

# ---- 15. Mapa de riesgo --------------------------------------------------------
tema_mapa <- theme_minimal() +
  theme(axis.text = element_blank(), axis.ticks = element_blank(), panel.grid = element_blank())

mapa_riesgo <- ggplot(p1) +
  geom_sf(aes(fill = riesgo_quintil), color = NA) +
  scale_fill_brewer(palette = "OrRd", name = "Riesgo\nclimatico", drop = FALSE) +
  labs(
    title = "Riesgo climatico por calor extremo — Manzanas, Montería",
    subtitle = "Exposicion (LST) x Vulnerabilidad social",
    caption = "Fuente: DANE + Landsat LST | Elaboracion propia"
  ) +
  tema_mapa

mapa_riesgo

# ---- 17. Mapa de LST por manzana -----------------------------------------------
paleta_lst <- c(
  '#026dff', '#8fbeff',
  '#fff705', '#ffb613', '#ff0000', '#911003'
)

mapa_lst_manzanas <- ggplot(p1) +
  geom_sf(aes(fill = lst_media), color = NA) +
  scale_fill_gradientn(
    colors = paleta_lst,
    limits = c(25, 50),
    breaks = seq(25, 50, 5),
    name = "LST media\n(°C)",
    na.value = NA,
    oob = scales::squish
  ) +
  labs(
    title = "Temperatura superficial (LST) — Manzanas, Montería",
    subtitle = "Media de LST por manzana",
    caption = "Fuente: Landsat | Elaboracion propia"
  ) +
  tema_mapa

mapa_lst_manzanas

# ---- 18. Mapa de vulnerabilidad social por manzana -----------------------------
mapa_ivs_manzanas <- ggplot(p1) +
  geom_sf(aes(fill = ivs_quintil), color = NA) +
  scale_fill_brewer(palette = "OrRd", name = "Vulnerabilidad\nsocial", drop = FALSE) +
  labs(
    title = "Vulnerabilidad social al calor extremo — Manzanas, Montería",
    subtitle = "Densidad poblacional + poblacion mayor de 65 anos + acceso a energia",
    caption = "Fuente: DANE | Elaboracion propia"
  ) +
  tema_mapa

mapa_ivs_manzanas

# ---- 19. Guardar los dos mapas nuevos -------------------------------------------
ggsave("./Riesgo/mapa_lst_monteria.png", mapa_lst_manzanas, width = 10, height = 10, dpi = 300, bg = "white")
ggsave("./Riesgo/mapa_ivs_monteria.png", mapa_ivs_manzanas, width = 10, height = 10, dpi = 300, bg = "white")
# ---- 16. Guardar resultados finales --------------------------------------------
st_write(p1, "./Riesgo/monteria_riesgo_manzanas.gpkg", delete_dsn = TRUE)
write.csv(top_riesgo, "./Riesgo/monteria_top20_riesgo.csv", row.names = FALSE)
ggsave("./Riesgo/mapa_riesgo_monteria.png", mapa_riesgo, width = 10, height = 10, dpi = 300, bg = "white")