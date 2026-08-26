################################################################################
# RIESGO CLIMATICO POR CALOR EXTREMO — RADIOS CENSALES, NEUQUEN
# Riesgo = Exposicion (LST normalizada) x Vulnerabilidad social (IVS normalizado)
#
# Metodologia: metodologia_IVS_calor_intraurbano.docx (BID-HWandCITIES, 2025)
# Cambios respecto al documento original:
# - Sensibilidad usa unicamente pct_pob_65_mas (no las 3 variables de edad).
# - Capacidad socioeconomica usa unicamente tasa_empleo (se elimina
#   tasa_actividad, redundante con tasa_empleo).
# - Infraestructura usa unicamente pct_sin_agua_red (se elimina
#   pct_desague_sin_red, poco informativo: casi todos los radios de riesgo
#   alto ya tienen desague de red).
# - Pesos de IVS y de Riesgo ahora se configuran en un solo lugar cada uno
#   (ver ZONA DE CONTROL DE PESOS), y el grafico de composicion (paso 9)
#   respeta automaticamente esos mismos pesos.
################################################################################

library(sf)
library(terra)
library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)

# ---- 0. Configuracion ---------------------------------------------------------
setwd("~/Documents/BID-HWandCITIES")

f_indicadores <- "./Datos_nico/salida_neuquen/confluencia_neuquen_indicadores.gpkg"
f_lst         <- "./Outputs/dLST/NQN/resultados/neuquen_LST_media.tif"
dir_salida    <- "./Outputs/riesgo_nqn"
dir.create(dir_salida, showWarnings = FALSE, recursive = TRUE)

# ---- 1. Cargar datos -----------------------------------------------------------
radios <- st_read(f_indicadores, layer = "confluencia_neuquen_indicadores", quiet = TRUE)
lst    <- rast(f_lst)

# ---- 2. Exposicion: LST media por radio censal ---------------------------------
radios <- st_transform(radios, crs(lst))

ext_sf <- st_as_sf(as.polygons(ext(lst), crs = crs(lst)))
radios <- radios[st_intersects(radios, ext_sf, sparse = FALSE)[, 1], ]  # solo radios dentro del raster

lst_medias <- terra::extract(lst, vect(radios), fun = mean, na.rm = TRUE)
radios$lst_media <- round(lst_medias[[2]], 2)

bbox_lst <- st_bbox(ext_sf)  # acota los mapas al area urbana real

# ---- 3. Normalizacion min-max (0-1) ---------------------------------------------
normalizar <- function(x) (x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE))

# ---- 4. Vulnerabilidad social (IVS) ---------------------------------------------
# Cada dimension es una unica variable normalizada -> no hace falta promediar
# (rowMeans) dentro de capacidad socioeconomica ni infraestructura.
radios <- radios %>%
  mutate(
    n_pct_pob_65_mas   = normalizar(pct_pob_65_mas),
    n_tasa_empleo_inv  = 1 - normalizar(tasa_empleo),   # mas alto = menos vulnerable -> se invierte
    n_pct_sin_agua_red = normalizar(pct_sin_agua_red),
    
    dim_sensibilidad             = n_pct_pob_65_mas,
    dim_capacidad_socioeconomica = n_tasa_empleo_inv,
    dim_infraestructura          = n_pct_sin_agua_red
  )

# =============================================================================
# ZONA DE CONTROL DE PESOS - IVS (editar solo aqui)
# Pesos RELATIVOS: no hace falta que sumen 1, el script los normaliza solo.
# Por defecto 1/1/1 = el tercio parejo de la metodologia original.
# =============================================================================
pesos_ivs <- c(
  sensibilidad             = 1,
  capacidad_socioeconomica = 0,
  infraestructura          = 0
)
pesos_ivs <- pesos_ivs / sum(pesos_ivs)  # normalizacion automatica (suman 1)

radios <- radios %>%
  mutate(
    ivs = dim_sensibilidad             * pesos_ivs["sensibilidad"] +
      dim_capacidad_socioeconomica * pesos_ivs["capacidad_socioeconomica"] +
      dim_infraestructura          * pesos_ivs["infraestructura"]
  )

# ---- 5. Exposicion normalizada + Riesgo climatico --------------------------------
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

radios <- radios %>%
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

# ---- 6. Quintiles para los mapas -----------------------------------------------
a_quintil <- function(x) {
  factor(ntile(x, 5), levels = 1:5,
         labels = c("Muy baja", "Baja", "Media", "Alta", "Muy alta"))
}

radios <- radios %>%
  mutate(
    riesgo_quintil = a_quintil(riesgo),
    ivs_quintil    = a_quintil(ivs)
  )

# ---- 7. Mapas ----------------------------------------------------------------------
tema_mapa <- theme_minimal() +
  theme(axis.text = element_blank(), axis.ticks = element_blank(), panel.grid = element_blank())

mapa_riesgo <- ggplot(radios) +
  geom_sf(aes(fill = riesgo_quintil), color = NA) +
  scale_fill_brewer(palette = "OrRd", name = "Riesgo\nclimatico", drop = FALSE) +
  coord_sf(xlim = c(bbox_lst["xmin"], bbox_lst["xmax"]),
           ylim = c(bbox_lst["ymin"], bbox_lst["ymax"]), expand = FALSE) +
  labs(title = "Riesgo climatico por calor extremo",
       subtitle = "Radios censales, Neuquen — Exposicion (LST) x Vulnerabilidad social",
       caption = "Fuente: INDEC + Landsat LST | Elaboracion propia") +
  tema_mapa

mapa_lst <- ggplot(radios) +
  geom_sf(aes(fill = lst_media), color = NA) +
  scale_fill_viridis_c(option = "inferno", name = "LST media\n(°C)") +
  coord_sf(xlim = c(bbox_lst["xmin"], bbox_lst["xmax"]),
           ylim = c(bbox_lst["ymin"], bbox_lst["ymax"]), expand = FALSE) +
  labs(title = "Temperatura superficial (LST)",
       subtitle = "Radios censales, Neuquen",
       caption = "Fuente: Landsat | Elaboracion propia") +
  tema_mapa

mapa_vulnerabilidad <- ggplot(radios) +
  geom_sf(aes(fill = ivs_quintil), color = NA) +
  scale_fill_brewer(palette = "OrRd", name = "Vulnerabilidad\nsocial", drop = FALSE) +
  coord_sf(xlim = c(bbox_lst["xmin"], bbox_lst["xmax"]),
           ylim = c(bbox_lst["ymin"], bbox_lst["ymax"]), expand = FALSE) +
  labs(title = "Vulnerabilidad social al calor extremo",
       subtitle = "Radios censales, Neuquen — Sensibilidad (65+) + capacidad adaptativa",
       caption = "Fuente: INDEC | Elaboracion propia") +
  tema_mapa

mapa_vulnerabilidad 

ggsave(file.path(dir_salida, "mapa_riesgo_nqn.png"), mapa_riesgo, width = 10, height = 10, dpi = 300, bg = "white")
ggsave(file.path(dir_salida, "mapa_lst_nqn.png"), mapa_lst, width = 10, height = 10, dpi = 300, bg = "white")
ggsave(file.path(dir_salida, "mapa_vulnerabilidad_nqn.png"), mapa_vulnerabilidad, width = 10, height = 10, dpi = 300, bg = "white")

# ---- 8. Tabla final: variables usadas + exposicion (LST) + vulnerabilidad + riesgo ---
tabla_riesgo <- radios %>%
  st_drop_geometry() %>%
  select(
    codigo, NOMDEPTO, TIPO,
    pct_pob_65_mas, tasa_empleo, pct_sin_agua_red,
    lst_media, exposicion_norm,
    dim_sensibilidad, dim_capacidad_socioeconomica, dim_infraestructura, ivs, ivs_quintil,
    riesgo, riesgo_quintil
  ) %>%
  arrange(desc(riesgo))

write.csv(tabla_riesgo, file.path(dir_salida, "neuquen_riesgo_radios.csv"), row.names = FALSE)
st_write(radios, file.path(dir_salida, "neuquen_riesgo_radios.gpkg"), delete_dsn = TRUE)

# ---- 9. Composicion del riesgo en radios Alta/Muy alta (grafico de tortas + tabla) ----
# La composicion ahora se pondera con los MISMOS pesos definidos arriba
# (pesos_ivs y pesos_riesgo), en vez de promediar las 4 variables crudas por
# igual. Asi, si subes el peso de "infraestructura" en pesos_ivs, la torta
# tambien le da mas protagonismo a "Sin agua de red" automaticamente.
# Nota: esta ponderacion es exacta para metodo_riesgo = "suma"; si se usa
# "producto" es una aproximacion lineal (el riesgo multiplicativo no se
# descompone linealmente en componentes, pero el reparto relativo entre
# sensibilidad/capacidad/infraestructura dentro del IVS si es exacto).
etiquetas <- c(
  exposicion_norm    = "Exposicion (LST)",
  n_pct_pob_65_mas    = "Pob. 65+ (sensibilidad)",
  n_tasa_empleo_inv   = "Baja tasa de empleo",
  n_pct_sin_agua_red  = "Sin agua de red"
)

pesos_componentes <- c(
  exposicion_norm    = unname(pesos_riesgo["exposicion"]),
  n_pct_pob_65_mas    = unname(pesos_riesgo["vulnerabilidad"] * pesos_ivs["sensibilidad"]),
  n_tasa_empleo_inv   = unname(pesos_riesgo["vulnerabilidad"] * pesos_ivs["capacidad_socioeconomica"]),
  n_pct_sin_agua_red  = unname(pesos_riesgo["vulnerabilidad"] * pesos_ivs["infraestructura"])
)

tabla_componentes <- radios %>%
  st_drop_geometry() %>%
  filter(riesgo_quintil %in% c("Alta", "Muy alta")) %>%
  summarise(across(all_of(names(etiquetas)), \(x) mean(x, na.rm = TRUE))) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "media") %>%
  mutate(
    etiqueta    = etiquetas[variable],
    aporte      = media * pesos_componentes[variable],
    proporcion  = aporte / sum(aporte)
  ) %>%
  arrange(desc(proporcion))

write.csv(tabla_componentes, file.path(dir_salida, "tabla_componentes_riesgo_alto.csv"), row.names = FALSE)

n_alto <- sum(radios$riesgo_quintil %in% c("Alta", "Muy alta"))
paleta <- c("#fdae6b", "#e6550d", "#08519c", "#74c476")

mapa_torta <- ggplot(tabla_componentes %>% mutate(etiqueta = factor(etiqueta, levels = etiqueta)),
                     aes(x = "", y = proporcion, fill = etiqueta)) +
  geom_col(width = 1, color = "white") +
  coord_polar(theta = "y") +
  geom_text(aes(label = percent(proporcion, accuracy = 1)),
            position = position_stack(vjust = 0.5), color = "white", size = 5, fontface = "bold") +
  scale_fill_manual(values = paleta, name = "Componente") +
  labs(title = "Composicion del riesgo en radios de riesgo Alta y Muy alta",
       subtitle = paste0("Neuquen, n = ", n_alto, " radios censales"),
       caption = "Elaboracion propia") +
  theme_void() +
  theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 14),
        plot.subtitle = element_text(hjust = 0.5, size = 10, margin = margin(b = 10)),
        legend.position = "right")

ggsave(file.path(dir_salida, "torta_componentes_riesgo_alto.png"), mapa_torta, width = 8, height = 6, dpi = 300, bg = "white")