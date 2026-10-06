# ==============================================================================
# Montería: mapa de la ciudad y sus establecimientos de salud
#
# Datos de entrada : monteria_base_manzanas.shp   (manzanas DANE, delimitan la
#                                                   ciudad por unión/disolución)
#                     Health_Facilities_ES.shp     (sedes del REPS, WGS84)
# Salida           : 02_establecimientos_salud_monteria.png (mapa ggplot)
# ==============================================================================

library(sf)
library(dplyr)
library(ggplot2)

# ---- 1. PARÁMETROS -----------------------------------------------------------
carpeta_datos <- "C:/Users/NICOLASGA/OneDrive - Inter-American Development Bank Group/General - SCL_SPH_SPH_CAR/Productos de conocimiento/Paper CCS/paper_ccs/data/monteria"

RUTA_MANZANAS <- file.path(carpeta_datos, "monteria_base_manzanas.shp")
RUTA_SALUD    <- file.path(carpeta_datos, "health_facilities_es", "Health_Facilities_ES.shp")

MUNICIPIO    <- "MONTERÍA"
DEPARTAMENTO <- "Córdoba"
CRS_METROS   <- 9377   # MAGNA-SIRGAS Origen-Nacional (igual que "mapeo PNA monteria.R")


# ---- 2. CIUDAD: disolver las manzanas en un solo polígono --------------------
manzanas <- st_read(RUTA_MANZANAS, quiet = TRUE) |> st_transform(CRS_METROS)
ciudad   <- st_union(manzanas) |> st_sf(geometry = _)


# ---- 3. ESTABLECIMIENTOS DE SALUD DENTRO DEL POLÍGONO DE LA CIUDAD -----------
salud_municipio <- st_read(RUTA_SALUD, quiet = TRUE) |>
  filter(NOM_MPIO == MUNICIPIO, NOM_DPTO == DEPARTAMENTO) |>
  st_transform(CRS_METROS)

stopifnot("No quedó ningún establecimiento: revisar NOM_MPIO/NOM_DPTO" = nrow(salud_municipio) > 0)

# Solo los que caen dentro del polígono urbano (descarta los de corregimientos
# rurales, que quedan fuera del límite disuelto de manzanas)
salud <- salud_municipio[st_intersects(salud_municipio, ciudad, sparse = FALSE)[, 1], ]

stopifnot("Ningún establecimiento cae dentro del polígono de la ciudad" = nrow(salud) > 0)
message(nrow(salud), " de ", nrow(salud_municipio),
        " establecimientos de salud dentro del polígono urbano de ", MUNICIPIO)


# ---- 4. MAPA (ggplot2) --------------------------------------------------------
mapa <- ggplot() +
  geom_sf(data = ciudad, fill = "grey95", color = "grey30", linewidth = 0.4) +
  geom_sf(data = salud, aes(color = naju_nombr), size = 1.6, alpha = 0.8) +
  scale_color_manual(values = c("Pública" = "#1b9e77", "Privada" = "#d95f02"),
                      name = "Naturaleza jurídica") +
  labs(
    title    = "Montería: establecimientos de salud",
    subtitle = paste(nrow(salud), "sedes del REPS dentro del área urbana"),
    caption  = "Fuente: REPS (establecimientos) y manzanas DANE (límite de la ciudad)."
  ) +
  theme_void() +
  theme(plot.title = element_text(face = "bold"),
        plot.margin = margin(8, 8, 8, 8))

mapa   # se muestra en el panel Plots de RStudio


# ---- 5. GUARDAR ----------------------------------------------------------------
ggsave(file.path(carpeta_datos, "02_establecimientos_salud_monteria.png"),
       mapa, width = 9, height = 8, dpi = 300, bg = "white")
