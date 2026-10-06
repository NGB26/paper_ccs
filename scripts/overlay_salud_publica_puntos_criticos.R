# ==============================================================================
# Montería: superponer establecimientos de salud públicos sobre el mapa de
# "puntos críticos" (mapa_puntos_criticos.png)
#
# El PNG de puntos críticos no tiene datos vectoriales ni script de origen en
# este repo (no está en RIESGO_monteria.R ni en el .Rhistory), así que no se
# puede regenerar el mapa desde cero. En su lugar, se calibra la imagen
# usando la posición en píxeles de las etiquetas de los ejes (lat/lon) —
# detectada analizando los píxeles oscuros de las franjas de texto de los
# ejes — y se dibujan los puntos encima como overlay rasterizado.
#
# Calibración (ver detección de bandas de texto de los ejes):
#   Eje Y (latitud):  8.80°N -> fila 241.5   |  8.78°N -> fila 393.5
#   Eje X (longitud): 75.90°W -> col 500.0   |  75.84°W -> col 954.0
#   (ambas relaciones son lineales; confirmadas contra las 5 etiquetas de
#   latitud y las 4 de longitud visibles en la imagen, con error < 2 px)
#
# Salida: 04_puntos_criticos_salud_publica_monteria.png
# ==============================================================================

library(sf)
library(dplyr)
library(png)

carpeta_datos <- "C:/Users/NICOLASGA/OneDrive - Inter-American Development Bank Group/General - SCL_SPH_SPH_CAR/Productos de conocimiento/Paper CCS/paper_ccs/data/monteria"

RUTA_IMG      <- file.path(carpeta_datos, "mapa_puntos_criticos.png")
RUTA_MANZANAS <- file.path(carpeta_datos, "monteria_base_manzanas.shp")
RUTA_SALUD    <- file.path(carpeta_datos, "health_facilities_es", "Health_Facilities_ES.shp")

MUNICIPIO    <- "MONTERÍA"
DEPARTAMENTO <- "Córdoba"
CRS_METROS   <- 9377


# ---- 1. CALIBRACIÓN DE PÍXELES A LAT/LON --------------------------------------
img <- readPNG(RUTA_IMG)
h <- dim(img)[1]; w <- dim(img)[2]

row_de_lat <- function(lat) 67121.5 - 7600 * lat
col_de_lon <- function(lon) 574811.3 + 7566.67 * lon


# ---- 2. ESTABLECIMIENTOS PÚBLICOS DENTRO DEL POLÍGONO URBANO ------------------
# Mismo criterio que "mapa_establecimientos_salud_publica_monteria.R"
manzanas <- st_read(RUTA_MANZANAS, quiet = TRUE) |> st_transform(CRS_METROS)
ciudad   <- st_union(manzanas) |> st_sf(geometry = _)

salud_municipio <- st_read(RUTA_SALUD, quiet = TRUE) |>
  filter(NOM_MPIO == MUNICIPIO, NOM_DPTO == DEPARTAMENTO, naju_nombr == "Pública") |>
  st_transform(CRS_METROS)

salud <- salud_municipio[st_intersects(salud_municipio, ciudad, sparse = FALSE)[, 1], ]
stopifnot("Ningún establecimiento público cae dentro del polígono de la ciudad" = nrow(salud) > 0)
message(nrow(salud), " establecimientos públicos a superponer")

coords  <- st_coordinates(st_transform(salud, 4326))   # lon, lat (WGS84)
px_col  <- col_de_lon(coords[, 1])
px_row  <- row_de_lat(coords[, 2])


# ---- 3. DIBUJAR EL OVERLAY -----------------------------------------------------
ruta_salida <- file.path(carpeta_datos, "04_puntos_criticos_salud_publica_monteria.png")
png(ruta_salida, width = w, height = h)
op <- par(mar = c(0, 0, 0, 0), xaxs = "i", yaxs = "i")
plot(NA, xlim = c(0, w), ylim = c(0, h), axes = FALSE, xlab = "", ylab = "")
rasterImage(img, 0, 0, w, h)
points(px_col, h - px_row, pch = 21, bg = "#1f78ff", col = "white", cex = 2.1, lwd = 1.5)
legend("topright", legend = "Establecimiento de salud público", pch = 21,
       pt.bg = "#1f78ff", col = "white", pt.cex = 1.6, pt.lwd = 1.2,
       bty = "n", cex = 0.9, inset = 0.01)
par(op)
dev.off()

message("Guardado: ", ruta_salida)
