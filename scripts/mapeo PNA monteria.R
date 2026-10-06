# ==============================================================================
# Isócronas a pie (5, 15 y 30 min) a establecimientos de salud de PRIMER NIVEL
# Municipio: Montería (Córdoba, Colombia)
#
# Datos de entrada : Health_Facilities_ES.shp  (sedes del REPS, WGS84)
# Red peatonal     : OpenStreetMap (se descarga sola, hace falta internet)
# Salidas          : mapa_isocronas_monteria.html  (mapa interactivo)
#                    mapa_estatico_urbano.png      (mapa ggplot, zona urbana)
#                    mapa_estatico_municipio.png   (mapa ggplot, todo el municipio)
#                    monteria_primer_nivel.gpkg    (capas para QGIS / R)
# ==============================================================================


# ---- 0. PAQUETES -------------------------------------------------------------
# Solo la primera vez:
# install.packages(c("sf", "dplyr", "stringi", "osmdata", "dodgr",
#                    "concaveman", "leaflet", "htmlwidgets",
#                    "ggplot2", "ggspatial"))

library(sf)           # datos espaciales
library(dplyr)        # filtros y uniones
library(stringi)      # quitar tildes
library(osmdata)      # descarga de calles de OpenStreetMap
library(dodgr)        # distancias a lo largo de la red de calles
library(concaveman)   # polígono que envuelve los puntos alcanzables
library(leaflet)      # mapa interactivo
library(htmlwidgets)  # guardar el mapa como .html
library(ggplot2)      # mapa estático
library(ggspatial)    # mapa base, escala y flecha norte para ggplot


# ---- 1. PARÁMETROS (lo único que hay que editar) ----------------------------
carpeta_datos  <- "C:/Users/NICOLASGA/OneDrive - Inter-American Development Bank Group/General - SCL_SPH_SPH_CAR/Productos de conocimiento/Paper CCS/paper_ccs/data/monteria"
carpeta_salida <- carpeta_datos    # salidas van al mismo lugar que el resto de Montería
dir.create(file.path(carpeta_salida, "cache_osm"), showWarnings = FALSE, recursive = TRUE)

RUTA_SHP     <- file.path(carpeta_datos, "health_facilities_es", "Health_Facilities_ES.shp")
MUNICIPIO    <- "MONTERIA"                   # sin tildes y en mayúsculas
DEPARTAMENTO <- "CORDOBA"
NIVEL        <- "1"                          # primer nivel (columna `nivel`)

MINUTOS      <- c(5, 15, 30)                 # tiempos de las isócronas
VELOCIDAD    <- 5                            # velocidad al caminar (km/h)

CRS_METROS   <- 9377                         # MAGNA-SIRGAS Origen-Nacional (m)
SNAP_MAX_M   <- 200                          # si la calle más cercana está a más
# de esto, se descarta el punto
SUAVIZADO_M  <- 50                           # engrosa el polígono para cubrir
# las calles del borde

# (Opcional) excluir sedes de apoyo que no son puntos de atención. Ejemplo:
# EXCLUIR_NOMBRE <- "TOMA DE MUESTRA|SERVICIOS DE APOYO|SERVICIOS AMIGABLES"
EXCLUIR_NOMBRE <- NULL

# Distancia máxima a pie para cada tiempo, en metros (5 min a 5 km/h = 417 m)
DIST_M <- VELOCIDAD * 1000 / 60 * MINUTOS


# ---- 2. LEER Y FILTRAR LOS ESTABLECIMIENTOS ----------------------------------
sin_tildes <- function(x) toupper(stri_trans_general(x, "Latin-ASCII"))

est <- st_read(RUTA_SHP, quiet = TRUE) |>
  filter(sin_tildes(NOM_MPIO) == MUNICIPIO,      # igualdad exacta: evita que
         sin_tildes(NOM_DPTO) == DEPARTAMENTO,   # entre Monterrey (Casanare)
         trimws(nivel) == NIVEL) |>
  st_transform(CRS_METROS)

if (!is.null(EXCLUIR_NOMBRE)) {
  est <- filter(est, !grepl(EXCLUIR_NOMBRE, Nombre, ignore.case = TRUE))
}

stopifnot("No quedó ningún establecimiento: revisar parámetros" = nrow(est) > 0)
est$id <- seq_len(nrow(est))
message(nrow(est), " establecimientos de primer nivel en ", MUNICIPIO)


# ---- 3. ZONAS DE DESCARGA DE CALLES ------------------------------------------
# Montería es muy grande: en vez de bajar toda la red, se agrupan los puntos que
# están cerca y se baja solo la red alrededor de cada grupo (radio = 30 min + 500 m).
zonas <- st_cast(st_union(st_buffer(est, max(DIST_M) + 500)), "POLYGON")
zonas <- st_sf(zona = seq_along(zonas), geometry = zonas)
est   <- st_join(est, zonas)                     # agrega la columna `zona`
message(nrow(zonas), " zonas de descarga")


# ---- 4. ISÓCRONAS DE UNA ZONA ------------------------------------------------
# guarda las descargas junto con el resto de las salidas de Montería
dir.create(file.path(carpeta_salida, "cache_osm"), showWarnings = FALSE, recursive = TRUE)

# Descarga robusta de calles OSM: el endpoint publico de Overpass (overpass-api.de)
# devuelve 504 Gateway Timeout cuando esta sobrecargado. Se reintenta con espera
# creciente y, si sigue fallando, se prueba con el mirror alternativo antes de
# abortar la zona.
descargar_calles_osm <- function(bb, intentos = 3, espera_seg = 15) {
  mirrors <- list_overpass_urls()
  for (mirror in mirrors) {
    mirror_ok <- tryCatch({ set_overpass_url(mirror); TRUE }, error = function(e) FALSE)
    if (!mirror_ok) next
    for (intento in seq_len(intentos)) {
      resultado <- tryCatch(
        opq(bbox = bb, timeout = 300) |> add_osm_feature(key = "highway") |> osmdata_sf(),
        error = function(e) e
      )
      if (!inherits(resultado, "error")) return(resultado)
      message("  Overpass (", mirror, ") intento ", intento, "/", intentos,
              " fallo: ", conditionMessage(resultado))
      if (intento < intentos) Sys.sleep(espera_seg * intento)
    }
  }
  stop("No se pudo descargar la red de calles tras agotar reintentos y mirrors de Overpass.")
}

isocronas_zona <- function(id_zona) {
  puntos <- est[est$zona == id_zona, ]
  zona   <- zonas[zonas$zona == id_zona, ]

  # 4a. Calles de OpenStreetMap (si ya se bajaron, se leen del disco)
  archivo <- file.path(carpeta_salida, "cache_osm", paste0("zona_", id_zona, ".rds"))
  if (file.exists(archivo)) {
    calles <- readRDS(archivo)
  } else {
    bb <- as.numeric(st_bbox(st_transform(zona, 4326)))   # xmin, ymin, xmax, ymax
    calles <- descargar_calles_osm(bb)
    calles <- calles$osm_lines
    saveRDS(calles, archivo)
  }
  if (is.null(calles) || nrow(calles) == 0) {
    message("Zona ", id_zona, ": sin calles en OSM, se omite")
    return(NULL)
  }
  
  # 4b. Grafo peatonal; se deja solo la red conectada principal
  grafo <- weight_streetnet(calles, wt_profile = "foot")
  grafo <- grafo[grafo$component == 1, ]
  v     <- dodgr_vertices(grafo)                          # nodos de la red
  v_sf  <- st_transform(st_as_sf(v, coords = c("x", "y"), crs = 4326), CRS_METROS)
  
  # 4c. Ubicar cada establecimiento en el nodo de calle más cercano
  cercano   <- st_nearest_feature(puntos, v_sf)
  dist_snap <- as.numeric(st_distance(puntos, v_sf[cercano, ], by_element = TRUE))
  lejos     <- dist_snap > SNAP_MAX_M
  if (any(lejos)) {
    message("Zona ", id_zona, ": se omiten ", sum(lejos),
            " establecimiento(s) a más de ", SNAP_MAX_M, " m de una calle de OSM")
    puntos  <- puntos[!lejos, ]
    cercano <- cercano[!lejos]
  }
  if (nrow(puntos) == 0) return(NULL)
  
  # 4d. Distancia por calle (m) desde cada origen hacia todos los nodos
  origenes <- unique(v$id[cercano])
  dist_mat <- dodgr_dists(grafo, from = origenes, to = v$id)
  fila     <- match(v$id[cercano], origenes)
  
  # 4e. Un polígono por establecimiento y tiempo: envuelve los nodos alcanzables
  salida <- list()
  for (i in seq_len(nrow(puntos))) {
    for (k in seq_along(MINUTOS)) {
      d_i <- dist_mat[fila[i], ]
      alcanzables <- !is.na(d_i) & d_i <= DIST_M[k]
      if (sum(alcanzables) < 3) next                    # muy pocos nodos
      poly <- concaveman(v_sf[alcanzables, ], concavity = 2)
      poly <- st_buffer(st_set_crs(st_geometry(poly), CRS_METROS), SUAVIZADO_M)
      salida[[length(salida) + 1]] <-
        st_sf(id = puntos$id[i], minutos = MINUTOS[k], geometry = poly)
    }
  }
  bind_rows(salida)
}


# ---- 5. CALCULAR PARA TODAS LAS ZONAS ----------------------------------------
iso <- bind_rows(lapply(zonas$zona, isocronas_zona)) |>
  arrange(desc(minutos))                                # 30 abajo, 5 arriba

# Cobertura total: unión de las isócronas de todos los establecimientos
cobertura <- iso |>
  group_by(minutos) |>
  summarise(geometry = st_union(geometry)) |>
  arrange(desc(minutos))


# ---- 6. MAPA INTERACTIVO -----------------------------------------------------
iso_w <- st_transform(cobertura, 4326)
est_w <- st_transform(est, 4326)

# verde = 5 min ... rojo = 30 min
colores <- setNames(rev(hcl.colors(length(MINUTOS), "RdYlGn")), MINUTOS)

mapa <- leaflet() |>
  addProviderTiles(providers$CartoDB.Positron)

for (m in sort(MINUTOS, decreasing = TRUE)) {           # primero el más grande
  mapa <- addPolygons(
    mapa, data = filter(iso_w, minutos == m),
    fillColor = colores[[as.character(m)]], fillOpacity = 0.45,
    color = colores[[as.character(m)]], weight = 1,
    label = paste(m, "min a pie"), group = paste(m, "min"))
}

mapa <- mapa |>
  addCircleMarkers(
    data = est_w, radius = 5, color = "black", weight = 2,
    fillColor = "white", fillOpacity = 1, group = "Establecimientos",
    popup = ~paste0("<b>", Nombre, "</b><br>", Direccion, "<br>", Barrio)) |>
  addLegend(colors = unname(colores), labels = paste(MINUTOS, "min"),
            title = "Caminando", position = "bottomright") |>
  addLayersControl(
    overlayGroups = c("Establecimientos", paste(sort(MINUTOS, decreasing = TRUE), "min")),
    options = layersControlOptions(collapsed = FALSE))

mapa   # se muestra en el panel Viewer de RStudio


# ---- 7. MAPA ESTÁTICO (ggplot2) ----------------------------------------------
# Se dibuja la cobertura total (unión de isócronas) sobre un mapa base claro.
cob_gg <- mutate(iso_w, minutos = factor(minutos, levels = MINUTOS))

mapa_estatico <- function(limites = NULL, titulo) {
  g <- ggplot() +
    # "cartolight" (CARTO) ahora exige API key; "osm" no requiere autenticacion.
    annotation_map_tile(type = "osm", progress = "none") +
    geom_sf(data = cob_gg, aes(fill = minutos), color = NA, alpha = 0.55) +
    geom_sf(data = est_w, shape = 21, fill = "white", color = "black",
            size = 2, stroke = 0.8) +
    scale_fill_manual(values = colores, name = "Caminando",
                      labels = function(x) paste(x, "min")) +
    annotation_scale(location = "bl") +
    annotation_north_arrow(location = "tr", which_north = "true",
                           style = north_arrow_minimal()) +
    labs(title = titulo,
         subtitle = "Círculos blancos: establecimientos de primer nivel",
         caption = "Fuente: REPS (establecimientos) y OpenStreetMap (red peatonal)") +
    theme_void() +
    theme(plot.title = element_text(face = "bold"),
          plot.margin = margin(8, 8, 8, 8))
  if (!is.null(limites)) {                     # recorte opcional del mapa
    g <- g + coord_sf(xlim = unname(limites[c("xmin", "xmax")]),
                      ylim = unname(limites[c("ymin", "ymax")]), expand = FALSE)
  }
  g
}

# Zona urbana = la zona de descarga con más establecimientos
id_urbana   <- as.integer(names(which.max(table(est$zona))))
lim_urbano  <- st_bbox(st_transform(zonas[zonas$zona == id_urbana, ], 4326))

g_urbano   <- mapa_estatico(lim_urbano, "Montería: acceso a pie a primer nivel (zona urbana)")
g_municipio <- mapa_estatico(NULL,      "Montería: acceso a pie a primer nivel (municipio)")

g_urbano   # se muestra en el panel Plots de RStudio


# ---- 8. GUARDAR RESULTADOS ---------------------------------------------------
ggsave(file.path(carpeta_salida, "mapa_estatico_urbano.png"),    g_urbano,    width = 9, height = 8, dpi = 300)
ggsave(file.path(carpeta_salida, "mapa_estatico_municipio.png"), g_municipio, width = 9, height = 9, dpi = 300)

saveWidget(mapa, file.path(carpeta_salida, "mapa_isocronas_monteria.html"), selfcontained = TRUE)

ruta_gpkg <- file.path(carpeta_salida, "monteria_primer_nivel.gpkg")
st_write(est,       ruta_gpkg, layer = "establecimientos",
         delete_dsn = TRUE, quiet = TRUE)
st_write(iso,       ruta_gpkg, layer = "isocronas_por_establecimiento",
         quiet = TRUE)
st_write(cobertura, ruta_gpkg, layer = "cobertura_total",
         quiet = TRUE)