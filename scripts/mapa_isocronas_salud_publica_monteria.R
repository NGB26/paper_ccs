# ==============================================================================
# Isócronas a pie (5, 15 y 30 min) a establecimientos de salud PÚBLICOS
# Municipio: Montería (Córdoba, Colombia)
#
# Variante de "mapeo PNA monteria.R": en vez de filtrar por nivel de atención
# (primer nivel), filtra por naturaleza jurídica (naju_nombr == "Pública").
#
# Datos de entrada : Health_Facilities_ES.shp  (sedes del REPS, WGS84)
# Red peatonal     : OpenStreetMap (se descarga sola, hace falta internet)
# Salidas          : mapa_isocronas_publicos_urbano.png    (mapa ggplot, zona urbana)
#                    mapa_isocronas_publicos_municipio.png (mapa ggplot, todo el municipio)
#                    monteria_salud_publica.gpkg           (capas para QGIS / R)
# ==============================================================================

library(sf)
library(dplyr)
library(stringi)
library(osmdata)
library(dodgr)
library(concaveman)
library(ggplot2)
library(ggspatial)


# ---- 1. PARÁMETROS -----------------------------------------------------------
carpeta_datos  <- "C:/Users/NICOLASGA/OneDrive - Inter-American Development Bank Group/General - SCL_SPH_SPH_CAR/Productos de conocimiento/Paper CCS/paper_ccs/data/monteria"
carpeta_salida <- carpeta_datos
dir.create(file.path(carpeta_salida, "cache_osm_publicos"), showWarnings = FALSE, recursive = TRUE)

RUTA_SHP     <- file.path(carpeta_datos, "health_facilities_es", "Health_Facilities_ES.shp")
MUNICIPIO    <- "MONTERIA"
DEPARTAMENTO <- "CORDOBA"

MINUTOS      <- c(5, 15, 30)
VELOCIDAD    <- 5

CRS_METROS   <- 9377
SNAP_MAX_M   <- 200
SUAVIZADO_M  <- 50

DIST_M <- VELOCIDAD * 1000 / 60 * MINUTOS


# ---- 2. LEER Y FILTRAR — SOLO ESTABLECIMIENTOS PÚBLICOS -----------------------
sin_tildes <- function(x) toupper(stri_trans_general(x, "Latin-ASCII"))

est <- st_read(RUTA_SHP, quiet = TRUE) |>
  filter(sin_tildes(NOM_MPIO) == MUNICIPIO,
         sin_tildes(NOM_DPTO) == DEPARTAMENTO,
         naju_nombr == "Pública") |>
  st_transform(CRS_METROS)

stopifnot("No quedó ningún establecimiento público: revisar parámetros" = nrow(est) > 0)
est$id <- seq_len(nrow(est))
message(nrow(est), " establecimientos públicos en ", MUNICIPIO)


# ---- 3. ZONAS DE DESCARGA DE CALLES ------------------------------------------
zonas <- st_cast(st_union(st_buffer(est, max(DIST_M) + 500)), "POLYGON")
zonas <- st_sf(zona = seq_along(zonas), geometry = zonas)
est   <- st_join(est, zonas)
message(nrow(zonas), " zonas de descarga")


# ---- 4. ISÓCRONAS DE UNA ZONA ------------------------------------------------
# Descarga robusta de calles OSM (igual que "mapeo PNA monteria.R"): reintenta
# con espera creciente y prueba mirrors alternativos de Overpass antes de
# abortar la zona. Usa su propia carpeta de caché (cache_osm_publicos) para no
# mezclarse con la caché de establecimientos de primer nivel.
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

  archivo <- file.path(carpeta_salida, "cache_osm_publicos", paste0("zona_", id_zona, ".rds"))
  if (file.exists(archivo)) {
    calles <- readRDS(archivo)
  } else {
    bb <- as.numeric(st_bbox(st_transform(zona, 4326)))
    calles <- descargar_calles_osm(bb)
    calles <- calles$osm_lines
    saveRDS(calles, archivo)
  }
  if (is.null(calles) || nrow(calles) == 0) {
    message("Zona ", id_zona, ": sin calles en OSM, se omite")
    return(NULL)
  }

  grafo <- weight_streetnet(calles, wt_profile = "foot")
  grafo <- grafo[grafo$component == 1, ]
  v     <- dodgr_vertices(grafo)
  v_sf  <- st_transform(st_as_sf(v, coords = c("x", "y"), crs = 4326), CRS_METROS)

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

  origenes <- unique(v$id[cercano])
  dist_mat <- dodgr_dists(grafo, from = origenes, to = v$id)
  fila     <- match(v$id[cercano], origenes)

  salida <- list()
  for (i in seq_len(nrow(puntos))) {
    for (k in seq_along(MINUTOS)) {
      d_i <- dist_mat[fila[i], ]
      alcanzables <- !is.na(d_i) & d_i <= DIST_M[k]
      if (sum(alcanzables) < 3) next
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
  arrange(desc(minutos))

cobertura <- iso |>
  group_by(minutos) |>
  summarise(geometry = st_union(geometry)) |>
  arrange(desc(minutos))


# ---- 6. MAPA ESTÁTICO (ggplot2) -----------------------------------------------
iso_w <- st_transform(cobertura, 4326)
est_w <- st_transform(est, 4326)

colores <- setNames(rev(hcl.colors(length(MINUTOS), "RdYlGn")), MINUTOS)
cob_gg  <- mutate(iso_w, minutos = factor(minutos, levels = MINUTOS))

mapa_estatico <- function(limites = NULL, titulo) {
  g <- ggplot() +
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
         subtitle = "Círculos blancos: establecimientos de salud públicos",
         caption = "Fuente: REPS (establecimientos) y OpenStreetMap (red peatonal)") +
    theme_void() +
    theme(plot.title = element_text(face = "bold"),
          plot.margin = margin(8, 8, 8, 8))
  if (!is.null(limites)) {
    g <- g + coord_sf(xlim = unname(limites[c("xmin", "xmax")]),
                      ylim = unname(limites[c("ymin", "ymax")]), expand = FALSE)
  }
  g
}

id_urbana   <- as.integer(names(which.max(table(est$zona))))
lim_urbano  <- st_bbox(st_transform(zonas[zonas$zona == id_urbana, ], 4326))

g_urbano    <- mapa_estatico(lim_urbano, "Montería: acceso a pie a salud pública (zona urbana)")
g_municipio <- mapa_estatico(NULL,      "Montería: acceso a pie a salud pública (municipio)")

g_urbano   # se muestra en el panel Plots de RStudio


# ---- 7. GUARDAR RESULTADOS ------------------------------------------------------
ggsave(file.path(carpeta_salida, "mapa_isocronas_publicos_urbano.png"),    g_urbano,    width = 9, height = 8, dpi = 300)
ggsave(file.path(carpeta_salida, "mapa_isocronas_publicos_municipio.png"), g_municipio, width = 9, height = 9, dpi = 300)

ruta_gpkg <- file.path(carpeta_salida, "monteria_salud_publica.gpkg")
st_write(est,       ruta_gpkg, layer = "establecimientos_publicos",
         delete_dsn = TRUE, quiet = TRUE)
st_write(iso,       ruta_gpkg, layer = "isocronas_por_establecimiento",
         quiet = TRUE)
st_write(cobertura, ruta_gpkg, layer = "cobertura_total",
         quiet = TRUE)

message("Listo.")
