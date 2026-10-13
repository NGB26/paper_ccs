# =============================================================================
# MÉRIDA (Yucatán, México): mapa estático de riesgo por calor, centros de salud
# de primer y segundo nivel e isócrona a pie
# -----------------------------------------------------------------------------
# Qué hace
#   1. Lee el KMZ de riesgo por calor (un polígono por manzana). De la ficha de
#      cada manzana toma la densidad y el % de personas de 65+, y con la
#      superficie reconstruye la población (densidad x superficie).
#   2. Lee el Excel del registro CLUES y se queda con las sedes de Mérida de
#      nivel 1 y 2 (en operación, con coordenadas válidas).
#   3. Descarga la red vial de OpenStreetMap con dodgr y calcula isócronas
#      (tiempo por la red) desde cada sede con dodgr.
#   4. Dibuja el mapa estático con ggplot2 y lo guarda en PNG.
#
# Para replicarlo en otra ciudad (por ejemplo Neuquén) cambian la sección 0 y la
# sección 2 (la fuente de las sedes). Las secciones 1, 3 y 4 son genéricas.
#
# Paquetes: install.packages(c("sf", "dplyr", "readxl", "dodgr", "geodist", "ggplot2"))
# Opcional: install.packages("ggspatial")   # barra de escala y flecha norte
# Internet: solo la primera vez, para bajar el extracto de OSM de México (~620 MB, Geofabrik).
# =============================================================================

suppressPackageStartupMessages({
  library(sf)
  library(dplyr)
  library(readxl)
  library(dodgr)
  library(ggplot2)
})
sf_use_s2(FALSE)
options(timeout = 600)

## 0. Parámetros ---------------------------------------------------------------

CIUDAD <- "Mérida"
DIR    <- "C:/Users/NICOLASGA/OneDrive - Inter-American Development Bank Group/General - SCL_SPH_SPH_CAR/Productos de conocimiento/Paper CCS/paper_ccs/data/merida"

ARCHIVO_ES     <- file.path(DIR, "ESTABLECIMIENTO_SALUD_202608.xlsx")
ARCHIVO_RIESGO <- file.path(DIR, "google_earth-merida", "google_earth", "merida_riesgo.kmz")
ARCHIVO_RED    <- file.path(DIR, "cache_osm_vias.rds")
SALIDA_PNG     <- file.path(DIR, "mapa_estatico_riesgo_calor_isocronas_merida.png")
SALIDA_GPKG    <- file.path(DIR, "merida_isocronas_osm.gpkg")
SALIDA_CSV     <- file.path(DIR, "cobertura_poblacion_isocronas_merida.csv")
GUARDAR_CAPAS  <- TRUE     # además del PNG, guardar el GeoPackage y el CSV de cobertura

# Sedes de salud (registro CLUES)
ENTIDAD_RE       <- "^YUCAT"      # expresión regular sobre la columna ENTIDAD
MUNICIPIO_RE     <- "^M.?RIDA$"   # expresión regular sobre la columna MUNICIPIO
NIVELES          <- c(1, 2)       # niveles de atención a mapear (1 = primer nivel, 2 = segundo)
INCLUIR_PRIVADOS <- FALSE         # FALSE: se excluyen los "SERVICIOS MEDICOS PRIVADOS"
TIPOS_ESTAB      <- c("CONSULTA EXTERNA", "HOSPITALIZ")  # se descartan apoyo y asistencia social
EXCLUIR_INST     <- NULL          # regex de instituciones a excluir; ej. "SEGURO SOCIAL|ISSSTE"
CAJA_COORD       <- c(xmin = -91, ymin = 19.5, xmax = -87.5, ymax = 21.8)  # coordenadas fuera de Yucatán = error de carga

# Isócronas
CORTES_MIN    <- 15          # minutos a calcular. Sumar 15 o 30 agranda la descarga y alarga la corrida
MIN_MOSTRAR   <- 15          # isócrona(s) que se dibujan en el mapa (deben estar en CORTES_MIN)
PERFIL        <- "foot"     # perfil de dodgr: "foot" (5 km/h), "bicycle", "motorcar"
VEL_KMH       <- 3.5          # velocidad de caminata en km/h (solo con PERFIL "foot"); 5 = la del perfil de dodgr.
# Ej.: 4 = paso lento; 3.6 (1 m/s) = adultos mayores. NULL = la del perfil.
BUFFER_M      <- 100        # ancho (m) alrededor de las calles alcanzadas
HUECOS_M2     <- 50000      # se rellenan huecos menores a esto (m2)
AMBITO        <- "urbano"   # "urbano": sedes dentro de la zona del KMZ; "municipio": todas las sedes
ARCHIVO_PBF   <- file.path(DIR, "mexico-latest.osm.pbf")
URL_PBF       <- "https://download.geofabrik.de/north-america/mexico-latest.osm.pbf"   # ~620 MB; se baja solo si falta
USAR_CACHE    <- TRUE       # reutilizar la red descargada (cache_osm_vias.rds)
CRS_M         <- 32616      # WGS 84 / UTM zona 16N (metros), la que corresponde a Mérida

# Mapa
RIESGO_ALTO <- c("Alta", "Muy alta")   # categorías que cuentan como "riesgo alto" en el subtítulo

stopifnot(all(MIN_MOSTRAR %in% CORTES_MIN))

## 1. Riesgo por calor (KMZ) ---------------------------------------------------

# La ficha de cada manzana es una tabla HTML: <td><b>Etiqueta</b></td><td>valor</td>
campo_kml <- function(desc, prefijo) {
  desc <- as.character(desc)
  desc[is.na(desc)] <- ""
  patron <- paste0("<td><b>", prefijo, "[^<]*</b></td><td>([^<]*)</td>")
  m <- regmatches(desc, regexec(patron, desc))
  vapply(m, function(z) if (length(z) > 1) z[2] else NA_character_, character(1))
}
# Números en formato español: "46.726,25" -> 46726.25 ; "sin dato" -> NA
num_es <- function(x) suppressWarnings(as.numeric(sub(",", ".", gsub(".", "", x, fixed = TRUE))))

if (!file.exists(ARCHIVO_RIESGO)) stop("No encuentro el KMZ de riesgo: ", ARCHIVO_RIESGO)
if (grepl("\\.kmz$", ARCHIVO_RIESGO, ignore.case = TRUE)) {
  dir_kmz <- file.path(tempdir(), "riesgo_kmz")
  unlink(dir_kmz, recursive = TRUE)
  unzip(ARCHIVO_RIESGO, exdir = dir_kmz, overwrite = TRUE)   # un KMZ es un zip con un doc.kml
  kml <- list.files(dir_kmz, pattern = "\\.kml$", full.names = TRUE, recursive = TRUE)[1]
} else {
  kml <- ARCHIVO_RIESGO
}
capas  <- st_layers(kml)
nfeat  <- replace(capas$features, is.na(capas$features), 0)
capa   <- capas$name[which.max(nfeat)]    # el KML trae también una capa vacía con la leyenda
rz     <- st_read(kml, layer = capa, quiet = TRUE)
cat("KMZ: capa '", capa, "' con ", nrow(rz), " manzanas\n", sep = "")

rz <- rz[!st_is_empty(rz), ]
rz <- st_transform(st_zm(rz, drop = TRUE, what = "ZM"), CRS_M)

# Se reparan polígonos con autointersecciones (el KML las trae) antes de medir áreas
g_rz <- st_make_valid(st_geometry(rz))
if (any(st_geometry_type(g_rz) == "GEOMETRYCOLLECTION")) {
  g_rz <- suppressWarnings(st_collection_extract(g_rz, "POLYGON"))
}
stopifnot(length(g_rz) == nrow(rz))
st_geometry(rz) <- g_rz
rz <- rz[st_geometry_type(rz) %in% c("POLYGON", "MULTIPOLYGON"), ]

# La categoría de riesgo viene al final del nombre: "230011...0101 - Alta"
niv_riesgo <- c("Muy baja", "Baja", "Media", "Alta", "Muy alta", "Sin dato")
# Según la versión de GDAL, las columnas se llaman "Name"/"Description" o "name"/"description"
names(rz)[tolower(names(rz)) == "name"]        <- "Name"
names(rz)[tolower(names(rz)) == "description"] <- "description"
col_nombre <- "Name"
if (!"Name" %in% names(rz)) stop("El KMZ no tiene el campo de nombre; columnas: ", paste(names(rz), collapse = ", "))
if (!"description" %in% names(rz) || all(is.na(rz$description))) {
  # Plan B: leer las fichas directamente del texto del KML y cruzarlas por nombre
  message("GDAL no devolvió la descripción; la leo directo del KML...")
  txt <- paste(readLines(kml, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  pm  <- strsplit(txt, "<Placemark>", fixed = TRUE)[[1]][-1]
  pm  <- substr(pm, 1, regexpr("</description>", pm, fixed = TRUE) + 12)   # solo el encabezado de cada ficha
  nm  <- sub("</name>.*$", "", sub("^.*?<name>", "", pm, perl = TRUE), perl = TRUE)
  ds  <- ifelse(grepl("<description>", pm, fixed = TRUE),
                sub("(\\]\\]>)?</description>$", "", sub("^.*?<description>(<!\\[CDATA\\[)?", "", pm, perl = TRUE), perl = TRUE),
                NA_character_)
  rz$description <- ds[match(rz$Name, nm)]
}
if (all(is.na(rz$description))) stop("No pude leer las fichas (descripción) del KMZ")
rz$cod    <- sub(" - .*$", "", rz[[col_nombre]])
rz$riesgo <- factor(sub("^.* - ", "", rz[[col_nombre]]), levels = niv_riesgo)
if (anyNA(rz$riesgo)) warning(sum(is.na(rz$riesgo)), " manzanas con categoría de riesgo no reconocida")
cat("Manzanas por categoría de riesgo:\n"); print(table(rz$riesgo))

# Población reconstruida: densidad (hab/km2) x superficie (km2); personas de 65+ con el %
rz$dens     <- num_es(campo_kml(rz$description, "Densidad"))
rz$p65      <- num_es(campo_kml(rz$description, "% 65"))
rz$area_km2 <- as.numeric(st_area(rz)) / 1e6
rz$pob      <- rz$dens * rz$area_km2
rz$pob_65   <- rz$pob * rz$p65 / 100
if (all(is.na(rz$dens))) stop("No pude leer 'Densidad' de la ficha del KMZ; no se puede reconstruir la población")
cat("Manzanas sin dato de densidad (población no estimable):", sum(is.na(rz$dens)), "de", nrow(rz),
    "| sin dato de % 65+:", sum(is.na(rz$p65)), "\n")
cat("Población reconstruida del KMZ:", format(round(sum(rz$pob, na.rm = TRUE)), big.mark = ".", decimal.mark = ","), "personas\n")

## 2. Establecimientos de salud (CLUES) ----------------------------------------
# Esta es la única sección que depende de la fuente de datos de cada ciudad.

hojas <- excel_sheets(ARCHIVO_ES)
hoja  <- hojas[grepl("^CLUES", hojas)][1]
if (is.na(hoja)) hoja <- hojas[1]
nm_todas <- names(read_excel(ARCHIVO_ES, sheet = hoja, n_max = 0))
usar <- c("CLUES", "ENTIDAD", "MUNICIPIO", "LOCALIDAD", "NOMBRE DE LA INSTITUCION",
          "NOMBRE DE LA UNIDAD", "NOMBRE TIPO ESTABLECIMIENTO", "NIVEL ATENCION",
          "ESTATUS DE OPERACION", "ESTRATO UNIDAD", "LATITUD", "LONGITUD")
faltan <- setdiff(usar, nm_todas)
if (length(faltan)) stop("Faltan columnas en el Excel de CLUES: ", paste(faltan, collapse = ", "))
cat("Leyendo CLUES (hoja '", hoja, "'); puede tardar un momento...\n", sep = "")
cl <- read_excel(ARCHIVO_ES, sheet = hoja, col_types = ifelse(nm_todas %in% usar, "text", "skip"))

num_punto <- function(x) suppressWarnings(as.numeric(gsub(",", ".", x)))
d <- data.frame(
  clues       = cl[["CLUES"]],
  entidad     = cl[["ENTIDAD"]],
  municipio   = cl[["MUNICIPIO"]],
  localidad   = cl[["LOCALIDAD"]],
  institucion = cl[["NOMBRE DE LA INSTITUCION"]],
  nombre      = cl[["NOMBRE DE LA UNIDAD"]],
  tipo        = cl[["NOMBRE TIPO ESTABLECIMIENTO"]],
  nivel_txt   = cl[["NIVEL ATENCION"]],
  estatus     = cl[["ESTATUS DE OPERACION"]],
  estrato     = cl[["ESTRATO UNIDAD"]],
  lat         = num_punto(cl[["LATITUD"]]),
  lon         = num_punto(cl[["LONGITUD"]]),
  stringsAsFactors = FALSE
)
d$nivel <- ifelse(grepl("^PRIMER", d$nivel_txt), 1L,
                  ifelse(grepl("^SEGUNDO", d$nivel_txt), 2L,
                         ifelse(grepl("^TERCER", d$nivel_txt), 3L, NA_integer_)))

# Embudo de filtros: se imprime cuántas sedes quedan en cada paso
embudo <- data.frame(paso = character(), n = int1eger(), stringsAsFactors = FALSE)
anota  <- function(x, paso) { embudo[nrow(embudo) + 1, ] <<- list(paso, nrow(x)); x }

d <- d |> filter(grepl(ENTIDAD_RE, entidad), grepl(MUNICIPIO_RE, municipio)) |> anota("Registros del municipio")
d <- d |> filter(estatus == "EN OPERACION") |> anota("En operación")
d <- d |> filter(!is.na(lat), !is.na(lon),
                 lon >= CAJA_COORD[["xmin"]], lon <= CAJA_COORD[["xmax"]],
                 lat >= CAJA_COORD[["ymin"]], lat <= CAJA_COORD[["ymax"]]) |> anota("Con coordenadas válidas")
d <- d |> filter(nivel %in% NIVELES) |> anota(paste0("Nivel de atención ", paste(NIVELES, collapse = " o ")))
d <- d |> filter(grepl(paste(TIPOS_ESTAB, collapse = "|"), tipo)) |> anota("Consulta externa u hospitalización")
if (!INCLUIR_PRIVADOS) {
  d <- d |> filter(!grepl("PRIVAD", institucion)) |> anota("Sin servicios médicos privados")
}
if (!is.null(EXCLUIR_INST)) {
  d <- d |> filter(!grepl(EXCLUIR_INST, institucion)) |> anota(paste0("Sin instituciones: ", EXCLUIR_INST))
}
cat("\nEmbudo de filtros de las sedes:\n"); print(embudo, row.names = FALSE)
if (nrow(d) == 0) stop("No quedó ninguna sede: revisá los filtros de la sección 2")

es <- st_as_sf(d, coords = c("lon", "lat"), crs = 4326, remove = FALSE) |>
  mutate(id = row_number(), zona = ifelse(grepl("RURAL", estrato), "Rural", "Urbana"))
cat("\nSedes por nivel y zona:\n"); print(table(nivel = es$nivel, zona = es$zona))
cat("\nSedes por institución:\n"); print(sort(table(es$institucion), decreasing = TRUE))

## 3. Isócronas con OpenStreetMap + dodgr --------------------------------------------

# 3.1 Sedes de origen: las que caen en la zona cubierta por el KMZ (más 0,01 grados)
bb_rz <- st_bbox(st_transform(rz, 4326))
cxy   <- st_coordinates(es)
en_zona <- cxy[, 1] > bb_rz[["xmin"]] - 0.01 & cxy[, 1] < bb_rz[["xmax"]] + 0.01 &
  cxy[, 2] > bb_rz[["ymin"]] - 0.01 & cxy[, 2] < bb_rz[["ymax"]] + 0.01
origen <- if (AMBITO == "urbano") es[en_zona, ] else es
if (nrow(origen) == 0) stop("Ninguna sede cae dentro de la zona del KMZ: revisá AMBITO o las coordenadas")
cat("Sedes con isócronas:", nrow(origen), "de", nrow(es), "\n")

# 3.2 Caja para descargar la red: sedes + alcance máximo + margen
vel_base <- c(foot = 5, bicycle = 15, motorcar = 30)[[PERFIL]]   # km/h que asume el perfil de dodgr
vel_kmh  <- if (is.null(VEL_KMH)) vel_base else VEL_KMH           # km/h que se usa en este análisis
if (vel_kmh != vel_base && PERFIL != "foot") warning("VEL_KMH se aplica como una proporción simple sobre los tiempos del perfil '", PERFIL,
                                                     "', cuyas velocidades varían por tipo de calle; es solo aproximado.")
margen   <- max(CORTES_MIN) / 60 * max(vel_base, vel_kmh) / 111 + 0.01   # en grados
bb <- st_bbox(origen)
bb <- c(xmin = bb[["xmin"]] - margen, ymin = bb[["ymin"]] - margen,
        xmax = bb[["xmax"]] + margen, ymax = bb[["ymax"]] + margen)

# 3.3 Calles de OpenStreetMap, leídas de un extracto local (.osm.pbf).
#     No se usa Overpass: es un servicio público que se satura (errores 504) con ciudades grandes.
# Lee las calles con el driver OSM de GDAL.
# El archivo se baja una sola vez (Geofabrik: país completo); se lee filtrando por la caja de la ciudad.
leer_vias_pbf <- function(bb) {
  if (!file.exists(ARCHIVO_PBF)) {
    cat("Descargando el extracto de OSM (~620 MB, una sola vez)...\n")
    old <- options(timeout = 7200); on.exit(options(old), add = TRUE)
    ok <- try(download.file(URL_PBF, ARCHIVO_PBF, mode = "wb"), silent = TRUE)
    if (inherits(ok, "try-error") || !file.exists(ARCHIVO_PBF) || file.size(ARCHIVO_PBF) < 1e6) {
      if (file.exists(ARCHIVO_PBF)) file.remove(ARCHIVO_PBF)
      stop("No se pudo descargar el extracto. Bajalo a mano de ", URL_PBF, " y guardalo como ", ARCHIVO_PBF)
    }
  }
  cat("Leyendo calles del extracto (puede tardar varios minutos)...\n")
  caja <- st_as_sfc(st_bbox(c(xmin = bb[["xmin"]], ymin = bb[["ymin"]], xmax = bb[["xmax"]], ymax = bb[["ymax"]]), crs = 4326))
  vias <- st_read(ARCHIVO_PBF, wkt_filter = st_as_text(caja),
                  query = "SELECT osm_id, highway FROM lines WHERE highway IS NOT NULL", quiet = TRUE)
  vias[!duplicated(vias$osm_id), ]
}

if (USAR_CACHE && file.exists(ARCHIVO_RED)) {
  vias <- readRDS(ARCHIVO_RED)
  cat("Red vial leída del caché:", nrow(vias), "tramos\n")
} else {
  vias <- leer_vias_pbf(bb)
  saveRDS(vias, ARCHIVO_RED)
  cat("Red vial descargada:", nrow(vias), "tramos\n")
}

# 3.4 Grafo ponderado por perfil (a pie, bicicleta o auto)
gr <- weight_streetnet(vias, wt_profile = PERFIL, type_col = "highway", id_col = "osm_id")
gr <- gr[gr$component == 1, ]                        # componente conexa principal
v  <- dodgr_vertices(gr)

# 3.5 Tiempos desde cada sede a todos los vértices (segundos).
#     dodgr une cada sede al vértice más cercano de la red.
desde <- st_coordinates(origen)
colnames(desde) <- c("x", "y")

# Control: distancia (m) de cada sede al vértice más cercano. Si es grande, la sede
# queda "pegada" a una calle lejana y su isócrona no es confiable.
vxy  <- as.matrix(v[, c("x", "y")])
dmin <- vapply(seq_len(nrow(desde)), function(i)
  min(geodist::geodist(desde[i, , drop = FALSE], vxy, measure = "haversine")), numeric(1))
cat("Distancia de las sedes a la red vial (m): mediana", round(median(dmin)), " máxima", round(max(dmin)), "\n")
if (any(dmin > 250)) {
  warning(sum(dmin > 250), " sede(s) a más de 250 m de la red vial: ",
          paste(origen$nombre[dmin > 250], collapse = "; "))
}

tiempos <- dodgr_times(gr, from = desde, to = v[, c("x", "y")], shortest = FALSE)
tiempos[is.na(tiempos)] <- Inf
if (vel_kmh != vel_base) {
  # dodgr calcula los tiempos con la velocidad de su perfil (vel_base); se reescalan a la velocidad elegida.
  # Las rutas óptimas no cambian (la velocidad es la misma en todas las calles), solo los tiempos.
  tiempos <- tiempos * (vel_base / vel_kmh)
  cat("Velocidad de caminata:", vel_kmh, "km/h (perfil:", vel_base, "km/h). Alcance en", max(CORTES_MIN),
      "min:", round(max(CORTES_MIN) / 60 * vel_kmh * 1000), "m por la red.\n")
}
cat("Matriz de tiempos:", nrow(tiempos), "sedes x", ncol(tiempos), "vértices\n")

# 3.6 Tramos de la red (una sola vez, en metros) para dibujar el área alcanzada
par_id <- paste(pmin(gr$from_id, gr$to_id), pmax(gr$from_id, gr$to_id))
u <- !duplicated(par_id)
tramos <- st_sfc(
  lapply(which(u), function(k) {
    st_linestring(matrix(c(gr$from_lon[k], gr$from_lat[k], gr$to_lon[k], gr$to_lat[k]),
                         ncol = 2, byrow = TRUE))
  }),
  crs = 4326
) |> st_transform(CRS_M)
t_desde <- match(gr$from_id[u], v$id)
t_hasta <- match(gr$to_id[u],   v$id)

quitar_huecos <- function(g, umbral) {
  anillos <- function(p) {
    mantener <- c(TRUE, vapply(p[-1], function(r) st_area(st_polygon(list(r))) >= umbral, logical(1)))
    p[mantener]
  }
  if (inherits(g, "MULTIPOLYGON")) st_multipolygon(lapply(unclass(g), anillos))
  else if (inherits(g, "POLYGON")) st_polygon(anillos(unclass(g)))
  else g
}

isocrona <- function(i, minutos) {
  ok <- tiempos[i, ] <= minutos * 60
  idx <- which(ok[t_desde] & ok[t_hasta])
  if (length(idx) == 0) return(st_sfc(st_polygon(), crs = CRS_M))
  area <- st_union(st_buffer(tramos[idx], BUFFER_M, nQuadSegs = 2))
  st_sfc(quitar_huecos(area[[1]], HUECOS_M2), crs = CRS_M)
}

cat("Calculando isócronas...\n")
iso_est <- do.call(rbind, lapply(seq_len(nrow(origen)), function(i) {
  do.call(rbind, lapply(CORTES_MIN, function(m) {
    st_sf(id = origen$id[i], minutos = m, geometry = isocrona(i, m))
  }))
}))

cobertura <- do.call(rbind, lapply(sort(CORTES_MIN, decreasing = TRUE), function(m) {
  st_sf(minutos = m, geometry = st_union(st_geometry(iso_est[iso_est$minutos == m, ])))
}))

# 3.7 Población cubierta (manzanas cuyo punto interior cae dentro de la isócrona)
pts_rz  <- suppressWarnings(st_point_on_surface(rz))
alto    <- rz$riesgo %in% RIESGO_ALTO
pob_tot <- sum(rz$pob, na.rm = TRUE)
pob_alto_tot <- sum(rz$pob[alto], na.rm = TRUE)
cobertura_pob <- do.call(rbind, lapply(sort(CORTES_MIN), function(m) {
  dentro <- lengths(st_intersects(pts_rz, cobertura[cobertura$minutos == m, ])) > 0
  data.frame(
    min          = m,
    manzanas     = sum(dentro),
    pob          = round(sum(rz$pob[dentro], na.rm = TRUE)),
    pob_65       = round(sum(rz$pob_65[dentro], na.rm = TRUE)),
    pct          = round(sum(rz$pob[dentro], na.rm = TRUE) / pob_tot * 100, 1),
    pob_riesgo_alto     = round(sum(rz$pob[dentro & alto], na.rm = TRUE)),
    pct_pob_riesgo_alto = round(sum(rz$pob[dentro & alto], na.rm = TRUE) / pob_alto_tot * 100, 1)
  )
}))
cat("\nCobertura de población por isócrona:\n"); print(cobertura_pob, row.names = FALSE)

if (GUARDAR_CAPAS) {
  # Si el archivo está abierto (QGIS, Excel) o OneDrive lo está sincronizando, no se puede
  # sobrescribir: en ese caso se guarda con la fecha y hora en el nombre, y el script sigue.
  nombre_libre <- function(ruta) {
    if (!file.exists(ruta)) return(ruta)
    if (suppressWarnings(file.remove(ruta))) return(ruta)
    nueva <- sub("(\\.[A-Za-z]+)$", paste0("_", format(Sys.time(), "%Y%m%d_%H%M%S"), "\\1"), ruta)
    warning("No pude sobrescribir '", basename(ruta), "' (¿abierto en QGIS/Excel o sincronizando OneDrive?). ",
            "Lo guardo como '", basename(nueva), "'.", call. = FALSE)
    nueva
  }
  csv_out  <- nombre_libre(SALIDA_CSV)
  gpkg_out <- nombre_libre(SALIDA_GPKG)
  write.csv(cobertura_pob, csv_out, row.names = FALSE)
  st_write(
    st_transform(es[, c("id", "clues", "nombre", "institucion", "zona", "nivel")], CRS_M),
    gpkg_out, layer = "establecimientos", quiet = TRUE
  )
  st_write(iso_est,   gpkg_out, layer = "isocronas_por_establecimiento", quiet = TRUE)
  st_write(cobertura, gpkg_out, layer = "cobertura_total", quiet = TRUE)
  cat("Capas guardadas en:", gpkg_out, "\nCobertura (CSV):", csv_out, "\n")
}

## 4. Mapa estático con ggplot2 ------------------------------------------------

# 4.1 Colores de riesgo: los de la leyenda del propio KMZ (ColorBrewer OrRd)
col_riesgo <- c("Muy baja" = "#FEF0D9", "Baja" = "#FDCC8A", "Media" = "#FC8D59",
                "Alta" = "#E34A33", "Muy alta" = "#B30000", "Sin dato" = "#BDBDBD")

# 4.2 Isócrona(s): contorno grueso azul oscuro + relleno azul muy suave
lab_modo <- switch(PERFIL, foot = "a pie", bicycle = "en bicicleta", motorcar = "en auto", PERFIL)
iso_r <- cobertura[cobertura$minutos %in% MIN_MOSTRAR, ]
iso_r <- iso_r[order(-iso_r$minutos), ]
etq   <- paste0("≤", iso_r$minutos, " min ", lab_modo)
iso_r$etiqueta <- factor(etq, levels = rev(etq))
col_iso_r <- setNames(colorRampPalette(c("#1e3a8a", "#60a5fa"))(nlevels(iso_r$etiqueta)),
                      levels(iso_r$etiqueta))

# 4.3 Sedes de salud: forma por nivel (círculo = 1, rombo = 2, triángulo = 3), todas en azul
es_r <- st_transform(es, CRS_M)
es_r$nivel_f <- factor(es_r$nivel, levels = c(1, 2, 3), labels = c("Nivel 1", "Nivel 2", "Nivel 3"))

# 4.4 Encuadre: toda la zona cubierta por el KMZ
ext_r <- st_bbox(rz)
dx_r  <- (ext_r[["xmax"]] - ext_r[["xmin"]]) * 0.03
dy_r  <- (ext_r[["ymax"]] - ext_r[["ymin"]]) * 0.03

# 4.5 Subtítulo: manzanas de riesgo alto dentro de la isócrona y población cubierta
m_ref  <- max(MIN_MOSTRAR)
en_iso <- lengths(st_intersects(pts_rz, cobertura[cobertura$minutos == m_ref, ])) > 0
sub_r  <- paste0("Manzanas de riesgo alto o muy alto a ≤", m_ref, " min: ", sum(alto & en_iso), " de ", sum(alto),
                 " (", round(100 * sum(alto & en_iso) / max(sum(alto), 1), 1), "%)")
pct_pob <- cobertura_pob$pct[cobertura_pob$min == m_ref]
if (length(pct_pob) == 1) sub_r <- paste0(sub_r, "\nPoblación de las manzanas a ≤", m_ref, " min: ", pct_pob, "%")

g_r <- ggplot() +
  geom_sf(data = rz, aes(fill = riesgo), color = NA, linewidth = 0) +
  geom_sf(data = iso_r, aes(color = etiqueta), fill = "#1d4ed8", alpha = 0.07, linewidth = 1) +
  geom_sf(data = es_r, aes(shape = nivel_f), fill = "#1d4ed8", color = "white", size = 3.5, stroke = 0.6) +
  scale_fill_manual(values = col_riesgo, breaks = niv_riesgo, drop = FALSE, name = "Riesgo por calor",
                    guide = guide_legend(order = 1)) +
  scale_color_manual(values = col_iso_r, name = "Isócrona",
                     guide = guide_legend(order = 2, override.aes = list(linewidth = 1, fill = NA))) +
  scale_shape_manual(values = c("Nivel 1" = 21, "Nivel 2" = 23, "Nivel 3" = 24), name = "Centro de salud",
                     guide = guide_legend(order = 3)) +
  coord_sf(xlim = c(ext_r[["xmin"]] - dx_r, ext_r[["xmax"]] + dx_r),
           ylim = c(ext_r[["ymin"]] - dy_r, ext_r[["ymax"]] + dy_r), expand = FALSE) +
  labs(
    title    = paste0(CIUDAD, ": riesgo por calor, centros de salud de nivel ", paste(NIVELES, collapse = " y "), " e isócrona"),
    subtitle = sub_r,
    caption  = paste0("Riesgo por calor: ", basename(ARCHIVO_RIESGO), " (por manzana; categorías según la leyenda del archivo). ",
                      "Sedes: ", basename(ARCHIVO_ES), ".\n",
                      "Isócrona: calles de OpenStreetMap y tiempos con dodgr, perfil \"", PERFIL, "\" a ", vel_kmh, " km/h.\n",
                      "Los porcentajes de población usan solo las manzanas del KMZ con dato de densidad.")
  ) +
  theme_void(base_size = 11) +
  theme(
    plot.background   = element_rect(fill = "white", color = NA),
    panel.background  = element_rect(fill = "#fafbf9", color = NA),
    plot.title        = element_text(face = "bold", size = 14, margin = margin(b = 4)),
    plot.subtitle     = element_text(color = "#5d6b64", margin = margin(b = 8)),
    plot.caption      = element_text(color = "#5d6b64", size = 8, hjust = 0, margin = margin(t = 8)),
    plot.margin       = margin(12, 12, 12, 12),
    legend.position   = "right",
    legend.title      = element_text(size = 9, face = "bold"),
    legend.text       = element_text(size = 8.5),
    legend.key.size   = unit(0.45, "cm")
  )

# Barra de escala y norte si está instalado ggspatial (opcional)
if (requireNamespace("ggspatial", quietly = TRUE)) {
  g_r <- g_r +
    ggspatial::annotation_scale(location = "bl", width_hint = 0.25, text_cex = 0.7) +
    ggspatial::annotation_north_arrow(location = "tr", which_north = "true",
                                      style = ggspatial::north_arrow_minimal(), height = unit(0.9, "cm"))
}

print(g_r)
ggsave(SALIDA_PNG, g_r, width = 9, height = 10, dpi = 300, bg = "white")
cat("\nMapa estático:", SALIDA_PNG, "\n")


## 5. Cobertura por nivel de riesgo: tabla cruzada (pegar al final del script) ----
# Para cada corte de minutos (CORTES_MIN) calcula:
#   - el % de la población TOTAL que queda dentro / fuera de la isócrona;
#   - para cada nivel de riesgo, el % de su población que queda dentro / fuera
#     (porcentajes por fila: cada fila suma 100);
#   - el peso de cada celda sobre la población total de la ciudad (pct_*_tot; suma 100 en toda la tabla).
# Usa objetos que el script ya creó: rz, pts_rz, cobertura, CORTES_MIN, niv_riesgo y SALIDA_CSV.
# La población de cada unidad (manzana o radio) es rz$pob; una unidad cuenta como "dentro" si su
# punto interior cae dentro de la isócrona (el mismo criterio de la sección 3.7).

# 5.1 Nivel de riesgo de cada unidad (si alguna no tiene categoría, va en "Sin categoría")
cat_r <- as.character(rz$riesgo)
cat_r[is.na(cat_r)] <- "Sin categoría"
cat_r <- droplevels(factor(cat_r, levels = c(niv_riesgo, "Sin categoría")))

# 5.2 Tabla cruzada para un corte de m minutos
cruce_riesgo <- function(m) {
  dentro <- lengths(st_intersects(pts_rz, cobertura[cobertura$minutos == m, ])) > 0
  d <- data.frame(
    riesgo    = cat_r,
    ubicacion = factor(ifelse(dentro, "Dentro", "Fuera"), levels = c("Dentro", "Fuera")),
    pob       = rz$pob
  )
  d <- d[!is.na(d$pob), ]                       # sin dato de población no se puede ponderar
  tp <- addmargins(xtabs(pob ~ riesgo + ubicacion, data = d))     # personas, con totales
  tu <- addmargins(xtabs(~ riesgo + ubicacion, data = d))         # unidades, con totales
  rownames(tp)[nrow(tp)] <- "Total"
  pct <- function(a, b) { r <- 100 * a / b; r[!(b > 0)] <- NA_real_; r }
  gran_total <- tp["Total", "Sum"]
  data.frame(
    minutos              = m,
    riesgo               = rownames(tp),
    unidades             = as.integer(tu[, "Sum"]),
    pob_total            = round(tp[, "Sum"]),
    pob_dentro           = round(tp[, "Dentro"]),
    pob_fuera            = round(tp[, "Fuera"]),
    pct_dentro           = round(pct(tp[, "Dentro"], tp[, "Sum"]), 1),   # % de la fila dentro
    pct_fuera            = round(pct(tp[, "Fuera"],  tp[, "Sum"]), 1),   # % de la fila fuera
    pct_dentro_tot       = round(pct(tp[, "Dentro"], gran_total), 1),    # % de TODA la población
    pct_fuera_tot        = round(pct(tp[, "Fuera"],  gran_total), 1),
    row.names = NULL
  )
}

cruce_riesgo_isocronas <- do.call(rbind, lapply(sort(CORTES_MIN), cruce_riesgo))

# 5.3 Resultados por consola
cat("\n=== Cobertura por nivel de riesgo (personas y % por fila) ===\n")
for (m in sort(CORTES_MIN)) {
  tt <- cruce_riesgo_isocronas[cruce_riesgo_isocronas$minutos == m, ]
  tot <- tt[tt$riesgo == "Total", ]
  cat(sprintf("\n--- Isócrona de %s min ---\n", m))
  cat(sprintf("Población total dentro de la isócrona: %.1f%% (%s de %s personas) | fuera: %.1f%%\n",
              tot$pct_dentro, format(tot$pob_dentro, big.mark = ".", decimal.mark = ","),
              format(tot$pob_total, big.mark = ".", decimal.mark = ","), tot$pct_fuera))
  print(tt[tt$pob_total > 0, c("riesgo", "pob_total", "pob_dentro", "pob_fuera", "pct_dentro", "pct_fuera",
                               "pct_dentro_tot", "pct_fuera_tot")], row.names = FALSE)
}
cat("\nLectura: 'pct_dentro' y 'pct_fuera' son % de la población de ESE nivel de riesgo (cada fila suma 100);",
    "\n'pct_dentro_tot' y 'pct_fuera_tot' son % de la población total de la ciudad (todas las celdas suman 100).\n")
sin_pob <- unique(cruce_riesgo_isocronas$riesgo[cruce_riesgo_isocronas$pob_total == 0])
if (length(sin_pob)) cat("Niveles sin población (no se muestran):", paste(sin_pob, collapse = ", "), "\n")
n_sin_pob <- sum(is.na(rz$pob))
if (n_sin_pob > 0) cat("Unidades sin dato de población (excluidas de la tabla):", n_sin_pob, "de", nrow(rz), "\n")

# 5.4 Control: el total debe coincidir con la cobertura de la sección 3.7
if (exists("cobertura_pob")) {
  chk <- merge(cobertura_pob[, c("min", "pct")],
               cruce_riesgo_isocronas[cruce_riesgo_isocronas$riesgo == "Total", c("minutos", "pct_dentro")],
               by.x = "min", by.y = "minutos")
  if (any(abs(chk$pct - chk$pct_dentro) > 0.1))
    warning("El % total dentro de la tabla cruzada no coincide con 'cobertura_pob'; revisá los datos.", call. = FALSE)
}

# 5.5 Guardar el CSV junto al de cobertura (si el archivo está abierto, lo guarda con fecha y hora)
if (!exists("GUARDAR_CAPAS") || GUARDAR_CAPAS) {
  if (!exists("nombre_libre")) nombre_libre <- function(ruta) {
    if (!file.exists(ruta)) return(ruta)
    if (suppressWarnings(file.remove(ruta))) return(ruta)
    nueva <- sub("(\\.[A-Za-z]+)$", paste0("_", format(Sys.time(), "%Y%m%d_%H%M%S"), "\\1"), ruta)
    warning("No pude sobrescribir '", basename(ruta), "'. Lo guardo como '", basename(nueva), "'.", call. = FALSE)
    nueva
  }
  ciudad_id  <- sub("^cobertura_poblacion_isocronas_(.*)\\.csv$", "\\1", basename(SALIDA_CSV))
  csv_cruce  <- nombre_libre(file.path(dirname(SALIDA_CSV), paste0("cruce_riesgo_isocronas_", ciudad_id, ".csv")))
  write.csv(cruce_riesgo_isocronas, csv_cruce, row.names = FALSE)
  cat("Tabla cruzada guardada en:", csv_cruce, "\n")
}