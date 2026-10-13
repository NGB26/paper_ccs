# =============================================================================
# NEUQUÉN (Argentina): mapa estático de riesgo por calor, establecimientos de
# salud públicos con atención a demanda espontánea e isócrona a pie
# -----------------------------------------------------------------------------
# Qué hace
#   1. Lee el KMZ de riesgo por calor (un polígono por radio censal). De la ficha
#      de cada radio toma la densidad y el % de personas de 65+, y con la
#      superficie reconstruye la población (densidad x superficie).
#   2. Lee el Excel del REFES, NORMALIZA las coordenadas y se queda con los
#      establecimientos públicos de la provincia de Neuquén que atienden demanda
#      espontánea: ESSIDT (centros de salud, puestos sanitarios y demás atención
#      ambulatoria, incluidas las unidades metabólicas/de diabetes), hospitales
#      con internación de cualquier nivel de riesgo y unidades de atención móviles.
#   3. Lee la red vial de OpenStreetMap (extracto de Argentina) y calcula
#      isócronas (tiempo por la red) desde cada sede con dodgr.
#   4. Dibuja el mapa estático con ggplot2 y lo guarda en PNG.
#
# Paquetes: install.packages(c("sf", "dplyr", "readxl", "dodgr", "geodist", "ggplot2"))
# Opcional: install.packages("ggspatial")   # barra de escala y flecha norte
# Internet: solo la primera vez, para bajar el extracto de OSM de Argentina (~410 MB, Geofabrik).
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

CIUDAD <- "Neuquén"
DIR    <- "C:/Users/NICOLASGA/OneDrive - Inter-American Development Bank Group/General - SCL_SPH_SPH_CAR/Productos de conocimiento/Paper CCS/paper_ccs/data/neuquen"

# Los archivos de entrada se buscan por patrón en la carpeta (acepta "refes 2025.xlsx", "refes_2025.xlsx", etc.)
buscar_archivo <- function(dir, patron, recursivo = FALSE) {
  if (!dir.exists(dir)) stop("No existe la carpeta: ", dir)
  f <- list.files(dir, pattern = patron, full.names = TRUE, recursive = recursivo, ignore.case = TRUE)
  f <- f[!grepl("^~\\$", basename(f))]                       # archivos temporales de Excel
  if (length(f) == 0) stop("No encuentro ningún archivo que coincida con '", patron, "' en ", dir,
                           "\nContenido de la carpeta:\n  ", paste(list.files(dir), collapse = "\n  "))
  if (length(f) > 1) message("Hay más de un archivo para '", patron, "'; uso: ", basename(f[1]))
  f[1]
}
ARCHIVO_ES     <- buscar_archivo(DIR, "refes.*\\.xlsx$")
ARCHIVO_RIESGO <- buscar_archivo(DIR, "riesgo.*\\.kmz$", recursivo = TRUE)   # busca también en las subcarpetas
cat("REFES:  ", ARCHIVO_ES, "\nRiesgo: ", ARCHIVO_RIESGO, "\n", sep = "")
ARCHIVO_RED    <- file.path(DIR, "cache_osm_vias.rds")
SALIDA_PNG     <- file.path(DIR, "mapa_estatico_riesgo_calor_isocronas_neuquen.png")
SALIDA_GPKG    <- file.path(DIR, "neuquen_isocronas_osm.gpkg")
SALIDA_CSV     <- file.path(DIR, "cobertura_poblacion_isocronas_neuquen.csv")
GUARDAR_CAPAS  <- TRUE     # además del PNG, guardar el GeoPackage y el CSV de cobertura

# Sedes de salud (REFES)
PROVINCIA_RE     <- "^neuqu"       # expresión regular (sin distinguir mayúsculas) sobre PROVINCIA
DEPARTAMENTO_RE  <- NULL           # NULL = toda la provincia (el mapa recorta a la zona del KMZ); ej. "^Confluencia$"
# CODIGO_TIPOLOGIA que se incluyen completas:
#   ESSIDT  sin internación de diagnóstico y tratamiento (CAPS, puestos sanitarios, consultorios;
#           el REFES registra aquí también los centros de diabetes/metabólicos)
#   ESCIG   con internación general (hospitales de bajo, mediano y alto riesgo)
#   ESCIEM  con internación especializada en maternidad/materno infantil
#   ESCIE   con internación especializada en otras especialidades
#   (no se incluyen ESCIESM salud mental, ESCIETE tercera edad ni ESCIRES residencias)
TIPOLOGIAS       <- c("ESSIDT", "ESCIG", "ESCIEM", "ESCIE")
# CATEGORIA_TIPOLOGIA (regex) que se agregan aunque su tipología no esté arriba
# (las unidades móviles son "Establecimiento de salud complementario", ESCL)
CATEGORIAS_EXTRA <- "Unidades de Atenci.n M.viles"   # NULL = no agregar ninguna
SOLO_PUBLICOS    <- TRUE           # ORIGEN_FINANCIAMIENTO == "Público"
DEPENDENCIA_EXCLUIR <- "Penitenciario|FFAA"   # regex de DEPENDENCIA sin acceso para la población general; NULL = ninguna
# Coordenadas cargadas a mano para sedes que el REFES trae sin LATITUD/LONGITUD (por ejemplo,
# las unidades móviles). CSV opcional con columnas CODIGO,LATITUD,LONGITUD; si no existe, se ignora.
ARCHIVO_COORD_MANUALES <- file.path(DIR, "refes_coordenadas_manuales.csv")
ESTADO_GEO_EXCLUIR <- NULL         # regex de ESTADO_GEO a excluir; ej. "SINCONFIRMAR" (coordenadas sin confirmar)
CAJA_COORD       <- c(xmin = -72.5, ymin = -41.3, xmax = -67.5, ymax = -36.0)  # coordenadas fuera de la provincia = error de carga

# Isócronas
CORTES_MIN    <- 15          # minutos a calcular. Sumar 15 o 30 agranda la zona de lectura y alarga la corrida
MIN_MOSTRAR   <- 15          # isócrona(s) que se dibujan en el mapa (deben estar en CORTES_MIN)
PERFIL        <- "foot"     # perfil de dodgr: "foot" (5 km/h), "bicycle", "motorcar"
VEL_KMH       <- 3.5          # velocidad de caminata en km/h (solo con PERFIL "foot"); 5 = la del perfil de dodgr.
# Ej.: 4 = paso lento; 3.6 (1 m/s) = adultos mayores. NULL = la del perfil.
BUFFER_M      <- 100        # ancho (m) alrededor de las calles alcanzadas
HUECOS_M2     <- 50000      # se rellenan huecos menores a esto (m2)
AMBITO        <- "urbano"   # "urbano": sedes dentro de la zona del KMZ; "municipio": todas las sedes cargadas
ARCHIVO_PBF   <- file.path(DIR, "argentina-latest.osm.pbf")
URL_PBF       <- "https://download.geofabrik.de/south-america/argentina-latest.osm.pbf"   # ~410 MB; se baja solo si falta
USAR_CACHE    <- TRUE       # reutilizar la red leída (cache_osm_vias.rds)
CRS_M         <- 32719      # WGS 84 / UTM zona 19S (metros), la que corresponde a Neuquén

# Mapa
RIESGO_ALTO <- c("Alta", "Muy alta")   # categorías que cuentan como "riesgo alto" en el subtítulo

stopifnot(all(MIN_MOSTRAR %in% CORTES_MIN))


## 1. Riesgo por calor (KMZ) ---------------------------------------------------

# La ficha de cada radio es una tabla HTML: <td><b>Etiqueta</b></td><td>valor</td>
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
cat("KMZ: capa '", capa, "' con ", nrow(rz), " radios\n", sep = "")

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

# La categoría de riesgo viene al final del nombre: "580351608 - Alta"
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
if (anyNA(rz$riesgo)) warning(sum(is.na(rz$riesgo)), " radios con categoría de riesgo no reconocida")
cat("Radios por categoría de riesgo:\n"); print(table(rz$riesgo))

# Población reconstruida: densidad (hab/km2) x superficie (km2); personas de 65+ con el %
rz$dens     <- num_es(campo_kml(rz$description, "Densidad"))
rz$p65      <- num_es(campo_kml(rz$description, "% 65"))
rz$area_km2 <- as.numeric(st_area(rz)) / 1e6
rz$pob      <- rz$dens * rz$area_km2
rz$pob_65   <- rz$pob * rz$p65 / 100
if (all(is.na(rz$dens))) stop("No pude leer 'Densidad' de la ficha del KMZ; no se puede reconstruir la población")
cat("Radios sin dato de densidad (población no estimable):", sum(is.na(rz$dens)), "de", nrow(rz),
    "| sin dato de % 65+:", sum(is.na(rz$p65)), "\n")
cat("Población reconstruida del KMZ:", format(round(sum(rz$pob, na.rm = TRUE)), big.mark = ".", decimal.mark = ","), "personas\n")


## 2. Establecimientos de salud (REFES) ----------------------------------------
# Esta es la única sección que depende de la fuente de datos de cada ciudad.

# 2.1 Normalización de coordenadas. En el REFES hay latitudes y longitudes con:
#     coma decimal, sin punto decimal ("-38968237"), signo positivo, valores en cero
#     o invertidas (latitud <-> longitud). Para Argentina: lat en [-56, -21], lon en [-74, -53].
normaliza_coord <- function(x) {
  x <- trimws(as.character(x))
  x <- gsub(",", ".", x, fixed = TRUE)          # coma decimal
  x <- gsub("[^0-9.+-]", "", x)                 # quita símbolos (º, espacios, letras)
  v <- suppressWarnings(as.numeric(x))
  v[!is.finite(v) | v == 0] <- NA
  # sin punto decimal: se lo reubica tras los 2 primeros dígitos enteros (-38968237 -> -38.968237)
  grande <- !is.na(v) & abs(v) >= 100
  v[grande] <- v[grande] / 10^(nchar(format(floor(abs(v[grande])), scientific = FALSE, trim = TRUE)) - 2)
  v
}
normaliza_par <- function(lat, lon) {
  la <- normaliza_coord(lat); lo <- normaliza_coord(lon)
  la <- ifelse(!is.na(la) & la > 0, -la, la)    # en Argentina ambas son negativas
  lo <- ifelse(!is.na(lo) & lo > 0, -lo, lo)
  inv <- !is.na(la) & !is.na(lo) & la < -56.5 & lo > -56.5 & lo < -20   # invertidas
  t <- la[inv]; la[inv] <- lo[inv]; lo[inv] <- t
  list(lat = la, lon = lo)
}

# 2.2 Lectura del Excel
hoja     <- excel_sheets(ARCHIVO_ES)[1]
nm_todas <- names(read_excel(ARCHIVO_ES, sheet = hoja, n_max = 0))
usar <- c("CODIGO", "NOMBRE", "CODIGO_TIPOLOGIA", "CATEGORIA_TIPOLOGIA", "DEPENDENCIA", "ORIGEN_FINANCIAMIENTO",
          "PROVINCIA", "DEPARTAMENTO", "LOCALIDAD", "LATITUD", "LONGITUD", "ESTADO_GEO")
faltan <- setdiff(usar, nm_todas)
if (length(faltan)) stop("Faltan columnas en el Excel del REFES: ", paste(faltan, collapse = ", "))
cat("Leyendo REFES (hoja '", hoja, "')...\n", sep = "")
rf <- read_excel(ARCHIVO_ES, sheet = hoja, col_types = ifelse(nm_todas %in% usar, "text", "skip"))

crd <- normaliza_par(rf[["LATITUD"]], rf[["LONGITUD"]])
d <- data.frame(
  codigo       = rf[["CODIGO"]],
  nombre       = rf[["NOMBRE"]],
  tipologia    = rf[["CODIGO_TIPOLOGIA"]],
  categoria    = rf[["CATEGORIA_TIPOLOGIA"]],
  dependencia  = rf[["DEPENDENCIA"]],
  financiam    = rf[["ORIGEN_FINANCIAMIENTO"]],
  provincia    = rf[["PROVINCIA"]],
  departamento = rf[["DEPARTAMENTO"]],
  localidad    = rf[["LOCALIDAD"]],
  estado_geo   = rf[["ESTADO_GEO"]],
  lat_orig     = rf[["LATITUD"]],
  lon_orig     = rf[["LONGITUD"]],
  lat          = crd$lat,
  lon          = crd$lon,
  stringsAsFactors = FALSE
)

# Completa las coordenadas faltantes con el CSV manual (si existe)
if (file.exists(ARCHIVO_COORD_MANUALES)) {
  cm  <- read.csv(ARCHIVO_COORD_MANUALES, colClasses = "character", check.names = FALSE)
  names(cm) <- toupper(trimws(names(cm)))
  if (!all(c("CODIGO", "LATITUD", "LONGITUD") %in% names(cm))) {
    stop("El CSV de coordenadas manuales debe tener las columnas CODIGO, LATITUD y LONGITUD")
  }
  crd_m <- normaliza_par(cm$LATITUD, cm$LONGITUD)
  k     <- match(d$codigo, trimws(cm$CODIGO))
  llenar <- !is.na(k) & (is.na(d$lat) | is.na(d$lon))
  d$lat[llenar] <- crd_m$lat[k[llenar]]
  d$lon[llenar] <- crd_m$lon[k[llenar]]
  cat("Coordenadas completadas con", basename(ARCHIVO_COORD_MANUALES), ":", sum(llenar), "sedes\n")
}

# 2.3 Embudo de filtros: se imprime cuántas sedes quedan en cada paso
embudo <- data.frame(paso = character(), n = integer(), stringsAsFactors = FALSE)
anota  <- function(x, paso) { embudo[nrow(embudo) + 1, ] <<- list(paso, nrow(x)); x }

d <- d |> filter(grepl(PROVINCIA_RE, provincia, ignore.case = TRUE)) |> anota("Registros de la provincia")
extra <- if (is.null(CATEGORIAS_EXTRA)) rep(FALSE, nrow(d)) else grepl(CATEGORIAS_EXTRA, d$categoria, ignore.case = TRUE)
d <- d |> filter(tipologia %in% TIPOLOGIAS | extra) |>
  anota(paste0("Tipología ", paste(TIPOLOGIAS, collapse = ", "), if (!is.null(CATEGORIAS_EXTRA)) " + unidades móviles"))
if (SOLO_PUBLICOS) {
  d <- d |> filter(grepl("^P.*blico", financiam, ignore.case = TRUE)) |> anota("Públicos")
}
if (!is.null(DEPENDENCIA_EXCLUIR)) {
  d <- d |> filter(is.na(dependencia) | !grepl(DEPENDENCIA_EXCLUIR, dependencia)) |> anota(paste0("Sin dependencia: ", DEPENDENCIA_EXCLUIR))
}
if (!is.null(DEPARTAMENTO_RE)) {
  d <- d |> filter(grepl(DEPARTAMENTO_RE, departamento, ignore.case = TRUE)) |> anota("En el departamento elegido")
}
if (!is.null(ESTADO_GEO_EXCLUIR)) {
  d <- d |> filter(is.na(estado_geo) | !grepl(ESTADO_GEO_EXCLUIR, estado_geo)) |> anota(paste0("Sin ESTADO_GEO: ", ESTADO_GEO_EXCLUIR))
}
sin_coord <- d |> filter(is.na(lat) | is.na(lon))
d <- d |> filter(!is.na(lat), !is.na(lon),
                 lon >= CAJA_COORD[["xmin"]], lon <= CAJA_COORD[["xmax"]],
                 lat >= CAJA_COORD[["ymin"]], lat <= CAJA_COORD[["ymax"]]) |> anota("Con coordenadas válidas (normalizadas)")
cat("\nEmbudo de filtros de las sedes:\n"); print(embudo, row.names = FALSE)
if (nrow(sin_coord) > 0) {
  cat("Sedes que no se pueden mapear por falta de coordenadas:", nrow(sin_coord),
      "(se pueden cargar en", basename(ARCHIVO_COORD_MANUALES), ")\n")
  print(head(sin_coord[, c("codigo", "nombre", "categoria", "localidad")], 20), row.names = FALSE)
}
if (nrow(d) == 0) stop("No quedó ninguna sede: revisá los filtros de la sección 2")

# Cuántas coordenadas cambió la normalización (para auditar)
orig_ok <- !is.na(suppressWarnings(as.numeric(d$lat_orig))) & !is.na(suppressWarnings(as.numeric(d$lon_orig))) &
  abs(suppressWarnings(as.numeric(d$lat_orig)) - d$lat) < 1e-6 & abs(suppressWarnings(as.numeric(d$lon_orig)) - d$lon) < 1e-6
cat("Sedes con coordenadas modificadas por la normalización:", sum(!orig_ok), "de", nrow(d), "\n")
if (any(!orig_ok)) print(head(d[!orig_ok, c("nombre", "lat_orig", "lon_orig", "lat", "lon")], 10), row.names = FALSE)

# Tipo de sede: hospital y unidad móvil según la tipología; dentro de ESSIDT, según el nombre
niv_tipo_es <- c("Hospital", "Centro de salud", "Puesto sanitario", "Otro ESSIDT", "Unidad móvil")
d$tipo_es <- ifelse(grepl("^ESCI", d$tipologia), "Hospital",
             ifelse(grepl("M.viles", d$categoria), "Unidad móvil",
             ifelse(grepl("^PUESTO", d$nombre), "Puesto sanitario",
             ifelse(grepl("CENTRO DE SALUD|CAPS|CENTRO INTEGRAL DE SALUD|CIS ", d$nombre), "Centro de salud", "Otro ESSIDT"))))

es <- st_as_sf(d, coords = c("lon", "lat"), crs = 4326, remove = FALSE) |>
  mutate(id = row_number())
cat("\nSedes por tipo:\n"); print(table(es$tipo_es))
cat("\nSedes por estado de la geolocalización (ESTADO_GEO):\n"); print(table(es$estado_geo, useNA = "ifany"))
cat("\nSedes por localidad:\n"); print(sort(table(es$localidad), decreasing = TRUE))


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

# 3.7 Población cubierta (radios cuyo punto interior cae dentro de la isócrona)
pts_rz  <- suppressWarnings(st_point_on_surface(rz))
alto    <- rz$riesgo %in% RIESGO_ALTO
pob_tot <- sum(rz$pob, na.rm = TRUE)
pob_alto_tot <- sum(rz$pob[alto], na.rm = TRUE)
cobertura_pob <- do.call(rbind, lapply(sort(CORTES_MIN), function(m) {
  dentro <- lengths(st_intersects(pts_rz, cobertura[cobertura$minutos == m, ])) > 0
  data.frame(
    min          = m,
    radios     = sum(dentro),
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
    st_transform(es[, c("id", "codigo", "nombre", "tipologia", "categoria", "dependencia", "localidad", "tipo_es")], CRS_M),
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

# 4.3 Sedes de salud: forma según el tipo (cuadrado = hospital, círculo = centro de salud,
#     triángulo = puesto sanitario, rombo = otro ESSIDT, triángulo invertido = unidad móvil), todas en azul
es_r <- st_transform(es, CRS_M)
es_r$tipo_f <- factor(es_r$tipo_es, levels = niv_tipo_es)

# 4.4 Encuadre: toda la zona cubierta por el KMZ
ext_r <- st_bbox(rz)
dx_r  <- (ext_r[["xmax"]] - ext_r[["xmin"]]) * 0.03
dy_r  <- (ext_r[["ymax"]] - ext_r[["ymin"]]) * 0.03
caja_r <- st_as_sfc(st_bbox(c(xmin = ext_r[["xmin"]] - dx_r, ymin = ext_r[["ymin"]] - dy_r,
                              xmax = ext_r[["xmax"]] + dx_r, ymax = ext_r[["ymax"]] + dy_r), crs = st_crs(CRS_M)))
es_r <- es_r[lengths(st_intersects(es_r, caja_r)) > 0, ]     # solo las sedes que caen dentro del mapa (para la leyenda)

# 4.5 Subtítulo: radios de riesgo alto dentro de la isócrona y población cubierta
m_ref  <- max(MIN_MOSTRAR)
en_iso <- lengths(st_intersects(pts_rz, cobertura[cobertura$minutos == m_ref, ])) > 0
sub_r  <- paste0("Radios de riesgo alto o muy alto a ≤", m_ref, " min: ", sum(alto & en_iso), " de ", sum(alto),
                 " (", round(100 * sum(alto & en_iso) / max(sum(alto), 1), 1), "%)")
pct_pob <- cobertura_pob$pct[cobertura_pob$min == m_ref]
if (length(pct_pob) == 1) sub_r <- paste0(sub_r, "\nPoblación de los radios a ≤", m_ref, " min: ", pct_pob, "%")

g_r <- ggplot() +
  geom_sf(data = rz, aes(fill = riesgo), color = NA, linewidth = 0) +
  geom_sf(data = iso_r, aes(color = etiqueta), fill = "#1d4ed8", alpha = 0.07, linewidth = 1) +
  geom_sf(data = es_r, aes(shape = tipo_f), fill = "#1d4ed8", color = "white", size = 3.5, stroke = 0.6) +
  scale_fill_manual(values = col_riesgo, breaks = niv_riesgo, drop = FALSE, name = "Riesgo por calor",
                    guide = guide_legend(order = 1)) +
  scale_color_manual(values = col_iso_r, name = "Isócrona",
                     guide = guide_legend(order = 2, override.aes = list(linewidth = 1, fill = NA))) +
  scale_shape_manual(values = c("Hospital" = 22, "Centro de salud" = 21, "Puesto sanitario" = 24, "Otro ESSIDT" = 23, "Unidad móvil" = 25),
                     name = "Establecimiento de salud (público)", drop = TRUE,
                     guide = guide_legend(order = 3)) +
  coord_sf(xlim = c(ext_r[["xmin"]] - dx_r, ext_r[["xmax"]] + dx_r),
           ylim = c(ext_r[["ymin"]] - dy_r, ext_r[["ymax"]] + dy_r), expand = FALSE) +
  labs(
    title    = paste0(CIUDAD, ": riesgo por calor, establecimientos de salud e isócrona"),
    subtitle = sub_r,
    caption  = paste0("Riesgo por calor: ", basename(ARCHIVO_RIESGO), " (por radio censal; categorías según la leyenda del archivo). ",
                      "Sedes: ", basename(ARCHIVO_ES), " (públicas, tipologías ", paste(TIPOLOGIAS, collapse = ", "),
                      if (!is.null(CATEGORIAS_EXTRA)) " y unidades móviles", "; las móviles, en la ubicación registrada).\n",
                      "Isócrona: calles de OpenStreetMap y tiempos con dodgr, perfil \"", PERFIL, "\" a ", vel_kmh, " km/h.\n",
                      "Los porcentajes de población usan solo los radios del KMZ con dato de densidad.")
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
# Alto de la figura según la forma de la zona (evita márgenes en blanco); 3,3 pulgadas para título, subtítulo y pie
ancho_png <- 11
alto_png  <- (ancho_png - 2.4) * (ext_r[["ymax"]] - ext_r[["ymin"]] + 2 * dy_r) / (ext_r[["xmax"]] - ext_r[["xmin"]] + 2 * dx_r) + 3.3
ggsave(SALIDA_PNG, g_r, width = ancho_png, height = alto_png, dpi = 300, bg = "white")
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