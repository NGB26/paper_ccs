# =============================================================================
# MONTERÍA: manzanas + centros de salud de primer y segundo nivel + isócronas
# -----------------------------------------------------------------------------
# Qué hace
#   1. Lee las manzanas (monteria_base_manzanas.shp) y la capa de salud
#      (health_facilities_es/Health_Facilities_ES.shp) de la carpeta de Montería.
#   2. Se queda con las sedes de MONTERÍA con nivel 1 o 2.
#   3. Descarga la red vial de OpenStreetMap con osmdata y calcula isócronas
#      (tiempo por la red) desde cada sede con dodgr.
#   4. Calcula cuánta población de las manzanas queda dentro de cada isócrona.
#   5. Escribe el mapa interactivo (mismo diseño de la página publicada),
#      un GeoPackage con las isócronas y un CSV con la cobertura.
#
# Paquetes: install.packages(c("sf", "dplyr", "jsonlite", "osmdata", "dodgr"))
# Necesita internet solo para descargar la red vial (después queda en caché).
# =============================================================================

suppressPackageStartupMessages({
  library(sf)
  library(dplyr)
  library(jsonlite)
  library(osmdata)
  library(dodgr)
})
sf_use_s2(FALSE)
options(timeout = 600)

## 0. Parámetros ---------------------------------------------------------------

DIR <- "C:/Users/NICOLASGA/OneDrive - Inter-American Development Bank Group/General - SCL_SPH_SPH_CAR/Productos de conocimiento/Paper CCS/paper_ccs/data/monteria"

NIVELES      <- c("1", "2")     # niveles de atención a mapear
CORTES_MIN   <- c(5, 15, 30)    # isócronas en minutos
PERFIL       <- "foot"          # perfil de dodgr: "foot" (5 km/h), "bicycle", "motorcar"
BUFFER_M     <- 100             # ancho (m) alrededor de las calles alcanzadas
HUECOS_M2    <- 50000           # se rellenan huecos menores a esto (m2)
AMBITO       <- "urbano"        # "urbano": sedes dentro del casco urbano; "municipio": todas
USAR_CACHE   <- TRUE            # reutilizar la red descargada (cache_osm_vias.rds)
CRS_M        <- 9377            # MAGNA-SIRGAS 2018 / Origen-Nacional (metros)

ARCHIVO_MZ   <- file.path(DIR, "monteria_base_manzanas.shp")
ARCHIVO_ES   <- file.path(DIR, "health_facilities_es", "Health_Facilities_ES.shp")
ARCHIVO_RED  <- file.path(DIR, "cache_osm_vias.rds")
SALIDA_HTML  <- file.path(DIR, "mapa_monteria_isocronas.html")
SALIDA_GPKG  <- file.path(DIR, "monteria_isocronas_osm.gpkg")
SALIDA_CSV   <- file.path(DIR, "cobertura_poblacion_isocronas.csv")

## 1. Manzanas -----------------------------------------------------------------

mz <- st_read(ARCHIVO_MZ, quiet = TRUE) |>
  st_transform(CRS_M) |>
  st_make_valid()

mz <- mz |>
  mutate(
    pob = ifelse(is.na(pob_tot), NA, round(pob_tot)),
    p65 = ifelse(is.na(pob_tot) | pob_tot == 0, NA, round(pob_65 / pob_tot * 100, 1)),
    acu = round(pct_acu, 1),
    cod = substr(cod_manz, nchar(cod_manz) - 5, nchar(cod_manz))
  )

## 2. Establecimientos de salud de Montería ------------------------------------

nombre_legible <- function(s) {
  # Pasa nombres en MAYÚSCULAS a "Tipo Título" y deja las palabras cortas en minúscula
  chicas <- c("de", "del", "la", "las", "los", "el", "y", "en")
  una <- function(x) {
    if (is.na(x) || !nzchar(x) || x != toupper(x)) return(x)
    w <- strsplit(tolower(x), " ", fixed = TRUE)[[1]]
    w <- paste0(toupper(substring(w, 1, 1)), substring(w, 2))
    w[seq_along(w) > 1 & tolower(w) %in% chicas] <- tolower(w[seq_along(w) > 1 & tolower(w) %in% chicas])
    w[tolower(w) %in% c("ese", "ips")] <- toupper(w[tolower(w) %in% c("ese", "ips")])
    paste(w, collapse = " ")
  }
  vapply(trimws(as.character(s)), una, character(1), USE.NAMES = FALSE)
}

es <- st_read(ARCHIVO_ES, quiet = TRUE)

es <- es |>
  mutate(
    nivel_c = trimws(as.character(nivel)),
    mpio_c  = trimws(as.character(NOM_MPIO)),
    dpto_c  = trimws(as.character(NOM_DPTO))
  ) |>
  filter(
    nivel_c %in% NIVELES,
    grepl("^MONTER.{1,2}A$", mpio_c, ignore.case = TRUE),
    grepl("rdoba", dpto_c, ignore.case = TRUE)
  )

# La capa trae unas 250 filas con coordenadas rotas: se descartan
xy <- st_coordinates(es)
es <- es[xy[, 1] > -77 & xy[, 1] < -74 & xy[, 2] > 7 & xy[, 2] < 10, ]

es <- es |>
  st_transform(4326) |>
  mutate(
    id     = row_number(),
    nombre = nombre_legible(Nombre),
    barrio = nombre_legible(Barrio),
    zona   = ifelse(trimws(as.character(TipoZona)) == "2", "Rural", "Urbana"),
    nivel  = as.integer(nivel_c)
  )

cat("Sedes de nivel", paste(NIVELES, collapse = " y "), "en Montería:", nrow(es), "\n")
print(table(nivel = es$nivel, zona = es$zona))

## 3. Isócronas con osmdata + dodgr --------------------------------------------

# 3.1 Sedes de origen según el ámbito
bb_mz <- st_bbox(st_transform(mz, 4326))
cxy <- st_coordinates(es)
en_urbano <- cxy[, 1] > bb_mz[["xmin"]] - 0.01 & cxy[, 1] < bb_mz[["xmax"]] + 0.01 &
             cxy[, 2] > bb_mz[["ymin"]] - 0.01 & cxy[, 2] < bb_mz[["ymax"]] + 0.01
origen <- if (AMBITO == "urbano") es[en_urbano, ] else es
cat("Sedes con isócronas:", nrow(origen), "\n")

# 3.2 Caja para descargar la red: sedes + alcance máximo + margen
vel_kmh <- c(foot = 5, bicycle = 15, motorcar = 30)[[PERFIL]]
margen  <- max(CORTES_MIN) / 60 * vel_kmh / 111 + 0.01      # en grados
bb <- st_bbox(origen)
bb <- c(xmin = bb[["xmin"]] - margen, ymin = bb[["ymin"]] - margen,
        xmax = bb[["xmax"]] + margen, ymax = bb[["ymax"]] + margen)

# 3.3 Descarga de calles (por cuadrantes para no pasarse del límite de Overpass)
descargar_vias <- function(bb, paso = 0.12, intentos = 3) {
  xs <- unique(c(seq(bb[["xmin"]], bb[["xmax"]], by = paso), bb[["xmax"]]))
  ys <- unique(c(seq(bb[["ymin"]], bb[["ymax"]], by = paso), bb[["ymax"]]))
  piezas <- list()
  for (i in seq_len(length(xs) - 1)) for (j in seq_len(length(ys) - 1)) {
    caja <- c(xs[i], ys[j], xs[i + 1], ys[j + 1])
    for (k in seq_len(intentos)) {
      r <- try(
        opq(bbox = caja, timeout = 300) |>
          add_osm_feature(key = "highway") |>
          osmdata_sf(),
        silent = TRUE
      )
      if (!inherits(r, "try-error")) break
      message("  reintentando cuadrante ", i, "-", j, " (", k, ")"); Sys.sleep(10)
    }
    if (inherits(r, "try-error")) stop("No se pudo descargar el cuadrante ", i, "-", j)
    if (!is.null(r$osm_lines)) piezas[[length(piezas) + 1]] <- r$osm_lines
    message("  cuadrante ", i, "-", j, " listo")
  }
  vias <- bind_rows(piezas)
  vias[!duplicated(vias$osm_id) & !is.na(vias$highway), ]
}

if (USAR_CACHE && file.exists(ARCHIVO_RED)) {
  vias <- readRDS(ARCHIVO_RED)
  cat("Red vial leída del caché:", nrow(vias), "tramos\n")
} else {
  cat("Descargando red vial de OpenStreetMap...\n")
  vias <- descargar_vias(bb)
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
tiempos <- dodgr_times(gr, from = desde, to = v[, c("x", "y")], shortest = FALSE)
tiempos[is.na(tiempos)] <- Inf
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
pts_mz <- suppressWarnings(st_point_on_surface(mz))
pob_tot <- sum(mz$pob, na.rm = TRUE)
cobertura_pob <- do.call(rbind, lapply(sort(CORTES_MIN), function(m) {
  dentro <- lengths(st_intersects(pts_mz, cobertura[cobertura$minutos == m, ])) > 0
  data.frame(
    min      = m,
    manzanas = sum(dentro),
    pob      = sum(mz$pob[dentro], na.rm = TRUE),
    pob_65   = round(sum(mz$pob_65[dentro], na.rm = TRUE)),
    pct      = round(sum(mz$pob[dentro], na.rm = TRUE) / pob_tot * 100, 1)
  )
}))
print(cobertura_pob)

write.csv(cobertura_pob, SALIDA_CSV, row.names = FALSE)
if (file.exists(SALIDA_GPKG)) invisible(file.remove(SALIDA_GPKG))
st_write(
  st_transform(es[, c("id", "nombre", "barrio", "zona", "nivel", "codigo_c12", "ese", "naju_nombr")], CRS_M),
  SALIDA_GPKG, layer = "establecimientos", quiet = TRUE
)
st_write(iso_est,   SALIDA_GPKG, layer = "isocronas_por_establecimiento", quiet = TRUE)
st_write(cobertura, SALIDA_GPKG, layer = "cobertura_total", quiet = TRUE)

## 4. Mapa interactivo ---------------------------------------------------------
# Las geometrías se escriben como rutas SVG en enteros (1 unidad = 0,01 de la
# unidad del mapa), proyectadas con una escala fija de longitud y latitud.

CX <- cos(8.6 * pi / 180)

anillo_svg <- function(m) {
  m <- m[-nrow(m), , drop = FALSE]
  if (nrow(m) < 3) return("")
  X <- as.integer(round((m[, 1] + 76.3) * CX * 1e5))
  Y <- as.integer(round((9 - m[, 2]) * 1e5))
  paste0("M", X[1], " ", Y[1], "l", paste(diff(X), diff(Y), collapse = " "), "z")
}

geom_svg <- function(g) {
  if (length(g) == 0) return("")
  anillos <- if (inherits(g, "MULTIPOLYGON")) unlist(unclass(g), recursive = FALSE) else unclass(g)
  paste0(vapply(anillos, anillo_svg, character(1)), collapse = "")
}

a_wgs84_simple <- function(x, tol = 0.5) {
  x <- st_simplify(st_transform(st_geometry(x), CRS_M), dTolerance = tol, preserveTopology = TRUE)
  st_transform(suppressWarnings(st_collection_extract(st_make_valid(x), "POLYGON")), 4326)
}

# 4.1 Manzanas
mz_s <- a_wgs84_simple(mz)
mz_json <- toJSON(list(
  d   = vapply(mz_s, geom_svg, character(1)),
  pob = mz$pob, p65 = mz$p65, acu = mz$acu, cod = mz$cod
), auto_unbox = TRUE, na = "null", digits = NA)

# 4.2 Establecimientos
xy_es <- st_coordinates(es)
es_json <- toJSON(
  data.frame(n = es$nombre, b = es$barrio, z = es$zona, l = es$nivel,
             lat = round(xy_es[, 2], 6), lon = round(xy_es[, 1], 6)),
  dataframe = "rows", na = "null", digits = NA
)

# 4.3 Isócronas (cobertura total de las sedes)
iso_s <- a_wgs84_simple(cobertura)
iso_json <- toJSON(list(
  d    = setNames(as.list(vapply(iso_s, geom_svg, character(1))), cobertura$minutos),
  cov  = cobertura_pob[, c("min", "pob", "pct")],
  modo = sprintf("Tiempo por la red de OpenStreetMap, perfil dodgr \"%s\" (%s km/h)", PERFIL, vel_kmh)
), auto_unbox = TRUE, na = "null", digits = NA)

# 4.4 Plantilla HTML (la misma página publicada)
plantilla <- r"-----(<title>Mapa de salud Montería</title>
<link rel="preconnect" href="https://fonts.googleapis.com">
<link rel="stylesheet" href="https://fonts.googleapis.com/css2?family=IBM+Plex+Mono:wght@400;500&family=IBM+Plex+Sans:wght@400;500;600&display=swap">
<style>
/* Layout: mapa de puntos a la izquierda (sin mapa base), lista filtrable a la derecha; en celular se apilan. */
:root{
  --bg:#f3f5f2; --panel:#ffffff; --fg:#17211d; --muted:#5d6b64; --line:#d3dbd5; --grid:#e1e7e2;
  --l1:#2563eb; --l2:#0a2f8f; --hl:#17211d; --map:#fafbf9; --ramp:#3d4a43; --iso:#b45309;
  --sans:"IBM Plex Sans",system-ui,-apple-system,"Segoe UI",sans-serif;
  --mono:"IBM Plex Mono",ui-monospace,Menlo,Consolas,monospace;
}
@media (prefers-color-scheme:dark){:root:not([data-theme="light"]){
  --bg:#0f1512; --panel:#172019; --fg:#e6ede8; --muted:#97a69d; --line:#2b3a31; --grid:#222e27;
  --l1:#5aa2ff; --l2:#b5d3ff; --hl:#ffffff; --map:#121a15; --ramp:#b7c7bd; --iso:#fbbf24; color-scheme:dark}}
:root[data-theme="dark"]{
  --bg:#0f1512; --panel:#172019; --fg:#e6ede8; --muted:#97a69d; --line:#2b3a31; --grid:#222e27;
  --l1:#5aa2ff; --l2:#b5d3ff; --hl:#ffffff; --map:#121a15; --ramp:#b7c7bd; --iso:#fbbf24; color-scheme:dark}
*{box-sizing:border-box}
body{background:var(--bg);color:var(--fg);font-family:var(--sans);font-size:14px;line-height:1.45;padding-inline:16px;padding-block:20px 28px}
main{max-width:1200px;margin-inline:auto}
header{display:flex;flex-wrap:wrap;gap:6px 24px;align-items:baseline;justify-content:space-between;margin-bottom:14px}
h1{font-size:1.5rem;font-weight:600;margin:0;letter-spacing:-.01em;text-wrap:balance}
header p{margin:0;color:var(--muted);font-size:.85rem}
.grid{display:grid;grid-template-columns:minmax(0,1.5fr) minmax(0,1fr);gap:16px;align-items:start}
@media(max-width:820px){.grid{grid-template-columns:minmax(0,1fr)}}
.panel{background:var(--panel);border:1px solid var(--line);border-radius:6px;min-width:0}
.bar{display:flex;flex-wrap:wrap;gap:8px 14px;align-items:center;padding:10px 12px;border-bottom:1px solid var(--line)}
.seg{display:inline-flex;border:1px solid var(--line);border-radius:5px;overflow:hidden}
.seg button{font:inherit;background:transparent;color:var(--fg);border:0;padding:5px 10px;cursor:pointer}
.seg button[aria-pressed="true"]{background:var(--fg);color:var(--panel)}
button:focus-visible,input:focus-visible,li:focus-visible{outline:2px solid var(--l1);outline-offset:1px}
label.chk{display:inline-flex;gap:6px;align-items:center;cursor:pointer;color:var(--fg)}
.sw{width:11px;height:11px;display:inline-block}
.sw.c{border-radius:50%;background:var(--l1)}
.sw.d{background:var(--l2);transform:rotate(45deg) scale(.85)}
.mapwrap{position:relative;padding:8px}
svg{display:block;width:100%;height:auto;max-height:78vh;background:var(--map);border-radius:4px}
svg text{font-family:var(--mono);fill:var(--muted)}
.gl{stroke:var(--grid);stroke-width:1;vector-effect:non-scaling-stroke}
.m1{fill:var(--l1);stroke:var(--panel);stroke-width:1;vector-effect:non-scaling-stroke;fill-opacity:.85}
.m2{fill:var(--l2);stroke:var(--panel);stroke-width:1;vector-effect:non-scaling-stroke}
.rur{fill-opacity:.55}
.sel{fill:none;stroke:var(--hl);stroke-width:2;vector-effect:non-scaling-stroke}
.pt{cursor:pointer}
.mz{fill:var(--ramp);stroke:var(--ramp);stroke-opacity:.35;stroke-width:.5;vector-effect:non-scaling-stroke}
select{font:inherit;background:var(--bg);color:var(--fg);border:1px solid var(--line);border-radius:5px;padding:5px 8px;max-width:100%}
.iso{fill:var(--iso);stroke:var(--iso);stroke-width:1.2;vector-effect:non-scaling-stroke;pointer-events:none;fill-rule:evenodd}
.mz{fill-rule:evenodd}
.legend{display:flex;flex-wrap:wrap;gap:4px 12px;align-items:center;padding:0 12px 4px;font-family:var(--mono);font-size:.74rem;color:var(--muted)}
.legend i{display:inline-block;width:14px;height:10px;background:var(--ramp);margin-right:4px;vertical-align:-1px;border:1px solid var(--line)}
#tip{position:absolute;pointer-events:none;background:var(--fg);color:var(--panel);padding:6px 9px;border-radius:4px;font-size:12px;max-width:240px;opacity:0;transition:opacity .08s}
#tip b{display:block;font-weight:600}
.cap{padding:8px 12px 12px;color:var(--muted);font-size:.8rem;display:flex;flex-wrap:wrap;gap:4px 16px}
.list h2{font-size:.8rem;text-transform:uppercase;letter-spacing:.08em;margin:0;color:var(--muted);font-weight:500}
.list .bar{justify-content:space-between}
input[type=search]{font:inherit;width:100%;background:var(--bg);color:var(--fg);border:1px solid var(--line);border-radius:5px;padding:6px 9px}
ul{list-style:none;margin:0;padding:0;max-height:62vh;overflow:auto}
li{display:grid;grid-template-columns:18px minmax(0,1fr) auto;gap:8px;padding:8px 12px;border-bottom:1px solid var(--line);cursor:pointer;align-items:start}
li:hover,li.on{background:var(--bg)}
li .n{font-weight:500;overflow-wrap:anywhere}
li .s{color:var(--muted);font-size:.78rem;overflow-wrap:anywhere}
li .z{font-family:var(--mono);font-size:.72rem;color:var(--muted);white-space:nowrap}
li .sw{margin-top:5px}
.note{margin:14px 0 0;color:var(--muted);font-size:.8rem;max-width:80ch}
</style>

<main>
<header>
  <h1>Montería: manzanas y centros de salud de primer y segundo nivel</h1>
  <p><span id="cnt"></span> sedes con nivel asignado · Córdoba, Colombia</p>
</header>
<div class="grid">
  <section class="panel" aria-label="Mapa">
    <div class="bar">
      <div class="seg" role="group" aria-label="Encuadre">
        <button id="vAll" aria-pressed="false">Municipio</button>
        <button id="vUrb" aria-pressed="true">Casco urbano</button>
      </div>
      <label class="chk"><input type="checkbox" id="f1" checked><span class="sw c"></span>Nivel 1</label>
      <label class="chk"><input type="checkbox" id="f2" checked><span class="sw d"></span>Nivel 2</label>
      <label class="chk"><input type="checkbox" id="fr" checked>Rurales</label>
      <label class="chk"><input type="checkbox" id="fu" checked>Urbanos</label>
      <label class="chk" id="isoLbl"><input type="checkbox" id="fi" checked><span class="sw" style="background:var(--iso);opacity:.6"></span>Isócronas</label>
    </div>
    <div class="bar" style="border-top:0;padding-top:0">
      <label for="cv" style="color:var(--muted)">Colorear manzanas por</label>
      <select id="cv"><option value="none">Sin color</option><option value="pob" selected>Población total</option><option value="p65">% de personas de 65 años o más</option><option value="acu">% con acueducto</option></select>
    </div>
    <div class="mapwrap"><svg id="svg" role="img" aria-label="Manzanas de Montería y ubicación de los establecimientos"><g id="gMz" transform="scale(0.01)"></g><g id="gIso" transform="scale(0.01)"></g><g id="gOv"></g></svg><div id="tip"></div></div>
    <div class="legend" id="leg"></div>
    <div class="legend" id="isoLg"></div>
    <div class="cap"><span id="vis"></span><span>Rural: punto más tenue · Coordenadas WGS84</span></div>
  </section>
  <section class="panel list" aria-label="Listado">
    <div class="bar"><h2>Listado</h2><span id="lc" style="font-family:var(--mono);font-size:.8rem"></span></div>
    <div class="bar"><input type="search" id="q" placeholder="Buscar por nombre o barrio" aria-label="Buscar"></div>
    <ul id="ul"></ul>
  </section>
</div>
<p class="note">Fuentes: capa Health_Facilities_ES (campo <code>nivel</code> igual a 1 o 2, municipio MONTERÍA) y capa monteria_base_manzanas (5.091 manzanas del casco urbano, con población total, población de 65 años o más y porcentaje de viviendas con acueducto y con energía). Los polígonos se simplificaron a unos 0,5 m. Las isócronas se calculan sobre la red de OpenStreetMap con <code>osmdata</code> y <code>dodgr</code>; la cobertura es el porcentaje de población de las manzanas cuyo centro cae dentro de la isócrona. Las sedes rurales quedan fuera de la mancha de manzanas. Se muestran sedes, no instituciones: un mismo hospital puede aparecer varias veces.</p>
</main>

<script>
const D=__DATA__;
const MZ=__MZ__;
const ISO=__ISO__;
const NS="http://www.w3.org/2000/svg", K=1000;
const svg=document.getElementById("svg"), tip=document.getElementById("tip"), ul=document.getElementById("ul");
const lat0=8.6, cx=Math.cos(lat0*Math.PI/180);
const X=l=>(l+76.3)*cx*K, Y=l=>(9-l)*K;
const views={
  all:{lon:[-76.20,-75.70],lat:[8.36,8.93]},
  urb:{lon:[-75.920,-75.832],lat:[8.708,8.820]}
};
let view="urb", sel=null;
const st={1:true,2:true,r:true,u:true,q:""};
const $=id=>document.getElementById(id);
const E=(n,a,p)=>{const e=document.createElementNS(NS,n);for(const k in a)e.setAttribute(k,a[k]);if(p)p.appendChild(e);return e};
document.getElementById("cnt").textContent=D.length;

function pass(d){return st[d.l]&&((d.z==="Rural")?st.r:st.u)&&(!st.q||(d.n+" "+d.b).toLowerCase().includes(st.q))}

function draw(){
  const v=views[view], x0=X(v.lon[0]),x1=X(v.lon[1]),y0=Y(v.lat[1]),y1=Y(v.lat[0]);
  const w=x1-x0,h=y1-y0;
  svg.setAttribute("viewBox",`${x0} ${y0} ${w} ${h}`);
  const svg0=svg, ov=$("gOv"); ov.innerHTML="";
  const px=w/(svg.getBoundingClientRect().width||600); // unidades por pixel
  const step=view==="all"?0.1:0.02, dec=view==="all"?1:2;
  for(let l=Math.ceil(v.lon[0]/step)*step;l<=v.lon[1];l+=step){
    const x=X(l);E("line",{x1:x,x2:x,y1:y0,y2:y1,class:"gl"},ov);
    const t=E("text",{x:x+3*px,y:y1-4*px,"font-size":10*px},ov);t.textContent=l.toFixed(dec)+"°";
  }
  for(let l=Math.ceil(v.lat[0]/step)*step;l<=v.lat[1];l+=step){
    const y=Y(l);E("line",{x1:x0,x2:x1,y1:y,y2:y,class:"gl"},ov);
    const t=E("text",{x:x0+4*px,y:y-3*px,"font-size":10*px},ov);t.textContent=l.toFixed(dec)+"°";
  }
  // barra de escala
  const km=view==="all"?10:2, len=km/111.32*K, bx=x1-len-14*px, by=y0+18*px;
  E("line",{x1:bx,x2:bx+len,y1:by,y2:by,stroke:"currentColor","stroke-width":2,style:"color:var(--fg)","vector-effect":"non-scaling-stroke"},ov);
  const st2=E("text",{x:bx,y:by-5*px,"font-size":10*px},ov);st2.textContent=km+" km";
  let shown=0,out=0;
  D.forEach((d,i)=>{
    if(!pass(d))return;
    const x=X(d.lon),y=Y(d.lat);
    if(x<x0||x>x1||y<y0||y>y1){out++;return}
    shown++;
    const g=E("g",{class:"pt"},ov);g.dataset.i=i;
    if(d.l===1){E("circle",{cx:x,cy:y,r:5*px,class:"m1"+(d.z==="Rural"?" rur":"")},g)}
    else{const r=7*px;E("path",{d:`M${x} ${y-r}L${x+r} ${y}L${x} ${y+r}L${x-r} ${y}Z`,class:"m2"},g)}
    E("circle",{cx:x,cy:y,r:10*px,fill:"transparent"},g);
    g.addEventListener("mouseenter",e=>show(i,e));g.addEventListener("mousemove",e=>show(i,e));
    g.addEventListener("mouseleave",()=>tip.style.opacity=0);
    g.addEventListener("click",()=>pick(i,false));
    if(sel===i)E("circle",{cx:x,cy:y,r:11*px,class:"sel"},ov);
  });
  $("vis").textContent=`${shown} en el encuadre`+(out?` · ${out} fuera de este encuadre`:"");
}
function show(i,e){
  const d=D[i],r=svg.parentNode.getBoundingClientRect();
  tip.innerHTML=`<b>${d.n}</b>${d.b?d.b+" · ":""}${d.z} · Nivel ${d.l}`;
  tip.style.left=Math.min(e.clientX-r.left+12,r.width-250)+"px";tip.style.top=(e.clientY-r.top+12)+"px";tip.style.opacity=1;
}
function list(){
  ul.innerHTML="";let n=0;
  D.forEach((d,i)=>{
    if(!pass(d))return;n++;
    const li=document.createElement("li");li.tabIndex=0;li.dataset.i=i;if(sel===i)li.className="on";
    li.innerHTML=`<span class="sw ${d.l===1?"c":"d"}"></span><div><div class="n"></div><div class="s"></div></div><span class="z">N${d.l} · ${d.z==="Rural"?"R":"U"}</span>`;
    li.querySelector(".n").textContent=d.n;li.querySelector(".s").textContent=d.b||"Sin barrio registrado";
    li.addEventListener("click",()=>pick(i,true));
    li.addEventListener("keydown",e=>{if(e.key==="Enter")pick(i,true)});
    ul.appendChild(li);
  });
  $("lc").textContent=n+" de "+D.length;
}
function pick(i,fromList){
  sel=i;const d=D[i];
  if(fromList){const v=views[view];if(d.lon<v.lon[0]||d.lon>v.lon[1]||d.lat<v.lat[0]||d.lat>v.lat[1]){setView("all")}}
  draw();list();
  const li=ul.querySelector(`li[data-i="${i}"]`);if(li&&!fromList)li.scrollIntoView({block:"nearest"});
}
function setView(v){view=v;$("vAll").setAttribute("aria-pressed",v==="all");$("vUrb").setAttribute("aria-pressed",v==="urb");draw()}
$("vAll").onclick=()=>setView("all");$("vUrb").onclick=()=>setView("urb");
[["f1",1],["f2",2],["fr","r"],["fu","u"]].forEach(([id,k])=>$(id).onchange=e=>{st[k]=e.target.checked;draw();list()});
$("q").oninput=e=>{st.q=e.target.value.trim().toLowerCase();draw();list()};

// ---- capa de manzanas (se construye una vez) ----
const gMz=$("gMz"), paths=[], OP=[.10,.26,.43,.61,.80];
MZ.d.forEach((d,i)=>{const p=E("path",{d,class:"mz"},gMz);p.dataset.i=i;paths.push(p)});
const VAR={pob:{a:MZ.pob,f:v=>Math.round(v).toLocaleString("es-AR"),u:"personas por manzana"},
  p65:{a:MZ.p65,f:v=>v.toFixed(0)+"%",u:"de la población"},acu:{a:MZ.acu,f:v=>v.toFixed(0)+"%",u:"de las viviendas"}};
function quant(a){const s=a.filter(v=>v!=null).sort((x,y)=>x-y);return [.2,.4,.6,.8].map(q=>s[Math.floor(q*(s.length-1))])}
function paint(){
  const k=$("cv").value, leg=$("leg");
  if(k==="none"){paths.forEach(p=>{p.style.fillOpacity=.13});leg.innerHTML="";return}
  const V=VAR[k], br=quant(V.a);
  paths.forEach((p,i)=>{const v=V.a[i];
    if(v==null){p.style.fillOpacity=0;return}
    let c=0;while(c<4&&v>br[c])c++;p.style.fillOpacity=OP[c]});
  const mn=Math.min(...V.a.filter(v=>v!=null)),mx=Math.max(...V.a.filter(v=>v!=null)),e=[mn,...br,mx];
  leg.innerHTML=OP.map((o,c)=>`<span><i style="opacity:${o+.1}"></i>${V.f(e[c])}–${V.f(e[c+1])}</span>`).join("")+`<span>${V.u} · quintiles · sin dato: transparente</span>`;
}
$("cv").onchange=paint;
gMz.addEventListener("mousemove",e=>{
  const i=e.target.dataset&&e.target.dataset.i;if(i==null)return;
  const r=svg.parentNode.getBoundingClientRect(),n=x=>x==null?"s/d":x;
  tip.innerHTML=`<b>Manzana ${MZ.cod[i]}</b>${n(MZ.pob[i])} personas · 65+: ${MZ.p65[i]==null?"s/d":MZ.p65[i]+"%"} · acueducto: ${MZ.acu[i]==null?"s/d":MZ.acu[i]+"%"}`;
  tip.style.left=Math.min(e.clientX-r.left+12,r.width-250)+"px";tip.style.top=(e.clientY-r.top+12)+"px";tip.style.opacity=1;
});
gMz.addEventListener("mouseleave",()=>tip.style.opacity=0);
paint();

// ---- isócronas (cobertura de todas las sedes, 30 > 15 > 5 min para que las menores queden encima) ----
const gIso=$("gIso");
const ISOK=(ISO&&ISO.d)?Object.keys(ISO.d).map(Number).sort((a,b)=>b-a):[];
if(ISOK.length){ISOK.forEach((m,j)=>{const f=ISOK.length>1?j/(ISOK.length-1):1;
  const p=E("path",{d:ISO.d[String(m)],class:"iso"},gIso);p.style.fillOpacity=.10+.14*f;p.style.strokeOpacity=.45+.35*f});}
else $("isoLbl").hidden=true;
function isoLegend(){
  const el=$("isoLg");if(!ISOK.length||!ISO.cov)return;
  const n=ISO.cov.length;
  el.innerHTML=ISO.cov.slice().sort((a,b)=>a.min-b.min).map((c,j)=>`<span><i style="background:var(--iso);opacity:${.7-.4*(n>1?j/(n-1):0)}"></i>${c.min} min: ${c.pct}% de la población urbana</span>`).join("")+`<span>${ISO.modo}</span>`;
}
$("fi").onchange=e=>{gIso.style.display=e.target.checked?"":"none";$("isoLg").style.display=e.target.checked?"":"none"};
isoLegend();
let rt;window.addEventListener("resize",()=>{clearTimeout(rt);rt=setTimeout(draw,120)});
draw();list();
</script>
)-----"
Encoding(plantilla) <- "UTF-8"   # la plantilla tiene tildes: se fuerza UTF-8 sin importar la configuración regional

reemplazar <- function(txt, clave, valor) {
  i <- regexpr(clave, txt, fixed = TRUE)
  paste0(substr(txt, 1, i - 1), valor, substring(txt, i + nchar(clave)))
}
html <- plantilla |>
  reemplazar("__DATA__", as.character(es_json)) |>
  reemplazar("__MZ__",   as.character(mz_json)) |>
  reemplazar("__ISO__",  as.character(iso_json))

writeLines(html, SALIDA_HTML, useBytes = TRUE)
cat("\nListo.\n  Mapa:      ", SALIDA_HTML, "\n  GeoPackage:", SALIDA_GPKG, "\n  Cobertura: ", SALIDA_CSV, "\n")
if (interactive()) utils::browseURL(SALIDA_HTML)


## 5. Mapa estático con ggplot2 -------------------------------------------------
# Bloque independiente: usa los objetos que ya creó el script (mz, es, cobertura,
# cobertura_pob, CRS_M, PERFIL, vel_kmh, DIR). Se puede pegar al final tal cual.

library(ggplot2)

VAR_MZ     <- "pob_65"  # "pob_65" (personas de 65+), "p65" (% de 65+), "pob" o "acu"
VISTA_GG   <- "urbano"  # "urbano" (casco urbano) o "municipio" (todas las sedes)
MIN_OCULTAR <- c(15, 30)  # isócronas (en minutos) que no se dibujan en el mapa; NULL = mostrar todas
SALIDA_PNG <- file.path(DIR, "mapa_estatico_monteria_isocronas.png")

# 5.1 Mapa de calor de manzanas (sin bordes). Un solo tono, de claro a oscuro.
#     El tope de la escala es el percentil 99 para que unas pocas manzanas no aplasten el resto.
titulo_var <- switch(VAR_MZ,
                     pob_65 = "Personas de 65 años o más\npor manzana",
                     p65    = "Población de 65 años o más\n(% de la manzana)",
                     pob    = "Población por manzana",
                     acu    = "Viviendas con acueducto (%)"
)
mz_gg <- mz
mz_gg$valor <- mz_gg[[VAR_MZ]]
tope  <- as.numeric(quantile(mz_gg$valor, 0.99, na.rm = TRUE))
fmt_leg <- function(x) if (VAR_MZ %in% c("p65", "acu")) paste0(round(x), "%") else format(round(x), big.mark = ".", decimal.mark = ",", trim = TRUE)

# 5.2 Isócronas: relleno suave y semitransparente + contorno grueso por tiempo
cob_gg <- cobertura[order(-cobertura$minutos), ]
cob_gg <- cob_gg[!(cob_gg$minutos %in% MIN_OCULTAR), ]
cob_gg$min_f <- factor(cob_gg$minutos, levels = sort(unique(cob_gg$minutos)))
col_iso <- setNames(colorRampPalette(c("#7c2d12", "#d97706", "#f59e0b"))(nlevels(cob_gg$min_f)),
                    levels(cob_gg$min_f))

# 5.3 Sedes de salud: forma por nivel (círculo = 1, rombo = 2), todas en azul
es_gg <- st_transform(es, CRS_M)
es_gg$nivel_f <- factor(es_gg$nivel, levels = c(1, 2), labels = c("Nivel 1", "Nivel 2"))

# 5.4 Encuadre
ext <- if (VISTA_GG == "urbano") st_bbox(mz_gg) else st_bbox(st_union(st_geometry(mz_gg), st_geometry(es_gg)))
dx <- (ext[["xmax"]] - ext[["xmin"]]) * 0.03
dy <- (ext[["ymax"]] - ext[["ymin"]]) * 0.03

# 5.5 Texto: cobertura de las isócronas
cob_mostrar <- cobertura_pob[!(cobertura_pob$min %in% MIN_OCULTAR), ]
cob_txt <- paste0("Población urbana a ≤", cob_mostrar$min, " min: ", cob_mostrar$pct, "%",
                  collapse = "  ·  ")

g <- ggplot() +
  geom_sf(data = mz_gg, aes(fill = valor), color = NA, linewidth = 0) +
  geom_sf(data = cob_gg, aes(color = min_f), fill = "#f59e0b", alpha = 0.08, linewidth = 1) +
  geom_sf(data = es_gg, aes(shape = nivel_f), fill = "#1d4ed8", color = "white", size = 3.5, stroke = 0.6) +
  scale_fill_gradientn(
    colours = c("#f3f5f2", "#c9d3cc", "#8fa297", "#55695d", "#2b3a31"),
    limits = c(0, tope), oob = scales::squish, na.value = "#f7f7f5",
    name = titulo_var, labels = fmt_leg,
    guide = guide_colourbar(order = 1, barheight = unit(4.5, "cm"), barwidth = unit(0.45, "cm"))
  ) +
  scale_color_manual(values = col_iso, name = "Isócrona (min)",
                     guide = guide_legend(order = 2, override.aes = list(linewidth = 1, fill = NA))) +
  scale_shape_manual(values = c("Nivel 1" = 21, "Nivel 2" = 23), name = "Centro de salud",
                     guide = guide_legend(order = 3)) +
  coord_sf(xlim = c(ext[["xmin"]] - dx, ext[["xmax"]] + dx),
           ylim = c(ext[["ymin"]] - dy, ext[["ymax"]] + dy), expand = FALSE) +
  labs(
    title    = "Montería: centros de salud de primer y segundo nivel e isócronas",
    subtitle = cob_txt,
    caption  = paste0("Tiempo por la red de OpenStreetMap (osmdata + dodgr), perfil \"", PERFIL, "\" a ", vel_kmh,
                      " km/h. Manzanas: base de manzanas de Montería. Sedes: Health_Facilities_ES.\n",
                      "La escala de color llega hasta el percentil 99 (", fmt_leg(tope), "); las manzanas por encima toman el color más oscuro.")
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
  g <- g +
    ggspatial::annotation_scale(location = "bl", width_hint = 0.25, text_cex = 0.7) +
    ggspatial::annotation_north_arrow(location = "tr", which_north = "true",
                                      style = ggspatial::north_arrow_minimal(), height = unit(0.9, "cm"))
}

print(g)
ggsave(SALIDA_PNG, g, width = 9, height = 10, dpi = 300, bg = "white")
cat("Mapa estático:", SALIDA_PNG, "\n")


## 6. Mapa estático: riesgo por calor (KMZ) + sedes de salud + isócrona de 5 min ----
# Bloque independiente: usa los objetos que ya creó el script (es, cobertura,
# cobertura_pob, CRS_M, PERFIL, vel_kmh, DIR). Se puede pegar al final tal cual.
# No pisa nada del bloque 5 (todos los objetos nuevos terminan en _r).

library(ggplot2)

ARCHIVO_RIESGO <- r"(C:\Users\NICOLASGA\OneDrive - Inter-American Development Bank Group\General - SCL_SPH_SPH_CAR\Productos de conocimiento\Paper CCS\paper_ccs\data\monteria\google_earth-monteria\google_earth\monteria_riesgo.kmz)"
MIN_MOSTRAR  <- 5                      # isócrona(s) a dibujar, en minutos
RIESGO_ALTO  <- c("Alta", "Muy alta")  # categorías que cuentan como "riesgo alto" en el subtítulo
SALIDA_PNG_R <- file.path(DIR, "mapa_estatico_riesgo_calor_isocronas.png")

# 6.1 Leer el KMZ (un KMZ es un zip con un doc.kml adentro)
if (grepl("\\.kmz$", ARCHIVO_RIESGO, ignore.case = TRUE)) {
  dir_kmz <- file.path(tempdir(), "riesgo_kmz")
  unzip(ARCHIVO_RIESGO, exdir = dir_kmz, overwrite = TRUE)
  kml <- list.files(dir_kmz, pattern = "\\.kml$", full.names = TRUE, recursive = TRUE)[1]
} else {
  kml <- ARCHIVO_RIESGO
}
capas <- st_layers(kml)
capa  <- capas$name[which.max(capas$features)]   # el KML trae también una capa vacía con la leyenda
rz <- st_read(kml, layer = capa, quiet = TRUE)
rz <- rz[!st_is_empty(rz), ]
rz <- st_transform(st_zm(rz, drop = TRUE, what = "ZM"), CRS_M)

# La categoría viene al final del nombre: "2300110000000000010101 - Alta"
niv_riesgo <- c("Muy baja", "Baja", "Media", "Alta", "Muy alta", "Sin dato")
col_nombre <- names(rz)[tolower(names(rz)) == "name"][1]
rz$riesgo  <- factor(sub("^.* - ", "", rz[[col_nombre]]), levels = niv_riesgo)
if (anyNA(rz$riesgo)) warning(sum(is.na(rz$riesgo)), " manzanas con categoría de riesgo no reconocida")
cat("Manzanas con riesgo por calor:", nrow(rz), "\n"); print(table(rz$riesgo))

# Colores = los de la leyenda del propio KMZ (ColorBrewer OrRd)
col_riesgo <- c("Muy baja" = "#FEF0D9", "Baja" = "#FDCC8A", "Media" = "#FC8D59",
                "Alta" = "#E34A33", "Muy alta" = "#B30000", "Sin dato" = "#BDBDBD")

# 6.2 Isócrona(s): contorno grueso azul oscuro + relleno azul muy suave
lab_modo <- switch(PERFIL, foot = "a pie", bicycle = "en bicicleta", motorcar = "en auto", PERFIL)
iso_r <- cobertura[cobertura$minutos %in% MIN_MOSTRAR, ]
iso_r <- iso_r[order(-iso_r$minutos), ]
etq   <- paste0("≤", iso_r$minutos, " min ", lab_modo)
iso_r$etiqueta <- factor(etq, levels = rev(etq))
col_iso_r <- setNames(colorRampPalette(c("#1e3a8a", "#60a5fa"))(nlevels(iso_r$etiqueta)),
                      levels(iso_r$etiqueta))

# 6.3 Sedes de salud
es_r <- st_transform(es, CRS_M)
es_r$nivel_f <- factor(es_r$nivel, levels = c(1, 2), labels = c("Nivel 1", "Nivel 2"))

# 6.4 Encuadre: toda la zona cubierta por el KMZ
ext_r <- st_bbox(rz)
dx_r  <- (ext_r[["xmax"]] - ext_r[["xmin"]]) * 0.03
dy_r  <- (ext_r[["ymax"]] - ext_r[["ymin"]]) * 0.03

# 6.5 Subtítulo: cuántas manzanas de riesgo alto quedan dentro de la isócrona
m_ref   <- max(MIN_MOSTRAR)
iso_ref <- cobertura[cobertura$minutos == m_ref, ]
pts_rz  <- suppressWarnings(st_point_on_surface(rz))
en_iso  <- lengths(st_intersects(pts_rz, iso_ref)) > 0
alto    <- rz$riesgo %in% RIESGO_ALTO
sub_r   <- paste0("Manzanas de riesgo alto o muy alto a ≤", m_ref, " min: ",
                  sum(alto & en_iso), " de ", sum(alto), " (", round(100 * sum(alto & en_iso) / sum(alto), 1), "%)")
pct_pob <- cobertura_pob$pct[cobertura_pob$min == m_ref]
if (length(pct_pob) == 1) sub_r <- paste0(sub_r, "\nPoblación urbana a ≤", m_ref, " min: ", pct_pob, "%")

g_r <- ggplot() +
  geom_sf(data = rz, aes(fill = riesgo), color = NA, linewidth = 0) +
  geom_sf(data = iso_r, aes(color = etiqueta), fill = "#1d4ed8", alpha = 0.07, linewidth = 1) +
  geom_sf(data = es_r, aes(shape = nivel_f), fill = "#1d4ed8", color = "white", size = 3.5, stroke = 0.6) +
  scale_fill_manual(values = col_riesgo, breaks = niv_riesgo, drop = FALSE, name = "Riesgo por calor",
                    guide = guide_legend(order = 1)) +
  scale_color_manual(values = col_iso_r, name = "Isócrona",
                     guide = guide_legend(order = 2, override.aes = list(linewidth = 1, fill = NA))) +
  scale_shape_manual(values = c("Nivel 1" = 21, "Nivel 2" = 23), name = "Centro de salud",
                     guide = guide_legend(order = 3)) +
  coord_sf(xlim = c(ext_r[["xmin"]] - dx_r, ext_r[["xmax"]] + dx_r),
           ylim = c(ext_r[["ymin"]] - dy_r, ext_r[["ymax"]] + dy_r), expand = FALSE) +
  labs(
    title    = "Montería: riesgo por calor, centros de salud de nivel 1 y 2 e isócrona",
    subtitle = sub_r,
    caption  = paste0("Riesgo por calor: monteria_riesgo.kmz (por manzana; categorías según la leyenda del archivo). Sedes: Health_Facilities_ES.\n",
                      "Isócrona: red de OpenStreetMap (osmdata + dodgr), perfil \"", PERFIL, "\" a ", vel_kmh, " km/h.")
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
ggsave(SALIDA_PNG_R, g_r, width = 9, height = 10, dpi = 300, bg = "white")
cat("Mapa estático (riesgo por calor):", SALIDA_PNG_R, "\n")
