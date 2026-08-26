# ==============================================================================
# Mapas de indicadores censales por radio censal — Departamento Confluencia
# (incluye Neuquén capital, Plottier, Centenario, Vista Alegre, Senillosa, etc.)
# Fuente: INDEC, Censo Nacional de Población, Hogares y Viviendas 2022
#         Procesado con Redatam 7, CEPAL/CELADE
# ==============================================================================

library(sf)
library(dplyr)
library(readxl)
library(ggplot2)

# ------------------------------------------------------------------------------
# RUTAS — ajustá según donde tengas los archivos descargados
# ------------------------------------------------------------------------------
carpeta_datos  <- "C:/Users/NICOLASGA/OneDrive - Inter-American Development Bank Group/General - SCL_SPH_SPH_CAR/Productos de conocimiento/Paper CCS/Neuquen/datos nqn"   # carpeta donde están el .shp y los 3 .xlsX
carpeta_salida <- "C:/Users/NICOLASGA/OneDrive - Inter-American Development Bank Group/General - SCL_SPH_SPH_CAR/Productos de conocimiento/Paper CCS/paper_ccs/data/neuquen"
dir.create(carpeta_salida, showWarnings = FALSE)

# ==============================================================================
# Mapas de indicadores censales por radio censal — Departamento Confluencia
# (incluye Neuquén capital, Plottier, Centenario, Vista Alegre, Senillosa, etc.)
# Fuente: INDEC, Censo Nacional de Población, Hogares y Viviendas 2022
#         Procesado con Redatam 7, CEPAL/CELADE
# ==============================================================================

# ------------------------------------------------------------------------------
# PASO 1. Cargar solo los radios censales de Confluencia (PROV 58, DEPTO 035)
# ------------------------------------------------------------------------------
# El shapefile nacional tiene ~66.500 radios censales (todo el país), así que
# filtramos con una consulta SQL en el momento de la lectura, igual que se hizo
# con Montería (WHERE MPIO_CDPMP), en vez de cargar todo y filtrar después.
radios_confluencia <- st_read(
  file.path(carpeta_datos, "radios2022_v1_0.shp"),
  query = "SELECT * FROM radios2022_v1_0 WHERE PROV = '58' AND DEPTO = '035'",
  quiet = TRUE
)

message("Radios cargados: ", nrow(radios_confluencia))
# Esperado: 645 radios (605 urbanos + 33 mixtos + 7 rurales)


# ------------------------------------------------------------------------------
# PASO 1-bis. Calcular el área de cada radio censal (para densidad poblacional)
# ------------------------------------------------------------------------------
# El shapefile viene en coordenadas geográficas (lat/lon), donde st_area() no da
# metros cuadrados reales. Se reproyecta a UTM zona 19S (EPSG:32719), la faja
# UTM que cubre Neuquén (~68°O), se calcula el área en esa proyección y se
# guarda en el objeto original en su CRS geográfico (no se pisa la geometría
# usada para los mapas).
radios_confluencia <- radios_confluencia |>
  mutate(area_km2 = as.numeric(st_area(st_transform(radios_confluencia, 32719))) / 1e6)


# ------------------------------------------------------------------------------
# PASO 2. Leer los 3 Excel de Redatam y armar una tabla única de indicadores
# ------------------------------------------------------------------------------
# Los tres archivos comparten el mismo layout de Redatam: 9 filas de encabezado,
# columna B = Código de radio (9 dígitos: PROV+DEPTO+FRAC+RADIO), columnas C en
# adelante = los indicadores. Las últimas filas son notas al pie (fuente INDEC),
# no datos — se descartan automáticamente al forzar tipo numérico en "codigo".

leer_redatam <- function(archivo, nombres_col) {
  df <- read_excel(file.path(carpeta_datos, archivo), sheet = "Output", skip = 9)
  df <- df[, 2:(1 + length(nombres_col))]
  names(df) <- nombres_col
  df$codigo <- suppressWarnings(as.numeric(df$codigo))
  df <- df[!is.na(df$codigo), ]
  df
}

envejecimiento <- leer_redatam(
  "n_tmp_12022801.xlsX",
  c("codigo", "indice_envejecimiento", "pct_pob_65_mas", "pct_pob_80_mas")
)

actividad_empleo <- leer_redatam(
  "n_tmp_12022821.xlsX",
  c("codigo", "tasa_actividad", "tasa_empleo")
)

servicios <- leer_redatam(
  "n_tmp_12022841.xlsX",
  c("codigo", "pct_sin_agua_red", "pct_desague_sin_red")
)

# Unir los tres indicadores por código de radio
indicadores <- envejecimiento |>
  full_join(actividad_empleo, by = "codigo") |>
  full_join(servicios, by = "codigo")

message("Radios con indicadores: ", nrow(indicadores))


# ------------------------------------------------------------------------------
# PASO 2-bis. Leer población total por radio (para densidad poblacional) y NBI
# ------------------------------------------------------------------------------
# A diferencia de los 3 archivos anteriores (formato "tabulado", un radio por
# fila con los indicadores ya calculados en columnas), la exportación Redatam
# de población viene en formato "frecuencias" (un bloque AREA # <radio> por
# radio, con la distribución por grupos de edad y la fila "Total" = población
# total). Ese archivo ya fue reordenado a dos columnas (codigo, poblacion_total)
# y guardado como neuquen_poblacion_total_radios.xlsx — se coloca en la misma
# carpeta que los otros 3 Excel (carpeta_datos) antes de correr el script.
poblacion <- read_excel(
  file.path(carpeta_datos, "neuquen_poblacion_total_radios.xlsx"),
  sheet = "poblacion_radios"
)
poblacion$codigo <- suppressWarnings(as.numeric(poblacion$codigo))
poblacion <- poblacion[!is.na(poblacion$codigo), ]


nbi <- read_excel(
  file.path(carpeta_datos, "nbi_nqn.xlsx"),
  sheet = "Output"
)
nbi$codigo <- suppressWarnings(as.numeric(nbi$radio))


# Unir los cinco indicadores por código de radio
indicadores <- envejecimiento |>
  full_join(actividad_empleo, by = "codigo") |>
  full_join(servicios, by = "codigo") |>
  full_join(poblacion, by = "codigo") |>
  full_join(nbi, by = "codigo")

message("Radios con indicadores: ", nrow(indicadores))


# ------------------------------------------------------------------------------
# PASO 3. Unir la geometría (LINK) con los indicadores (Código)
# ------------------------------------------------------------------------------
# LINK en el shapefile es texto (9 dígitos, ej. "580350101"); codigo en los
# Excel es numérico. Se homologa a texto en ambos lados antes del join.
radios_confluencia <- radios_confluencia |>
  mutate(link_chr = as.character(LINK))

indicadores <- indicadores |>
  mutate(link_chr = as.character(codigo))

confluencia_ind <- radios_confluencia |>
  left_join(indicadores, by = "link_chr")

# Diagnóstico rápido de la unión (debería dar match completo, ya verificado 1:1)
sin_match <- sum(is.na(confluencia_ind$indice_envejecimiento))
message("Radios sin indicadores tras el join: ", sin_match, " de ", nrow(confluencia_ind))

# ------------------------------------------------------------------------------
# PASO 3-ter. Densidad poblacional (habitantes / km2)
# ------------------------------------------------------------------------------
# poblacion_total viene del Excel de Redatam; area_km2 se calculó en PASO 1-bis
# reproyectando a UTM 19S. Radios sin población (NA) o sin área (polígono nulo)
# quedan como NA, igual que el resto de los indicadores.
confluencia_ind <- confluencia_ind |>
  mutate(densidad_poblacional = poblacion_total / area_km2)

message("Densidad poblacional — resumen (hab/km2): ")
print(summary(confluencia_ind$densidad_poblacional))

# ------------------------------------------------------------------------------
# PASO 3-ter A . % DE HOGARES CON NBI
# ------------------------------------------------------------------------------
confluencia_ind <- confluencia_ind |>
  mutate(nbi_pct = viv_nbi / total_viv*100)


# ------------------------------------------------------------------------------
# PASO 3-bis. Recorte al conglomerado urbano Neuquén–Plottier–Centenario–Vista Alegre
# ------------------------------------------------------------------------------
# El departamento Confluencia (7.352 km2) incluye zonas rurales y localidades
# alejadas del conglomerado (p. ej. Villa El Chocón, San Patricio del Chañar),
# que distorsionan tanto el mapa como los cortes por cuantiles. Se aplican dos
# recortes complementarios:
#
# (a) Se descartan los radios puramente rurales (TIPO == "R"), que son polígonos
#     muy extensos y poco representativos de la trama urbana.
# (b) Se acota la ventana del mapa (coord_sf) a la caja del conglomerado
#     Neuquén–Plottier–Centenario–Vista Alegre, cuyas coordenadas de referencia son:
#     Neuquén capital (-38.9516, -68.0591) y Plottier (-38.9523, -68.2270)
#     (fuente: latitude.to y db-city.com). El recorte solo afecta la
#     visualización (zoom), no los datos ni los cortes por cuantiles.
#
# Si preferís un recorte administrativo exacto (por localidad censal, en vez de
# una caja aproximada), la capa oficial de INDEC con polígonos de localidad está
# en https://geonode.indec.gob.ar/layers/geonode_data:geonode:localidades_censales
# (campo "nam" = nombre de localidad, "aglomerado" = conglomerado al que
# pertenece). Se puede leer directamente en R con sf::read_sf() apuntando a la
# URL WFS de esa capa y cruzar por intersección de centroides con st_join().
# Dejo comentado un bloque con esa alternativa por si el recorte por caja no
# resulta suficientemente preciso.

confluencia_urbano <- confluencia_ind |> filter(TIPO != "R")

bbox_metro <- c(xmin = -68.20, xmax = -67.99, ymin = -38.99, ymax = -38.86)

# --- Alternativa por localidad censal (INDEC), desactivada por defecto ------
# localidades <- read_sf(paste0(
#   "https://geonode.indec.gob.ar/geoserver/ows?service=WFS&version=2.0.0&",
#   "request=GetFeature&typeName=geonode:localidades_censales&",
#   "outputFormat=application/json&CQL_FILTER=cpr=%2758%27"
# ))
# metro_neuquen <- localidades |>
#   filter(nam %in% c("Neuquén", "Plottier", "Centenario", "Vista Alegre Sur"))
# confluencia_urbano <- confluencia_ind |>
#   st_join(st_transform(metro_neuquen, st_crs(confluencia_ind)), left = FALSE)

# ------------------------------------------------------------------------------
# PASO 4. Función de mapa (reutilizable para todas las variables)
# ------------------------------------------------------------------------------
# Mismo criterio estético usado en Montería/Mérida:
# - sin líneas de borde entre radios (colour = NA)
# - clasificación por cuantiles (6 grupos)
# - paleta GnBu
# - NA en blanco absoluto
# - recorte a la ventana del conglomerado metropolitano (bbox_metro)
mapa_variable <- function(datos, var, titulo, leyenda, archivo,
                          subtitulo = "Censo Nacional de Población, Hogares y Viviendas 2022, INDEC",
                          bbox = bbox_metro) {
  
  vals <- datos[[var]]
  vals_validos <- vals[!is.na(vals)]
  cortes <- unique(quantile(vals_validos, probs = seq(0, 1, length.out = 7), na.rm = TRUE))
  
  datos$clas <- case_when(
    is.na(vals) ~ NA_character_,
    TRUE ~ as.character(cut(vals, breaks = c(-Inf, cortes, Inf),
                            include.lowest = TRUE, dig.lab = 5))
  )
  datos$clas <- factor(datos$clas,
                       levels = levels(cut(cortes, breaks = c(-Inf, cortes, Inf),
                                           include.lowest = TRUE, dig.lab = 5)))
  
  mapa <- ggplot(datos) +
    geom_sf(aes(fill = clas), colour = NA) +
    coord_sf(xlim = c(bbox["xmin"], bbox["xmax"]),
             ylim = c(bbox["ymin"], bbox["ymax"]),
             expand = FALSE) +
    scale_fill_brewer(palette = "GnBu", direction = 1, name = leyenda,
                      na.value = "white", na.translate = TRUE) +
    labs(
      title    = titulo,
      subtitle = subtitulo,
      caption  = "Fuente: INDEC. Procesado con Redatam 7, CEPAL/CELADE."
    ) +
    theme_void() +
    theme(legend.title = element_text(size = 9), legend.text = element_text(size = 8))
  
  print(mapa)
  ggsave(file.path(carpeta_salida, archivo), mapa, width = 8, height = 8, dpi = 300, bg = "white")
  invisible(mapa)
}

mapa_variable <- function(datos, var, titulo, leyenda, archivo,
                         subtitulo = "Censo Nacional de Población, Hogares y Viviendas 2022, INDEC",
                         bbox = bbox_metro) {
  
  mapa <- ggplot(datos) +
    geom_sf(aes(fill = .data[[var]]), colour = "grey20") +
    coord_sf(xlim = c(bbox["xmin"], bbox["xmax"]),
             ylim = c(bbox["ymin"], bbox["ymax"]),
             expand = FALSE) +
    scale_fill_distiller(palette = "GnBu", direction = 1, name = leyenda,
                         na.value = "white") +
    labs(
      title    = titulo,
      subtitle = subtitulo,
      caption  = "Fuente: INDEC. Procesado con Redatam 7, CEPAL/CELADE."
    ) +
    theme_void() +
    theme(legend.title = element_text(size = 9), legend.text = element_text(size = 8))
  
  print(mapa)
  ggsave(file.path(carpeta_salida, archivo), mapa, width = 8, height = 8, dpi = 300, bg = "white")
  invisible(mapa)
}
# ------------------------------------------------------------------------------
# PASO 5. Generar los 7 mapas (ya recortados al conglomerado metropolitano)
# ------------------------------------------------------------------------------
mapa_variable(confluencia_urbano, "indice_envejecimiento",
              "Índice de envejecimiento — Confluencia, Neuquén",
              "Índice", "01_indice_envejecimiento.png")

mapa_variable(confluencia_urbano, "pct_pob_65_mas",
              "Población de 65 años y más (%) — Confluencia, Neuquén",
              "% pob. 65+", "02_pct_poblacion_65_mas.png")

mapa_variable(confluencia_urbano, "pct_pob_80_mas",
              "Población de 80 años y más (%) — Confluencia, Neuquén",
              "% pob. 80+", "03_pct_poblacion_80_mas.png")

mapa_variable(confluencia_urbano, "tasa_actividad",
              "Tasa de actividad — Confluencia, Neuquén",
              "Tasa (%)", "04_tasa_actividad.png")

mapa_variable(confluencia_urbano, "tasa_empleo",
              "Tasa de empleo — Confluencia, Neuquén",
              "Tasa (%)", "05_tasa_empleo.png")

mapa_variable(confluencia_urbano, "pct_sin_agua_red",
              "Hogares sin agua por red pública (%) — Confluencia, Neuquén",
              "% sin agua", "06_pct_sin_agua_red.png")

mapa_variable(confluencia_urbano, "pct_desague_sin_red",
              "Hogares con desagüe no conectado a la red pública (%) — Confluencia, Neuquén",
              "% sin desagüe", "07_pct_desague_sin_red.png")


mapa_variable(confluencia_urbano, "densidad_poblacional",
              "Densidad poblacional — Confluencia, Neuquén",
              "Hab./km²", "08_densidad_poblacional.png")

mapa_variable(confluencia_urbano, "nbi_pct",
              "% de NBI — Confluencia, Neuquén",
              "08_densidad_poblacional.png")

# ------------------------------------------------------------------------------
# PASO 6. Guardar la base final (para usarla en otros análisis del paper)
# ------------------------------------------------------------------------------
st_write(
  confluencia_urbano,
  file.path(carpeta_salida, "confluencia_neuquen_indicadores.gpkg"),
  delete_dsn = TRUE, quiet = TRUE
)

write.csv(
  st_drop_geometry(confluencia_urbano),
  file.path(carpeta_salida, "confluencia_neuquen_indicadores.csv"),
  row.names = FALSE
)

message("Listo. Mapas y bases guardados en: ", carpeta_salida)
