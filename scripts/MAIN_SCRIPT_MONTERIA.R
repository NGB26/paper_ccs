# ==============================================================================
# MONTERÍA — Población por manzana a partir del MGN 2018 (DANE)
# ==============================================================================
#
# QUÉ HACE ESTE SCRIPT:
#   1. Importa el shapefile de manzanas del Marco Geoestadístico Nacional (MGN)
#      2018, integrado con el Censo Nacional de Población y Vivienda (CNPV) 2018.
#   2. Filtra solamente Montería (código DANE 23001).
#   3. Mapea la ciudad.
#   4. Mapea la población total y la población de 65 años y más por manzana.
#
# NO HACE FALTA UN MERGE: el shapefile del DANE ya trae las variables del censo
# incorporadas en su propia tabla de atributos (el .dbf). Lo confirma el
# diccionario de datos oficial que compartiste
# (Diccionario_Datos_Niveles_Variables_MGN_CNPV2018Int.xlsx, hoja "Manzana").
#
# CAMPOS DEL .dbf QUE USAMOS (nivel manzana):
#   TP27_PERSO   -> Población total de la manzana
#   TP34_7_EDA   -> Personas entre 60 y 69 años
#   TP34_8_EDA   -> Personas entre 70 y 79 años
#   TP34_9_EDA   -> Personas de 80 años y más
#   CLAS_CCDGO   -> 1 = cabecera municipal (la "ciudad"), 2 = centro poblado,
#                   3 = rural disperso
#
# LIMITACIÓN A TENER EN CUENTA:
#   El censo publica la edad en grupos DECENALES (60-69, 70-79, 80+), no en
#   quinquenios. Por lo tanto, "65 años y más" no es un dato directo: se
#   aproxima tomando la MITAD del grupo 60-69 (asumiendo que esas personas se
#   reparten uniformemente entre 60 y 69) más los grupos 70-79 y 80+ completos.
#   Esto es una aproximación estándar cuando no hay datos en quinquenios y debe
#   mencionarse como tal en el paper.
#
# FUENTES:
#   Descarga del shapefile:
#     https://geoportal.dane.gov.co/servicios/descarga-y-metadatos/descarga-mgn-marco-geoestadistico-nacional/
#   Diccionario de variables (documento oficial DANE):
#     https://geoportal.dane.gov.co/descargas/descarga_mgn/instructivousomgn2018integradocnpv.pdf
# ==============================================================================


# ------------------------------------------------------------------------------
# PASO 0. Cargar paquetes necesarios
# ------------------------------------------------------------------------------
paquetes_necesarios <- c("sf", "dplyr", "ggplot2")
paquetes_faltantes  <- paquetes_necesarios[!paquetes_necesarios %in% rownames(installed.packages())]
if (length(paquetes_faltantes) > 0) install.packages(paquetes_faltantes)

library(sf)       # para leer y manipular el shapefile
library(dplyr)    # para filtrar y transformar los datos
library(ggplot2)  # para hacer los mapas

sf::sf_use_s2(FALSE)  # evita errores de geometría en polígonos censales de Colombia


# ------------------------------------------------------------------------------
# PASO 1. Definir rutas y parámetros
# ------------------------------------------------------------------------------

# Carpeta donde está el shapefile (ajustar solo si cambia de ubicación)
carpeta_shp <- "C:/Users/NICOLASGA/OneDrive - Inter-American Development Bank Group/General - SCL_SPH_SPH_CAR/Productos de conocimiento/Paper CCS/Monteria/SHP_MGN2018_INTGRD_MANZ"
archivo_shp <- file.path(carpeta_shp, "MGN_ANM_MANZANA.shp")

# Carpeta donde se guardarán los mapas y la base final
carpeta_salida <- "C:/Users/NICOLASGA/OneDrive - Inter-American Development Bank Group/General - SCL_SPH_SPH_CAR/Productos de conocimiento/Paper CCS/Monteria/Outputs"
dir.create(carpeta_salida, showWarnings = FALSE, recursive = TRUE)

# Código DANE del municipio: Montería, Córdoba
codigo_monteria <- "23001"

# NOTA sobre los archivos del shapefile:
# Un shapefile son en realidad varios archivos (.shp, .shx, .dbf, .prj, ...)
# que deben tener el MISMO nombre y estar en la MISMA carpeta.
# NO hace falta cargar el .dbf por separado: al leer el .shp con st_read(),
# R toma automáticamente los archivos que lo acompañan (incluido el .dbf,
# que es justamente donde están las variables de población).
stopifnot(file.exists(archivo_shp))


# ------------------------------------------------------------------------------
# PASO 2. Importar el shapefile, filtrando Montería directamente en la lectura
# ------------------------------------------------------------------------------
# El archivo nacional tiene ~505.000 manzanas (~380 MB). En vez de cargar todo
# el país y filtrar después, usamos una consulta SQL en la lectura para traer
# a R únicamente las manzanas de Montería. Es más rápido y usa menos memoria.

consulta_sql <- sprintf(
  "SELECT * FROM MGN_ANM_MANZANA WHERE MPIO_CDPMP = '%s'",
  codigo_monteria
)

manzanas <- sf::st_read(
  dsn     = archivo_shp,
  query   = consulta_sql,
  options = "ENCODING=LATIN1"   # para que tildes y "ñ" se lean bien
)

cat("Manzanas de Montería importadas:", nrow(manzanas), "\n")
stopifnot(nrow(manzanas) > 0)

# Nos quedamos solo con la cabecera urbana (la "ciudad" de Montería).
# CLAS_CCDGO: 1 = cabecera municipal | 2 = centro poblado | 3 = rural disperso
manzanas_ciudad <- dplyr::filter(manzanas, CLAS_CCDGO == "1")
cat("Manzanas en la zona urbana (ciudad):", nrow(manzanas_ciudad), "\n")


# ------------------------------------------------------------------------------
# PASO 3. Mapa base de la ciudad
# ------------------------------------------------------------------------------
mapa_ciudad <- ggplot(manzanas_ciudad) +
  geom_sf(fill = "grey85", colour = "white", linewidth = 0.05) +
  labs(
    title    = "Montería — manzanas censales (zona urbana)",
    subtitle = paste(nrow(manzanas_ciudad), "manzanas · MGN 2018"),
    caption  = "Fuente: DANE, Marco Geoestadístico Nacional 2018"
  ) +
  theme_void()

print(mapa_ciudad)

ggsave(
  filename = file.path(carpeta_salida, "01_mapa_monteria.png"),
  plot = mapa_ciudad, width = 8, height = 8, dpi = 300, bg = "white"
)


# ------------------------------------------------------------------------------
# PASO 4. Calcular la población total y la población de 65 años y más
# ------------------------------------------------------------------------------
# Las variables de población ya están en el shapefile (columnas TP27_PERSO,
# TP34_7_EDA, TP34_8_EDA, TP34_9_EDA), así que solo hay que sumarlas y limpiar
# valores faltantes (NA -> 0, manzanas sin datos por anonimización estadística).

manzanas_ciudad <- manzanas_ciudad |>
  dplyr::mutate(
    poblacion_total = TP27_PERSO,   # se deja NA si el dato no existe (no se fuerza a 0)
    
    # Aproximación de 65+ a partir de los grupos decenales del censo:
    #   - mitad del grupo 60-69 (se asume repartida uniformemente)
    #   - grupo 70-79 completo
    #   - grupo 80+ completo
    # Si falta cualquiera de los tres grupos, el resultado queda NA
    # (no se asume 0 para una manzana de la que no hay información).
    poblacion_65_mas = 0.5 * TP34_7_EDA + TP34_8_EDA + TP34_9_EDA
  )

cat("\n--- Totales de control para Montería (zona urbana) ---\n")
cat("Población total:      ", format(sum(manzanas_ciudad$poblacion_total), big.mark = "."), "\n")
cat("Población 65 y más:   ", format(round(sum(manzanas_ciudad$poblacion_65_mas)), big.mark = "."), "\n")
cat("Porcentaje 65+:       ",
    round(100 * sum(manzanas_ciudad$poblacion_65_mas) / sum(manzanas_ciudad$poblacion_total), 1),
    "%\n")


# ------------------------------------------------------------------------------
# PASO 5. Mapa de población total por manzana
# ------------------------------------------------------------------------------
# Se usan cuantiles (en vez de una escala lineal continua) porque la población
# por manzana está muy concentrada en valores bajos con pocas manzanas muy
# pobladas; una escala lineal dejaría casi todo el mapa de un solo color.


mapa_poblacion_total <- ggplot(manzanas_ciudad) +
  geom_sf(aes(fill = poblacion_total), colour = "grey70", linewidth = 0.05) +
  scale_fill_viridis_c(
    option = "mako", direction = -1, name = "Personas",
    trans = "sqrt",
    na.value = "white"   # las manzanas sin dato (NA) quedan en blanco
  ) +  labs(
    title    = "Población total por manzana — Montería",
    subtitle = "Censo Nacional de Población y Vivienda (CNPV) 2018",
    caption  = "Fuente: DANE, MGN 2018 integrado con CNPV 2018"
  ) +
  theme_void()


print(mapa_poblacion_total)

ggsave(
  filename = file.path(carpeta_salida, "02_poblacion_total.png"),
  plot = mapa_poblacion_total, width = 8, height = 8, dpi = 300, bg = "white"
)




# --- Clasificación por cuantiles (6 grupos, solo manzanas con población > 0) ---
cortes_poblacion <- manzanas_ciudad |>
  dplyr::filter(!is.na(poblacion_total), poblacion_total > 0) |>
  dplyr::pull(poblacion_total) |>
  quantile(probs = seq(0, 1, length.out = 7), na.rm = TRUE) |>
  unique()

manzanas_ciudad <- manzanas_ciudad |>
  dplyr::mutate(
    poblacion_total_clas = dplyr::case_when(
      is.na(poblacion_total)   ~ NA_character_,
      poblacion_total == 0     ~ NA_character_,   # 0 se trata igual que sin dato -> blanco
      TRUE ~ as.character(
        cut(poblacion_total, breaks = c(-Inf, cortes_poblacion, Inf),
            include.lowest = TRUE, dig.lab = 5)
      )
    ),
    poblacion_total_clas = factor(
      poblacion_total_clas,
      levels = levels(cut(cortes_poblacion, breaks = c(-Inf, cortes_poblacion, Inf),
                          include.lowest = TRUE, dig.lab = 5))
    )
  )

# --- Mapa ---------------------------------------------------------------------
mapa_poblacion_total <- ggplot(manzanas_ciudad) +
  geom_sf(aes(fill = poblacion_total_clas), colour = NA) +   # sin borde
  scale_fill_brewer(
    palette      = "GnBu",
    direction    = 1,
    name         = "Personas",
    na.value     = "white",
    na.translate = TRUE,
    labels       = function(x) x
  ) +
  labs(
    title    = "Población total por manzana — Montería",
    subtitle = "Censo Nacional de Población y Vivienda (CNPV) 2018",
    caption  = "Fuente: DANE, MGN 2018 integrado con CNPV 2018"
  ) +
  theme_void() +
  theme(legend.title = element_text(size = 9),
        legend.text  = element_text(size = 8))

print(mapa_poblacion_total)

ggsave(
  filename = file.path(carpeta_salida, "02_poblacion_total.png"),
  plot = mapa_poblacion_total, width = 8, height = 8, dpi = 300, bg = "white"
)


# --- Clasificación por cuantiles (6 grupos, solo manzanas con 65+ > 0) ---------
cortes_65 <- manzanas_ciudad |>
  dplyr::filter(!is.na(poblacion_65_mas), poblacion_65_mas > 0) |>
  dplyr::pull(poblacion_65_mas) |>
  quantile(probs = seq(0, 1, length.out = 7), na.rm = TRUE) |>
  unique()

manzanas_ciudad <- manzanas_ciudad |>
  dplyr::mutate(
    poblacion_65_clas = dplyr::case_when(
      is.na(poblacion_65_mas)  ~ NA_character_,
      poblacion_65_mas == 0    ~ NA_character_,   # 0 se trata igual que sin dato -> blanco
      TRUE ~ as.character(
        cut(poblacion_65_mas, breaks = c(-Inf, cortes_65, Inf),
            include.lowest = TRUE, dig.lab = 5)
      )
    ),
    poblacion_65_clas = factor(
      poblacion_65_clas,
      levels = levels(cut(cortes_65, breaks = c(-Inf, cortes_65, Inf),
                          include.lowest = TRUE, dig.lab = 5))
    )
  )



# --- Clasificación por cuantiles (6 grupos, solo manzanas con 65+ > 0) ---------
cortes_65 <- manzanas_ciudad |>
  dplyr::filter(!is.na(poblacion_65_mas), poblacion_65_mas > 0) |>
  dplyr::pull(poblacion_65_mas) |>
  quantile(probs = seq(0, 1, length.out = 7), na.rm = TRUE) |>
  unique()

manzanas_ciudad <- manzanas_ciudad |>
  dplyr::mutate(
    poblacion_65_clas = dplyr::case_when(
      is.na(poblacion_65_mas)  ~ NA_character_,
      poblacion_65_mas == 0    ~ NA_character_,
      TRUE ~ as.character(
        cut(poblacion_65_mas, breaks = c(-Inf, cortes_65, Inf),
            include.lowest = TRUE, dig.lab = 5)
      )
    ),
    poblacion_65_clas = factor(
      poblacion_65_clas,
      levels = levels(cut(cortes_65, breaks = c(-Inf, cortes_65, Inf),
                          include.lowest = TRUE, dig.lab = 5))
    )
  )

# --- Mapa -----------------------------------------------------------------------
mapa_poblacion_65 <- ggplot(manzanas_ciudad) +
  geom_sf(aes(fill = poblacion_65_clas), colour = NA) +
  scale_fill_brewer(
    palette      = "GnBu",
    direction    = 1,
    name         = "Personas 65+",
    na.value     = "white",
    na.translate = TRUE,
    labels       = function(x) x
  ) +
  labs(
    title    = "Población de 65 años y más por manzana — Montería",
    subtitle = "Aproximación: 1/2 del grupo 60-69 + grupo 70-79 + grupo 80 y más (CNPV 2018)",
    caption  = "Fuente: DANE, MGN 2018 integrado con CNPV 2018"
  ) +
  theme_void() +
  theme(legend.title = element_text(size = 9),
        legend.text  = element_text(size = 8))

print(mapa_poblacion_65)


# ------------------------------------------------------------------------------
# Variable: % de viviendas con energía eléctrica por manzana
# ------------------------------------------------------------------------------
manzanas_ciudad <- manzanas_ciudad |>
  dplyr::mutate(
    pct_energia = dplyr::case_when(
      is.na(TVIVIENDA) | TVIVIENDA == 0 ~ NA_real_,   # sin viviendas -> sin dato (blanco)
      TRUE ~ 100 * TP19_EE_1 / TVIVIENDA
    )
  )

cat("Manzanas sin dato de energía (sin viviendas):",
    sum(is.na(manzanas_ciudad$pct_energia)), "\n")
cat("Cobertura eléctrica promedio (ciudad):",
    round(mean(manzanas_ciudad$pct_energia, na.rm = TRUE), 1), "%\n")

# --- Clasificación por cuantiles (6 grupos, sobre manzanas con dato) -----------
cortes_energia <- manzanas_ciudad |>
  dplyr::filter(!is.na(pct_energia)) |>
  dplyr::pull(pct_energia) |>
  quantile(probs = seq(0, 1, length.out = 7), na.rm = TRUE) |>
  unique()

manzanas_ciudad <- manzanas_ciudad |>
  dplyr::mutate(
    pct_energia_clas = dplyr::case_when(
      is.na(pct_energia) ~ NA_character_,
      TRUE ~ as.character(
        cut(pct_energia, breaks = c(-Inf, cortes_energia, Inf),
            include.lowest = TRUE, dig.lab = 5)
      )
    ),
    pct_energia_clas = factor(
      pct_energia_clas,
      levels = levels(cut(cortes_energia, breaks = c(-Inf, cortes_energia, Inf),
                          include.lowest = TRUE, dig.lab = 5))
    )
  )

# --- Mapa -----------------------------------------------------------------------
mapa_energia <- ggplot(manzanas_ciudad) +
  geom_sf(aes(fill = pct_energia_clas), colour = NA) +
  scale_fill_brewer(
    palette      = "GnBu",
    direction    = 1,      # claro = baja cobertura, oscuro = alta cobertura
    name         = "% con energía",
    na.value     = "white",
    na.translate = TRUE,
    labels       = function(x) x
  ) +
  labs(
    title    = "Cobertura de energía eléctrica por manzana — Montería",
    subtitle = "Porcentaje de viviendas con servicio de energía eléctrica (CNPV 2018)",
    caption  = "Fuente: DANE, MGN 2018 integrado con CNPV 2018"
  ) +
  theme_void() +
  theme(legend.title = element_text(size = 9),
        legend.text  = element_text(size = 8))

print(mapa_energia)



# ------------------------------------------------------------------------------
# Variable: % de viviendas con servicio de acueducto por manzana
# ------------------------------------------------------------------------------
manzanas_ciudad <- manzanas_ciudad |>
  dplyr::mutate(
    pct_acueducto = dplyr::case_when(
      is.na(TVIVIENDA) | TVIVIENDA == 0 ~ NA_real_,   # sin viviendas -> sin dato (blanco)
      TRUE ~ 100 * TP19_ACU_1 / TVIVIENDA
    )
  )

cat("Manzanas sin dato de acueducto (sin viviendas):",
    sum(is.na(manzanas_ciudad$pct_acueducto)), "\n")
cat("Cobertura de acueducto promedio (ciudad):",
    round(mean(manzanas_ciudad$pct_acueducto, na.rm = TRUE), 1), "%\n")

# --- Clasificación por cuantiles (6 grupos, sobre manzanas con dato) -----------
cortes_acueducto <- manzanas_ciudad |>
  dplyr::filter(!is.na(pct_acueducto)) |>
  dplyr::pull(pct_acueducto) |>
  quantile(probs = seq(0, 1, length.out = 7), na.rm = TRUE) |>
  unique()

manzanas_ciudad <- manzanas_ciudad |>
  dplyr::mutate(
    pct_acueducto_clas = dplyr::case_when(
      is.na(pct_acueducto) ~ NA_character_,
      TRUE ~ as.character(
        cut(pct_acueducto, breaks = c(-Inf, cortes_acueducto, Inf),
            include.lowest = TRUE, dig.lab = 5)
      )
    ),
    pct_acueducto_clas = factor(
      pct_acueducto_clas,
      levels = levels(cut(cortes_acueducto, breaks = c(-Inf, cortes_acueducto, Inf),
                          include.lowest = TRUE, dig.lab = 5))
    )
  )

# --- Mapa -----------------------------------------------------------------------
mapa_acueducto <- ggplot(manzanas_ciudad) +
  geom_sf(aes(fill = pct_acueducto_clas), colour = NA) +
  scale_fill_brewer(
    palette      = "GnBu",
    direction    = 1,      # claro = baja cobertura, oscuro = alta cobertura
    name         = "% con acueducto",
    na.value     = "white",
    na.translate = TRUE,
    labels       = function(x) x
  ) +
  labs(
    title    = "Cobertura de acueducto por manzana — Montería",
    subtitle = "Porcentaje de viviendas con servicio de acueducto (CNPV 2018)",
    caption  = "Fuente: DANE, MGN 2018 integrado con CNPV 2018"
  ) +
  theme_void() +
  theme(legend.title = element_text(size = 9),
        legend.text  = element_text(size = 8))

print(mapa_acueducto)


manzanas_ciudad=manzanas_ciudad%>%rename(geometry= `_ogr_geometry_`)

# ------------------------------------------------------------------------------
# Base de datos final para el paper (CSV con geometría en formato WKT)
# ------------------------------------------------------------------------------
base_final <- manzanas_ciudad |>
  dplyr::mutate(
    codigo_manzana   = COD_DANE_A,
    geometria_wkt    = sf::st_as_text(geometry),   # geometría como texto (WKT)
    poblacion_total  = poblacion_total,
    poblacion_65_mas = poblacion_65_mas,
    pct_acueducto    = pct_acueducto,
    pct_energia      = pct_energia
  ) |>
  sf::st_drop_geometry() |>   # ya guardamos la geometría como texto arriba
  dplyr::select(
    codigo_manzana, geometria_wkt,
    poblacion_total, poblacion_65_mas,
    pct_acueducto, pct_energia
  )

cat("Filas en la base final:", nrow(base_final), "\n")
head(base_final)

write.csv(
  base_final,
  file.path(carpeta_salida, "monteria_base_manzanas.csv"),
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

# ------------------------------------------------------------------------------
# Guardar la misma base como Shapefile (.shp)
# ------------------------------------------------------------------------------
base_shp <- manzanas_ciudad |>
  dplyr::transmute(
    cod_manz  = COD_DANE_A,      # código de manzana
    pob_tot   = poblacion_total, # población total
    pob_65    = poblacion_65_mas,# población 65+
    pct_acu   = pct_acueducto,   # % viviendas con acueducto
    pct_ener  = pct_energia,     # % viviendas con energía eléctrica
    geometry  = geometry
  )

# Todos los nombres anteriores ya tienen 10 caracteres o menos,
# para que el shapefile no los trunque de forma imprevisible.

sf::st_write(
  base_shp,
  file.path(carpeta_salida, "monteria_base_manzanas.shp"),
  delete_dsn = TRUE,   # sobreescribe si ya existe
  quiet = FALSE
)



