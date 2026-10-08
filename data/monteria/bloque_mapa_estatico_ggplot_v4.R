
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
