library(sf)
library(readr)
library(data.table)
library(dplyr)
library(ggplot2)
library(ggspatial)
library(ggrepel)
library(grid)

# -----------------------------
# Paths + CRS
# -----------------------------
PATH_GPS <- "D:/flavia/Downloads/OneDrive_2025-07-10/GPS"
CRS_USED <- 25832  # ETRS89 / UTM32

# -----------------------------
# 1) Read polygons (txt) -> sf polygons
# -----------------------------
dfc_machine <- read_delim(file.path(PATH_GPS, "DFC_machine.txt"), delim = "\t", trim_ws = TRUE) %>%
  mutate(type = "DFC-Machine Plot", polygon_id = "DFC-Machine Plot")

dfc_manual <- read_delim(file.path(PATH_GPS, "DFC_manual.txt"), delim = "\t", trim_ws = TRUE) %>%
  mutate(type = "DFC-Manual Plot", polygon_id = "DFC-Manual Plot")

bls_machine <- read_delim(file.path(PATH_GPS, "Plot1.txt"), delim = "\t", trim_ws = TRUE) %>%
  mutate(type = "Machine Plot", polygon_id = "Machine Plot")

all_polygons <- rbindlist(
  list(as.data.frame(dfc_machine), as.data.frame(dfc_manual), as.data.frame(bls_machine)),
  fill = TRUE
) %>% as.data.frame()

# legend/order
poly_levels <- c("Machine Plot", "DFC-Manual Plot", "DFC-Machine Plot")
all_polygons$type <- factor(all_polygons$type, levels = poly_levels)

# Build sf polygons (vertex order = row order)
poly_split <- split(all_polygons, all_polygons$polygon_id)
poly_geom <- lapply(poly_split, function(d) {
  m <- as.matrix(d[, c("x", "y")])
  if (!all(m[1, ] == m[nrow(m), ])) m <- rbind(m, m[1, ])  # close ring
  st_polygon(list(m))
})

polygons_sf <- st_sf(
  polygon_id = names(poly_geom),
  type       = vapply(poly_split, function(d) as.character(d$type[1]), character(1)),
  geometry   = st_sfc(poly_geom, crs = CRS_USED)
)

polygons_sf$type <- factor(polygons_sf$type, levels = poly_levels)


# -----------------------------
# 2) Read points (txt) -> sf points (USA removed)
# -----------------------------
crds <- read_delim(file.path(PATH_GPS, "CRDS_1.txt"), delim = "\t", trim_ws = TRUE) %>%
  mutate(type = "CRDS-bLS")

crds_bg <- read_delim(file.path(PATH_GPS, "CRDS_1_BG.txt"), delim = "\t", trim_ws = TRUE) %>%
  mutate(type = "CRDS-BG-bLS")

ptrms <- read_delim(file.path(PATH_GPS, "PTRMS_1.txt"), delim = "\t", trim_ws = TRUE) %>%
  mutate(type = "PTR-MS-bLS")

points_df <- rbindlist(
  list(as.data.frame(crds), as.data.frame(crds_bg), as.data.frame(ptrms)),
  fill = TRUE
) %>% as.data.frame()

points_df$label <- points_df$type

points_sf <- st_as_sf(points_df, coords = c("x", "y"), crs = CRS_USED, remove = FALSE)
points_sf <- st_zm(points_sf, drop = TRUE, what = "ZM")

# -----------------------------
# 3) Padding so nothing is clipped + label shift
# -----------------------------
bb_poly <- st_bbox(polygons_sf)
bb_pts  <- st_bbox(points_sf)

xmin <- min(bb_poly["xmin"], bb_pts["xmin"])
xmax <- max(bb_poly["xmax"], bb_pts["xmax"])
ymin <- min(bb_poly["ymin"], bb_pts["ymin"])
ymax <- max(bb_poly["ymax"], bb_pts["ymax"])

dx <- as.numeric(xmax - xmin)
dy <- as.numeric(ymax - ymin)

pad_x <- 0.12 * dx
pad_y <- 0.12 * dy

pt_label_shift <- 0.05 * dx

# -----------------------------
# 4) Styles
# -----------------------------
poly_fill <- c(
  "DFC-Machine Plot" = "#006400",
  "DFC-Manual Plot"  = "#FFC107",
  "Machine Plot"     = "#90EE90"
)

point_colors <- c(
  "CRDS-bLS"   = "#e31a1c",
  "CRDS-BG-bLS"     = "grey60",
  "PTR-MS-bLS" = "#6a3d9a"
)

point_shapes <- c(
  "CRDS-bLS"   = 16,  # filled circle
  "CRDS-BG-bLS"     = 16,  # filled circle
  "PTR-MS-bLS" = 18   # filled diamond
)

# -----------------------------
# 5) Plot (polygon legend only, bottom, no title)
# -----------------------------
p <- ggplot() +
  # polygons (legend ON)
  geom_sf(
    data = polygons_sf, aes(fill = type),
    color = "black", alpha = 0.35, linewidth = 0.6
  ) +
  scale_fill_manual(values = poly_fill, breaks = poly_levels, name = NULL) +
  
  # points (legend OFF)
  geom_sf(
    data = points_sf,
    aes(color = type, shape = type),
    size = 4, stroke = 1,
    show.legend = FALSE
  ) +
  scale_color_manual(values = point_colors) +
  scale_shape_manual(values = point_shapes) +
  
  geom_text_repel(
    data = points_df,
    aes(x = x, y = y, label = label, color = type),
    min.segment.length = 0,
    segment.color = "grey30",
    segment.size = 0.7,
    segment.alpha = 0.9,
    
    box.padding = 0.6,      # more space around label
    point.padding = 0.8,    # more space around point
    force = 3,              # stronger repulsion → longer lines
    
    max.overlaps = Inf,
    size = 4,
    seed = 42,
    show.legend = FALSE
  )+
  
  # true north
  annotation_north_arrow(
    location = "tr",
    which_north = "true",
    style = north_arrow_fancy_orienteering,
    height = unit(1.6, "cm"),
    width  = unit(1.6, "cm"),
    pad_x  = unit(0.2, "cm"),
    pad_y  = unit(0.2, "cm")
  ) +
  
  # scale bar
  annotation_scale(
    location = "bl",
    width_hint = 0.25,
    pad_x = unit(0.25, "cm"),
    pad_y = unit(0.25, "cm")
  ) +
  
  # padded limits
  coord_sf(
    crs = st_crs(CRS_USED),
    xlim = c(xmin - pad_x, xmax + pad_x),
    ylim = c(ymin - pad_y, ymax + pad_y),
    expand = FALSE
  ) +
  
  theme_void(base_size = 12) +
  theme(
    legend.position = "bottom",
    legend.title = element_blank(),
    legend.box = "horizontal",
    legend.text = element_text(size = 12)
  ) +
  guides(
    fill = guide_legend(nrow = 1, byrow = TRUE)
  )

print(p)

# -----------------------------
# 6) Save (recommended)
# -----------------------------
out_png <- file.path(PATH_GPS, "Figure1.png")
ggsave(out_png, p, width = 8, height = 5, bg="white",dpi = 300)



