# Convert hexa object to shape file.
hexa_sf <-
  hex_list[[2]]@sf |> 
  sf::st_transform(crs = 4326) # Transform (WGS 84/Equal Earth Greenwich).

# Add occurrence data to hexa_sf object.
hexa_sf <-
  hexa_sf |> 
  left_join(campanian_cells |> group_by(cell) |> summarise(cam_genera = n_distinct(Genus)),by = c("faces" = "cell")) |> 
  left_join(maastrich_cells |> group_by(cell) |> summarise(mas_genera = n_distinct(Genus)),by = c("faces" = "cell")) |> 
  left_join(daniansel_cells |> group_by(cell) |> summarise(dan_genera = n_distinct(Genus)),by = c("faces" = "cell")) |> 
  left_join(thanetian_cells |> group_by(cell) |> summarise(tha_genera = n_distinct(Genus)),by = c("faces" = "cell"))

# Define boundaries of hexagrid to pass to raster layer.
bbox <- sf::st_bbox(hexa_sf)

xmin <- bbox["xmin"]
xmax <- bbox["xmax"]
ymin <- bbox["ymin"]
ymax <- bbox["ymax"]

# Function.

# ato <- function(hexa_sf, age_cells, age_name) {
#   unique_age <- 
#     age_cells |> 
#     group_by(cell) |> summarise(X = n_distinct(Genus))
#     # select(cell,occs) |> 
#     # distinct(cell, .keep_all = TRUE)   # keeps the first occurrences value per cell.
#   
#   hexa_sf_updated <- 
#     hexa_sf |> 
#     left_join(unique_age |> select(cell, X),by = c("faces" = "cell"))
#   
#   colnames(hexa_sf_updated)[colnames(hexa_sf_updated) == "X"] <- age_name
#   return(hexa_sf_updated)
# }

# hexa_sf <- ato(hexa_sf, maastrich_cells, "Maastrichtian")
# hexa_sf <- ato(hexa_sf, daniansel_cells, "dani_occs")
# hexa_sf <- ato(hexa_sf, thanetian_cells, "than_occs")

# Read in map files and convert to rasters 
camp_80Ma_rast <-
  readJPEG("_maps/Map19a LtK Early Campanian_080_light.jpg") |> 
  rasterGrob(width = unit(1,"npc"), height = unit(1,"npc"))

maas_70Ma_rast <- 
  readJPEG("_maps/Map17a LtK Maastrichtian_070_light.jpg") |> 
  rasterGrob(width = unit(1,"npc"), height = unit(1,"npc"))

dani_60Ma_rast <- 
  readJPEG("_maps/Map15a Paleocene_060_light.jpg") |> 
  rasterGrob(width = unit(1,"npc"), height = unit(1,"npc"))

than_55Ma_rast <- 
  readJPEG("_maps/Map14a PETM_055_light.jpg") |> 
  rasterGrob(width = unit(1,"npc"), height = unit(1,"npc"))

# Common gradient legend.

# all_vals <- c(
#   cmp_hexa$n_genera,
#   mas_hexa$n_genera,
#   dan_hexa$n_genera,
#   tha_hexa$n_genera
# )
# 
# fill_limits <- range(all_vals, na.rm = TRUE)

# Campanian.
camp_map <-
  ggplot() +
  # Apply hexagrid bounding box to rasterized image.
  annotation_custom(camp_80Ma_rast,
                    xmin = bbox["xmin"], xmax = bbox["xmax"],
                    ymin = bbox["ymin"], ymax = bbox["ymax"]) +
  # Plot time slice occurrences saved to hexa_sf.
  geom_sf(data = hexa_sf |> filter(!is.na(cam_genera)), aes(fill = cam_genera), size = 0.01, color = "black") +
  # scale_fill_distiller(palette = "Spectral",values = scales::rescale(c(min(fill_limits), 17, max(fill_limits))),limits = fill_limits) +
  # scale_fill_viridis_c(option = "E",name = "Genera",limits = fill_limits,oob = scales::squish) +
  # scale_fill_viridis_c(option = "magma", na.value = "black") +
  # scale_fill_gradientn(
  #   colours = rev(c("#B83A4B", "white","#F59E0B")),
  #   values = scales::rescale(c(min(fill_limits), 17, max(fill_limits))),
  #   name = "Genera",
  #   limits = fill_limits,
  #   oob = scales::squish) +
  # Add text labels to select grid cells.
  geom_sf_text(data = hexa_sf |> filter(!is.na(cam_genera)),
               aes(label = faces),
               size = 2, color = "beige",
               na.rm = TRUE) +
  # Coordinate system.
  coord_sf(xlim = c(-180, 180), ylim = c(-90, 90), expand = FALSE) +
  labs(x = "Longitude", y = "Latitude") +
  theme(axis.text.y = element_text(angle = 90,hjust = .5,vjust = .5)) +
  theme(aspect.ratio = .5) |> 
  suppressWarnings()

# Maastrichtian.
maas_map <- 
  ggplot() +
  # Apply hexagrid bounding box to rasterized image.
  annotation_custom(maas_70Ma_rast,
                    xmin = bbox["xmin"], xmax = bbox["xmax"],
                    ymin = bbox["ymin"], ymax = bbox["ymax"]) +
  # Plot time slice occurrences saved to hexa_sf.
  geom_sf(data = hexa_sf |> filter(!is.na(mas_genera)), aes(fill = mas_genera), size = 0.01, color = "black") +
  # scale_fill_viridis_c(option = "magma",name = "Genera",limits = fill_limits,oob = scales::squish) +
  # scale_fill_distiller(palette = "Spectral",values = scales::rescale(c(min(fill_limits), 17, max(fill_limits))),limits = fill_limits) +
  # scale_fill_gradientn(
  #   colours = rev(c("#B83A4B", "white","#F59E0B")),
  #   values = scales::rescale(c(min(fill_limits), 17, max(fill_limits))),
  #   name = "Genera",
  #   limits = fill_limits,
  #   oob = scales::squish) +
  # Add text labels to select grid cells.
  geom_sf_text(data = hexa_sf |> filter(!is.na(mas_genera)),
               aes(label = faces),
               size = 2, color = "beige",
               na.rm = TRUE) +
  # geom_curve(data = links_sf,
  #            aes(x = x1, y = y1,xend = x2, yend = y2,color = value),
  #            curvature = 0.25,
  #            linewidth = 1,
  #            alpha = 0.8) +  
  # scale_color_gradient(low = "#8B008B", high = "white") +
  # scale_color_viridis_c(name = "value") +
  # geom_hline(yintercept = 0, color = "white") +
  # Coordinate system.
  coord_sf(xlim = c(-180, 180), ylim = c(-90, 90), expand = FALSE) +
  labs(x = "Longitude", y = "Latitude") +
  theme(axis.text.y = element_text(angle = 90,hjust = .5,vjust = .5)) +
  theme(aspect.ratio = .5) |> 
  suppressWarnings()

# Danian-Selandian.
dani_map <- 
  ggplot() +
  annotation_custom(dani_60Ma_rast,
                    xmin = bbox["xmin"], xmax = bbox["xmax"],
                    ymin = bbox["ymin"], ymax = bbox["ymax"]) +
  # Plot time slice occurrences saved to hexa_sf.
  # geom_sf(data = hexa_sf |> filter(!is.na(dan_genera)) |> filter(faces %in% shared_cells_kpg), aes(fill = dan_genera), size = 0.01) +
  geom_sf(data = hexa_sf |> filter(!is.na(dan_genera)), aes(fill = dan_genera), size = 0.01, color = "black") +
  # scale_fill_distiller(palette = "Spectral",values = scales::rescale(c(min(fill_limits), 17, max(fill_limits))),limits = fill_limits) +
  # scale_fill_viridis_c(option = "magma",name = "Genera",limits = fill_limits,oob = scales::squish) +
  # scale_fill_gradient(low =  "black",
  #                     high = "yellow",
  #                     name = "Genera",
  #                     na.value = scales::alpha("white", 0)) +
  # scale_fill_gradientn(
  #   colours = rev(c("#B83A4B", "#FFFFFF", "#2F2424")),
  #   values = scales::rescale(c(min(fill_limits), 17, max(fill_limits))),
  #   name = "Genera",
  #   limits = fill_limits,
  #   oob = scales::squish) +
  # Add text labels to select grid cells.
  geom_sf_text(data = hexa_sf |> filter(!is.na(dan_genera)),
               aes(label = faces),
               size = 2, color = "beige",
               na.rm = TRUE) +
  # geom_curve(data = links_sf,
  #            aes(x = x1, y = y1,xend = x2, yend = y2,color = value),
  #            curvature = 0.25,
  #            linewidth = 1,
  #            alpha = 0.8) +  
  # scale_color_gradient(low = "#8B008B", high = "white") +
  # Coordinate system.
  coord_sf(xlim = c(-180, 180), ylim = c(-90, 90), expand = FALSE) +
  labs(x = "Longitude", y = "Latitude") +
  theme(axis.text.y = element_text(angle = 90,hjust = .5,vjust = .5)) +
  theme(aspect.ratio = .5) |> 
  suppressWarnings()

# Thanetian.
than_map <- 
  ggplot() +
  annotation_custom(than_55Ma_rast,
                    xmin = bbox["xmin"], xmax = bbox["xmax"],
                    ymin = bbox["ymin"], ymax = bbox["ymax"]) +
  # Plot time slice occurrences saved to hexa_sf.
  geom_sf(data = hexa_sf |> filter(!is.na(tha_genera)), aes(fill = tha_genera), size = 0.01, color = "black") +
  # scale_fill_distiller(palette = "Spectral",values = scales::rescale(c(min(fill_limits), 17, max(fill_limits))),limits = fill_limits) +
  # scale_fill_gradientn(
  #   colours = rev(c("#B83A4B", "#FFFFFF", "#2F2424")),
  #   values = scales::rescale(c(min(fill_limits), 17, max(fill_limits))),
  #   name = "Genera",
  #   limits = fill_limits,
  #   oob = scales::squish) +
  # scale_fill_gradient(low =  "black",
  #                     high = "#C85A85",
  #                     name = "Genera",,limits = fill_limits,oob = scales::squish,
  #                     na.value = scales::alpha("white", 0)) +
  # scale_fill_viridis_c(option = "magma",name = "Genera",limits = fill_limits,oob = scales::squish) +
  # scale_fill_gradient(low =  "#F4E3D7",
  #                     high = "#C85A85",
  #                     name = "Occurrences",
  #                     na.value = scales::alpha("white", 0)) +
  # Add text labels to select grid cells.
  # geom_sf_text(data = hexa_sf |> filter(!is.na(tha_genera)),
  #              aes(label = faces),
  #              size = 2, color = "white", 
  #              na.rm = TRUE) +
  # Coordinate system.
  coord_sf(xlim = c(-180, 180), ylim = c(-90, 90), expand = FALSE) +
  labs(x = "Longitude", y = "Latitude") +
  theme(axis.text.y = element_text(angle = 90,hjust = .5,vjust = .5)) +
  theme(aspect.ratio = .5) |> 
  suppressWarnings()

ggarrange(than_map,dani_map,maas_map,camp_map,nrow = 4, ncol = 1,
          align = "hv",
          common.legend = T,
          legend = "right")

# Connecting lines Maastrichtian and Danian
sf::sf_use_s2(FALSE)

# Can pie-charts showing number of formations/biozones each cell includes
centroids <- 
  hexa_sf |> 
  st_centroid() |> 
  mutate(face_id = faces) |> 
  select(face_id, geometry)

links_sf <- links %>%
  left_join(centroids, by = c("cell_x" = "face_id")) %>%
  rename(geom_x = geometry) %>%
  left_join(centroids, by = c("cell_y" = "face_id")) %>%
  rename(geom_y = geometry)

links_sf <- links_sf %>%
  mutate(
    x1 = st_coordinates(geom_x)[,1],
    y1 = st_coordinates(geom_x)[,2],
    x2 = st_coordinates(geom_y)[,1],
    y2 = st_coordinates(geom_y)[,2]
  )

fill_limits <- range(c(0.6363636, 1.0000000), na.rm = TRUE)

## Connecting lines
maas_shared_cell_map <- 
  ggplot() +
  # Apply hexagrid bounding box to rasterized image.
  annotation_custom(maas_70Ma_rast,
                    xmin = bbox["xmin"], xmax = bbox["xmax"],
                    ymin = bbox["ymin"], ymax = bbox["ymax"]) +
  # Plot time slice occurrences saved to hexa_sf.
  geom_sf(data = hexa_sf |> filter(!is.na(mas_genera)) |> filter(faces %in% shared_cells_kpg),
          aes(fill = mas_genera), size = 0.01, color = "black",show.legend = F) +
  # scale_fill_viridis_c(option = "magma",name = "Genera",limits = fill_limits,oob = scales::squish) +
  # scale_fill_distiller(palette = "Spectral",values = scales::rescale(c(min(fill_limits), 17, max(fill_limits))),limits = fill_limits) +
  # scale_fill_gradientn(
  #   colours = rev(c("#B83A4B", "white","#F59E0B")),
  #   values = scales::rescale(c(min(fill_limits), 17, max(fill_limits))),
  #   name = "Genera",
  #   limits = fill_limits,
  #   oob = scales::squish) +
  # Add text labels to select grid cells.
  geom_sf_text(data = hexa_sf |> filter(!is.na(mas_genera)) |> filter(faces %in% shared_cells_kpg),
               aes(label = faces),
               size = 2, color = "white",
               na.rm = TRUE) +
  geom_curve(data = links_sf,
             aes(x = x1, y = y1,xend = x2, yend = y2,color = value),
             curvature = 0.25,
             linewidth = 1,
             alpha = 0.8) +
  # scale_color_gradient(low = "white", high = "#8B008B",
  #                      limits = c(0.6, 1),breaks = seq(0.6, 1, by = 0.1),
  #                      oob = scales::squish) +
  scale_color_viridis_c(name = "value",
                        limits = c(0.6, 1),breaks = seq(0.6, 1, by = 0.1),
                        oob = scales::squish) +
  # geom_hline(yintercept = 0, color = "white") +
  # Coordinate system.
  coord_sf(xlim = c(-80, 25),ylim = c(15, 60)) +
  # coord_sf(xlim = c(-180, 180), ylim = c(-90, 90), expand = FALSE) +
  labs(x = "Longitude", y = "Latitude") +
  theme(axis.text.y = element_text(angle = 90,hjust = .5,vjust = .5)) |> 
  suppressWarnings()

# Danian-Selandian.
dani_shared_cell_map <-
  ggplot() +
  annotation_custom(dani_60Ma_rast,
                    xmin = bbox["xmin"], xmax = bbox["xmax"],
                    ymin = bbox["ymin"], ymax = bbox["ymax"]) +
  geom_sf(data = hexa_sf |> filter(!is.na(dan_genera)) |> filter(faces %in% shared_cells_kpg), 
          aes(fill = dan_genera), 
          size = 0.01, 
          color = "black",show.legend = F) +
  # Add text labels to select grid cells.
  geom_sf_text(data = hexa_sf |> filter(!is.na(dan_genera)) |> filter(faces %in% shared_cells_kpg),
               aes(label = faces),
               size = 2, color = "white",
               na.rm = TRUE) +
  geom_curve(data = links_sf,
             aes(x = x1, y = y1,xend = x2, yend = y2,color = value),
             curvature = 0.25,
             linewidth = 1,
             alpha = 0.8) +
  scale_color_viridis_c(name = "value",
                        limits = c(0.6, 1),breaks = seq(0.6, 1, by = 0.1),
                        oob = scales::squish) +
  # scale_color_gradient(low = "white", high = "#8B008B",
  #                      limits = c(0.6, 1),breaks = seq(0.6, 1, by = 0.1),
  #                      oob = scales::squish) +
  # Coordinate system.
  coord_sf(xlim = c(-80, 25),ylim = c(15, 60)) +
  # coord_sf(xlim = c(-180, 180), ylim = c(-90, 90), expand = FALSE) +
  labs(x = "Longitude", y = "Latitude") +
  theme(axis.text.y = element_text(angle = 90,hjust = .5,vjust = .5)) |> 
  suppressWarnings()

ggarrange(maas_shared_cell_map,dani_shared_cell_map,ncol = 2)
