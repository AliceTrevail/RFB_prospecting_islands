#### Create islands shape file for visitation metrics / at-sea suitability areas ####
pacman::p_load(tidyverse, sf, ggspatial)


# 1. read in Pete's colony file for colony lat longs and attributes ####

df_cols <- read_csv("Data/Island_data/CA_Colonies_BRNumbers.csv") %>%
  rowwise() %>%
  dplyr::mutate(TotalAllSp = sum(c_across(Ardenna.pacifica:Gygis.alba)))%>%
  ungroup()%>%
  select(Island, Group, Atoll, lon, lat, Rattus.rattus, Sula.sula, TotalAllSp)

sf_cols <- df_cols %>%
  st_as_sf(., coords=c("lon","lat"), crs=4326)


# 2. read in chagos shape file ####

chagos <- st_read("Data/Chagos_islands_shape_files/Chagos_v6.shp")
chagos$DEPTHLABEL <- fct_relevel(chagos$DEPTHLABEL, "land", "shallow", "variable", "deep")

sf_land <- chagos %>%
  filter(LAND == "1")

# 3. Add colony attribute data to chagos shape file from Pete's dataframe ####

sf_cols_poly_contains <- st_join(sf_land, sf_cols, st_contains, left = T) %>%
  filter(!is.na(Island))

# 3.5 Check for 'missing' colonies

sf_cols_missing <- sf_cols %>%
  filter(!Island %in% sf_cols_poly_contains$Island)

sf_cols_poly_indist <- st_join(sf_land, sf_cols_missing, st_is_within_distance, dist = 100, left = T) %>%
  filter(!is.na(Island))

map_base <- ggplot() + 
  scale_y_continuous(expand = c(0.01,0.01))+
  scale_x_continuous(expand = c(0.01,0.01))+
  geom_sf(data = filter(chagos, DEPTHLABEL == c("deep")), inherit.aes = FALSE, fill = "gray90", col = NA)+
  geom_sf(data = filter(chagos, DEPTHLABEL == c("variable")), inherit.aes = FALSE, fill = "gray90", col = NA)+
  geom_sf(data = filter(chagos, DEPTHLABEL == c("shallow")), inherit.aes = FALSE, fill = "gray80", col = NA)+
  geom_sf(data = filter(chagos, DEPTHLABEL == c("land")), inherit.aes = FALSE, fill = "grey30", col = "grey20", lwd = 0.6)+
  theme_minimal()+
  theme(panel.border = element_rect(fill = NA))+
  annotation_scale(style = "ticks", location = "br")+
  ylab(NULL)+
  xlab(NULL)

map_base_v <- ggplot() + 
  scale_y_continuous(expand = c(0.01,0.01))+
  scale_x_continuous(expand = c(0.01,0.01))+
  geom_sf(data = filter(chagos, DEPTHLABEL == c("deep")), inherit.aes = FALSE, fill = "gray90", col = NA)+
  geom_sf(data = filter(chagos, DEPTHLABEL == c("variable")), inherit.aes = FALSE, fill = "gray90", col = NA)+
  geom_sf(data = filter(chagos, DEPTHLABEL == c("shallow")), inherit.aes = FALSE, fill = "gray80", col = NA)+
  geom_sf(data = filter(chagos, DEPTHLABEL == c("land")), inherit.aes = FALSE, fill = "grey30", col = "grey20", lwd = 0.6)+
  theme_void()+
  ylab(NULL)+
  xlab(NULL)
map_base_v


# ggsave(plot = map_base_v, filename = "Figures/Islands_void.png",
#        width = 3, height = 5, units = "cm", bg = 'white')


map_base +
  geom_sf(data = sf_cols_poly_contains, fill = "forestgreen", col = "forestgreen")+
  geom_sf(data = sf_cols_poly_indist, fill = "aquamarine", col = "aquamarine")+
  geom_sf(data = sf_cols_missing, fill = "yellow", col = "yellow")+
  # zoom on DG:
  scale_x_continuous(limits = c(72.3, 72.55), expand = c(0,0))+
  scale_y_continuous(limits = c(-7.5, -7.21), expand = c(0,0))+
  # zoom on Perros banhos
  #scale_x_continuous(limits = c(71.7, 72.0), expand = c(0,0))+
  #scale_y_continuous(limits = c(-5.5, -5.2), expand = c(0,0))+
  # zoom on Salomon
  #scale_x_continuous(limits = c(72.18, 72.3), expand = c(0,0))+
  #scale_y_continuous(limits = c(-5.38, -5.28), expand = c(0,0))+
  theme_minimal()+
  theme(panel.border = element_rect(fill = NA, colour = "grey70"))


sf_cols_poly <- sf_cols_poly_contains %>%
  bind_rows(sf_cols_poly_indist)


sf_cols_stillmissing <- sf_cols %>%
  filter(!Island %in% sf_cols_poly$Island)

# Just east, middle, and west island, DG, still missing
# East is in shallow polyon

sf_shallow <- chagos %>%
  filter(DEPTHLABEL == "shallow") %>%
  st_make_valid()

sf_East <- sf_cols_stillmissing %>%
  filter(Island == "East Island") %>%
  st_join(sf_shallow, ., st_contains, left = T) %>%
  filter(!is.na(Island))

# Buffer point by 100m for West and Middle
sf_West_Middle <- sf_cols_stillmissing %>%
  filter(Island %in% c("West Island", "Middle Island")) %>%
  st_buffer(dist = 100)

# combine all polygons together
sf_cols_poly <- sf_cols_poly_contains %>%
  bind_rows(sf_cols_poly_indist, sf_East, sf_West_Middle) %>%
  select(Island, Group, Atoll, Rattus.rattus, Sula.sula, TotalAllSp, geometry)


map_base +
  geom_sf(data = sf_cols_poly_contains, fill = "forestgreen", col = "forestgreen")+
  geom_sf(data = sf_West_Middle, fill = "aquamarine", col = "aquamarine")+
  geom_sf(data = sf_East, fill = "yellow", col = "yellow")+
  # zoom on DG:
  scale_x_continuous(limits = c(72.3, 72.55), expand = c(0,0))+
  scale_y_continuous(limits = c(-7.5, -7.21), expand = c(0,0))+
  # zoom on Perros banhos
  #scale_x_continuous(limits = c(71.7, 72.0), expand = c(0,0))+
  #scale_y_continuous(limits = c(-5.5, -5.2), expand = c(0,0))+
  # zoom on Salomon
  #scale_x_continuous(limits = c(72.18, 72.3), expand = c(0,0))+
  #scale_y_continuous(limits = c(-5.38, -5.28), expand = c(0,0))+
  theme_minimal()+
  theme(panel.border = element_rect(fill = NA, colour = "grey70"))

map_base +
  geom_sf(data = sf_cols_poly, fill = "forestgreen", col = "forestgreen")+
  theme_minimal()+
  theme(panel.border = element_rect(fill = NA, colour = "grey70"))

# 4. Save island polygons ####

st_write(sf_cols_poly, "Data/Island_data/island_polygons.shp", append=FALSE)


# 5. Buffer islands by mean max foraging range of RFB from Trevail et al 2023, MEPS = 112.9km ####

sf_cols_MEPSbuffer <- sf_cols_poly %>%
  rowwise() %>%
  st_buffer(112900)

# st_write(sf_cols_MEPSbuffer, "Data/Island data/island_polygons_buffer_MEPSforagingrange.shp", append=FALSE)

map_base +
  geom_sf(data = sf_cols_poly, fill = "forestgreen", col = "forestgreen")+
  geom_sf(data = sf_cols_MEPSbuffer, fill = NA, col = "gray50")+
  theme_minimal()

# 6. Buffer islands by overall max foraging range of RFB from Trevail et al 2023, MEPS = 424.4km ####

sf_cols_MEPSbuffer_max <- sf_cols_poly %>%
  rowwise() %>%
  st_buffer(424400)

map_base +
  geom_sf(data = sf_cols_poly, fill = "forestgreen", col = "forestgreen")+
  geom_sf(data = sf_cols_MEPSbuffer_max, fill = NA, col = "gray50")+
  theme_minimal()

# 7. Buffer islands by overall max foraging range of immature RFB from first study = 661.9km ####

sf_cols_MEPSbuffer_maximm <- sf_cols_poly %>%
  rowwise() %>%
  st_buffer(661900)

map_base +
  geom_sf(data = sf_cols_poly, fill = "forestgreen", col = "forestgreen")+
  geom_sf(data = sf_cols_MEPSbuffer_maximm, fill = NA, col = "gray50")+
  theme_minimal()

# 7. Hex grid across whole area ####

# use maximum extent of immature tracks to define area for grid

sf_imm_tracks <- read_csv("Data/imm_tracks_prospecting.csv") %>% 
  st_as_sf(., coords=c("lon","lat"), crs=4326)

bbox_maxfrange <- sf_cols_MEPSbuffer_maximm %>%
  st_union() %>%
  st_make_valid() %>%
  st_buffer(10000) %>% # buffer by 10km
  st_bbox() %>%
  st_as_sfc()

map_base +
  geom_sf(data = sf_cols_poly, fill = "forestgreen", col = "forestgreen")+
  geom_sf(data = sf_cols_MEPSbuffer_max, fill = NA, col = "gray50")+
  geom_sf(data = bbox_maxfrange, fill = NA, col = "blue")+
  geom_sf(data = sf_imm_tracks, col = "gray40")+
  theme_light()

sf_hex <- bbox_maxfrange %>%
  st_transform(crs = 3857) %>%
  st_make_grid(.,
               cellsize = units::as_units(20, "km"),
               what = "polygons",
               square = FALSE) %>%
  st_transform(crs = 4326) %>%
  st_as_sf() %>%
  mutate(hex_id = c(1:n())) %>%
  rename(geometry = x)

map_base +
  geom_sf(data = sf_cols_poly, fill = "forestgreen", col = "forestgreen")+
  geom_sf(data = sf_cols_MEPSbuffer_max, fill = NA, col = "gray50")+
  geom_sf(data = bbox_maxfrange, fill = NA, col = "blue")+
  geom_sf(data = sf_hex, fill = NA, col = "gray40")+
  theme_light()

# 8. Distance from hex grid cell to each island ####


sf_hex_centroids <- sf_hex %>%
  st_centroid()

hex_dists <- tibble(
  hex_id = numeric(),
  Island = character(),
  Group = character(),
  Atoll = character(),
  dist_m = numeric())

for (i in 1:NROW(sf_cols_poly)){
  sf_is <- slice(sf_cols_poly, i)
  
  hex_dists_is <- tibble(
    hex_id = sf_hex_centroids$hex_id,
    Island = sf_is$Island,
    Group = sf_is$Group,
    Atoll = sf_is$Atoll,
    dist_m = as.numeric(st_distance(sf_hex_centroids, sf_is)))
  
  hex_dists <- bind_rows(hex_dists, hex_dists_is)
}

glimpse(hex_dists)

sf_hex_dists <- sf_hex %>%
  right_join(hex_dists)

glimpse(sf_hex_dists)



map_base +
  geom_sf(data = sf_hex_dists %>% filter(Island == "Diego Garcia"), aes(fill = dist_m), col = "gray40")+
  geom_sf(data = sf_cols_poly, fill = "black", col = "black")+
  scale_fill_viridis_c(direction = -1)+
  theme_light()


st_write(sf_hex_dists, "Data/Island_data/hex_grid_island_distances.shp", append=FALSE)
