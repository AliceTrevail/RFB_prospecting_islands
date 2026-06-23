pacman::p_load(tidyverse, sf, ggspatial)

#### Create map base ####

# Read in chagos shape file 

chagos <- st_read("Data/Chagos_islands_shape_files/Chagos_v6.shp")
chagos$DEPTHLABEL <- fct_relevel(chagos$DEPTHLABEL, "land", "shallow", "variable", "deep")

sf_land <- chagos %>%
  filter(LAND == "1")

# Read in island polygons

sf_cols_poly <- st_read("Data/Island_data/island_polygons.shp")

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
map_base

### read in foraging probabilities ####

sf_hex_fprob <- st_read("Data/adult_foraging_probability/hex_grid_model_predictions.shp") %>%
  filter(CPdist < 425) %>% #crop to max observed adult foraging range
  rename(suitability_softOR = Stbl_md,
         suitability_ensemblemean = ensmb__) %>%
  mutate(pred_f = as_factor(case_when(suitability_softOR > 0 ~1, .default = 0)))

glimpse(sf_hex_fprob)
unique(sf_hex_fprob$Island)

# visualise for example colony

map_base +
  geom_sf(data = sf_hex_fprob %>% filter(Island == "Nelson's Island"), aes(fill = suitability_ensemblemean), col = "gray40")+
  facet_grid(~Year)+
  geom_sf(data = sf_cols_poly, fill = "white", col = "white")+
  scale_fill_viridis_c()+
  theme_light()

map_base +
  geom_sf(data = sf_hex_fprob %>% filter(Island == "Nelson's Island"), aes(fill = pred_f), col = "gray40")+
  facet_grid(~Year)+
  geom_sf(data = sf_cols_poly, fill = "white", col = "white")+
  scale_fill_viridis_d()+
  theme_light()

# # cumulative presence 

library(dplyr)

df_fprob_cumulative <- sf_hex_fprob %>%
  st_drop_geometry() %>%
  arrange(Island, Group, Atoll, Year, dist_m) %>%
  group_by(Island, Group, Atoll, Year) %>%
  mutate(cum_presence = cumsum(pred_num)) %>%
  ungroup() %>%
  mutate(Is.Group.Atoll = paste(Island, Group, Atoll, sep = ", "))
str(df_fprob_cumulative)


ggplot(df_fprob_cumulative %>% filter(Island == "Nelson's Island"), aes(x = dist_km, y = cum_presence)) +
  geom_line() +
  facet_grid(~Year)+
  labs(x = "Distance from colony",
    y = "Cumulative number of presence cells\n i.e., area of suitable habitat") +
  theme_minimal()

ggplot(df_fprob_cumulative, aes(x = dist_km, y = cum_presence, col = Is.Group.Atoll)) +
  geom_line() +
  facet_grid(~Year)+
  labs(x = "Distance from colony",
       y = "Cumulative number of presence cells\n i.e., area of suitable habitat") +
  scale_color_viridis_d(guide = "none")+
  theme_minimal()

r50_df <- df_fprob_cumulative %>%
  group_by(Island, Group, Atoll, Year) %>%
  summarise(
    total_pres = max(cum_presence, na.rm = TRUE),
    r50 = dist_km[which(cum_presence >= 0.5 * total_pres)[1]],
    r29 = dist_km[which(cum_presence >= 29)[1]],
    .groups = "drop"
  )
r50_df
min(r50_df$total_pres)

ggplot(r50_df, aes(x = total_pres, y = r50))+
  facet_grid(~Year)+
  geom_point()+
  labs(x = "Availability: maximum area of suitable habitat",
       y = "Accessibility: Distance to 50% of total availability")+
  theme_minimal()


ggplot(r50_df, aes(x = total_pres, y = r29))+
  facet_grid(~Year)+
  geom_point()+
  labs(x = "Availability: maximum area of suitable habitat",
       y = "Accessibility: Distance to minimum availability")+
  theme_minimal()

library(pracma)

auc_metrics <- df_fprob_cumulative %>%
  group_by(Island, Group, Atoll, Year) %>%
  summarise(
    AUC = trapz(dist_km, cum_presence),
    .groups = "drop"
  ) %>%
  right_join(r50_df)

auc_metrics

ggplot(auc_metrics, aes(x = total_pres, y = AUC))+
  facet_grid(~Year)+
  geom_point()+
  labs(x = "Availability: maximum area of suitable habitat",
       y = "Area Under the Curve")+
  theme_minimal()

### to do: 
# - do this for both years = make year long
# - rename columns to something useful
# - plot colony values on map
# - pull out some plots for group meeting

# AUC and r29 are very closely correlated with total availability
# so, use total availability and r50 as metrics of foraging habitat

### Save values ####

r50_df %>%
  rename(forage_avail = total_pres,
         forage_access = r50) %>%
  select(-r29) %>%
  write_csv("Data/hex_grid_foraging_probs/Is_predicted_foraging_habitat.csv")
