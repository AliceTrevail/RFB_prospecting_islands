### Immature RFB maps ###
pacman::p_load(tidyverse, ggspatial, readxl, patchwork, magick, cowplot, sf, rnaturalearth, rnaturalearthdata, corrr)


### read in map layers ####

chagos <- st_read("~/Library/CloudStorage/OneDrive-UniversityofExeter/Projects/2019-2026 BPMS/Breeding data/Project1_RFB Chagos foraging ecology/Rcode/Chagos/Chagos_v6.shp")
chagos$DEPTHLABEL <- fct_relevel(chagos$DEPTHLABEL, "land", "shallow", "variable", "deep")

RFB_ad_img <- image_read("~/Library/CloudStorage/OneDrive-UniversityofExeter/Species images/white morph red footed booby adult.png") %>%
  image_flop()


### read in island data: ####

# Pete's data from excel
pete_veg_file <- "~/Library/CloudStorage/OneDrive-UniversityofExeter/Projects/2019-2026 BPMS/2021 Pete Carr/20200409-Carr-Chagos_VegMan_Paper-Data-MASTER.xlsx"

# metadata for island names / abbreviations
# there are differences in formats and spellings that we need to fix!
meta_island_names <- read_excel(path = pete_veg_file, sheet = "Metadata", range = "E31:F101") %>%
  janitor::clean_names() %>%
  filter(!is.na(island_names)) %>%
  mutate(island_snake = snakecase::to_snake_case(island_names)) %>%
  rename(ISLAND_NAMES = island_names,
         island_abbreviation = abbreviation)
meta_island_names
unique(meta_island_names$island_snake)
length(unique(meta_island_names$island_abbreviation))


#### island coordinates & seabird col sizes ####
CAcols <- read_csv("Data/Island data/Breeding numbers.csv") %>%
  mutate(Island = case_when(Island == "Grand Coquillage" ~ "Grande Coquillage", .default = Island), # fix spelling difference
         island_snake = snakecase::to_snake_case(Island), .after = Island,
         lat = case_when(island_snake == "diego_garcia" ~ -7.33, .default = lat))
glimpse(CAcols)
# join = island_snake
# check complete island names
setdiff(CAcols$island_snake, meta_island_names$island_snake)
setdiff(meta_island_names$island_snake, CAcols$island_snake)


#### island size and vegetation ####
island_veg <- read_excel(path = pete_veg_file, sheet = "VegbyIsland") %>%
  janitor::clean_names() %>%
  rename(island_abbreviation = island,
         atoll_abbreviation = atoll) %>%
  filter(!is.na(atoll_abbreviation)) # remove total rows
glimpse(island_veg)
# join = island_veg
# check complete island names
setdiff(meta_island_names$island_abbreviation, island_veg$island_abbreviation)
setdiff(island_veg$island_abbreviation, meta_island_names$island_abbreviation)

#### island diversity metrics ####
island_vars <- read_excel(path = pete_veg_file, sheet = "Variables") %>%
  janitor::clean_names() %>%
  dplyr::select(island, dist, inha, bird, bira) %>%
  dplyr::rename(seabird_sp = bird, 
                seabird_abundance = bira,
                island_abbreviation = island)
glimpse(island_vars)
# join = island_abbreviation
# check complete island names
setdiff(island_vars$island_abbreviation, meta_island_names$island_abbreviation)
setdiff(meta_island_names$island_abbreviation, island_vars$island_abbreviation)

#### at-sea foraging habitat suitability ####
island_foraging <- read_csv("Data/hex_grid_foraging_probs/Is_predicted_foraging_habitat.csv") %>%
  pivot_wider(names_from = Year, values_from = c(forage_avail, forage_access), names_sep = "_") %>%
  mutate(Is_Group_Atoll = paste(Island, Group, Atoll, sep = "_")) %>%
  rename(Island_duplicates = Island) %>%
  # in CAcols, all island names are unique. So, replace Island names here:
  mutate(Island = case_when(Is_Group_Atoll == "Anglaise_Peros Banhos West_Peros Banhos" ~ "Anglaise (PEBA)",
                            Is_Group_Atoll == "Anglaise_Salomon Islands_Salomon Islands" ~ "Anglaise (SOIS)",
                            Is_Group_Atoll == "Coin de Mire_Peros Banhos East_Peros Banhos" ~ "Coin du Mire",
                            Is_Group_Atoll == "Eagle Island_Eagle Islands_Great Chagos Bank" ~ "Eagle",
                            Is_Group_Atoll == "Fouquet_Peros Banhos West_Peros Banhos" ~ "Fouquet (PEBA)",
                            Is_Group_Atoll == "Fouquet_Salomon Islands_Salomon Islands" ~ "Fouquet (SOIS)",
                            Is_Group_Atoll == "Grand Coquillage_Peros Banhos East_Peros Banhos" ~ "Grande Coquillage",
                            Is_Group_Atoll == "Grand Souer_Peros Banhos West_Peros Banhos" ~ "Grand Soeur",
                            Is_Group_Atoll == "Petit Souer_Peros Banhos West_Peros Banhos" ~ "Petite Soeur",
                            Is_Group_Atoll == "Grande Mapou_Peros Banhos West_Peros Banhos" ~ "Grand Mapou",
                            Is_Group_Atoll == "Mapou de Coin_Peros Banhos West_Peros Banhos" ~ "Mapou du Coin",
                            Is_Group_Atoll == "Monpatre complex_Peros Banhos West_Peros Banhos" ~ "Gabrielle/Montpatre complex",
                            Is_Group_Atoll == "Nelson's Island_Nelson's Island_Great Chagos Bank" ~ "Nelson's",
                            Is_Group_Atoll == "Passe_Peros Banhos West_Peros Banhos" ~ "Passe (PEBA)",
                            Is_Group_Atoll == "Passe_Salomon Islands_Salomon Islands" ~ "Passe (SOIS)",
                            Is_Group_Atoll == "Poule_Peros Banhos West_Peros Banhos" ~ "Poule (PEBA)",
                            Is_Group_Atoll == "Poule_Salomon Islands_Salomon Islands" ~ "Poule (SOIS)",
                            Is_Group_Atoll == "Unnamed (Burtle)_Peros Banhos West_Peros Banhos" ~ "Burtle (Not Named)",
                            Is_Group_Atoll == "Unnamed (Marlin)_Peros Banhos East_Peros Banhos" ~ "Marlin (Not Named)",
                            .default = Island_duplicates)) %>%
  group_by(Island) %>%
  mutate(forage_avail_mean = mean(c(forage_avail_2022, forage_avail_2023)),
         forage_access_mean = mean(c(forage_access_2022, forage_access_2023))) %>%
  ungroup() %>%
  select(-Is_Group_Atoll)
glimpse(island_foraging)

# join = Island, Group, Atoll
# check complete island names

setdiff(island_foraging$Island, CAcols$Island)
setdiff(CAcols$Island, island_foraging$Island)

setdiff(island_foraging$Atoll, CAcols$Atoll)
setdiff(CAcols$Atoll, island_foraging$Atoll)

setdiff(island_foraging$Group, CAcols$Group)
setdiff(CAcols$Group, island_foraging$Group)


#### combine island metrics ####
island_metrics <- island_veg %>%
  left_join(., meta_island_names, join_by(island_abbreviation)) %>% 
  left_join(., CAcols, join_by(island_snake)) %>%
  left_join(., island_vars, join_by(island_abbreviation)) %>%
  left_join(.,island_foraging, join_by(Island, Group, Atoll)) %>%
  mutate(rats_f = case_when(rats == "1" ~ "absent",
                            rats == "2" ~ "present"),
         RFB_veg = dplyr::select(., c(wetl, natf, mish)) %>% rowSums(),
         RFB_veg_percent = dplyr::select(., c(wetl_percent, natf_percent, mish_percent)) %>% rowSums()) %>%
  select(-c(rats, Rattus.rattus, ISLAND_NAMES)) %>%
  relocate(c(Island, Group, Atoll, island_abbreviation, atoll_abbreviation, island_snake, lat, lon, rats_f))
glimpse(island_metrics)

write_csv(island_metrics, "Data/island_metrics.csv")
island_metrics <- read_csv("Data/island_metrics.csv")

### Create maps: ####

#### Base map ####

map_base <- ggplot() + 
  scale_y_continuous(expand = c(0.01,0.01))+
  scale_x_continuous(expand = c(0.01,0.01))+
  geom_sf(data = filter(chagos, DEPTHLABEL == c("deep")), inherit.aes = FALSE, fill = "gray90", col = NA)+
  geom_sf(data = filter(chagos, DEPTHLABEL == c("variable")), inherit.aes = FALSE, fill = "gray90", col = NA)+
  geom_sf(data = filter(chagos, DEPTHLABEL == c("shallow")), inherit.aes = FALSE, fill = "gray80", col = NA)+
  geom_sf(data = filter(chagos, DEPTHLABEL == c("land")), inherit.aes = FALSE, fill = "grey30", col = "grey20", lwd = 0.6)+
  theme_minimal()+
  theme(panel.border = element_rect(fill = NA))+
  annotation_scale(style = "ticks")+
  ylab(NULL)+
  xlab(NULL)



#### Island names ####
map_names <- map_base+
  geom_text(aes(x = 72, y=-6.32), label = "Great Chagos\nBank", cex = 2.9, lineheight = 1)+
  geom_text(aes(x = 72.32, y=-7.35), label = "Diego\nGarcia\natoll", hjust = 1, cex = 2.9, lineheight = 1)+
  geom_text(aes(x = 71.3, y=-6.75), label = "Egmont\natoll", hjust = 1, cex = 2.9, lineheight = 1)+
  geom_text(aes(x = 71.21, y=-6.45), label = "Danger\nIsland", hjust = 1, cex = 2.9, lineheight = 1)+
  geom_text(aes(x = 71.27, y=-6.15), label = "Eagle\nIsland", hjust = 1, cex = 2.9, lineheight = 1)+
  geom_text(aes(x = 71.55, y=-6.03), label = "Three\nBrothers", hjust = 0, cex = 2.9, lineheight = 1)+
  geom_text(aes(x = 72.35, y=-5.77), label = "Nelson's\nIsland", hjust = 0, cex = 2.9, lineheight = 1)+
  geom_text(aes(x = 72.3, y=-5.37), label = "Salomon\natoll", hjust = 0, cex = 2.9, lineheight = 1)+
  geom_text(aes(x = 71.7, y=-5.34), label = "Peros Banhos\natoll", hjust = 1, cex = 2.9, lineheight = 1)


ggsave(plot = map_names, filename = "Figures/Separate Chagos maps/map_names.png",
       width = 12, height = 12, units = "cm")


#### Island size & rat presence ####

min(island_metrics$size/100)
map_rats <- map_base+
  geom_point(data = island_metrics, aes(x = lon, y = lat, col = rats_f, cex = size/100), inherit.aes = FALSE)+
  #scale_colour_manual(values = c("tomato1", "tomato1"), name = "Rats")+
  scale_colour_viridis_d(option = "rocket", name = "Rats", begin = 0.85, end = 0.6)+
  scale_size_area(name = "Island size\n(km²)", breaks = c(0.1,0.5,1,2.5,5,10,20))
map_rats


#### Coverage of RFB breeding habitat ####

map_RFBveg_area <- map_base+
  geom_point(data = island_metrics, aes(x = lon, y = lat, cex = RFB_veg/100), inherit.aes = FALSE, col = "forestgreen")+
  geom_point(data = subset(island_metrics, RFB_veg == 0), aes(x = lon, y = lat), pch = 4, inherit.aes = FALSE, col = "forestgreen", cex = 1)+
  scale_size(name = "RFB breeding\nhabitat area \n(km²)", range=c(0,6), breaks = c(0, 0.5, 1, 2.5, 5, 10))
map_RFBveg_area


# pull out legend details to override 
map_RFBveg_area_sizedata <- ggplot_build(map_RFBveg_area)$data[[6]] %>%
  mutate(RFBveg_area = island_metrics$RFB_veg/100, 
         sqrt_RFBveg_area = sqrt(RFBveg_area))
ggplot(data = map_RFBveg_area_sizedata, aes(x = sqrt_RFBveg_area, y=size))+geom_point()
legend_RFBveg_areas <- predict(lm(size ~ sqrt_RFBveg_area, data = map_RFBveg_area_sizedata), newdata = tibble(sqrt_RFBveg_area = sqrt(c(0.5,1,2.5,5,10))))

map_RFBveg_area <- map_RFBveg_area + guides(size = guide_legend(override.aes = list(shape = c(4,19,19,19,19,19),
                                                                            size = c(1,legend_RFBveg_areas))))
map_RFBveg_area



map_RFBveg_percent <- map_base+
  geom_point(data = island_metrics, aes(x = lon, y = lat, cex = RFB_veg_percent), inherit.aes = FALSE, col = "forestgreen")+
  geom_point(data = subset(island_metrics, RFB_veg == 0), aes(x = lon, y = lat), pch = 4, inherit.aes = FALSE, col = "forestgreen", cex = 1)+
  scale_radius(name = "RFB breeding\nhabitat\ncoverage (%)", range = c(0,3), breaks = c(0,20,40,60,80))
map_RFBveg_percent


# pull out legend details to override 
map_RFBveg_percent_sizedata <- ggplot_build(map_RFBveg_percent)$data[[6]] %>%
  mutate(RFBveg_percent = island_metrics$RFB_veg_percent, 
         sqrt_RFBveg_percent = sqrt(RFBveg_percent))
ggplot(data = map_RFBveg_percent_sizedata, aes(x = RFBveg_percent, y=size))+geom_point()
legend_RFBveg_percents <- predict(lm(size ~ RFBveg_percent, data = map_RFBveg_percent_sizedata), newdata = tibble(RFBveg_percent = (c(20,40,60,80))))

map_RFBveg_percent <- map_RFBveg_percent + guides(size = guide_legend(override.aes = list(shape = c(4,19,19,19,19),
                                                                                    size = c(1,legend_RFBveg_percents))))
map_RFBveg_percent


#### RFB colony size ####

map_colsize <- map_base+
  geom_point(data = island_metrics, aes(x = lon, y = lat, cex = Sula.sula), inherit.aes = FALSE, col = "steelblue2")+
  geom_point(data = subset(island_metrics, Sula.sula == 0), aes(x = lon, y = lat), pch = 4, inherit.aes = FALSE, col = "steelblue2", cex = 1)+
  scale_size(name = "RFB\nbreeding \npairs", range=c(0,8), breaks = c(0, 50, 1000, 2000, 4000, 8000))

# pull out legend details to override 
map_colsize_sizedata <- ggplot_build(map_colsize)$data[[6]] %>%
  mutate(colsize = island_metrics$Sula.sula, sqrt_colsize = sqrt(colsize))
#ggplot(data = map_colsize_sizedata, aes(x = sqrt_colsize, y=size))+geom_point()
legend_colsizes <- predict(lm(size ~ sqrt_colsize, data = map_colsize_sizedata), newdata = tibble(sqrt_colsize = sqrt(c(50,1000,2000,4000,8000))))

map_colsize <- map_colsize + guides(size = guide_legend(override.aes = list(shape = c(4,19,19,19,19,19),
                                                 size = c(1,legend_colsizes))))
map_colsize


ggsave(plot = map_colsize, filename = "Figures/Separate Chagos maps/map_RFB.png",
       width = 16, height = 12, units = "cm")


#### Seabird diversity & richness ####

map_diversity <- map_base+
  geom_point(data = island_metrics, aes(x = lon, y = lat, cex = seabird_abundance, col = seabird_sp), inherit.aes = FALSE)+
  scale_size(name = "Seabird\nabundance\n(pairs)")+
  scale_colour_viridis_c(name = "Seabird\nrichness\n(no. species)")
map_diversity


ggsave(plot = map_diversity, filename = "Figures/Separate Chagos maps/map_diversity.png",
       width = 16, height = 12, units = "cm")



#### Foraging suitability ####

map_foraging <- map_base+
  geom_point(data = island_metrics, aes(x = lon, y = lat, cex = forage_avail_mean, col = forage_access_mean), inherit.aes = FALSE)+
  scale_size(name = "Available\nforaging area\n(grid cells)")+
  scale_colour_viridis_c(name = "Proximity of\nforaging area\nto colony (km)", option = "plasma", direction = -1)
map_foraging


### Combine and save full figure ####
FigS1_fullislands <- map_names + map_rats + map_RFBveg_percent + map_colsize + map_diversity + map_foraging +
  plot_annotation(tag_levels = 'a') + plot_layout(ncol = 3)

map_img2 <- ggdraw() +
 draw_plot(FigS1_fullislands) +
 draw_image(RFB_ad_img, x = 0.05, y = -0.06, width = 0.08)

ggsave(plot = map_img2, filename = "Figures/Manuscript/SuppMat/Island_metrics.png",
       width = 33, height = 24, units = "cm")

### Plot correlation between variables ####


metrics_cor <- island_metrics %>% 
  select(size, RFB_veg_percent, Sula.sula, seabird_abundance, seabird_sp, forage_avail_mean, forage_access_mean) %>% 
  correlate()

rplot(metrics_cor)+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

### save metrics ####
write_csv(island_metrics, "Data/island_metrics.csv")

# Presentation map of whole Indian ocean ####
# 
# Ch <- st_read("Data/Chagos/ChagosEEZ.shp")
# world_map <- ne_countries(scale = "medium", returnclass = "sf")
# map_IO <- ggplot() + 
#   scale_y_continuous(limits = c(-40,25), expand = c(0.01,0.01))+
#   scale_x_continuous(limits = c(25,120), expand = c(0.01,0.01))+
#   geom_sf(data = world_map, inherit.aes = FALSE, fill = "gray90", col = NA)+
#   geom_sf(data = filter(chagos, DEPTHLABEL == c("deep")), inherit.aes = FALSE, fill = "gray90", col = NA)+
#   geom_sf(data = Ch, alpha = 0.4, col = "black", fill = "#5aa5e3", linewidth = 0.4)+
#   theme_minimal()+
#   theme(panel.border = element_rect(fill = NA))+
#   ylab(NULL)+
#   xlab(NULL)
# map_IO
# 
# map_IO_presentation <- ggplot() + 
#   scale_y_continuous(limits = c(-40,25), expand = c(0.01,0.01))+
#   scale_x_continuous(limits = c(25,120), expand = c(0.01,0.01))+
#   geom_sf(data = world_map, inherit.aes = FALSE, fill = "gray90", col = NA)+
#   geom_sf(data = filter(chagos, DEPTHLABEL == c("deep")), inherit.aes = FALSE, fill = "gray90", col = NA)+
#   geom_sf(data = Ch, alpha = 0.4, col = "black", fill = "#5aa5e3", linewidth = 0.4)+
#   theme_void()
# map_IO_presentation


### Plot other species for Steve

### Sooty tern 

map_colsize <- map_base+
  geom_point(data = island_metrics, aes(x = lon, y = lat, cex = Onychoprion.fuscatus), inherit.aes = FALSE, col = "steelblue2")+
  geom_point(data = subset(island_metrics, Onychoprion.fuscatus == 0), aes(x = lon, y = lat), pch = 4, inherit.aes = FALSE, col = "steelblue2", cex = 1)+
  scale_size(name = "Sooty tern\nbreeding \npairs", range=c(0,8), breaks = c(0, 50, 1000, 2000, 4000, 8000))

# pull out legend details to override 
map_colsize_sizedata <- ggplot_build(map_colsize)$data[[6]] %>%
  mutate(colsize = island_metrics$Onychoprion.fuscatus, sqrt_colsize = sqrt(colsize))
#ggplot(data = map_colsize_sizedata, aes(x = sqrt_colsize, y=size))+geom_point()
legend_colsizes <- predict(lm(size ~ sqrt_colsize, data = map_colsize_sizedata), newdata = tibble(sqrt_colsize = sqrt(c(50,1000,2000,4000,8000))))

map_Onychoprion.fuscatus <- map_colsize + guides(size = guide_legend(override.aes = list(shape = c(4,19,19,19,19,19),
                                                                            size = c(1,legend_colsizes))))
map_Onychoprion.fuscatus


ggsave(plot = map_colsize, filename = "Figures/Separate Chagos maps/map_Onychoprion_fuscatus.png",
       width = 16, height = 12, units = "cm")



### Brown noddy

map_colsize <- map_base+
  geom_point(data = island_metrics, aes(x = lon, y = lat, cex = Anous.stolidus), inherit.aes = FALSE, col = "steelblue2")+
  geom_point(data = subset(island_metrics, Anous.stolidus == 0), aes(x = lon, y = lat), pch = 4, inherit.aes = FALSE, col = "steelblue2", cex = 1)+
  scale_size(name = "Brown noddy\nbreeding \npairs", range=c(0,8), breaks = c(0, 50, 500))

# pull out legend details to override 
map_colsize_sizedata <- ggplot_build(map_colsize)$data[[6]] %>%
  mutate(colsize = island_metrics$Anous.stolidus, sqrt_colsize = sqrt(colsize))
#ggplot(data = map_colsize_sizedata, aes(x = sqrt_colsize, y=size))+geom_point()
legend_colsizes <- predict(lm(size ~ sqrt_colsize, data = map_colsize_sizedata), newdata = tibble(sqrt_colsize = sqrt(c(50,500))))

map_Anous.stolidus <- map_colsize + guides(size = guide_legend(override.aes = list(shape = c(4,19,19),
                                                                                         size = c(1,legend_colsizes))))
map_Anous.stolidus


ggsave(plot = map_colsize, filename = "Figures/Separate Chagos maps/map_Anous_stolidus.png",
       width = 16, height = 12, units = "cm")


### Lesser noddy

map_colsize <- map_base+
  geom_point(data = island_metrics, aes(x = lon, y = lat, cex = Anous.tenuirostris), inherit.aes = FALSE, col = "steelblue2")+
  geom_point(data = subset(island_metrics, Anous.tenuirostris == 0), aes(x = lon, y = lat), pch = 4, inherit.aes = FALSE, col = "steelblue2", cex = 1)+
  scale_size(name = "Lesser noddy\nbreeding \npairs", range=c(0,8), breaks = c(0, 50, 1000, 2000, 4000, 8000))

# pull out legend details to override 
map_colsize_sizedata <- ggplot_build(map_colsize)$data[[6]] %>%
  mutate(colsize = island_metrics$Anous.tenuirostris, sqrt_colsize = sqrt(colsize))
#ggplot(data = map_colsize_sizedata, aes(x = sqrt_colsize, y=size))+geom_point()
legend_colsizes <- predict(lm(size ~ sqrt_colsize, data = map_colsize_sizedata), newdata = tibble(sqrt_colsize = sqrt(c(50,1000,2000,4000,8000))))

map_Anous.tenuirostris <- map_colsize + guides(size = guide_legend(override.aes = list(shape = c(4,19,19,19,19,19),
                                                                                         size = c(1,legend_colsizes))))
map_Anous.tenuirostris


ggsave(plot = map_colsize, filename = "Figures/Separate Chagos maps/map_Anous_tenuirostris.png",
       width = 16, height = 12, units = "cm")



### White tern

map_colsize <- map_base+
  geom_point(data = island_metrics, aes(x = lon, y = lat, cex = Gygis.alba), inherit.aes = FALSE, col = "steelblue2")+
  geom_point(data = subset(island_metrics, Gygis.alba == 0), aes(x = lon, y = lat), pch = 4, inherit.aes = FALSE, col = "steelblue2", cex = 1)+
  scale_size(name = "White tern\nbreeding \npairs", range=c(0,8), breaks = c(0, 50, 100))

# pull out legend details to override 
map_colsize_sizedata <- ggplot_build(map_colsize)$data[[6]] %>%
  mutate(colsize = island_metrics$Gygis.alba, sqrt_colsize = sqrt(colsize))
#ggplot(data = map_colsize_sizedata, aes(x = sqrt_colsize, y=size))+geom_point()
legend_colsizes <- predict(lm(size ~ sqrt_colsize, data = map_colsize_sizedata), newdata = tibble(sqrt_colsize = sqrt(c(50,100))))

map_Gygis.alba <- map_colsize + guides(size = guide_legend(override.aes = list(shape = c(4,19,19),
                                                                                       size = c(1,legend_colsizes))))
map_Gygis.alba


ggsave(plot = map_colsize, filename = "Figures/Separate Chagos maps/map_Gygis_alba.png",
       width = 16, height = 12, units = "cm")




### Tropical shearwater

map_colsize <- map_base+
  geom_point(data = island_metrics, aes(x = lon, y = lat, cex = Puffinus.bailloni), inherit.aes = FALSE, col = "steelblue2")+
  geom_point(data = subset(island_metrics, Puffinus.bailloni == 0), aes(x = lon, y = lat), pch = 4, inherit.aes = FALSE, col = "steelblue2", cex = 1)+
  scale_size(name = "Tropical shearwater\nbreeding \npairs", range=c(0,8), breaks = c(0, 50, 500, 1000))

# pull out legend details to override 
map_colsize_sizedata <- ggplot_build(map_colsize)$data[[6]] %>%
  mutate(colsize = island_metrics$Puffinus.bailloni, sqrt_colsize = sqrt(colsize))
#ggplot(data = map_colsize_sizedata, aes(x = sqrt_colsize, y=size))+geom_point()
legend_colsizes <- predict(lm(size ~ sqrt_colsize, data = map_colsize_sizedata), newdata = tibble(sqrt_colsize = sqrt(c(50,500, 1000))))

map_Puffinus.bailloni <- map_colsize + guides(size = guide_legend(override.aes = list(shape = c(4,19,19,19),
                                                                                       size = c(1,legend_colsizes))))
map_Puffinus.bailloni


ggsave(plot = map_colsize, filename = "Figures/Separate Chagos maps/map_Puffinus_bailloni.png",
       width = 16, height = 12, units = "cm")



### Wedge-tailed shearwater

map_colsize <- map_base+
  geom_point(data = island_metrics, aes(x = lon, y = lat, cex = Ardenna.pacifica), inherit.aes = FALSE, col = "steelblue2")+
  geom_point(data = subset(island_metrics, Ardenna.pacifica == 0), aes(x = lon, y = lat), pch = 4, inherit.aes = FALSE, col = "steelblue2", cex = 1)+
  scale_size(name = "Wedge-tailed shearwater\nbreeding \npairs", range=c(0,8), breaks = c(0, 50, 500, 1000, 2000))

# pull out legend details to override 
map_colsize_sizedata <- ggplot_build(map_colsize)$data[[6]] %>%
  mutate(colsize = island_metrics$Ardenna.pacifica, sqrt_colsize = sqrt(colsize))
#ggplot(data = map_colsize_sizedata, aes(x = sqrt_colsize, y=size))+geom_point()
legend_colsizes <- predict(lm(size ~ sqrt_colsize, data = map_colsize_sizedata), newdata = tibble(sqrt_colsize = sqrt(c(50,500,1000,2000))))

map_Ardenna.pacifica <- map_colsize + guides(size = guide_legend(override.aes = list(shape = c(4,19,19,19, 19),
                                                                                      size = c(1,legend_colsizes))))
map_Ardenna.pacifica


ggsave(plot = map_colsize, filename = "Figures/Separate Chagos maps/map_Ardenna_pacifica.png",
       width = 16, height = 12, units = "cm")

