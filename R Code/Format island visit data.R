pacman::p_load(tidyverse, mgcv, gratia)


# read in and combine prospecting data with island metrics
island_metrics <- read_csv("Data/island_metrics.csv") %>%
  mutate(Island_Atoll = paste0(Island, "_", Atoll))
glimpse(island_metrics)

imm_tracks <- read_csv("Data/imm_tracks_prospecting.csv")
glimpse(imm_tracks)

# filter island visits and add types
imm_islands <- imm_tracks %>%
  rename(tracking_duration = trckn__,
         deployment_colony = dply_cl) %>%
  mutate(visit_type = case_when(at_island == T & cpf == T ~ "CPF",
                                at_island == T & prospect == T & visit_number == 1 & Island == deployment_colony ~ "Deployment",
                                at_island == T & prospect == T & label %in% c("single", "first") & lag(at_sea) == T ~ "Prospect_Forage",
                                at_island == T & prospect == T & label %in% c("single", "first") & lag(at_island) == T ~ "Direct_Prospect",
                                .default = NA)) %>%
  group_by(VisitID) %>%
  fill(visit_type) %>%
  filter(at_island == TRUE) %>%
  select(BirdID, age, sex, Year, DateTime, deployment_colony, tracking_duration, 
         VisitID, night_visit, visit_type,
         Island_Atoll, Island, Group, Atoll) %>%
  
glimpse(imm_islands)


# summarise the number of times an individual visits each island by visit type

imm_islands_count <- imm_islands %>%
  ungroup() %>%
  # keep distinct island visits
  summarise(.by = c(BirdID, age, sex, Year, deployment_colony, tracking_duration, 
                    VisitID, night_visit, visit_type,
                    Island_Atoll, Island, Group, Atoll),
            visit_start = min(DateTime),
            visit_duration_hrs = n()*4) %>%
  arrange(BirdID, Year, visit_start) %>%
  summarise(.by = c(BirdID, age, sex, Year, deployment_colony, tracking_duration, 
            Island_Atoll, Island, Group, Atoll),
            n_visits = n(),
            total_visit_duration = sum(visit_duration_hrs),
            n_cpf = sum(visit_type == "CPF"),
            n_Prospect_Forage = sum(visit_type == "Prospect_Forage"),
            n_Direct_Prospect = sum(visit_type == "Direct_Prospect"))
  
glimpse(imm_islands_count)
  
# we want to include all bird - island combinations, including when not visited
# Extract unique bird-level information
bird_info <- imm_islands_count %>%
  distinct(
    BirdID,
    age,
    sex,
    Year,
    deployment_colony,
    tracking_duration
  )

island_info <- island_metrics %>%
  distinct(
    Island_Atoll,
    Island,
    Group,
    Atoll,
    lat,
    lon,
    rats_f,
    size,
    RFB_veg_percent,
    Sula.sula,
    seabird_sp,
    seabird_abundance,
    forage_avail_mean,
    forage_access_mean
  )

# Create all Bird × Island combinations
all_combinations <- bird_info %>%
  crossing(island_info)


# Add visit counts
combined_data <- all_combinations %>%
  left_join(
    imm_islands_count %>%
      select(
        BirdID,
        Island_Atoll,
        n_visits,
        total_visit_duration,
        n_cpf,
        n_Prospect_Forage,
        n_Direct_Prospect),
    by = c("BirdID", "Island_Atoll")) %>%
  mutate(n_visits = replace_na(n_visits, 0),
         total_visit_duration = replace_na(total_visit_duration, 0),
         n_cpf = replace_na(n_cpf, 0),
         n_Prospect_Forage = replace_na(n_Prospect_Forage, 0),
         n_Direct_Prospect = replace_na(n_Direct_Prospect, 0)) %>%
  rename(col_size_RFB_adults = Sula.sula,
         seabird_sp_richness = seabird_sp)

glimpse(combined_data)



### save data

write_csv(combined_data, "Data/imm_island_visits_metrics.csv")

# plot number of visits to each island by size
ggplot(combined_data, aes(x = size, y = n_visits, col = age)) +
  geom_point() +
  geom_smooth(method = "gam") +
  scale_x_log10()+
  scale_colour_manual(values = c("#841E5AFF", "#DD2C45FF"), labels = c("Immature age 1-2", "Immature age 2-3"), name = "Age")+
  theme_minimal()+
  theme(panel.border = element_rect())+
  labs(x = "Island size", y = "Number of visits")


ggplot(combined_data %>% filter(!is.na(rats_f)), aes(x = rats_f, y = n_visits, col = age, fill = age)) +
  geom_boxplot() +
  #geom_smooth(method = "gam") +
  scale_colour_manual(values = c("#841E5AFF", "#DD2C45FF"), labels = c("Immature age 1-2", "Immature age 2-3"), name = "Age")+
  scale_fill_manual(values = c("#841E5AFF", "#DD2C45FF"), labels = c("Immature age 1-2", "Immature age 2-3"), name = "Age")+
  theme_minimal()+
  theme(panel.border = element_rect())+
  labs(x = "Rat presence", y = "Number of visits")
