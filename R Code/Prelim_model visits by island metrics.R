pacman::p_load(tidyverse, mgcv, gratia)


# read in and combine prospecting data with island metrics
island_metrics <- read_csv("Data/island_metrics.csv") %>%
  mutate(Island_Atoll = paste0(Island, "_", Atoll))
glimpse(island_metrics)
imm_tracks <- read_csv("Data/imm_tracks_prospecting.csv")
glimpse(imm_tracks)

imm_islands <- imm_tracks %>%
  left_join(island_metrics, by = "Island_Atoll") %>%
  filter(at_island = TRUE)
glimpse(imm_islands)

# summarise the number of times an individual visits each island

imm_islands_count <- imm_islands %>%
  # keep distinct island visits
  filter(!duplicated(VisitID)) %>%
  group_by(BirdID, Island_Atoll, Year, age, sex, trckn__, size, rats_f, RFB_veg_percent, Sula.sula, seabird_sp, seabird_abundance) %>%
  summarise(n_visits = n()) %>%
  ungroup() %>%
  # remove nas in Island_Atoll, size, Sula.sula, seabird_sp, seabird_abundance
  filter(!is.na(Island_Atoll), !is.na(size), !is.na(Sula.sula), !is.na(seabird_sp), !is.na(seabird_abundance)) %>%
  mutate(rats_f = as.factor(rats_f))
glimpse(imm_islands_count)
str(imm_islands_count)

gam_model <- gam(
  n_visits ~ 
    s(size, k = 4) + 
    rats_f + 
    s(RFB_veg_percent, k = 4) + 
    s(Sula.sula, k = 4) + 
    s(seabird_sp, k = 4) + 
    s(seabird_abundance, k = 4),
  family = nb(),
  data = imm_islands_count,
  method = "REML"
)

draw(gam_model)

smooth_df <- smooth_estimates(gam_model)

smooth_df_long <- smooth_df |>
  pivot_longer(
    cols = c(size, RFB_veg_percent, Sula.sula, seabird_abundance),
    names_to = "variable",
    values_to = ".x"
  ) |>
  filter(variable == gsub("s\\((.*)\\)", "\\1", .smooth))


ggplot(smooth_df_long, aes(x = .x, y = .estimate)) +
  geom_line() +
  geom_ribbon(aes(
    ymin = .estimate - 2 * .se,
    ymax = .estimate + 2 * .se
  ), alpha = 0.2) +
  facet_wrap(~ .smooth, scales = "free_x") +
  theme_minimal()+
  theme(panel.border = element_rect(), panel.grid = element_blank())

coef_df <- data.frame(
  rats_f = c("absent", "present"),
  effect = c(0, coef(gam_model)["rats_fpresent"])
)

coef_df$response <- exp(coef_df$effect)

ggplot(coef_df, aes(x = rats_f, y = response)) +
  geom_point(size = 3)

# plot number of visits to each island by size
ggplot(imm_islands_count, aes(x = size, y = n_visits, col = age)) +
  geom_point() +
  geom_smooth(method = "gam") +
  scale_x_log10()+
  scale_colour_manual(values = c("#841E5AFF", "#DD2C45FF"), labels = c("Immature age 1-2", "Immature age 2-3"), name = "Age")+
  theme_minimal()+
  theme(panel.border = element_rect())+
  labs(x = "Island size", y = "Number of visits")


ggplot(imm_islands_count %>% filter(!is.na(rats_f)), aes(x = rats_f, y = n_visits, col = age, fill = age)) +
  geom_boxplot() +
  #geom_smooth(method = "gam") +
  scale_colour_manual(values = c("#841E5AFF", "#DD2C45FF"), labels = c("Immature age 1-2", "Immature age 2-3"), name = "Age")+
  scale_fill_manual(values = c("#841E5AFF", "#DD2C45FF"), labels = c("Immature age 1-2", "Immature age 2-3"), name = "Age")+
  theme_minimal()+
  theme(panel.border = element_rect())+
  labs(x = "Rat presence", y = "Number of visits")
