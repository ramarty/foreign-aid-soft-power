# Independent Variable Summary Stats

library(gt)
library(paletteer)

min_color <- "#e6f2ff"
max_color <- "#0073e6"

# Load Data --------------------------------------------------------------------
df <- readRDS(file.path(data_file_path, "afro_china_data.Rds"))

df <- df %>%
  dplyr::select(iso, age, muslim, urban, male, distance_capital, in_leader_adm1, 
                riotsprotests_china_before1stplanned_30km_average) %>%
  group_by(iso) %>%
  summarise_all(mean, na.rm=T) %>%
  mutate(age = age %>% round(2),
         muslim = muslim %>% round(2),
         urban = urban %>% round(2),
         male = male %>% round(3),
         distance_capital = distance_capital %>% round(2),
         in_leader_adm1 = in_leader_adm1 %>% round(2),
         riotsprotests_china_before1stplanned_30km_average = riotsprotests_china_before1stplanned_30km_average %>% round(2)) %>%
  
  dplyr::rename(ISO = iso,
                Age = age,
                Muslim = muslim,
                Urban = urban,
                Male = male,
                "Distance Capital (km)" = distance_capital,
                "In Leader ADM1" = in_leader_adm1,
                "Average Annual Riots/Protests before First Planned Chinese Project within 30km" = riotsprotests_china_before1stplanned_30km_average)



df %>%
  gt() %>%
  cols_width(
    vars("Average Annual Riots/Protests before First Planned Chinese Project within 30km") ~ px(130),
    everything() ~ px(80)
  ) %>%
  cols_align(
    align = "center"#,
  ) %>%
  
  data_color(
    columns = vars(Age),
    colors = scales::col_numeric(
      palette = c(
        min_color, max_color),
      domain = c(min(df$Age), max(df$Age)))
  ) %>%
  
  data_color(
    columns = vars("Muslim", "Urban", "Male", "In Leader ADM1"),
    colors = scales::col_numeric(
      palette = c(
        min_color, max_color),
      domain = c(0, 1))
  ) %>%
  
  data_color(
    columns = vars("Distance Capital (km)"),
    colors = scales::col_numeric(
      palette = c(
        min_color, max_color),
      domain = c(min(df$`Distance Capital (km)`), 
                 max(df$`Distance Capital (km)`)))
  ) %>%
  
  data_color(
    columns = vars("Average Annual Riots/Protests before First Planned Chinese Project within 30km"),
    colors = scales::col_numeric(
      palette = c(
        min_color, max_color),
      domain = c(min(df$`Average Annual Riots/Protests before First Planned Chinese Project within 30km`), 
                 max(df$`Average Annual Riots/Protests before First Planned Chinese Project within 30km`)))
  ) %>%
  gtsave(file.path(figures_file_path, "indep_var_mean.png"))


