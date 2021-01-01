# Independent Variable Summary Stats

library(gt)
library(paletteer)

min_color <- "#e6f2ff"
max_color <- "#0073e6"

# Load Data --------------------------------------------------------------------
df <- readRDS(file.path(data_file_path, "afro_china_data.Rds"))

df <- df %>%
  filter(!is.na(age),
         !is.na(muslim),
         !is.na(urban),
         !is.na(male),
         !is.na(distance_capital),
         !is.na(in_leader_adm1)) %>%
  filter(sample_full %in% T)

df <- df %>%
  dplyr::select(iso, 
                planned_near_china.pl10.30km.bin,
                completed_near_china.pl10.30km.bin) %>%
  group_by(iso) %>%
  dplyr::summarise(planned_near_china.pl10.30km.bin = planned_near_china.pl10.30km.bin %>% sum(),
                   completed_near_china.pl10.30km.bin = completed_near_china.pl10.30km.bin %>% sum()) %>% 
  dplyr::rename(ISO = iso,
                "N planned Chinese projects" = planned_near_china.pl10.30km.bin,
                "N completed Chinese projects" = completed_near_china.pl10.30km.bin)


df %>%
  gt() %>%
  cols_width(
    everything() ~ px(130)
  ) %>%
  cols_align(
    align = "center"#,
  ) %>%
  
  data_color(
    columns = vars("N planned Chinese projects"),
    colors = scales::col_numeric(
      palette = c(
        min_color, max_color),
      domain = c(min(df$`N planned Chinese projects`), 
                 max(df$`N planned Chinese projects`)))
  ) %>%
  
  data_color(
    columns = vars("N completed Chinese projects"),
    colors = scales::col_numeric(
      palette = c(
        min_color, max_color),
      domain = c(min(df$`N completed Chinese projects`), 
                 max(df$`N completed Chinese projects`)))
  ) %>%
  
  gtsave(file.path(figures_file_path, "figure_a2.png"))


