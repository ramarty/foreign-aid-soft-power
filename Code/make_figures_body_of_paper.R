# Figures: Body of Paper

df_results <- readRDS(file.path(dropbox_file_path, 
                                paste0("models_type_", 1), 
                                "coefficients",
                                "coefficients.Rds"))

df_results <- df_results %>%
  filter(buffer == 30) %>%
  filter(is.na(planned_year) | planned_year %in% 2010)

dv_lookup <- df_results %>%
  dplyr::select(dv, dv_clean) %>%
  distinct()

# Figure 1 ---------------------------------------------------------------------
df_results %>%
  filter(dv %in% c("china_positive_influence_index",
                   "china.best.dev.model",
                   "usa.best.dev.model")) %>%
  mutate(dv_clean = dv_clean %>% 
           factor(levels = c("Belives\nChinese presence\nis positive\n(index)",
                             "Believes\nChinese model\nis best",
                             "Believes\nUS model\nis best") %>% rev())) %>%
  make_plot_all(height = 6,
                width = 10,
                file_name = "figure_01.png")

# Figure 2 ---------------------------------------------------------------------
df_results %>%
  filter(subset %in% c("full", "restricted"),
         dv %in% c("lib_dem_val_index"),
         plcompltd %in% F) %>%
  make_plot_all(height = 6,
                width = 10,
                file_name = "figure_02.png")

# Figure 3 ---------------------------------------------------------------------
df_results %>%
  filter(dv %in% c("formcolnpower.best.dev.model")) %>%
  make_plot_all(height = 6,
                width = 10,
                file_name = "figure_03.png")

# Figure 4 ---------------------------------------------------------------------
df_results %>%
  filter(subset %in% "full",
         dv %in% c("posimage_businessinvetment", 
                   "posimage_chinesepeople", 
                   "posimage_infordevinvetment", 
                   "posimage_noninterference", 
                   "posimage_productcost", 
                   "posimage_supportinintlaffiars")) %>%
  mutate(dv_clean = dv_clean %>% str_replace_all("Positive Image:\n", "")) %>%
  make_plot_all(height = 9,
                width = 10,
                file_name = "figure_04.png")

# Figure 5 ---------------------------------------------------------------------
df_results %>%
  filter(subset %in% "full",
         dv %in% c("negimage_chinesecitizenbehavior", 
                   "negimage_cooperateundemocratic", 
                   "negimage_landgrabbing", 
                   "negimage_productquality", 
                   "negimage_resourceextraction", 
                   "negimage_takingjobsbusiness")) %>%
  mutate(dv_clean = dv_clean %>% str_replace_all("Negative Image:\n", "")) %>%
  make_plot_all(height = 9,
                width = 10,
                file_name = "figure_05.png")




