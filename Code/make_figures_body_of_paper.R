# Figures: Body of Paper

df_results <- readRDS(file.path(dropbox_file_path, 
                                paste0("models_type_", 1), 
                                "coefficients",
                                "coefficients.Rds"))

dv_lookup <- df_results %>%
  dplyr::select(dv, dv_clean) %>%
  distinct()

# Figure 1 ---------------------------------------------------------------------
df_results %>%
  filter(dv %in% c("china_positive_influence_index",
                   "china.best.dev.model",
                   "usa.best.dev.model")) %>%
  make_fig_full_restr(nrow_figure = 1,
                      ncol_arrange = 2,
                      height = 6,
                      width = 16,
                      file_name = "figure_01.png")

# Figure 2 ---------------------------------------------------------------------
df_results %>%
  filter(subset %in% c("full", "restricted"),
         dv %in% c("lib_dem_val_index"),
         plcompltd %in% F) %>%
  make_fig_full_restr(nrow_figure = 1,
                      ncol_arrange = 2,
                      height = 5,
                      width = 10,
                      file_name = "figure_02.png")

# Figure 3 ---------------------------------------------------------------------
df_results %>%
  filter(dv %in% c("formcolnpower.best.dev.model")) %>%
  make_fig_full_restr(nrow_figure = 1,
                      ncol_arrange = 2,
                      height = 6,
                      width = 12,
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
  make_fig(nrow_figure = 1,
           height = 6.5,
           width = 12,
           legend_pos = "bottom",
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
  make_fig(nrow_figure = 1,
           height = 6.5,
           width = 12,
           legend_pos = "bottom",
           file_name = "figure_05.png")




