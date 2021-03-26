# Figures: Appendix 2

# ** 2008/2009/Planned-Completed ===============================================

df_results <- readRDS(file.path(data_results_file_path, 
                                paste0("models_type_", 1), 
                                "coefficients",
                                "coefficients.Rds"))

df_results <- df_results %>%
  filter(is.na(infrastructure))

dv_lookup <- df_results %>%
  dplyr::select(dv, dv_clean) %>%
  distinct()

# Figure A4 --------------------------------------------------------------------
df_results %>%
  filter(buffer %in% 30,
         planned_year %in% 2008,
         dv %in% c("china_positive_influence_index",
                   "china.best.dev.model",
                   "usa.best.dev.model")) %>%
  mutate(dv_clean = dv_clean %>% 
           factor(levels = c("Belives\nChinese presence\nis positive\n(index)",
                             "Believes\nChinese model\nis best",
                             "Believes\nUS model\nis best") %>% rev())) %>%
  make_plot_all(height = 4.5,
                width = 8.5,
                file_name = "figure_a4.png",
                x_axis_breaks = 4)

# Figure A5 --------------------------------------------------------------------
df_results %>%
  filter(buffer %in% 30,
         planned_year %in% 2008,
         dv %in% c("formcolnpower.best.dev.model")) %>%
  make_plot_all(height = 3,
                width = 8.5,
                file_name = "figure_a5.png",
                x_axis_breaks = 3)

# Figure A6 --------------------------------------------------------------------
df_results %>%
  filter(buffer %in% 30,
         planned_year %in% 2009,
         dv %in% c("china_positive_influence_index",
                   "china.best.dev.model",
                   "usa.best.dev.model")) %>%
  mutate(dv_clean = dv_clean %>% 
           factor(levels = c("Belives\nChinese presence\nis positive\n(index)",
                             "Believes\nChinese model\nis best",
                             "Believes\nUS model\nis best") %>% rev())) %>%
  make_plot_all(height = 4.5,
                width = 8.5,
                file_name = "figure_a6.png",
                x_axis_breaks = 4)

# Figure A7 --------------------------------------------------------------------
df_results %>%
  filter(buffer %in% 30,
         planned_year %in% 2009,
         dv %in% c("formcolnpower.best.dev.model")) %>%
  make_plot_all(height = 3,
                width = 8.5,
                file_name = "figure_a7.png",
                x_axis_breaks = 3)

# Figure A8 ---------------------------------------------------------------------
df_results %>%
  filter(buffer %in% 30,
         subset %in% c("full", "restricted"),
         dv %in% c("lib_dem_val_index"),
         plcompltd %in% T) %>%
  make_plot_all(height = 3,
                width = 8.5,
                file_name = "figure_a10.png", 
                x_axis_breaks = 3)

# ** ADM1 FE ===================================================================

# Figure A9 --------------------------------------------------------------------
df_results <- readRDS(file.path(data_results_file_path, 
                                paste0("models_type_", 4), 
                                "coefficients",
                                "coefficients.Rds"))

#df_results <- df_results %>%
#  filter(is.na(infrastructure))

df_results %>%
  filter(buffer %in% 30,
         subset %in% c("full", "restricted"),
         dv %in% c("lib_dem_val_index"),
         plcompltd %in% F) %>%
  make_plot_all(height = 3,
                width = 8.5,
                file_name = "figure_a11.png", 
                x_axis_breaks = 3)

# ** Varrying Buffers ==========================================================

# Figure A10 -------------------------------------------------------------------
df_results <- readRDS(file.path(data_results_file_path, 
                                paste0("models_type_", 1), 
                                "coefficients",
                                "coefficients.Rds"))

df_results <- df_results %>%
  filter(is.na(infrastructure))

df_results %>%
  filter(planned_year %in% 2010,
         dv %in% c("china_positive_influence_index",
                   "china.best.dev.model",
                   "usa.best.dev.model")) %>%
  mutate(dv_clean = dv_clean %>% 
           factor(levels = c("Belives\nChinese presence\nis positive\n(index)",
                             "Believes\nChinese model\nis best",
                             "Believes\nUS model\nis best"))) %>%
  make_fig_full_restr(nrow_figure = 1,
                      ncol_arrange = 2,
                      height = 7,
                      width = 18,
                      file_name = "figure_a12.png", 
                      x_axis_breaks = 3)

# Figure A11 -------------------------------------------------------------------
df_results %>%
  filter(subset %in% c("full", "restricted"),
         dv %in% c("lib_dem_val_index"),
         plcompltd %in% F) %>%
  make_fig_full_restr(nrow_figure = 1,
                      ncol_arrange = 2,
                      height = 7,
                      width = 11,
                      file_name = "figure_a13.png", 
                      x_axis_breaks = 4)

# Figure A12 -------------------------------------------------------------------
df_results %>%
  filter(dv %in% c("formcolnpower.best.dev.model"),
         planned_year %in% 2010) %>%
  make_fig_full_restr(nrow_figure = 1,
                      ncol_arrange = 2,
                      height = 7,
                      width = 11,
                      file_name = "figure_a14.png", 
                      x_axis_breaks = 4)

# ** Additional Controls =======================================================
df_results <- readRDS(file.path(data_results_file_path, 
                                paste0("models_type_", 3), 
                                "coefficients",
                                "coefficients.Rds"))

df_results <- df_results %>%
  filter(is.na(infrastructure))

# Figure A13 -------------------------------------------------------------------
df_results %>%
  filter(buffer %in% 30,
         planned_year %in% 2010,
         dv %in% c("china_positive_influence_index",
                   "china.best.dev.model",
                   "usa.best.dev.model")) %>%
  mutate(dv_clean = dv_clean %>% 
           factor(levels = c("Belives\nChinese presence\nis positive\n(index)",
                             "Believes\nChinese model\nis best",
                             "Believes\nUS model\nis best") %>% rev())) %>%
  make_plot_all(height = 4.5,
                width = 8.5,
                file_name = "figure_a15.png", 
                x_axis_breaks = 4)

# Figure A14 -------------------------------------------------------------------
df_results %>%
  filter(buffer %in% 30,
         subset %in% c("full", "restricted"),
         dv %in% c("lib_dem_val_index"),
         plcompltd %in% F) %>%
  make_plot_all(height = 3,
                width = 8.5,
                file_name = "figure_a16.png", 
                x_axis_breaks = 3)

# Figure A15 --------------------------------------------------------------------
df_results %>%
  filter(buffer %in% 30,
         planned_year %in% 2010,
         dv %in% c("formcolnpower.best.dev.model")) %>%
  make_plot_all(height = 3,
                width = 8.5,
                file_name = "figure_a17.png", 
                x_axis_breaks = 3)




