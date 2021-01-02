# Figures: Appendix 2

# ** 2008/2009/Planned-Completed ===============================================

df_results <- readRDS(file.path(dropbox_file_path, 
                                paste0("models_type_", 1), 
                                "coefficients",
                                "coefficients.Rds"))

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
  make_plot_all(height = 6,
                width = 10,
                file_name = "figure_a4.png")

# Figure A5 --------------------------------------------------------------------
df_results %>%
  filter(buffer %in% 30,
         planned_year %in% 2008,
         dv %in% c("formcolnpower.best.dev.model")) %>%
  make_plot_all(height = 6,
                width = 10,
                file_name = "figure_a5.png")

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
  make_plot_all(height = 6,
                width = 10,
                file_name = "figure_a6.png")

# Figure A7 --------------------------------------------------------------------
df_results %>%
  filter(buffer %in% 30,
         planned_year %in% 2009,
         dv %in% c("formcolnpower.best.dev.model")) %>%
  make_plot_all(height = 6,
                width = 10,
                file_name = "figure_a7.png")

# Figure A8 ---------------------------------------------------------------------
df_results %>%
  filter(buffer %in% 30,
         subset %in% c("full", "restricted"),
         dv %in% c("lib_dem_val_index"),
         plcompltd %in% T) %>%
  make_plot_all(height = 6,
                width = 10,
                file_name = "figure_a8.png")

# ** ADM1 FE ===================================================================

# Figure A9 --------------------------------------------------------------------
df_results <- readRDS(file.path(dropbox_file_path, 
                                paste0("models_type_", 4), 
                                "coefficients",
                                "coefficients.Rds"))

df_results %>%
  filter(buffer %in% 30,
         subset %in% c("full", "restricted"),
         dv %in% c("lib_dem_val_index"),
         plcompltd %in% F) %>%
  make_plot_all(height = 6,
                width = 10,
                file_name = "figure_a9.png")

# ** Varrying Buffers ==========================================================

# Figure A10 -------------------------------------------------------------------
df_results <- readRDS(file.path(dropbox_file_path, 
                                paste0("models_type_", 1), 
                                "coefficients",
                                "coefficients.Rds"))

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
                      height = 6,
                      width = 16,
                      file_name = "figure_a10.png")

# Figure A11 -------------------------------------------------------------------
df_results %>%
  filter(subset %in% c("full", "restricted"),
         dv %in% c("lib_dem_val_index"),
         plcompltd %in% F) %>%
  make_fig_full_restr(nrow_figure = 1,
                      ncol_arrange = 2,
                      height = 5,
                      width = 10,
                      file_name = "figure_a11.png")

# Figure A12 -------------------------------------------------------------------
df_results %>%
  filter(dv %in% c("formcolnpower.best.dev.model")) %>%
  make_fig_full_restr(nrow_figure = 1,
                      ncol_arrange = 2,
                      height = 6,
                      width = 12,
                      file_name = "figure_a12.png")

# ** Additional Controls =======================================================
df_results <- readRDS(file.path(dropbox_file_path, 
                                paste0("models_type_", 3), 
                                "coefficients",
                                "coefficients.Rds"))

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
  make_plot_all(height = 6,
                width = 10,
                file_name = "figure_a13.png")

# Figure A14 -------------------------------------------------------------------
df_results %>%
  filter(buffer %in% 30,
         subset %in% c("full", "restricted"),
         dv %in% c("lib_dem_val_index"),
         plcompltd %in% F) %>%
  make_plot_all(height = 6,
                width = 10,
                file_name = "figure_a14.png")

# Figure A15 --------------------------------------------------------------------
df_results %>%
  filter(buffer %in% 30,
         planned_year %in% 2010,
         dv %in% c("formcolnpower.best.dev.model")) %>%
  make_plot_all(height = 6,
                width = 10,
                file_name = "figure_a15.png")




