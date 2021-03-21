# Figures: Appendix 2

# ** Model Type 1 ==============================================================

df_results <- readRDS(file.path(data_results_file_path, 
                                paste0("models_type_", 1), 
                                "coefficients",
                                "coefficients.Rds"))

dv_lookup <- df_results %>%
  dplyr::select(dv, dv_clean) %>%
  distinct()

# Figure A16 -------------------------------------------------------------------
df_results %>%
  filter(buffer %in% 30,
         plcompltd %in% F,
         is.na(infrastructure),
         dv %in% c("china.help.country",
                   "usa.help.country")) %>%
  make_plot_all(height = 4,
                width = 7,
                file_name = "figure_a16.png",
                x_axis_breaks = 5)

# Figure A17 -------------------------------------------------------------------
df_results %>%
  filter(buffer %in% 30,
         planned_year %in% 2010,
         infrastructure %in% T,
         dv %in% c("china_positive_influence_index",
                   "china.best.dev.model",
                   "usa.best.dev.model")) %>%
  mutate(dv_clean = dv_clean %>% 
           factor(levels = c("Belives\nChinese presence\nis positive\n(index)",
                             "Believes\nChinese model\nis best",
                             "Believes\nUS model\nis best") %>% rev())) %>%
  mutate(var = var %>% 
           factor(levels = c("Chinese Aid Planned [Non-Infrastructure]",
                             "Chinese Aid Completed [Non-Infrastructure]",
                             "Chinese Aid Planned [Infrastructure]",
                             "Chinese Aid Completed [Infrastructure]") %>% rev())) %>%
  make_plot_all(height = 4.5,
                width = 9,
                file_name = "figure_a17.png",
                x_axis_breaks = 4)

# Figure A18 -------------------------------------------------------------------
# df_results %>%
#   filter(buffer %in% 30,
#          planned_year %in% 2010,
#          is.na(infrastructure),
#          dv %in% c("china_influential_index",
#                    "china.most.influence",
#                    "usa.most.influence")) %>%
#   mutate(dv_clean = dv_clean %>% 
#            factor(levels = c("Believes\nChina is\ninfluencial\n(index)",
#                              "Believes\nChinese model\nis most\ninfluential",
#                              "Believes\nUS model\nis most\ninfluential") %>% rev())) %>%
#   make_plot_all(height = 4.5,
#                 width = 8.5,
#                 file_name = "figure_a18.png",
#                 x_axis_breaks = 4)

# Figure A19 -------------------------------------------------------------------
df_results %>%
  filter(buffer %in% 30,
         is.na(infrastructure),
         subset %in% c("full", "restricted"),
         dv %in% c("blvs_mult_parties_good",
                   "blvs_mult_parties_create_choice",
                   "blvs_ctzn_should_join_any_cso",
                   "blvs_democ_best_system",
                   "blvs_elec_good"),
         plcompltd %in% F) %>%
  make_plot_all(height = 7,
                width = 8.75,
                file_name = "figure_a19.png",
                x_axis_breaks = 4)

# Figure A20 -------------------------------------------------------------------
# df_results %>%
#   filter(buffer %in% 30,
#          planned_year %in% 2010,
#          is.na(infrastructure),
#          dv %in% c("formcolnpower.most.influence")) %>%
#   make_plot_all(height = 3,
#                 width = 8.5,
#                 file_name = "figure_a20.png",
#                 x_axis_breaks = 3)

# Figure A21 -------------------------------------------------------------------
df_results %>%
  filter(buffer %in% 30,
         subset %in% c("restrictedusuk"),
         dv %in% c("lib_dem_val_index")) %>%
  make_plot_all(height = 3,
                width = 6.75,
                file_name = "figure_a21.png")
 

