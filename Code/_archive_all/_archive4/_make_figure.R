# Figure 2

# Load Data --------------------------------------------------------------------
for(model_type in 1:4){
  df_results <- readRDS(file.path(dropbox_file_path, 
                                  paste0("models_type_", model_type), 
                                  "coefficients",
                                  "coefficients.Rds"))
  
  figures_file_path <- file.path(dropbox_file_path, 
                                 paste0("models_type_", model_type),
                                 "figures")
  
  lookup <- df_results %>%
    dplyr::select(dv, dv_clean) %>%
    distinct()
  
  # Figure 1 ---------------------------------------------------------------------
  df_results %>%
    filter(dv %in% c("china_influential_index",
                     "china_positive_influence_index")) %>%
    make_fig_full_restr(nrow_figure = 1,
                        ncol_arrange = 2,
                        height = 5,
                        width = 12,
                        file_name = "figure_01.png")
  
  # Figure 1 - Components --------------------------------------------------------
  df_results %>%
    filter(dv %in% c("china.most.influence", 
                     "usa.most.influence", 
                     "china.best.dev.model",
                     "usa.best.dev.model")) %>%
    make_fig_full_restr(nrow_figure = 1,
                        ncol_arrange = 1,
                        height = 10,
                        width = 12,
                        file_name = "figure_01_components.png")
  
  # Figure 2 ---------------------------------------------------------------------
  df_results %>%
    filter(subset %in% c("full", "restricted"),
           dv %in% c("lib_dem_val_index"),
           plcompltd %in% F) %>%
    make_fig_full_restr(nrow_figure = 1,
                        ncol_arrange = 2,
                        height = 5,
                        width = 12,
                        file_name = "figure_02.png")
  
  # Figure 2 - Components ---------------------------------------------------------
  df_results %>%
    filter(subset %in% c("full", "restricted"),
           dv %in% c("blvs_ctzn_should_join_any_cso", 
                     "blvs_democ_best_system", 
                     "blvs_elec_good", 
                     "blvs_mult_parties_create_choice", 
                     "blvs_mult_parties_good"),
           plcompltd %in% F) %>%
    make_fig_full_restr(nrow_figure = 1,
                        ncol_arrange = 1,
                        height = 12,
                        width = 12.5, 
                        legend_pos = "right",
                        file_name = "figure_02_components.png")
  
  # Figure 2 - plcompltd ---------------------------------------------------------------------
  df_results %>%
    filter(subset %in% c("full", "restricted"),
           dv %in% c("lib_dem_val_index"),
           plcompltd %in% T) %>%
    make_fig_full_restr(nrow_figure = 1,
                        ncol_arrange = 2,
                        height = 5,
                        width = 12,
                        file_name = "figure_02_plannedCompleted.png")
  
  # Figure 2 - Components - plcompltd ---------------------------------------------------------
  df_results %>%
    filter(subset %in% c("full", "restricted"),
           dv %in% c("blvs_ctzn_should_join_any_cso", 
                     "blvs_democ_best_system", 
                     "blvs_elec_good", 
                     "blvs_mult_parties_create_choice", 
                     "blvs_mult_parties_good"),
           plcompltd %in% T) %>%
    make_fig_full_restr(nrow_figure = 1,
                        ncol_arrange = 1,
                        height = 12,
                        width = 12.5, 
                        legend_pos = "right",
                        file_name = "figure_02_plannedCompleted_components.png")
  
  # Figure 3a --------------------------------------------------------------------
  df_results %>%
    filter(dv %in% c("formcolnpower.most.influence")) %>%
    make_fig_full_restr(nrow_figure = 1,
                        ncol_arrange = 2,
                        height = 6,
                        width = 12,
                        file_name = "figure_03a.png")
  
  # Figure 3b --------------------------------------------------------------------
  df_results %>%
    filter(dv %in% c("formcolnpower.best.dev.model")) %>%
    make_fig_full_restr(nrow_figure = 1,
                        ncol_arrange = 2,
                        height = 6,
                        width = 12,
                        file_name = "figure_03b.png")
  
  # Figure 4 ---------------------------------------------------------------------
  df_results %>%
    filter(subset %in% "restrictedusuk",
           dv %in% c("lib_dem_val_index")) %>%
    make_fig(nrow_figure = 1,
             height = 6.5,
             width = 6,
             file_name = "figure_04.png")
  
  # Figure 4 - Components --------------------------------------------------------
  df_results %>%
    filter(subset %in% "restrictedusuk",
           dv %in% c("blvs_ctzn_should_join_any_cso", 
                     "blvs_democ_best_system", 
                     "blvs_elec_good", 
                     "blvs_mult_parties_create_choice", 
                     "blvs_mult_parties_good")) %>%
    make_fig(nrow_figure = 1,
             height = 6.5,
             width = 12,
             legend_pos = "bottom",
             file_name = "figure_04_components.png")
  
  # Figure 5 ---------------------------------------------------------------------
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
             file_name = "figure_05.png")
  
  # Figure 6 ---------------------------------------------------------------------
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
             file_name = "figure_06.png")
  
  # Figure 7 ---------------------------------------------------------------------
  df_results %>%
    filter(subset %in% "full",
           dv %in% c("china_dontknow_index")) %>%
    make_fig(nrow_figure = 1,
             height = 6.5,
             width = 5,
             legend_pos = "bottom",
             file_name = "figure_07.png")
  
  # Figure 7 ---------------------------------------------------------------------
  df_results %>%
    filter(subset %in% "full",
           dv %in% c("china.influence.econ.activity_DONTKNOW",
                     "china.econpol.influence.positive_DONTKNOW",
                     "china.aid.good.job.meet.country.needs_DONTKNOW")) %>%
    make_fig(nrow_figure = 1,
             height = 6.5,
             width = 10,
             legend_pos = "bottom",
             file_name = "figure_07_components.png")
}


