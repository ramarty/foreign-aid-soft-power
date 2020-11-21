# Figure 1

# TODO: usaid - no .pl10??

# Load Data --------------------------------------------------------------------
df <- readRDS(file.path(project_file_path, "Data", "afro_china_data.Rds"))

# Regressions ------------------------------------------------------------------
#### Full Sample
china_china.most.influence.lm <- felm(as.formula(paste0("china.most.influence ~ completed_near_china.pl10.30km.bin + planned_near_china.pl10.30km.bin + ",              IVs_china," | iso + afro.round | 0 | townvill")), data=df[df$sample_full %in% T,]) 
china_usa.most.influence.lm <- felm(as.formula(paste0("usa.most.influence ~ completed_near_china.pl10.30km.bin + planned_near_china.pl10.30km.bin + ",IVs_china," | iso + afro.round | 0 | townvill")), data=df[df$sample_full %in% T,]) 
china_china.best.dev.model.lm <- felm(as.formula(paste0("china.best.dev.model ~ completed_near_china.pl10.30km.bin + planned_near_china.pl10.30km.bin + ",              IVs_china," | iso + afro.round | 0 | townvill")), data=df[df$sample_full %in% T,]) 
china_usa.best.dev.model.lm <- felm(as.formula(paste0("usa.best.dev.model ~ completed_near_china.pl10.30km.bin + planned_near_china.pl10.30km.bin + ",IVs_china," | iso + afro.round | 0 | townvill")), data=df[df$sample_full %in% T,]) 

coef_full_df <- bind_rows(
  extract_coefs(china_china.most.influence.lm) %>%
    mutate(model = "Believes\nChinese model\nis most\ninfluential"),
  
  extract_coefs(china_usa.most.influence.lm) %>%
    mutate(model = "Believes\nUS model\nis most\ninfluential"),
  
  extract_coefs(china_china.best.dev.model.lm) %>%
    mutate(model = "Believes\nChinese model\nis best"),
  
  extract_coefs(china_usa.best.dev.model.lm) %>%
    mutate(model = "Believes\nUS model\nis best")
) %>%
  mutate(subset = "Full Sample")

#### Restricted Sample
chinausa_china.most.influence.lm <- felm(as.formula(paste0("china.most.influence ~ completed_near_china.pl10.30km.bin + planned_near_china.pl10.30km.bin + completed_near_usaid.30km.bin + planned_near_usaid.30km.bin + ",               IVs_china_usaid," | iso + afro.round | 0 | townvill")), data=df[df$sample_restricted %in% T,]) 
chinausa_usa.most.influence.lm <- felm(as.formula(paste0("usa.most.influence ~ completed_near_china.pl10.30km.bin + planned_near_china.pl10.30km.bin + completed_near_usaid.30km.bin + planned_near_usaid.30km.bin + ", IVs_china_usaid," | iso + afro.round | 0 | townvill")), data=df[df$sample_restricted %in% T,]) 
chinausa_china.best.dev.model.lm <- felm(as.formula(paste0("china.best.dev.model ~ completed_near_china.pl10.30km.bin + planned_near_china.pl10.30km.bin + completed_near_usaid.30km.bin + planned_near_usaid.30km.bin + ",               IVs_china_usaid," | iso + afro.round | 0 | townvill")), data=df[df$sample_restricted %in% T,]) 
chinausa_usa.best.dev.model.lm <- felm(as.formula(paste0("usa.best.dev.model ~ completed_near_china.pl10.30km.bin + planned_near_china.pl10.30km.bin + completed_near_usaid.30km.bin + planned_near_usaid.30km.bin + ", IVs_china_usaid," | iso + afro.round | 0 | townvill")), data=df[df$sample_restricted %in% T,]) 

coef_restricted_df <- bind_rows(
  extract_coefs(chinausa_china.most.influence.lm) %>%
    mutate(model = "Believes\nChinese model\nis most\ninfluential"),
  
  extract_coefs(chinausa_usa.most.influence.lm) %>%
    mutate(model = "Believes\nUS model\nis most\ninfluential"),
  
  extract_coefs(chinausa_china.best.dev.model.lm) %>%
    mutate(model = "Believes\nChinese model\nis best"),
  
  extract_coefs(chinausa_usa.best.dev.model.lm) %>%
    mutate(model = "Believes\nUS model\nis best")
) %>%
  mutate(subset = "Restricted Sample")

#### Append Coefficients
coef_df <- bind_rows(coef_full_df,
                     coef_restricted_df) %>%
  dplyr::mutate(model = model %>%
           factor(levels = rev(c("Believes\nChinese model\nis most\ninfluential",
                             "Believes\nUS model\nis most\ninfluential",
                             "Believes\nChinese model\nis best",
                             "Believes\nUS model\nis best"))))

# Figure -----------------------------------------------------------------------
coef_df %>%
  mutate(var = var %>% 
           str_replace("completed_near_china.pl10.30km.bin", 
                                   "Chinese Aid Completed") %>%
           str_replace("planned_near_china.pl10.30km.bin", 
                       "Chinese Aid Planned") %>%
           str_replace("completed_near_usaid.30km.bin", 
                       "USA Aid Completed") %>%
           str_replace("planned_near_usaid.30km.bin", 
                       "USA Aid Planned")) %>%
  filter(var %in% c("Chinese Aid Completed",
                    "Chinese Aid Planned",
                    "USA Aid Completed",
                    "USA Aid Planned")) %>%
  make_plot_all(height = 7,
                width = 10,
                file_name = "figure_01_components.png")



  