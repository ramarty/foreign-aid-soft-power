# Figure 2 - Componenets

# Load Data --------------------------------------------------------------------
df <- readRDS(file.path(data_file_path, "afro_china_data.Rds"))

## Restrict to rounds 2-5
df <- df %>%
  filter(afro.round %in% 2:5)

# Regressions ------------------------------------------------------------------
#### Full Sample
blvs_mult_parties_good.lm <- felm(as.formula(paste0("blvs_mult_parties_good ~ completed_near_china.pl10.30km.bin + planned_near_china.pl10.30km.bin + ",              IVs_china," | ",FEs," | 0 | ", CLUSTER_VAR)), data=df[df$sample_full %in% T,]) 
blvs_mult_parties_create_choice.lm <- felm(as.formula(paste0("blvs_mult_parties_create_choice ~ blvs_mult_parties_create_choice_splag + completed_near_china.pl10.30km.bin + planned_near_china.pl10.30km.bin + ",IVs_china," | ",FEs," | 0 | ", CLUSTER_VAR)), data=df[df$sample_full %in% T,]) 
blvs_ctzn_should_join_any_cso.lm <- felm(as.formula(paste0("blvs_ctzn_should_join_any_cso ~ blvs_ctzn_should_join_any_cso_splag + completed_near_china.pl10.30km.bin + planned_near_china.pl10.30km.bin + ",              IVs_china," | ",FEs," | 0 | ", CLUSTER_VAR)), data=df[df$sample_full %in% T,]) 
blvs_democ_best_system.lm <- felm(as.formula(paste0("blvs_democ_best_system ~ completed_near_china.pl10.30km.bin + planned_near_china.pl10.30km.bin + ",IVs_china," | ",FEs," | 0 | ", CLUSTER_VAR)), data=df[df$sample_full %in% T,]) 
blvs_elec_good.lm <- felm(as.formula(paste0("blvs_elec_good ~ completed_near_china.pl10.30km.bin + planned_near_china.pl10.30km.bin + ",IVs_china," | ",FEs," | 0 | ", CLUSTER_VAR)), data=df[df$sample_full %in% T,]) 

mi_full_df <- bind_rows(
  calc_morans_i(blvs_mult_parties_good.lm),
  calc_morans_i(blvs_mult_parties_create_choice.lm),
  calc_morans_i(blvs_ctzn_should_join_any_cso.lm),
  calc_morans_i(blvs_democ_best_system.lm),
  calc_morans_i(blvs_elec_good.lm)
) %>%
  mutate(sample = "full")

coef_full_df <- bind_rows(
  extract_coefs(blvs_mult_parties_good.lm) %>%
    mutate(model = "Believes\nmultiple\nparties\nare good"),
  
  extract_coefs(blvs_mult_parties_create_choice.lm) %>%
    mutate(model = "Believes\nmultiple\nparties\ncreate choice"),
  
  extract_coefs(blvs_ctzn_should_join_any_cso.lm) %>%
    mutate(model = "Believes\ncitizens\nshould join\nany CSO"),
  
  extract_coefs(blvs_democ_best_system.lm) %>%
    mutate(model = "Believes\ndemocracy\nis best\nsystem"),
  
  extract_coefs(blvs_elec_good.lm) %>%
    mutate(model = "Believes\nelections\nare good")
) %>%
  mutate(subset = "Full Sample")

#### Restricted Sample
blvs_mult_parties_good.lm <- felm(as.formula(paste0("blvs_mult_parties_good ~ blvs_mult_parties_good_splag + completed_near_china.pl10.30km.bin + planned_near_china.pl10.30km.bin + completed_near_usaid.30km.bin + planned_near_usaid.30km.bin + ",              IVs_china," | ",FEs," | 0 | ", CLUSTER_VAR)), data=df[df$sample_restricted %in% T,]) 
blvs_mult_parties_create_choice.lm <- felm(as.formula(paste0("blvs_mult_parties_create_choice ~ blvs_mult_parties_create_choice_splag + completed_near_china.pl10.30km.bin + planned_near_china.pl10.30km.bin + completed_near_usaid.30km.bin + planned_near_usaid.30km.bin + ",IVs_china," | ",FEs," | 0 | ", CLUSTER_VAR)), data=df[df$sample_restricted %in% T,]) 
blvs_ctzn_should_join_any_cso.lm <- felm(as.formula(paste0("blvs_ctzn_should_join_any_cso ~ blvs_ctzn_should_join_any_cso_splag + completed_near_china.pl10.30km.bin + planned_near_china.pl10.30km.bin + completed_near_usaid.30km.bin + planned_near_usaid.30km.bin + ",              IVs_china," | ",FEs," | 0 | ", CLUSTER_VAR)), data=df[df$sample_restricted %in% T,]) 
blvs_democ_best_system.lm <- felm(as.formula(paste0("blvs_democ_best_system ~ blvs_democ_best_system_splag + completed_near_china.pl10.30km.bin + planned_near_china.pl10.30km.bin + completed_near_usaid.30km.bin + planned_near_usaid.30km.bin + ",IVs_china," | ",FEs," | 0 | ", CLUSTER_VAR)), data=df[df$sample_restricted %in% T,]) 
blvs_elec_good.lm <- felm(as.formula(paste0("blvs_elec_good ~ blvs_elec_good_splag + completed_near_china.pl10.30km.bin + planned_near_china.pl10.30km.bin + completed_near_usaid.30km.bin + planned_near_usaid.30km.bin + ",IVs_china," | ",FEs," | 0 | ", CLUSTER_VAR)), data=df[df$sample_restricted %in% T,]) 

mi_restricted_df <- bind_rows(
  calc_morans_i(blvs_mult_parties_good.lm),
  calc_morans_i(blvs_mult_parties_create_choice.lm),
  calc_morans_i(blvs_ctzn_should_join_any_cso.lm),
  calc_morans_i(blvs_democ_best_system.lm),
  calc_morans_i(blvs_elec_good.lm)
) %>%
  mutate(sample = "restricted")

coef_restricted_df <- bind_rows(
  extract_coefs(blvs_mult_parties_good.lm) %>%
    mutate(model = "Believes\nmultiple\nparties\nare good"),
  
  extract_coefs(blvs_mult_parties_create_choice.lm) %>%
    mutate(model = "Believes\nmultiple\nparties\ncreate choice"),
  
  extract_coefs(blvs_ctzn_should_join_any_cso.lm) %>%
    mutate(model = "Believes\ncitizens\nshould join\nany CSO"),
  
  extract_coefs(blvs_democ_best_system.lm) %>%
    mutate(model = "Believes\ndemocracy\nis best\nsystem"),
  
  extract_coefs(blvs_elec_good.lm) %>%
    mutate(model = "Believes\nelections\nare good")
) %>%
  mutate(subset = "Restricted Sample")

#### Append Coefficients
coef_df <- bind_rows(coef_full_df,
                     coef_restricted_df) %>%
  dplyr::mutate(model = model %>%
           factor(levels = rev(c("Believes\nmultiple\nparties\nare good",
                                 "Believes\nmultiple\nparties\ncreate choice",
                             "Believes\ncitizens\nshould join\nany CSO",
                             "Believes\ndemocracy\nis best\nsystem",
                             "Believes\nelections\nare good"))))

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
                file_name = "figure_02_components.png")

# Morans I ---------------------------------------------------------------------
bind_rows(mi_full_df,
          mi_restricted_df) %>%
  mutate(figure = "fig_02_components") %>%
  write.csv(file.path(data_file_path, "morans_i", "mi_02_components.csv"), row.names = F)





  