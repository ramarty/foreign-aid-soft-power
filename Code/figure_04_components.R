# Figure 2 - Componenets

# Load Data --------------------------------------------------------------------
df <- readRDS(file.path(project_file_path, "Data", "afro_china_data.Rds"))

## Restrict to rounds 2-5
# df <- df %>%
#   filter(afro.round %in% 2:5)

# Regressions ------------------------------------------------------------------
#### Restricted Sample
blvs_mult_parties_good.lm <- felm(as.formula(paste0("blvs_mult_parties_good ~ completed_near_ukaid.30km.bin + planned_near_ukaid.30km.bin + completed_near_usaid.30km.bin + planned_near_usaid.30km.bin + ",              IVs_china," | iso + afro.round | 0 | townvill")), data=df[df$sample_restricted_uk %in% T,]) 
blvs_mult_parties_create_choice.lm <- felm(as.formula(paste0("blvs_mult_parties_create_choice ~ completed_near_ukaid.30km.bin + planned_near_ukaid.30km.bin + completed_near_usaid.30km.bin + planned_near_usaid.30km.bin + ",IVs_china," | iso + afro.round | 0 | townvill")), data=df[df$sample_restricted_uk %in% T,]) 
blvs_ctzn_should_join_any_cso.lm <- felm(as.formula(paste0("blvs_ctzn_should_join_any_cso ~ completed_near_ukaid.30km.bin + planned_near_ukaid.30km.bin + completed_near_usaid.30km.bin + planned_near_usaid.30km.bin + ",              IVs_china," | iso + afro.round | 0 | townvill")), data=df[df$sample_restricted_uk %in% T,]) 
blvs_democ_best_system.lm <- felm(as.formula(paste0("blvs_democ_best_system ~ completed_near_ukaid.30km.bin + planned_near_ukaid.30km.bin + completed_near_usaid.30km.bin + planned_near_usaid.30km.bin + ",IVs_china," | iso + afro.round | 0 | townvill")), data=df[df$sample_restricted_uk %in% T,]) 
blvs_elec_good.lm <- felm(as.formula(paste0("blvs_elec_good ~ completed_near_ukaid.30km.bin + planned_near_ukaid.30km.bin + completed_near_usaid.30km.bin + planned_near_usaid.30km.bin + ",IVs_china," | iso + afro.round | 0 | townvill")), data=df[df$sample_restricted_uk %in% T,]) 

coef_df <- bind_rows(
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
coef_df <- coef_df %>%
  dplyr::mutate(model = model %>%
           factor(levels = rev(c("Believes\nmultiple\nparties\nare good",
                                 "Believes\nmultiple\nparties\ncreate choice",
                             "Believes\ncitizens\nshould join\nany CSO",
                             "Believes\ndemocracy\nis best\nsystem",
                             "Believes\nelections\nare good"))))

# Figure -----------------------------------------------------------------------
coef_df %>%
  mutate(var = var %>% 
           str_replace("completed_near_ukaid.30km.bin", 
                                   "UK Aid Completed") %>%
           str_replace("planned_near_ukaid.30km.bin", 
                       "UK Aid Planned") %>%
           str_replace("completed_near_usaid.30km.bin", 
                       "USA Aid Completed") %>%
           str_replace("planned_near_usaid.30km.bin", 
                       "USA Aid Planned")) %>%
  filter(var %in% c("USA Aid Completed",
                    "USA Aid Planned",
                    "UK Aid Completed",
                    "UK Aid Planned")) %>%
  make_plot_all(height = 7,
                width = 10,
                file_name = "figure_04_components.png")



  