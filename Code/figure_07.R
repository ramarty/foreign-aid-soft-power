# Figure 7 - Don't Know to China Questions

# Load Data --------------------------------------------------------------------
df <- readRDS(file.path(data_file_path, "afro_china_data.Rds"))

# Regressions ------------------------------------------------------------------
#### Full Sample
china_dontknow_index.lm <- felm(as.formula(paste0("china_dontknow_index ~ completed_near_china.pl10.30km.bin + planned_near_china.pl10.30km.bin + ",               IVs_china," | ",FEs," | 0 | ", CLUSTER_VAR)), data=df[df$sample_full %in% T,]) 
china.influence.econ.activity_DONTKNOW.lm <- felm(as.formula(paste0("china.influence.econ.activity_DONTKNOW ~ completed_near_china.pl10.30km.bin + planned_near_china.pl10.30km.bin + ",               IVs_china," | ",FEs," | 0 | ", CLUSTER_VAR)), data=df[df$sample_full %in% T,]) 
china.econpol.influence.positive_DONTKNOW.lm <- felm(as.formula(paste0("china.econpol.influence.positive_DONTKNOW ~ completed_near_china.pl10.30km.bin + planned_near_china.pl10.30km.bin + ",               IVs_china," | ",FEs," | 0 | ", CLUSTER_VAR)), data=df[df$sample_full %in% T,]) 
china.aid.good.job.meet.country.needs_DONTKNOW.lm <- felm(as.formula(paste0("china.aid.good.job.meet.country.needs_DONTKNOW ~ completed_near_china.pl10.30km.bin + planned_near_china.pl10.30km.bin + ",               IVs_china," | ",FEs," | 0 | ", CLUSTER_VAR)), data=df[df$sample_full %in% T,]) 
china.help.country_DONTKNOW.lm <- felm(as.formula(paste0("china.help.country_DONTKNOW ~ completed_near_china.pl10.30km.bin + planned_near_china.pl10.30km.bin + ",               IVs_china," | ",FEs," | 0 | ", CLUSTER_VAR)), data=df[df$sample_full %in% T,]) 

mi_full_df <- bind_rows(
  calc_morans_i(china_dontknow_index.lm),
  calc_morans_i(china.influence.econ.activity_DONTKNOW.lm),
  calc_morans_i(china.econpol.influence.positive_DONTKNOW.lm),
  calc_morans_i(china.aid.good.job.meet.country.needs_DONTKNOW.lm),
  calc_morans_i(china.help.country_DONTKNOW.lm)
) %>%
  mutate(sample = "full")

coef_df <- bind_rows(
  extract_coefs(china_dontknow_index.lm) %>%
    mutate(model = "China Questions\nDon't Know Index"),
  
  extract_coefs(china.influence.econ.activity_DONTKNOW.lm) %>%
    mutate(model = "How much influence does\nChina economic activities\nhave on economy [Don't Know]"),
  
  extract_coefs(china.econpol.influence.positive_DONTKNOW.lm) %>%
    mutate(model = "China has positive or\nnegative economic and\npolitical influence [Don't Know]"),
  
  extract_coefs(china.aid.good.job.meet.country.needs_DONTKNOW.lm) %>%
    mutate(model = "Chinese aid does good or\nbad job to meet\ncountry's needs [Don't Know]"),

  extract_coefs(china.help.country_DONTKNOW.lm) %>%
    mutate(model = "How much does China\nhelp the country [Don't Know]")
) %>%
  mutate(subset = "Full Sample")

#### Append Coefficients
coef_df <- coef_df %>%
  dplyr::mutate(model = model %>%
                  factor(levels = rev(c("China Questions\nDon't Know Index",
                                        "How much influence does\nChina economic activities\nhave on economy [Don't Know]",
                                        "China has positive or\nnegative economic and\npolitical influence [Don't Know]",
                                        "Chinese aid does good or\nbad job to meet\ncountry's needs [Don't Know]",
                                        "How much does China\nhelp the country [Don't Know]"))))

# Figure -----------------------------------------------------------------------
coef_df %>%
  mutate(var = var %>% 
           str_replace("completed_near_china.pl10.30km.bin", 
                       "China Aid Completed") %>%
           str_replace("planned_near_china.pl10.30km.bin", 
                       "China Aid Planned") %>%
           str_replace("completed_near_usaid.30km.bin", 
                       "USA Aid Completed") %>%
           str_replace("planned_near_usaid.30km.bin", 
                       "USA Aid Planned")) %>%
  filter(var %in% c("China Aid Completed",
                    "China Aid Planned",
                    "USA Aid Completed",
                    "USA Aid Planned")) %>%
  make_plot_all(height = 7,
                width = 10,
                file_name = "figure_07.png")

# Morans I ---------------------------------------------------------------------
bind_rows(mi_full_df) %>%
  mutate(figure = "fig_07") %>%
  write.csv(file.path(data_file_path, "morans_i", "mi_07.csv"), row.names = F)



