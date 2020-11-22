# Figure 1

# Load Data --------------------------------------------------------------------
df <- readRDS(file.path(data_file_path, "afro_china_data.Rds"))

## Restrict to round 6
df <- df %>%
  filter(afro.round %in% 6)

# Regressions ------------------------------------------------------------------
#### Full Sample
china_influential_index.lm <- felm(as.formula(paste0("china_influential_index ~ completed_near_china.pl10.30km.bin + planned_near_china.pl10.30km.bin + ",              IVs_china," | ",FEs," | 0 | ", CLUSTER_VAR)), data=df[df$sample_full %in% T,]) 
china_positive_influence_index.lm <- felm(as.formula(paste0("china_positive_influence_index ~ completed_near_china.pl10.30km.bin + planned_near_china.pl10.30km.bin + ",IVs_china," | ",FEs," | 0 | ", CLUSTER_VAR)), data=df[df$sample_full %in% T,]) 

mi_full_df <- bind_rows(
  calc_morans_i(china_influential_index.lm),
  calc_morans_i(china_positive_influence_index.lm)
) %>%
  mutate(sample = "full")

coef_full_df <- bind_rows(
  extract_coefs(china_influential_index.lm) %>%
    mutate(model = "Believes\nChina is\ninfluencial\n(index)"),
  extract_coefs(china_positive_influence_index.lm) %>%
    mutate(model = "Belives\nChinese presence\nis positive\n(index)")
) %>%
  mutate(subset = "Full Sample")

#### Restricted Sample
chinausa_influential_index.lm <- felm(as.formula(paste0("china_influential_index ~ completed_near_china.pl10.30km.bin + planned_near_china.pl10.30km.bin + completed_near_usaid.30km.bin + planned_near_usaid.30km.bin + ",               IVs_china_usaid," | ",FEs," | 0 | ", CLUSTER_VAR)), data=df[df$sample_restricted %in% T,]) 
chinausa_positive_influence_index.lm <- felm(as.formula(paste0("china_positive_influence_index ~ completed_near_china.pl10.30km.bin + planned_near_china.pl10.30km.bin + completed_near_usaid.30km.bin + planned_near_usaid.30km.bin + ", IVs_china_usaid," | ",FEs," | 0 | ", CLUSTER_VAR)), data=df[df$sample_restricted %in% T,]) 

mi_restricted_df <- bind_rows(
  calc_morans_i(chinausa_influential_index.lm),
  calc_morans_i(chinausa_positive_influence_index.lm)
) %>%
  mutate(sample = "restricted")

coef_restricted_df <- bind_rows(
  extract_coefs(chinausa_influential_index.lm) %>%
    mutate(model = "Believes\nChina is\ninfluencial\n(index)"),
  extract_coefs(chinausa_positive_influence_index.lm) %>%
    mutate(model = "Belives\nChinese presence\nis positive\n(index)")
) %>%
  mutate(subset = "Restricted Sample")

#### Append Coefficients
coef_df <- bind_rows(coef_full_df,
                     coef_restricted_df) %>%
  mutate(model = model %>%
           factor(levels = c("Belives\nChinese presence\nis positive\n(index)",
                             "Believes\nChina is\ninfluencial\n(index)")))

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
  make_plot_all(height = 3,
                width = 8,
                file_name = "figure_01.png")

# Morans I ---------------------------------------------------------------------
bind_rows(mi_full_df,
          mi_restricted_df) %>%
  mutate(figure = "fig_01") %>%
  write.csv(file.path(data_file_path, "morans_i", "mi_01.csv"), row.names = F)


  