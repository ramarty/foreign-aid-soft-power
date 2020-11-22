# Figure 2

# Load Data --------------------------------------------------------------------
df <- readRDS(file.path(data_file_path, "afro_china_data.Rds"))

# Regressions ------------------------------------------------------------------
#### Restricted Sample
lib_dem_val_index.lm <- felm(as.formula(paste0("lib_dem_val_index ~ completed_near_ukaid.30km.bin + planned_near_ukaid.30km.bin + completed_near_usaid.30km.bin + planned_near_usaid.30km.bin + ",               IVs_china_usaid," | ",FEs," | 0 | ", CLUSTER_VAR)), data=df[df$sample_restricted_uk %in% T,]) 

mi_full_df <- bind_rows(
  calc_morans_i(lib_dem_val_index.lm)
) %>%
  mutate(sample = "full")

coef_df <-
  extract_coefs(lib_dem_val_index.lm) %>%
  mutate(model = "Liberal\ndemocratic\nvalues\n(index)") %>%
  mutate(subset = "Restricted Sample")

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
  make_plot_all(height = 3,
                width = 8,
                file_name = "figure_04.png")

# Morans I ---------------------------------------------------------------------
bind_rows(mi_full_df) %>%
  mutate(figure = "fig_04") %>%
  write.csv(file.path(data_file_path, "morans_i", "mi_04.csv"), row.names = F)

