# Figure 2

# Load Data --------------------------------------------------------------------
df <- readRDS(file.path(project_file_path, "Data", "afro_china_data.Rds"))

## Restrict to rounds 2-5
# df <- df %>%
#   filter(afro.round %in% 2:5)

# Regressions ------------------------------------------------------------------
#### Restricted Sample
lib_dem_val_index.lm <- felm(as.formula(paste0("lib_dem_val_index ~ completed_near_ukaid.30km.bin + planned_near_ukaid.30km.bin + completed_near_usaid.30km.bin + planned_near_usaid.30km.bin + ",               IVs_china_usaid," | iso + afro.round | 0 | townvill")), data=df[df$sample_restricted_uk %in% T,]) 

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
