# Figure 2

# Load Data --------------------------------------------------------------------
df <- readRDS(file.path(project_file_path, "Data", "afro_china_data.Rds"))

## Restrict to rounds 2-5
# df <- df %>%
#   filter(afro.round %in% 2:5)

# Regressions ------------------------------------------------------------------
#### Full Sample
formcolnpower.best.dev.model.lm <- felm(as.formula(paste0("formcolnpower.best.dev.model ~ completed_near_china.pl10.30km.bin + planned_near_china.pl10.30km.bin + ",              IVs_china," | iso + afro.round | 0 | townvill")), data=df[df$sample_full %in% T,]) 

coef_full_df <-
  extract_coefs(formcolnpower.best.dev.model.lm) %>%
  mutate(model = "Believes\nformer colonial\npower is best\nmodel") %>%
  mutate(subset = "Full Sample")

#### Restricted Sample
formcolnpower.best.dev.model.lm <- felm(as.formula(paste0("formcolnpower.best.dev.model ~ completed_near_china.pl10.30km.bin + planned_near_china.pl10.30km.bin + completed_near_usaid.30km.bin + planned_near_usaid.30km.bin + ",               IVs_china_usaid," | iso + afro.round | 0 | townvill")), data=df[df$sample_restricted %in% T,]) 

coef_restricted_df <-
  extract_coefs(formcolnpower.best.dev.model.lm) %>%
  mutate(model = "Believes\nformer colonial\npower is best\nmodel") %>%
  mutate(subset = "Restricted Sample")

#### Append Coefficients
coef_df <- bind_rows(coef_full_df,
                     coef_restricted_df)

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
                file_name = "figure_03b.png")
