# Figure 2 - Componenets

# Load Data --------------------------------------------------------------------
df <- readRDS(file.path(project_file_path, "Data", "afro_china_data.Rds"))

## Restrict to rounds 2-5
# df <- df %>%
#   filter(afro.round %in% 2:5)

# Regressions ------------------------------------------------------------------
#### Full Sample
posimage_chinesepeople.lm <- felm(as.formula(paste0("posimage_chinesepeople ~ completed_near_china.pl10.30km.bin + planned_near_china.pl10.30km.bin + ",               IVs_china," | iso + afro.round | 0 | townvill")), data=df[df$sample_full %in% T,]) 
posimage_businessinvetment.lm <- felm(as.formula(paste0("posimage_businessinvetment ~ completed_near_china.pl10.30km.bin + planned_near_china.pl10.30km.bin + ",               IVs_china," | iso + afro.round | 0 | townvill")), data=df[df$sample_full %in% T,]) 
posimage_infordevinvetment.lm <- felm(as.formula(paste0("posimage_infordevinvetment ~ completed_near_china.pl10.30km.bin + planned_near_china.pl10.30km.bin + ",               IVs_china," | iso + afro.round | 0 | townvill")), data=df[df$sample_full %in% T,]) 
posimage_noninterference.lm <- felm(as.formula(paste0("posimage_noninterference ~ completed_near_china.pl10.30km.bin + planned_near_china.pl10.30km.bin + ",               IVs_china," | iso + afro.round | 0 | townvill")), data=df[df$sample_full %in% T,]) 
posimage_supportinintlaffiars.lm <- felm(as.formula(paste0("posimage_supportinintlaffiars ~ completed_near_china.pl10.30km.bin + planned_near_china.pl10.30km.bin + ",               IVs_china," | iso + afro.round | 0 | townvill")), data=df[df$sample_full %in% T,]) 
posimage_productcost.lm <- felm(as.formula(paste0("posimage_productcost ~ completed_near_china.pl10.30km.bin + planned_near_china.pl10.30km.bin + ",               IVs_china," | iso + afro.round | 0 | townvill")), data=df[df$sample_full %in% T,]) 

coef_df <- bind_rows(
  extract_coefs(posimage_chinesepeople.lm) %>%
    mutate(model = "Chinese\npeople and\nculture"),
  
  extract_coefs(posimage_businessinvetment.lm) %>%
    mutate(model = "Chinese\nbusiness\ninvestment"),
  
  extract_coefs(posimage_infordevinvetment.lm) %>%
    mutate(model = "Chinese\ninfrastructure\ninvestment"),
  
  extract_coefs(posimage_noninterference.lm) %>%
    mutate(model = "Chinese\npolice of non-interference"),
  
  extract_coefs(posimage_supportinintlaffiars.lm) %>%
    mutate(model = "Chinese\nsupport in\ninternational\naffairs"),
  
  extract_coefs(posimage_productcost.lm) %>%
    mutate(model = "Cost of\nChinese\nproducts")
) %>%
  mutate(subset = "Full Sample")

#### Append Coefficients
coef_df <- coef_df %>%
  dplyr::mutate(model = model %>%
                  factor(levels = rev(c("Chinese\npeople and\nculture",
                                        "Chinese\nbusiness\ninvestment",
                                        "Chinese\ninfrastructure\ninvestment",
                                        "Chinese\npolice of non-interference",
                                        "Chinese\nsupport in\ninternational\naffairs",
                                        "Cost of\nChinese\nproducts"))))

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
                file_name = "figure_05.png")



