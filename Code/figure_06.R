# Figure 2 - Componenets

# Load Data --------------------------------------------------------------------
df <- readRDS(file.path(project_file_path, "Data", "afro_china_data.Rds"))

## Restrict to rounds 2-5
# df <- df %>%
#   filter(afro.round %in% 2:5)

# Regressions ------------------------------------------------------------------
#### Full Sample
negimage_cooperateundemocratic.lm <- felm(as.formula(paste0("negimage_cooperateundemocratic ~ completed_near_china.pl10.30km.bin + planned_near_china.pl10.30km.bin + ",               IVs_china," | iso + afro.round | 0 | townvill")), data=df[df$sample_full %in% T,]) 
negimage_chinesecitizenbehavior.lm <- felm(as.formula(paste0("negimage_chinesecitizenbehavior ~ completed_near_china.pl10.30km.bin + planned_near_china.pl10.30km.bin + ",               IVs_china," | iso + afro.round | 0 | townvill")), data=df[df$sample_full %in% T,]) 
negimage_resourceextraction.lm <- felm(as.formula(paste0("negimage_resourceextraction ~ completed_near_china.pl10.30km.bin + planned_near_china.pl10.30km.bin + ",               IVs_china," | iso + afro.round | 0 | townvill")), data=df[df$sample_full %in% T,]) 
negimage_takingjobsbusiness.lm <- felm(as.formula(paste0("negimage_takingjobsbusiness ~ completed_near_china.pl10.30km.bin + planned_near_china.pl10.30km.bin + ",               IVs_china," | iso + afro.round | 0 | townvill")), data=df[df$sample_full %in% T,]) 
negimage_landgrabbing.lm <- felm(as.formula(paste0("negimage_landgrabbing ~ completed_near_china.pl10.30km.bin + planned_near_china.pl10.30km.bin + ",               IVs_china," | iso + afro.round | 0 | townvill")), data=df[df$sample_full %in% T,]) 
negimage_productquality.lm <- felm(as.formula(paste0("negimage_productquality ~ completed_near_china.pl10.30km.bin + planned_near_china.pl10.30km.bin + ",               IVs_china," | iso + afro.round | 0 | townvill")), data=df[df$sample_full %in% T,]) 

coef_df <- bind_rows(
  extract_coefs(negimage_cooperateundemocratic.lm) %>%
    mutate(model = "Chinese\ncooperation\nw/ undemocratic\nleaders"),
  
  extract_coefs(negimage_chinesecitizenbehavior.lm) %>%
    mutate(model = "Behavious of\nChinese\ncitizens"),
  
  extract_coefs(negimage_resourceextraction.lm) %>%
    mutate(model = "Chinese\nextraction of\nnatural\nresources"),
  
  extract_coefs(negimage_takingjobsbusiness.lm) %>%
    mutate(model = "Chinese\nfirms taking\nlocal jobs\nand businesses"),
  
  extract_coefs(negimage_landgrabbing.lm) %>%
    mutate(model = "Chinese\nland\ngrabbing"),
  
  extract_coefs(negimage_productquality.lm) %>%
    mutate(model = "Quality of\nChinese\nproducts")
) %>%
  mutate(subset = "Full Sample")

#### Append Coefficients
coef_df <- coef_df %>%
  dplyr::mutate(model = model %>%
                  factor(levels = rev(c("Chinese\ncooperation\nw/ undemocratic\nleaders",
                                        "Behavious of\nChinese\ncitizens",
                                        "Chinese\nextraction of\nnatural\nresources",
                                        "Chinese\nfirms taking\nlocal jobs\nand businesses",
                                        "Chinese\nland\ngrabbing",
                                        "Quality of\nChinese\nproducts"))))

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
                file_name = "figure_06.png")



