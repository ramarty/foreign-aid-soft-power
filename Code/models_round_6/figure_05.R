# Figure 2 - Componenets

# Load Data --------------------------------------------------------------------
df <- readRDS(file.path(data_file_path, "afro_china_data.Rds"))

# Regressions ------------------------------------------------------------------
#### Full Sample
posimage_chinesepeople.lm        <- felm(as.formula(paste0("posimage_chinesepeople ~                                        completed_near_china.pl10.30km.bin + planned_near_china.pl10.30km.bin + ", IVs_china," | ",FEs," | 0 | ", CLUSTER_VAR)), data=df[df$sample_full %in% T,]) 
posimage_businessinvetment.lm    <- felm(as.formula(paste0("posimage_businessinvetment ~ posimage_businessinvetment_splag + completed_near_china.pl10.30km.bin + planned_near_china.pl10.30km.bin + ", IVs_china," | ",FEs," | 0 | ", CLUSTER_VAR)), data=df[df$sample_full %in% T,]) 
posimage_infordevinvetment.lm    <- felm(as.formula(paste0("posimage_infordevinvetment ~                                    completed_near_china.pl10.30km.bin + planned_near_china.pl10.30km.bin + ", IVs_china," | ",FEs," | 0 | ", CLUSTER_VAR)), data=df[df$sample_full %in% T,]) 
posimage_noninterference.lm      <- felm(as.formula(paste0("posimage_noninterference ~                                      completed_near_china.pl10.30km.bin + planned_near_china.pl10.30km.bin + ", IVs_china," | ",FEs," | 0 | ", CLUSTER_VAR)), data=df[df$sample_full %in% T,]) 
posimage_supportinintlaffiars.lm <- felm(as.formula(paste0("posimage_supportinintlaffiars ~                                 completed_near_china.pl10.30km.bin + planned_near_china.pl10.30km.bin + ", IVs_china," | ",FEs," | 0 | ", CLUSTER_VAR)), data=df[df$sample_full %in% T,]) 
posimage_productcost.lm          <- felm(as.formula(paste0("posimage_productcost ~                                          completed_near_china.pl10.30km.bin + planned_near_china.pl10.30km.bin + ", IVs_china," | ",FEs," | 0 | ", CLUSTER_VAR)), data=df[df$sample_full %in% T,]) 

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

# Full Table -------------------------------------------------------------------
buffer <- 30
planned_cutoff_year <- 2010

stargazer(posimage_chinesepeople.lm,
          posimage_businessinvetment.lm,
          posimage_infordevinvetment.lm,
          posimage_noninterference.lm,
          posimage_supportinintlaffiars.lm,
          posimage_productcost.lm,
          dep.var.labels.include = T,
          dep.var.labels = c("Chinese People ","Business","Inf / Development","Non-Interference","Support in","Cost of"),
          column.labels   = c("and Culture", "Investment", "Investment",      "Policy",           "Intl Affairs", "Chinese Products"),
          keep=c("completed_near_china.pl10.30km.bin", "planned_near_china.pl10.30km.bin"),
          covariate.labels = c("China Completed","China Planned"),
          dep.var.caption = "",
          omit.stat = c("f","ser"), 
          align=TRUE,
          no.space=TRUE,
          float=FALSE,
          column.sep.width = "8pt",
          report="vcs*",
          digits = 2,
          add.lines = list(
            c("Completed vs. planned $p$-value", 
              tryCatch(round(linearHypothesis(posimage_chinesepeople.lm, "completed_near_china.pl10.30km.bin = planned_near_china.pl10.30km.bin")[2,4],3), error = function(e) print("NA")),
              tryCatch(round(linearHypothesis(posimage_businessinvetment.lm, "completed_near_china.pl10.30km.bin = planned_near_china.pl10.30km.bin")[2,4],3), error = function(e) print("NA")),
              tryCatch(round(linearHypothesis(posimage_infordevinvetment.lm, "completed_near_china.pl10.30km.bin = planned_near_china.pl10.30km.bin")[2,4],3), error = function(e) print("NA")),
              tryCatch(round(linearHypothesis(posimage_noninterference.lm, "completed_near_china.pl10.30km.bin = planned_near_china.pl10.30km.bin")[2,4],3), error = function(e) print("NA")),
              tryCatch(round(linearHypothesis(posimage_supportinintlaffiars.lm, "completed_near_china.pl10.30km.bin = planned_near_china.pl10.30km.bin")[2,4],3), error = function(e) print("NA")),
              tryCatch(round(linearHypothesis(posimage_productcost.lm, "completed_near_china.pl10.30km.bin = planned_near_china.pl10.30km.bin")[2,4],3), error = function(e) print("NA")) ),
            c("Planned Year Cut Off",planned_cutoff_year,planned_cutoff_year,planned_cutoff_year,planned_cutoff_year,planned_cutoff_year,planned_cutoff_year),
            c("Morans I P-Value",
              calc_morans_i(posimage_chinesepeople.lm)$p.value %>% round(ROUND_NUM),
              calc_morans_i(posimage_businessinvetment.lm)$p.value %>% round(ROUND_NUM),
              calc_morans_i(posimage_infordevinvetment.lm)$p.value %>% round(ROUND_NUM),
              calc_morans_i(posimage_noninterference.lm)$p.value %>% round(ROUND_NUM),
              calc_morans_i(posimage_supportinintlaffiars.lm)$p.value %>% round(ROUND_NUM),
              calc_morans_i(posimage_productcost.lm)$p.value %>% round(ROUND_NUM)
            ),
            c("Spatial Lag of Dep Var Included", "N", "Y", "N", "N", "N", "N"),
            c("Country Fixed Effects", "Y", "Y", "Y","Y", "Y","Y"),
            c("Buffer",buffer,buffer,buffer,buffer, buffer,buffer)),
          out=file.path(tables_file_path, "table_05_full.tex"))


