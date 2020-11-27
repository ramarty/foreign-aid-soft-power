# Figure 2 - Componenets

# Load Data --------------------------------------------------------------------
df <- readRDS(file.path(data_file_path, "afro_china_data.Rds"))

# Regressions ------------------------------------------------------------------
#### Full Sample
negimage_cooperateundemocratic.lm  <- felm(as.formula(paste0("negimage_cooperateundemocratic ~  completed_near_china.pl10.30km.bin + planned_near_china.pl10.30km.bin + ", IVs_china," | ",FEs," | 0 | ", CLUSTER_VAR)), data=df[df$sample_full %in% T,]) 
negimage_chinesecitizenbehavior.lm <- felm(as.formula(paste0("negimage_chinesecitizenbehavior ~ completed_near_china.pl10.30km.bin + planned_near_china.pl10.30km.bin + ", IVs_china," | ",FEs," | 0 | ", CLUSTER_VAR)), data=df[df$sample_full %in% T,]) 
negimage_resourceextraction.lm     <- felm(as.formula(paste0("negimage_resourceextraction ~     completed_near_china.pl10.30km.bin + planned_near_china.pl10.30km.bin + ", IVs_china," | ",FEs," | 0 | ", CLUSTER_VAR)), data=df[df$sample_full %in% T,]) 
negimage_takingjobsbusiness.lm     <- felm(as.formula(paste0("negimage_takingjobsbusiness ~     completed_near_china.pl10.30km.bin + planned_near_china.pl10.30km.bin + ", IVs_china," | ",FEs," | 0 | ", CLUSTER_VAR)), data=df[df$sample_full %in% T,]) 
negimage_landgrabbing.lm           <- felm(as.formula(paste0("negimage_landgrabbing ~           completed_near_china.pl10.30km.bin + planned_near_china.pl10.30km.bin + ", IVs_china," | ",FEs," | 0 | ", CLUSTER_VAR)), data=df[df$sample_full %in% T,]) 
negimage_productquality.lm         <- felm(as.formula(paste0("negimage_productquality ~         completed_near_china.pl10.30km.bin + planned_near_china.pl10.30km.bin + ", IVs_china," | ",FEs," | 0 | ", CLUSTER_VAR)), data=df[df$sample_full %in% T,]) 

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
                width = 8,
                file_name = "figure_06.png")

# Full Table -------------------------------------------------------------------
buffer <- 30
planned_cutoff_year <- 2010

stargazer(negimage_cooperateundemocratic.lm,
          negimage_chinesecitizenbehavior.lm,
          negimage_resourceextraction.lm,
          negimage_takingjobsbusiness.lm,
          negimage_landgrabbing.lm,
          negimage_productquality.lm,
          dep.var.labels.include = T,
          dep.var.labels = c("Coop With",     "Behavior of ",    "Resource",   "Taking Jobs",   "Land",     "Product"),
          column.labels   = c("Undem Leaders", "Chinese Citzns", "Extraction", "and Business",  "Grabbing", "Quality"),
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
              tryCatch(round(linearHypothesis(negimage_cooperateundemocratic.lm, "completed_near_china.pl10.30km.bin = planned_near_china.pl10.30km.bin")[2,4],3), error = function(e) print("NA")),
              tryCatch(round(linearHypothesis(negimage_chinesecitizenbehavior.lm, "completed_near_china.pl10.30km.bin = planned_near_china.pl10.30km.bin")[2,4],3), error = function(e) print("NA")),
              tryCatch(round(linearHypothesis(negimage_resourceextraction.lm, "completed_near_china.pl10.30km.bin = planned_near_china.pl10.30km.bin")[2,4],3), error = function(e) print("NA")),
              tryCatch(round(linearHypothesis(negimage_takingjobsbusiness.lm, "completed_near_china.pl10.30km.bin = planned_near_china.pl10.30km.bin")[2,4],3), error = function(e) print("NA")),
              tryCatch(round(linearHypothesis(negimage_landgrabbing.lm, "completed_near_china.pl10.30km.bin = planned_near_china.pl10.30km.bin")[2,4],3), error = function(e) print("NA")),
              tryCatch(round(linearHypothesis(negimage_productquality.lm, "completed_near_china.pl10.30km.bin = planned_near_china.pl10.30km.bin")[2,4],3), error = function(e) print("NA")) ),
            c("Planned Year Cut Off",planned_cutoff_year,planned_cutoff_year,planned_cutoff_year,planned_cutoff_year,planned_cutoff_year,planned_cutoff_year),
            c("Morans I P-Value",
              calc_morans_i(negimage_cooperateundemocratic.lm)$p.value %>% round(ROUND_NUM),
              calc_morans_i(negimage_chinesecitizenbehavior.lm)$p.value %>% round(ROUND_NUM),
              calc_morans_i(negimage_resourceextraction.lm)$p.value %>% round(ROUND_NUM),
              calc_morans_i(negimage_takingjobsbusiness.lm)$p.value %>% round(ROUND_NUM),
              calc_morans_i(negimage_landgrabbing.lm)$p.value %>% round(ROUND_NUM),
              calc_morans_i(negimage_productquality.lm)$p.value %>% round(ROUND_NUM)
            ),
            c("Spatial Lag of Dep Var Included", "N", "N", "N", "N", "N", "N"),
            c("Country Fixed Effects", "Y", "Y", "Y","Y", "Y","Y"),
            c("Buffer",buffer,buffer,buffer,buffer, buffer,buffer)),
          out=file.path(tables_file_path, "table_06_full.tex"))

