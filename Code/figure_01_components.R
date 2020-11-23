# Figure 1

ROUND_NUM <- 2

# Load Data --------------------------------------------------------------------
df <- readRDS(file.path(data_file_path, "afro_china_data.Rds"))

# Regressions ------------------------------------------------------------------
#### Full Sample
china.most.influence.full.lm <- felm(as.formula(paste0("china.most.influence ~ completed_near_china.pl10.30km.bin + planned_near_china.pl10.30km.bin + ",              IVs_china," | ",FEs," | 0 | ", CLUSTER_VAR)), data=df[df$sample_full %in% T,]) 
usa.most.influence.full.lm <- felm(as.formula(paste0("usa.most.influence ~ completed_near_china.pl10.30km.bin + planned_near_china.pl10.30km.bin + ",IVs_china," | ",FEs," | 0 | ", CLUSTER_VAR)), data=df[df$sample_full %in% T,]) 
china.best.dev.model.full.lm <- felm(as.formula(paste0("china.best.dev.model ~ completed_near_china.pl10.30km.bin + planned_near_china.pl10.30km.bin + ",              IVs_china," | ",FEs," | 0 | ", CLUSTER_VAR)), data=df[df$sample_full %in% T,]) 
usa.best.dev.model.full.lm <- felm(as.formula(paste0("usa.best.dev.model ~ completed_near_china.pl10.30km.bin + planned_near_china.pl10.30km.bin + ",IVs_china," | ",FEs," | 0 | ", CLUSTER_VAR)), data=df[df$sample_full %in% T,]) 

mi_full_df <- bind_rows(
  calc_morans_i(china.most.influence.full.lm),
  calc_morans_i(usa.most.influence.full.lm),
  calc_morans_i(china.best.dev.model.full.lm),
  calc_morans_i(usa.best.dev.model.full.lm)
) %>%
  mutate(sample = "full")

coef_full_df <- bind_rows(
  extract_coefs(china.most.influence.full.lm) %>%
    mutate(model = "Believes\nChinese model\nis most\ninfluential"),
  
  extract_coefs(usa.most.influence.full.lm) %>%
    mutate(model = "Believes\nUS model\nis most\ninfluential"),
  
  extract_coefs(china.best.dev.model.full.lm) %>%
    mutate(model = "Believes\nChinese model\nis best"),
  
  extract_coefs(usa.best.dev.model.full.lm) %>%
    mutate(model = "Believes\nUS model\nis best")
) %>%
  mutate(subset = "Full Sample")

#### Restricted Sample
china.most.influence.restricted.lm <- felm(as.formula(paste0("china.most.influence ~ completed_near_china.pl10.30km.bin + planned_near_china.pl10.30km.bin + completed_near_usaid.30km.bin + planned_near_usaid.30km.bin + ",               IVs_china_usaid," | ",FEs," | 0 | ", CLUSTER_VAR)), data=df[df$sample_restricted %in% T,]) 
usa.most.influence.restricted.lm <- felm(as.formula(paste0("usa.most.influence ~ completed_near_china.pl10.30km.bin + planned_near_china.pl10.30km.bin + completed_near_usaid.30km.bin + planned_near_usaid.30km.bin + ", IVs_china_usaid," | ",FEs," | 0 | ", CLUSTER_VAR)), data=df[df$sample_restricted %in% T,]) 
china.best.dev.model.restricted.lm <- felm(as.formula(paste0("china.best.dev.model ~ completed_near_china.pl10.30km.bin + planned_near_china.pl10.30km.bin + completed_near_usaid.30km.bin + planned_near_usaid.30km.bin + ",               IVs_china_usaid," | ",FEs," | 0 | ", CLUSTER_VAR)), data=df[df$sample_restricted %in% T,]) 
usa.best.dev.model.restricted.lm <- felm(as.formula(paste0("usa.best.dev.model ~ completed_near_china.pl10.30km.bin + planned_near_china.pl10.30km.bin + completed_near_usaid.30km.bin + planned_near_usaid.30km.bin + ", IVs_china_usaid," | ",FEs," | 0 | ", CLUSTER_VAR)), data=df[df$sample_restricted %in% T,]) 

mi_restricted_df <- bind_rows(
  calc_morans_i(china.most.influence.restricted.lm),
  calc_morans_i(usa.most.influence.restricted.lm),
  calc_morans_i(china.best.dev.model.restricted.lm),
  calc_morans_i(usa.best.dev.model.restricted.lm)
) %>%
  mutate(sample = "restricted")

coef_restricted_df <- bind_rows(
  extract_coefs(china.most.influence.restricted.lm) %>%
    mutate(model = "Believes\nChinese model\nis most\ninfluential"),
  
  extract_coefs(usa.most.influence.restricted.lm) %>%
    mutate(model = "Believes\nUS model\nis most\ninfluential"),
  
  extract_coefs(china.best.dev.model.restricted.lm) %>%
    mutate(model = "Believes\nChinese model\nis best"),
  
  extract_coefs(usa.best.dev.model.restricted.lm) %>%
    mutate(model = "Believes\nUS model\nis best")
) %>%
  mutate(subset = "Restricted Sample")

#### Append Coefficients
coef_df <- bind_rows(coef_full_df,
                     coef_restricted_df) %>%
  dplyr::mutate(model = model %>%
                  factor(levels = rev(c("Believes\nChinese model\nis most\ninfluential",
                                        "Believes\nUS model\nis most\ninfluential",
                                        "Believes\nChinese model\nis best",
                                        "Believes\nUS model\nis best"))))

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
  make_plot_all(height = 7,
                width = 10,
                file_name = "figure_01_components.png")

# Full Table -------------------------------------------------------------------
buffer <- 30
stargazer(china.most.influence.full.lm,
          usa.most.influence.full.lm,
          china.best.dev.model.full.lm,
          usa.best.dev.model.full.lm,
          dep.var.labels.include = T,
          dep.var.labels = c("China Most","US Most","China Best","US Best"),
          column.labels   = c("Influence", "Influence",  "Model", "Model"),
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
              tryCatch(round(linearHypothesis(china.most.influence.full.lm, "completed_near_china.pl10.30km.bin = planned_near_china.pl10.30km.bin")[2,4],3), error = function(e) print("NA")),
              tryCatch(round(linearHypothesis(usa.most.influence.full.lm, "completed_near_china.pl10.30km.bin = planned_near_china.pl10.30km.bin")[2,4],3), error = function(e) print("NA")),
              tryCatch(round(linearHypothesis(china.best.dev.model.full.lm, "completed_near_china.pl10.30km.bin = planned_near_china.pl10.30km.bin")[2,4],3), error = function(e) print("NA")),
              tryCatch(round(linearHypothesis(usa.best.dev.model.full.lm, "completed_near_china.pl10.30km.bin = planned_near_china.pl10.30km.bin")[2,4],3), error = function(e) print("NA")) ),
            #c("Planned Year Cut Off",planned_cutoff_year,planned_cutoff_year,planned_cutoff_year,planned_cutoff_year),
            c("Morans I P-Value",
              calc_morans_i(china.most.influence.full.lm)$p.value %>% round(ROUND_NUM),
              calc_morans_i(usa.most.influence.full.lm)$p.value %>% round(ROUND_NUM),
              calc_morans_i(china.best.dev.model.full.lm)$p.value %>% round(ROUND_NUM),
              calc_morans_i(usa.best.dev.model.full.lm)$p.value %>% round(ROUND_NUM)
            ),
            c("Spatial Lag of Dep Var Included", "N", "N", "N", "N"),
            c("Country Fixed Effects", "Y", "Y", "Y","Y"),
            c("Buffer",buffer,buffer,buffer,buffer)),
          out=file.path(tables_file_path, "table_01_components_full.tex"))

# Restricted Table -------------------------------------------------------------
stargazer(china.most.influence.restricted.lm,
          usa.most.influence.restricted.lm,
          china.best.dev.model.restricted.lm,
          usa.best.dev.model.restricted.lm,
          dep.var.labels.include = T,
          dep.var.labels = c("China Most","US Most","China Best","US Best"),
          column.labels   = c("Influence", "Influence",  "Model", "Model"),
          keep=c("completed_near_china.pl10.30km.bin", "planned_near_china.pl10.30km.bin",
                 "completed_near_usaid.30km.bin", "planned_near_usaid.30km.bin"),
          covariate.labels = c("China Completed","China Planned",
                               "USA Completed","USA Planned"),
          dep.var.caption = "",
          omit.stat = c("f","ser"), 
          align=TRUE,
          no.space=TRUE,
          float=FALSE,
          column.sep.width = "8pt",
          report="vcs*",
          digits = 2,
          add.lines = list(
            c("Completed vs. planned $p$-value [China]", 
              tryCatch(round(linearHypothesis(china.most.influence.restricted.lm, "completed_near_china.pl10.30km.bin = planned_near_china.pl10.30km.bin")[2,4],3), error = function(e) print("NA")),
              tryCatch(round(linearHypothesis(usa.most.influence.restricted.lm, "completed_near_china.pl10.30km.bin = planned_near_china.pl10.30km.bin")[2,4],3), error = function(e) print("NA")),
              tryCatch(round(linearHypothesis(china.best.dev.model.restricted.lm, "completed_near_china.pl10.30km.bin = planned_near_china.pl10.30km.bin")[2,4],3), error = function(e) print("NA")),
              tryCatch(round(linearHypothesis(usa.best.dev.model.restricted.lm, "completed_near_china.pl10.30km.bin = planned_near_china.pl10.30km.bin")[2,4],3), error = function(e) print("NA")) ),
            c("Completed vs. planned $p$-value [US]", 
              tryCatch(round(linearHypothesis(china.most.influence.restricted.lm, "completed_near_usaid.30km.bin = planned_near_usaid.30km.bin")[2,4],3), error = function(e) print("NA")),
              tryCatch(round(linearHypothesis(usa.most.influence.restricted.lm, "completed_near_usaid.30km.bin = planned_near_usaid.30km.bin")[2,4],3), error = function(e) print("NA")),
              tryCatch(round(linearHypothesis(china.best.dev.model.restricted.lm, "completed_near_usaid.30km.bin = planned_near_usaid.30km.bin")[2,4],3), error = function(e) print("NA")),
              tryCatch(round(linearHypothesis(usa.best.dev.model.restricted.lm, "completed_near_usaid.30km.bin = planned_near_usaid.30km.bin")[2,4],3), error = function(e) print("NA")) ),
            #c("Planned Year Cut Off",planned_cutoff_year,planned_cutoff_year,planned_cutoff_year,planned_cutoff_year),
            c("Morans I P-Value",
              calc_morans_i(china.most.influence.restricted.lm)$p.value %>% round(ROUND_NUM),
              calc_morans_i(usa.most.influence.restricted.lm)$p.value %>% round(ROUND_NUM),
              calc_morans_i(china.best.dev.model.restricted.lm)$p.value %>% round(ROUND_NUM),
              calc_morans_i(usa.best.dev.model.restricted.lm)$p.value %>% round(ROUND_NUM)
              ),
            c("Spatial Lag of Dep Var Included", "N", "N", "N", "N"),
            c("Country Fixed Effects", "Y", "Y", "Y","Y"),
            c("Buffer",buffer,buffer,buffer,buffer)),
          out=file.path(tables_file_path, "table_01_components_restricted.tex"))

# Morans I ---------------------------------------------------------------------
bind_rows(mi_full_df,
          mi_restricted_df) %>%
  mutate(figure = "fig_01_components") %>%
  write.csv(file.path(data_file_path, "morans_i", "mi_01_components.csv"), row.names = F)



