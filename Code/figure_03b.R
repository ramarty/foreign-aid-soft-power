# Figure 2

# Load Data --------------------------------------------------------------------
df <- readRDS(file.path(data_file_path, "afro_china_data.Rds"))

# Regressions ------------------------------------------------------------------
#### Full Sample
formcolnpower.best.dev.model.full.lm <- felm(as.formula(paste0("formcolnpower.best.dev.model ~ completed_near_china.pl10.30km.bin + planned_near_china.pl10.30km.bin + ",              IVs_china," | ",FEs," | 0 | ", CLUSTER_VAR)), data=df[df$sample_full %in% T,]) 

mi_full_df <- bind_rows(
  calc_morans_i(formcolnpower.best.dev.model.full.lm)
) %>%
  mutate(sample = "full")

coef_full_df <-
  extract_coefs(formcolnpower.best.dev.model.full.lm) %>%
  mutate(model = "Believes\nformer colonial\npower is best\nmodel") %>%
  mutate(subset = "Full Sample")

#### Restricted Sample
formcolnpower.best.dev.model.restricted.lm <- felm(as.formula(paste0("formcolnpower.best.dev.model ~ completed_near_china.pl10.30km.bin + planned_near_china.pl10.30km.bin + completed_near_usaid.30km.bin + planned_near_usaid.30km.bin + ",               IVs_china_usaid," | ",FEs," | 0 | ", CLUSTER_VAR)), data=df[df$sample_restricted %in% T,]) 

mi_restricted_df <- bind_rows(
  calc_morans_i(formcolnpower.best.dev.model.restricted.lm)
) %>%
  mutate(sample = "restricted")

coef_restricted_df <-
  extract_coefs(formcolnpower.best.dev.model.restricted.lm) %>%
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

# Full Table -------------------------------------------------------------------
buffer <- 30
stargazer(formcolnpower.best.dev.model.full.lm,
          dep.var.labels.include = T,
          dep.var.labels = c("Former Colonial Power"),
          column.labels   = c("Best Model"),
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
              tryCatch(round(linearHypothesis(formcolnpower.best.dev.model.full.lm, "completed_near_china.pl10.30km.bin = planned_near_china.pl10.30km.bin")[2,4],3), error = function(e) print("NA")) ),
            #c("Planned Year Cut Off",planned_cutoff_year,planned_cutoff_year,planned_cutoff_year,planned_cutoff_year),
            c("Morans I P-Value",
              calc_morans_i(formcolnpower.best.dev.model.full.lm)$p.value %>% round(ROUND_NUM)
            ),
            c("Spatial Lag of Dep Var Included", "N"),
            c("Country Fixed Effects", "Y"),
            c("Buffer",buffer)),
          out=file.path(tables_file_path, "table_03b_full.tex"))

# Restricted Table -------------------------------------------------------------
stargazer(formcolnpower.best.dev.model.restricted.lm,
          dep.var.labels.include = T,
          dep.var.labels = c("Former Colonial Power"),
          column.labels   = c("Best Model"),
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
              tryCatch(round(linearHypothesis(formcolnpower.best.dev.model.restricted.lm, "completed_near_china.pl10.30km.bin = planned_near_china.pl10.30km.bin")[2,4],3), error = function(e) print("NA")) ),
            c("Completed vs. planned $p$-value [US]", 
              tryCatch(round(linearHypothesis(formcolnpower.best.dev.model.restricted.lm, "completed_near_usaid.30km.bin = planned_near_usaid.30km.bin")[2,4],3), error = function(e) print("NA")) ),
            #c("Planned Year Cut Off",planned_cutoff_year,planned_cutoff_year,planned_cutoff_year,planned_cutoff_year),
            c("Morans I P-Value",
              calc_morans_i(formcolnpower.best.dev.model.restricted.lm)$p.value %>% round(ROUND_NUM)
            ),
            c("Spatial Lag of Dep Var Included", "N"),
            c("Country Fixed Effects", "Y"),
            c("Buffer",buffer)),
          out=file.path(tables_file_path, "table_03b_restricted.tex"))


# Morans I ---------------------------------------------------------------------
bind_rows(mi_full_df,
          mi_restricted_df) %>%
  mutate(figure = "fig_03b") %>%
  write.csv(file.path(data_file_path, "morans_i", "mi_03b.csv"), row.names = F)

