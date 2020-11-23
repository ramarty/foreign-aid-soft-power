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

# Restricted Table -------------------------------------------------------------
stargazer(lib_dem_val_index.lm,
          dep.var.labels.include = T,
          dep.var.labels = c("Liberal Democratic"),
          column.labels   = c("Values Index"),
          keep=c("completed_near_usaid.30km.bin", "planned_near_usaid.30km.bin",
                 "completed_near_ukaid.30km.bin", "planned_near_ukaid.30km.bin"),
          covariate.labels = c("USA Completed","USA Planned",
                               "UK Completed","UK Planned"),
          dep.var.caption = "",
          omit.stat = c("f","ser"), 
          align=TRUE,
          no.space=TRUE,
          float=FALSE,
          column.sep.width = "8pt",
          report="vcs*",
          digits = 2,
          add.lines = list(
            c("Completed vs. planned $p$-value [US]", 
              tryCatch(round(linearHypothesis(lib_dem_val_index.lm, "completed_near_usaid.30km.bin = planned_near_usaid.30km.bin")[2,4],3), error = function(e) print("NA")) ),
            c("Completed vs. planned $p$-value [UK]", 
              tryCatch(round(linearHypothesis(lib_dem_val_index.lm, "completed_near_ukaid.30km.bin = planned_near_ukaid.30km.bin")[2,4],3), error = function(e) print("NA")) ),
            #c("Planned Year Cut Off",planned_cutoff_year,planned_cutoff_year,planned_cutoff_year,planned_cutoff_year),
            c("Morans I P-Value",
              calc_morans_i(lib_dem_val_index.lm)$p.value %>% round(ROUND_NUM)
            ),
            c("Spatial Lag of Dep Var Included", "N"),
            c("Country Fixed Effects", "Y"),
            c("Buffer",buffer)),
          out=file.path(tables_file_path, "table_04_restricted.tex"))

# Morans I ---------------------------------------------------------------------
bind_rows(mi_full_df) %>%
  mutate(figure = "fig_04") %>%
  write.csv(file.path(data_file_path, "morans_i", "mi_04.csv"), row.names = F)

