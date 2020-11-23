# Figure 1

# Load Data --------------------------------------------------------------------
df <- readRDS(file.path(data_file_path, "afro_china_data.Rds"))

## Restrict to round 6
#df <- df %>%
#  filter(afro.round %in% 6)

# Regressions ------------------------------------------------------------------
#### Full Sample
influential_index.full.lm <- felm(as.formula(paste0("china_influential_index ~ completed_near_china.pl10.30km.bin + planned_near_china.pl10.30km.bin + ",              IVs_china," | ",FEs," | 0 | ", CLUSTER_VAR)), data=df[df$sample_full %in% T,]) 
positive_influence_index.full.lm <- felm(as.formula(paste0("china_positive_influence_index ~ completed_near_china.pl10.30km.bin + planned_near_china.pl10.30km.bin + ",IVs_china," | ",FEs," | 0 | ", CLUSTER_VAR)), data=df[df$sample_full %in% T,]) 

mi_full_df <- bind_rows(
  calc_morans_i(influential_index.full.lm),
  calc_morans_i(positive_influence_index.full.lm)
) %>%
  mutate(sample = "full")

coef_full_df <- bind_rows(
  extract_coefs(influential_index.full.lm) %>%
    mutate(model = "Believes\nChina is\ninfluencial\n(index)"),
  extract_coefs(positive_influence_index.full.lm) %>%
    mutate(model = "Belives\nChinese presence\nis positive\n(index)")
) %>%
  mutate(subset = "Full Sample")

#### Restricted Sample
influential_index.restricted.lm <- felm(as.formula(paste0("china_influential_index ~ completed_near_china.pl10.30km.bin + planned_near_china.pl10.30km.bin + completed_near_usaid.30km.bin + planned_near_usaid.30km.bin + ",               IVs_china_usaid," | ",FEs," | 0 | ", CLUSTER_VAR)), data=df[df$sample_restricted %in% T,]) 
positive_influence_index.restricted.lm <- felm(as.formula(paste0("china_positive_influence_index ~ completed_near_china.pl10.30km.bin + planned_near_china.pl10.30km.bin + completed_near_usaid.30km.bin + planned_near_usaid.30km.bin + ", IVs_china_usaid," | ",FEs," | 0 | ", CLUSTER_VAR)), data=df[df$sample_restricted %in% T,]) 

mi_restricted_df <- bind_rows(
  calc_morans_i(influential_index.restricted.lm),
  calc_morans_i(positive_influence_index.restricted.lm)
) %>%
  mutate(sample = "restricted")

coef_restricted_df <- bind_rows(
  extract_coefs(influential_index.restricted.lm) %>%
    mutate(model = "Believes\nChina is\ninfluencial\n(index)"),
  extract_coefs(positive_influence_index.restricted.lm) %>%
    mutate(model = "Belives\nChinese presence\nis positive\n(index)")
) %>%
  mutate(subset = "Restricted Sample")

#### Append Coefficients
coef_df <- bind_rows(coef_full_df,
                     coef_restricted_df) %>%
  mutate(model = model %>%
           factor(levels = c("Belives\nChinese presence\nis positive\n(index)",
                             "Believes\nChina is\ninfluencial\n(index)")))

# Full Table -------------------------------------------------------------------
buffer <- 30
stargazer(influential_index.full.lm,
          positive_influence_index.full.lm,
          dep.var.labels.include = T,
          dep.var.labels = c("China Influential ","China Positive"),
          column.labels   = c("[Index]", "[Index]"),
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
              tryCatch(round(linearHypothesis(influential_index.full.lm, "completed_near_china.pl10.30km.bin = planned_near_china.pl10.30km.bin")[2,4],3), error = function(e) print("NA")),
              tryCatch(round(linearHypothesis(positive_influence_index.full.lm, "completed_near_china.pl10.30km.bin = planned_near_china.pl10.30km.bin")[2,4],3), error = function(e) print("NA")) ),
            #c("Planned Year Cut Off",planned_cutoff_year,planned_cutoff_year),
            c("Morans I P-Value",
              calc_morans_i(influential_index.full.lm)$p.value %>% round(ROUND_NUM),
              calc_morans_i(positive_influence_index.full.lm)$p.value %>% round(ROUND_NUM)
            ),
            c("Spatial Lag of Dep Var Included", "N", "N"),
            c("Country Fixed Effects", "Y", "Y"),
            c("Buffer",buffer,buffer)),
          out=file.path(tables_file_path, "table_01_full.tex"))

# Restricted Table -------------------------------------------------------------
stargazer(influential_index.restricted.lm,
          positive_influence_index.restricted.lm,
          dep.var.labels.include = T,
          dep.var.labels = c("China Influential ","China Positive"),
          column.labels   = c("[Index]", "[Index]"),
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
              tryCatch(round(linearHypothesis(influential_index.restricted.lm, "completed_near_china.pl10.30km.bin = planned_near_china.pl10.30km.bin")[2,4],3), error = function(e) print("NA")),
              tryCatch(round(linearHypothesis(positive_influence_index.restricted.lm, "completed_near_china.pl10.30km.bin = planned_near_china.pl10.30km.bin")[2,4],3), error = function(e) print("NA")) ),
            c("Completed vs. planned $p$-value [US]", 
              tryCatch(round(linearHypothesis(influential_index.restricted.lm, "completed_near_usaid.30km.bin = planned_near_usaid.30km.bin")[2,4],3), error = function(e) print("NA")),
              tryCatch(round(linearHypothesis(positive_influence_index.restricted.lm, "completed_near_usaid.30km.bin = planned_near_usaid.30km.bin")[2,4],3), error = function(e) print("NA")) ),
            #c("Planned Year Cut Off",planned_cutoff_year,planned_cutoff_year),
            c("Morans I P-Value",
              calc_morans_i(influential_index.restricted.lm)$p.value %>% round(ROUND_NUM),
              calc_morans_i(positive_influence_index.restricted.lm)$p.value %>% round(ROUND_NUM)
            ),
            c("Spatial Lag of Dep Var Included","N", "N"),
            c("Country Fixed Effects", "Y","Y"),
            c("Buffer",buffer,buffer)),
          out=file.path(tables_file_path, "table_01_restricted.tex"))

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
                file_name = "figure_01.png")

# Morans I ---------------------------------------------------------------------
bind_rows(mi_full_df,
          mi_restricted_df) %>%
  mutate(figure = "fig_01") %>%
  write.csv(file.path(data_file_path, "morans_i", "mi_01.csv"), row.names = F)


  