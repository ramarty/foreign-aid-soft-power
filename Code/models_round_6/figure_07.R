# Figure 7 - Don't Know to China Questions

# Load Data --------------------------------------------------------------------
df <- readRDS(file.path(data_file_path, "afro_china_data.Rds"))

# Regressions ------------------------------------------------------------------
#### Full Sample
china_dontknow_index.lm <- felm(as.formula(paste0("china_dontknow_index ~ completed_near_china.pl10.30km.bin + planned_near_china.pl10.30km.bin + ",               IVs_china," | ",FEs," | 0 | ", CLUSTER_VAR)), data=df[df$sample_full %in% T,]) 
china.influence.econ.activity_DONTKNOW.lm <- felm(as.formula(paste0("china.influence.econ.activity_DONTKNOW ~ completed_near_china.pl10.30km.bin + planned_near_china.pl10.30km.bin + ",               IVs_china," | ",FEs," | 0 | ", CLUSTER_VAR)), data=df[df$sample_full %in% T,]) 
china.econpol.influence.positive_DONTKNOW.lm <- felm(as.formula(paste0("china.econpol.influence.positive_DONTKNOW ~ china.econpol.influence.positive_DONTKNOW_splag + completed_near_china.pl10.30km.bin + planned_near_china.pl10.30km.bin + ",               IVs_china," | ",FEs," | 0 | ", CLUSTER_VAR)), data=df[df$sample_full %in% T,]) 
china.aid.good.job.meet.country.needs_DONTKNOW.lm <- felm(as.formula(paste0("china.aid.good.job.meet.country.needs_DONTKNOW ~ completed_near_china.pl10.30km.bin + planned_near_china.pl10.30km.bin + ",               IVs_china," | ",FEs," | 0 | ", CLUSTER_VAR)), data=df[df$sample_full %in% T,]) 
china.help.country_DONTKNOW.lm <- felm(as.formula(paste0("china.help.country_DONTKNOW ~ china.help.country_DONTKNOW_splag + completed_near_china.pl10.30km.bin + planned_near_china.pl10.30km.bin + ",               IVs_china," | ",FEs," | 0 | ", CLUSTER_VAR)), data=df[df$sample_full %in% T,]) 

coef_df <- bind_rows(
  extract_coefs(china_dontknow_index.lm) %>%
    mutate(model = "China Questions\nDon't Know Index"),
  
  extract_coefs(china.influence.econ.activity_DONTKNOW.lm) %>%
    mutate(model = "How much influence does\nChina economic activities\nhave on economy [Don't Know]"),
  
  extract_coefs(china.econpol.influence.positive_DONTKNOW.lm) %>%
    mutate(model = "China has positive or\nnegative economic and\npolitical influence [Don't Know]"),
  
  extract_coefs(china.aid.good.job.meet.country.needs_DONTKNOW.lm) %>%
    mutate(model = "Chinese aid does good or\nbad job to meet\ncountry's needs [Don't Know]"),

  extract_coefs(china.help.country_DONTKNOW.lm) %>%
    mutate(model = "How much does China\nhelp the country [Don't Know]")
) %>%
  mutate(subset = "Full Sample")

#### Append Coefficients
coef_df <- coef_df %>%
  dplyr::mutate(model = model %>%
                  factor(levels = rev(c("China Questions\nDon't Know Index",
                                        "How much influence does\nChina economic activities\nhave on economy [Don't Know]",
                                        "China has positive or\nnegative economic and\npolitical influence [Don't Know]",
                                        "Chinese aid does good or\nbad job to meet\ncountry's needs [Don't Know]",
                                        "How much does China\nhelp the country [Don't Know]"))))

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
                file_name = "figure_07.png")

# Full Table -------------------------------------------------------------------
buffer <- 30
planned_cutoff_year <- 2010

stargazer(china_dontknow_index.lm,
          china.influence.econ.activity_DONTKNOW.lm,
          china.econpol.influence.positive_DONTKNOW.lm,
          china.aid.good.job.meet.country.needs_DONTKNOW.lm,
          china.help.country_DONTKNOW.lm,
          dep.var.labels.include = T,
          dep.var.labels = c("Chinese Q Don't", "Chinese Econ Activity",  "Econ/Pol Influ.",   "Chinese Aid Meets",     "China Helps"),
          column.labels   = c("Know [Index]",   "Influ. Economy [DK]",    "Pos. or Neg. [DK]", "Country's Needs [DK]",  "Country [DK]"),
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
              tryCatch(round(linearHypothesis(china_dontknow_index.lm, "completed_near_china.pl10.30km.bin = planned_near_china.pl10.30km.bin")[2,4],3), error = function(e) print("NA")),
              tryCatch(round(linearHypothesis(china.influence.econ.activity_DONTKNOW.lm, "completed_near_china.pl10.30km.bin = planned_near_china.pl10.30km.bin")[2,4],3), error = function(e) print("NA")),
              tryCatch(round(linearHypothesis(china.econpol.influence.positive_DONTKNOW.lm, "completed_near_china.pl10.30km.bin = planned_near_china.pl10.30km.bin")[2,4],3), error = function(e) print("NA")),
              tryCatch(round(linearHypothesis(china.aid.good.job.meet.country.needs_DONTKNOW.lm, "completed_near_china.pl10.30km.bin = planned_near_china.pl10.30km.bin")[2,4],3), error = function(e) print("NA")),
              tryCatch(round(linearHypothesis(china.help.country_DONTKNOW.lm, "completed_near_china.pl10.30km.bin = planned_near_china.pl10.30km.bin")[2,4],3), error = function(e) print("NA")) ),
            c("Planned Year Cut Off",planned_cutoff_year,planned_cutoff_year,planned_cutoff_year,planned_cutoff_year,planned_cutoff_year),
            c("Morans I P-Value",
              calc_morans_i(china_dontknow_index.lm)$p.value %>% round(ROUND_NUM),
              calc_morans_i(china.influence.econ.activity_DONTKNOW.lm)$p.value %>% round(ROUND_NUM),
              calc_morans_i(china.econpol.influence.positive_DONTKNOW.lm)$p.value %>% round(ROUND_NUM),
              calc_morans_i(china.aid.good.job.meet.country.needs_DONTKNOW.lm)$p.value %>% round(ROUND_NUM),
              calc_morans_i(china.help.country_DONTKNOW.lm)$p.value %>% round(ROUND_NUM)
            ),
            c("Spatial Lag of Dep Var Included", "N", "N", "Y", "N", "Y"),
            c("Country Fixed Effects", "Y", "Y", "Y","Y", "Y"),
            c("Buffer",buffer,buffer,buffer,buffer, buffer)),
          out=file.path(tables_file_path, "table_07_full.tex"))



