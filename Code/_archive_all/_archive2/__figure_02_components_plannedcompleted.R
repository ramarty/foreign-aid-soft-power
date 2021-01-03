# Figure 2 - Componenets

# Load Data --------------------------------------------------------------------
df <- readRDS(file.path(data_file_path, "afro_china_data.Rds"))

# Restrict to rounds 2-5
df <- df %>%
  filter(afro.round %in% 2:5)

# Regressions ------------------------------------------------------------------
#### Full Sample
blvs_mult_parties_good.full.lm <- felm(as.formula(paste0("blvs_mult_parties_good ~                   completed_near_china.plNA.30km.bin + planned_near_china.plNA.30km.bin + ",              IVs_china," | ",FEs," | 0 | ", CLUSTER_VAR)), data=df[df$sample_full %in% T,]) 
blvs_mult_parties_create_choice.full.lm <- felm(as.formula(paste0("blvs_mult_parties_create_choice ~ completed_near_china.plNA.30km.bin + planned_near_china.plNA.30km.bin + ",IVs_china," | ",FEs," | 0 | ", CLUSTER_VAR)), data=df[df$sample_full %in% T,]) 
blvs_ctzn_should_join_any_cso.full.lm <- felm(as.formula(paste0("blvs_ctzn_should_join_any_cso ~     completed_near_china.plNA.30km.bin + planned_near_china.plNA.30km.bin + ",              IVs_china," | ",FEs," | 0 | ", CLUSTER_VAR)), data=df[df$sample_full %in% T,]) 
blvs_democ_best_system.full.lm <- felm(as.formula(paste0("blvs_democ_best_system ~         completed_near_china.plNA.30km.bin + planned_near_china.plNA.30km.bin + ",IVs_china," | ",FEs," | 0 | ", CLUSTER_VAR)), data=df[df$sample_full %in% T,]) 
blvs_elec_good.full.lm <- felm(as.formula(paste0("blvs_elec_good ~ completed_near_china.plNA.30km.bin + planned_near_china.plNA.30km.bin + ",IVs_china," | ",FEs," | 0 | ", CLUSTER_VAR)), data=df[df$sample_full %in% T,]) 

mi_full_df <- bind_rows(
  calc_morans_i(blvs_mult_parties_good.full.lm),
  calc_morans_i(blvs_mult_parties_create_choice.full.lm),
  calc_morans_i(blvs_ctzn_should_join_any_cso.full.lm),
  calc_morans_i(blvs_democ_best_system.full.lm),
  calc_morans_i(blvs_elec_good.full.lm)
) %>%
  mutate(sample = "full")

coef_full_df <- bind_rows(
  extract_coefs(blvs_mult_parties_good.full.lm) %>%
    mutate(model = "Believes\nmultiple\nparties\nare good"),
  
  extract_coefs(blvs_mult_parties_create_choice.full.lm) %>%
    mutate(model = "Believes\nmultiple\nparties\ncreate choice"),
  
  extract_coefs(blvs_ctzn_should_join_any_cso.full.lm) %>%
    mutate(model = "Believes\ncitizens\nshould join\nany CSO"),
  
  extract_coefs(blvs_democ_best_system.full.lm) %>%
    mutate(model = "Believes\ndemocracy\nis best\nsystem"),
  
  extract_coefs(blvs_elec_good.full.lm) %>%
    mutate(model = "Believes\nelections\nare good")
) %>%
  mutate(subset = "Full Sample")

#### Restricted Sample
blvs_mult_parties_good.restricted.lm <- felm(as.formula(paste0("blvs_mult_parties_good ~ blvs_mult_parties_good_splag + completed_near_china.plNA.30km.bin + planned_near_china.plNA.30km.bin + completed_near_usaid.30km.bin + planned_near_usaid.30km.bin + ",              IVs_china," | ",FEs," | 0 | ", CLUSTER_VAR)), data=df[df$sample_restricted %in% T,]) 
blvs_mult_parties_create_choice.restricted.lm <- felm(as.formula(paste0("blvs_mult_parties_create_choice ~ blvs_mult_parties_create_choice_splag + completed_near_china.plNA.30km.bin + planned_near_china.plNA.30km.bin + completed_near_usaid.30km.bin + planned_near_usaid.30km.bin + ",IVs_china," | ",FEs," | 0 | ", CLUSTER_VAR)), data=df[df$sample_restricted %in% T,]) 
blvs_ctzn_should_join_any_cso.restricted.lm <- felm(as.formula(paste0("blvs_ctzn_should_join_any_cso ~ blvs_ctzn_should_join_any_cso_splag + completed_near_china.plNA.30km.bin + planned_near_china.plNA.30km.bin + completed_near_usaid.30km.bin + planned_near_usaid.30km.bin + ",              IVs_china," | ",FEs," | 0 | ", CLUSTER_VAR)), data=df[df$sample_restricted %in% T,]) 
blvs_democ_best_system.restricted.lm <- felm(as.formula(paste0("blvs_democ_best_system ~ blvs_democ_best_system_splag + completed_near_china.plNA.30km.bin + planned_near_china.plNA.30km.bin + completed_near_usaid.30km.bin + planned_near_usaid.30km.bin + ",IVs_china," | ",FEs," | 0 | ", CLUSTER_VAR)), data=df[df$sample_restricted %in% T,]) 
blvs_elec_good.restricted.lm <- felm(as.formula(paste0("blvs_elec_good ~ blvs_elec_good_splag + completed_near_china.plNA.30km.bin + planned_near_china.plNA.30km.bin + completed_near_usaid.30km.bin + planned_near_usaid.30km.bin + ",IVs_china," | ",FEs," | 0 | ", CLUSTER_VAR)), data=df[df$sample_restricted %in% T,]) 

mi_restricted_df <- bind_rows(
  calc_morans_i(blvs_mult_parties_good.restricted.lm),
  calc_morans_i(blvs_mult_parties_create_choice.restricted.lm),
  calc_morans_i(blvs_ctzn_should_join_any_cso.restricted.lm),
  calc_morans_i(blvs_democ_best_system.restricted.lm),
  calc_morans_i(blvs_elec_good.restricted.lm)
) %>%
  mutate(sample = "restricted")

coef_restricted_df <- bind_rows(
  extract_coefs(blvs_mult_parties_good.restricted.lm) %>%
    mutate(model = "Believes\nmultiple\nparties\nare good"),
  
  extract_coefs(blvs_mult_parties_create_choice.restricted.lm) %>%
    mutate(model = "Believes\nmultiple\nparties\ncreate choice"),
  
  extract_coefs(blvs_ctzn_should_join_any_cso.restricted.lm) %>%
    mutate(model = "Believes\ncitizens\nshould join\nany CSO"),
  
  extract_coefs(blvs_democ_best_system.restricted.lm) %>%
    mutate(model = "Believes\ndemocracy\nis best\nsystem"),
  
  extract_coefs(blvs_elec_good.restricted.lm) %>%
    mutate(model = "Believes\nelections\nare good")
) %>%
  mutate(subset = "Restricted Sample")

#### Append Coefficients
coef_df <- bind_rows(coef_full_df,
                     coef_restricted_df) %>%
  dplyr::mutate(model = model %>%
           factor(levels = rev(c("Believes\nmultiple\nparties\nare good",
                                 "Believes\nmultiple\nparties\ncreate choice",
                             "Believes\ncitizens\nshould join\nany CSO",
                             "Believes\ndemocracy\nis best\nsystem",
                             "Believes\nelections\nare good"))))

# Figure -----------------------------------------------------------------------
coef_df %>%
  mutate(var = var %>% 
           str_replace("completed_near_china.plNA.30km.bin", 
                                   "Chinese Aid Completed") %>%
           str_replace("planned_near_china.plNA.30km.bin", 
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
                file_name = "figure_02plannedcompleted_components.png")

# Full Table -------------------------------------------------------------------
buffer <- 30
stargazer(blvs_mult_parties_good.full.lm,
          blvs_mult_parties_create_choice.full.lm,
          blvs_ctzn_should_join_any_cso.full.lm,
          blvs_democ_best_system.full.lm,
          blvs_elec_good.full.lm,
          dep.var.labels.include = T,
          dep.var.labels = c("Multiple Pol.", "Multiple Pol.",          "Can Join","Democracy", "Elections"),
          column.labels   = c("Parties Good", "Parties Create Choice",  "Any CSO", "Best System", "Good"),
          keep=c("completed_near_china.plNA.30km.bin", "planned_near_china.plNA.30km.bin"),
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
              tryCatch(round(linearHypothesis(blvs_mult_parties_good.full.lm, "completed_near_china.plNA.30km.bin = planned_near_china.plNA.30km.bin")[2,4],3), error = function(e) print("NA")),
              tryCatch(round(linearHypothesis(blvs_mult_parties_create_choice.full.lm, "completed_near_china.plNA.30km.bin = planned_near_china.plNA.30km.bin")[2,4],3), error = function(e) print("NA")),
              tryCatch(round(linearHypothesis(blvs_ctzn_should_join_any_cso.full.lm, "completed_near_china.plNA.30km.bin = planned_near_china.plNA.30km.bin")[2,4],3), error = function(e) print("NA")),
              tryCatch(round(linearHypothesis(blvs_democ_best_system.full.lm, "completed_near_china.plNA.30km.bin = planned_near_china.plNA.30km.bin")[2,4],3), error = function(e) print("NA")),
              tryCatch(round(linearHypothesis(blvs_elec_good.full.lm, "completed_near_china.plNA.30km.bin = planned_near_china.plNA.30km.bin")[2,4],3), error = function(e) print("NA")) ),
            #c("Planned Year Cut Off",planned_cutoff_year,planned_cutoff_year,planned_cutoff_year,planned_cutoff_year),
            c("Morans I P-Value",
              calc_morans_i(blvs_mult_parties_good.full.lm)$p.value %>% round(ROUND_NUM),
              calc_morans_i(blvs_mult_parties_create_choice.full.lm)$p.value %>% round(ROUND_NUM),
              calc_morans_i(blvs_ctzn_should_join_any_cso.full.lm)$p.value %>% round(ROUND_NUM),
              calc_morans_i(blvs_democ_best_system.full.lm)$p.value %>% round(ROUND_NUM),
              calc_morans_i(blvs_elec_good.full.lm)$p.value %>% round(ROUND_NUM)
            ),
            c("Spatial Lag of Dep Var Included", "N", "Y", "Y", "N", "N"),
            c("Country Fixed Effects", "Y", "Y", "Y","Y", "Y"),
            c("Buffer",buffer,buffer,buffer,buffer, buffer)),
          out=file.path(tables_file_path, "table_02plannedcompleted_components_full.tex"))

# Restricted Table -------------------------------------------------------------
stargazer(blvs_mult_parties_good.restricted.lm,
          blvs_mult_parties_create_choice.restricted.lm,
          blvs_ctzn_should_join_any_cso.restricted.lm,
          blvs_democ_best_system.restricted.lm,
          blvs_elec_good.restricted.lm,
          dep.var.labels.include = T,
          dep.var.labels = c("Multiple Pol.", "Multiple Pol.",          "Can Join","Democracy", "Elections"),
          column.labels   = c("Parties Good", "Parties Create Choice",  "Any CSO", "Best System", "Good"),
          keep=c("completed_near_china.plNA.30km.bin", "planned_near_china.plNA.30km.bin",
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
              tryCatch(round(linearHypothesis(blvs_mult_parties_good.restricted.lm, "completed_near_china.plNA.30km.bin = planned_near_china.plNA.30km.bin")[2,4],3), error = function(e) print("NA")),
              tryCatch(round(linearHypothesis(blvs_mult_parties_create_choice.restricted.lm, "completed_near_china.plNA.30km.bin = planned_near_china.plNA.30km.bin")[2,4],3), error = function(e) print("NA")),
              tryCatch(round(linearHypothesis(blvs_ctzn_should_join_any_cso.restricted.lm, "completed_near_china.plNA.30km.bin = planned_near_china.plNA.30km.bin")[2,4],3), error = function(e) print("NA")),
              tryCatch(round(linearHypothesis(blvs_democ_best_system.restricted.lm, "completed_near_china.plNA.30km.bin = planned_near_china.plNA.30km.bin")[2,4],3), error = function(e) print("NA")),
              tryCatch(round(linearHypothesis(blvs_elec_good.restricted.lm, "completed_near_china.plNA.30km.bin = planned_near_china.plNA.30km.bin")[2,4],3), error = function(e) print("NA")) ),
            c("Completed vs. planned $p$-value [US]", 
              tryCatch(round(linearHypothesis(blvs_mult_parties_good.restricted.lm, "completed_near_usaid.30km.bin = planned_near_usaid.30km.bin")[2,4],3), error = function(e) print("NA")),
              tryCatch(round(linearHypothesis(blvs_mult_parties_create_choice.restricted.lm, "completed_near_usaid.30km.bin = planned_near_usaid.30km.bin")[2,4],3), error = function(e) print("NA")),
              tryCatch(round(linearHypothesis(blvs_ctzn_should_join_any_cso.restricted.lm, "completed_near_usaid.30km.bin = planned_near_usaid.30km.bin")[2,4],3), error = function(e) print("NA")),
              tryCatch(round(linearHypothesis(blvs_democ_best_system.restricted.lm, "completed_near_usaid.30km.bin = planned_near_usaid.30km.bin")[2,4],3), error = function(e) print("NA")),
              tryCatch(round(linearHypothesis(blvs_elec_good.restricted.lm, "completed_near_usaid.30km.bin = planned_near_usaid.30km.bin")[2,4],3), error = function(e) print("NA")) ),
            #c("Planned Year Cut Off",planned_cutoff_year,planned_cutoff_year,planned_cutoff_year,planned_cutoff_year),
            c("Morans I P-Value",
              calc_morans_i(blvs_mult_parties_good.restricted.lm)$p.value %>% round(ROUND_NUM),
              calc_morans_i(blvs_mult_parties_create_choice.restricted.lm)$p.value %>% round(ROUND_NUM),
              calc_morans_i(blvs_ctzn_should_join_any_cso.restricted.lm)$p.value %>% round(ROUND_NUM),
              calc_morans_i(blvs_democ_best_system.restricted.lm)$p.value %>% round(ROUND_NUM),
              calc_morans_i(blvs_elec_good.restricted.lm)$p.value %>% round(ROUND_NUM)
            ),
            c("Spatial Lag of Dep Var Included", "Y", "Y", "Y", "Y", "Y"),
            c("Country Fixed Effects", "Y", "Y", "Y","Y", "Y"),
            c("Buffer",buffer,buffer,buffer,buffer, buffer)),
          out=file.path(tables_file_path, "table_02plannedcompleted_components_restricted.tex"))




  