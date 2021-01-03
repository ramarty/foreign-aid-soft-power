# Figure 2 - Componenets

# Load Data --------------------------------------------------------------------
df <- readRDS(file.path(data_file_path, "afro_china_data.Rds"))

# Restrict to rounds 2-5
df <- df %>%
  filter(afro.round %in% 2:5)

# Regressions ------------------------------------------------------------------
#### Restricted Sample
lib_dem_val_index.lm                          <- felm(as.formula(paste0("lib_dem_val_index ~               lib_dem_val_index_splag +               completed_near_ukaid.30km.bin + planned_near_ukaid.30km.bin + completed_near_usaid.30km.bin + planned_near_usaid.30km.bin + ", IVs_china_usaid," | ",FEs," | 0 | ", CLUSTER_VAR)), data=df[df$sample_restricted_uk %in% T,]) 
blvs_mult_parties_good.restricted.lm          <- felm(as.formula(paste0("blvs_mult_parties_good ~          blvs_mult_parties_good_splag +          completed_near_ukaid.30km.bin + planned_near_ukaid.30km.bin + completed_near_usaid.30km.bin + planned_near_usaid.30km.bin + ", IVs_china_usaid," | ",FEs," | 0 | ", CLUSTER_VAR)), data=df[df$sample_restricted_uk %in% T,]) 
blvs_mult_parties_create_choice.restricted.lm <- felm(as.formula(paste0("blvs_mult_parties_create_choice ~ blvs_mult_parties_create_choice_splag + completed_near_ukaid.30km.bin + planned_near_ukaid.30km.bin + completed_near_usaid.30km.bin + planned_near_usaid.30km.bin + ", IVs_china_usaid," | ",FEs," | 0 | ", CLUSTER_VAR)), data=df[df$sample_restricted_uk %in% T,]) 
blvs_ctzn_should_join_any_cso.restricted.lm   <- felm(as.formula(paste0("blvs_ctzn_should_join_any_cso ~   blvs_ctzn_should_join_any_cso_splag +   completed_near_ukaid.30km.bin + planned_near_ukaid.30km.bin + completed_near_usaid.30km.bin + planned_near_usaid.30km.bin + ", IVs_china_usaid," | ",FEs," | 0 | ", CLUSTER_VAR)), data=df[df$sample_restricted_uk %in% T,]) 
blvs_democ_best_system.restricted.lm          <- felm(as.formula(paste0("blvs_democ_best_system ~          blvs_democ_best_system_splag +          completed_near_ukaid.30km.bin + planned_near_ukaid.30km.bin + completed_near_usaid.30km.bin + planned_near_usaid.30km.bin + ", IVs_china_usaid," | ",FEs," | 0 | ", CLUSTER_VAR)), data=df[df$sample_restricted_uk %in% T,]) 
blvs_elec_good.restricted.lm                  <- felm(as.formula(paste0("blvs_elec_good ~                  blvs_elec_good_splag +                  completed_near_ukaid.30km.bin + planned_near_ukaid.30km.bin + completed_near_usaid.30km.bin + planned_near_usaid.30km.bin + ", IVs_china_usaid," | ",FEs," | 0 | ", CLUSTER_VAR)), data=df[df$sample_restricted_uk %in% T,]) 

coef_df <- bind_rows(
  extract_coefs(lib_dem_val_index.lm) %>%
    mutate(model = "Liberal\ndemocratic\nvalues\n(index)"),
  
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
coef_df <- coef_df %>%
  dplyr::mutate(model = model %>%
                  factor(levels = rev(c("Liberal\ndemocratic\nvalues\n(index)",
                                        "Believes\nmultiple\nparties\nare good",
                                        "Believes\nmultiple\nparties\ncreate choice",
                                        "Believes\ncitizens\nshould join\nany CSO",
                                        "Believes\ndemocracy\nis best\nsystem",
                                        "Believes\nelections\nare good"))))

coef_df <- coef_df %>%
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
                    "UK Aid Planned"))

# Figure -----------------------------------------------------------------------
coef_df %>%
  filter(model == "Liberal\ndemocratic\nvalues\n(index)") %>%
  make_plot_all(height = 3,
                width = 7,
                file_name = "figure_04.png")

# Figure -----------------------------------------------------------------------
coef_df %>%
  filter(model != "Liberal\ndemocratic\nvalues\n(index)") %>%
  make_plot_all(height = 7,
                width = 7,
                file_name = "figure_04_components.png")

# Restricted Table -------------------------------------------------------------
buffer <- 30
planned_cutoff_year <- "N/A"
stargazer(lib_dem_val_index.lm,
          blvs_mult_parties_good.restricted.lm,
          blvs_mult_parties_create_choice.restricted.lm,
          blvs_ctzn_should_join_any_cso.restricted.lm,
          blvs_democ_best_system.restricted.lm,
          blvs_elec_good.restricted.lm,
          dep.var.labels.include = T,
          dep.var.labels = c("Liberal Democratic", "Multiple Pol.", "Multiple Pol.",          "Can Join","Democracy", "Elections"),
          column.labels   = c("Values Index", "Parties Good", "Parties Create Choice",  "Any CSO", "Best System", "Good"),
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
              tryCatch(round(linearHypothesis(lib_dem_val_index.lm, "completed_near_usaid.30km.bin = planned_near_usaid.30km.bin")[2,4],3), error = function(e) print("NA")),
              tryCatch(round(linearHypothesis(blvs_mult_parties_good.restricted.lm, "completed_near_usaid.30km.bin = planned_near_usaid.30km.bin")[2,4],3), error = function(e) print("NA")),
              tryCatch(round(linearHypothesis(blvs_mult_parties_create_choice.restricted.lm, "completed_near_usaid.30km.bin = planned_near_usaid.30km.bin")[2,4],3), error = function(e) print("NA")),
              tryCatch(round(linearHypothesis(blvs_ctzn_should_join_any_cso.restricted.lm, "completed_near_usaid.30km.bin = planned_near_usaid.30km.bin")[2,4],3), error = function(e) print("NA")),
              tryCatch(round(linearHypothesis(blvs_democ_best_system.restricted.lm, "completed_near_usaid.30km.bin = planned_near_usaid.30km.bin")[2,4],3), error = function(e) print("NA")),
              tryCatch(round(linearHypothesis(blvs_elec_good.restricted.lm, "completed_near_usaid.30km.bin = planned_near_usaid.30km.bin")[2,4],3), error = function(e) print("NA")) ),
            c("Completed vs. planned $p$-value [UK]", 
              tryCatch(round(linearHypothesis(lib_dem_val_index.lm, "completed_near_ukaid.30km.bin = planned_near_ukaid.30km.bin")[2,4],3), error = function(e) print("NA")),
              tryCatch(round(linearHypothesis(blvs_mult_parties_good.restricted.lm, "completed_near_ukaid.30km.bin = planned_near_ukaid.30km.bin")[2,4],3), error = function(e) print("NA")),
              tryCatch(round(linearHypothesis(blvs_mult_parties_create_choice.restricted.lm, "completed_near_ukaid.30km.bin = planned_near_ukaid.30km.bin")[2,4],3), error = function(e) print("NA")),
              tryCatch(round(linearHypothesis(blvs_ctzn_should_join_any_cso.restricted.lm, "completed_near_ukaid.30km.bin = planned_near_ukaid.30km.bin")[2,4],3), error = function(e) print("NA")),
              tryCatch(round(linearHypothesis(blvs_democ_best_system.restricted.lm, "completed_near_ukaid.30km.bin = planned_near_ukaid.30km.bin")[2,4],3), error = function(e) print("NA")),
              tryCatch(round(linearHypothesis(blvs_elec_good.restricted.lm, "completed_near_ukaid.30km.bin = planned_near_ukaid.30km.bin")[2,4],3), error = function(e) print("NA")) ),
            c("Planned Year Cut Off",planned_cutoff_year,planned_cutoff_year,planned_cutoff_year,planned_cutoff_year,planned_cutoff_year,planned_cutoff_year),
            c("Morans I P-Value",
              calc_morans_i(lib_dem_val_index.lm)$p.value %>% round(ROUND_NUM),
              calc_morans_i(blvs_mult_parties_good.restricted.lm)$p.value %>% round(ROUND_NUM),
              calc_morans_i(blvs_mult_parties_create_choice.restricted.lm)$p.value %>% round(ROUND_NUM),
              calc_morans_i(blvs_ctzn_should_join_any_cso.restricted.lm)$p.value %>% round(ROUND_NUM),
              calc_morans_i(blvs_democ_best_system.restricted.lm)$p.value %>% round(ROUND_NUM),
              calc_morans_i(blvs_elec_good.restricted.lm)$p.value %>% round(ROUND_NUM)
            ),
            c("Spatial Lag of Dep Var Included", "Y", "Y", "Y", "Y", "Y", "Y"),
            c("Country Fixed Effects", "Y", "Y", "Y","Y", "Y", "Y"),
            c("Buffer",buffer,buffer,buffer,buffer, buffer)),
          out=file.path(tables_file_path, "table_04_restricted.tex"))


