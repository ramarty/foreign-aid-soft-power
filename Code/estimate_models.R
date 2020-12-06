# Figure 2

# Load Data --------------------------------------------------------------------
df <- readRDS(file.path(data_file_path, "afro_china_data.Rds"))

# Regressions ------------------------------------------------------------------
coefs_all_df <- lapply(seq(from = 5, to = 50, by = 30), function(buffer){
  
  print(paste(buffer, "------------------------------------------------------"))
  
  # Rounds 2-5 Models ----------------------------------------------------------
  r2_5 <- lapply(c("lib_dem_val_index",
                   "blvs_mult_parties_good",
                   "blvs_mult_parties_create_choice",
                   "blvs_ctzn_should_join_any_cso",
                   "blvs_democ_best_system",
                   "blvs_democ_best_system"), function(dv){
                     print(dv)
                     
                     lm.full       <- felm(as.formula(paste0(dv, " ~ completed_near_china.pl10.",buffer,"km.bin + planned_near_china.pl10.",buffer,"km.bin + ",                                                                               IVs_china,      " | ",FEs," | 0 | ", CLUSTER_VAR)), data=df[(df$sample_full %in% T) &       (df$afro.round %in% 2:5),]) 
                     lm.restricted <- felm(as.formula(paste0(dv, " ~ completed_near_china.pl10.",buffer,"km.bin + planned_near_china.pl10.",buffer,"km.bin + completed_near_usaid.",buffer,"km.bin + planned_near_usaid.",buffer,"km.bin + ", IVs_china_usaid," | ",FEs," | 0 | ", CLUSTER_VAR)), data=df[(df$sample_restricted %in% T) & (df$afro.round %in% 2:5),]) 
                     
                     coefs <- bind_rows(
                       lm.full       %>% extract_coefs() %>% mutate(subset = "full"),
                       lm.restricted %>% extract_coefs() %>% mutate(subset = "restricted")
                     ) %>%
                       mutate(dv = all_of(dv),
                              round = "2-5")
                     
                     out <- list()
                     out[["coefs"]] <- coefs
                     out[[paste(dv, buffer, "full", sep="_")]] <- lm.full
                     out[[paste(dv, buffer, "restricted", sep="_")]] <- lm.restricted
                     
                     return(out)
                     
                   })
  
  r2_5_coefs <- lapply(r2_5, function(l){
    l$coefs
  }) %>%
    bind_rows()
  
  r2_5_models <- lapply(r2_5, function(l){
    l_model <- l
    l_model$coefs <- NULL
    l_model
  }) %>%
    do.call(what = "c")
  
  # Rounds 4 Models ------------------------------------------------------------
  r4 <- lapply(c("china.help.country", 
                 "usa.help.country"), function(dv){
                   print(dv)
                   
                   lm.full <- felm(as.formula(paste0(dv, " ~              completed_near_china.pl10.",buffer,"km.bin + planned_near_china.pl10.",buffer,"km.bin + ", IVs_china," | ",FEs," | 0 | ", CLUSTER_VAR)), data=df[(df$sample_full %in% T) & (df$afro.round %in% 4),]) 
                   
                   coefs <- lm.full %>% 
                     extract_coefs() %>% 
                     mutate(subset = "full",
                            dv = all_of(dv),
                            round = "4")
                   
                   out <- list()
                   out[["coefs"]] <- coefs
                   out[[paste(dv, buffer, "full", sep="_")]] <- lm.full
                   
                   return(out)
                   
                 })
  
  r4_coefs <- lapply(r4, function(l){
    l$coefs
  }) %>%
    bind_rows()
  
  r4_models <- lapply(r4, function(l){
    l_model <- l
    l_model$coefs <- NULL
    l_model
  }) %>%
    do.call(what = "c")
  
  # Rounds 6 Models ------------------------------------------------------------
  r6 <- lapply(c("china_influential_index",
                 "china_positive_influence_index",
                 "china.most.influence", 
                 "usa.most.influence", 
                 "china.best.dev.model",
                 "usa.best.dev.model",
                 "formcolnpower.most.influence",
                 "formcolnpower.best.dev.model",
                 "posimage_chinesepeople",
                 "posimage_businessinvetment",
                 "posimage_infordevinvetment",
                 "posimage_noninterference",
                 "posimage_supportinintlaffiars",
                 "posimage_productcost",
                 "negimage_cooperateundemocratic",
                 "negimage_chinesecitizenbehavior",
                 "negimage_resourceextraction",
                 "negimage_takingjobsbusiness",
                 "negimage_landgrabbing",
                 "negimage_productquality",
                 "china_dontknow_index"), function(dv){
                   print(dv)
                   
                   lm.full       <- felm(as.formula(paste0(dv, " ~ completed_near_china.pl10.",buffer,"km.bin + planned_near_china.pl10.",buffer,"km.bin + ",                                                                               IVs_china,      " | ",FEs," | 0 | ", CLUSTER_VAR)), data=df[(df$sample_full %in% T)       & (df$afro.round %in% 6),]) 
                   lm.restricted <- felm(as.formula(paste0(dv, " ~ completed_near_china.pl10.",buffer,"km.bin + planned_near_china.pl10.",buffer,"km.bin + completed_near_usaid.",buffer,"km.bin + planned_near_usaid.",buffer,"km.bin + ", IVs_china_usaid," | ",FEs," | 0 | ", CLUSTER_VAR)), data=df[(df$sample_restricted %in% T) & (df$afro.round %in% 6),]) 
                   
                   coefs <- bind_rows(
                     lm.full       %>% extract_coefs() %>% mutate(subset = "full"),
                     lm.restricted %>% extract_coefs() %>% mutate(subset = "restricted")
                   ) %>%
                     mutate(dv = all_of(dv),
                            round = "6")
                   
                   out <- list()
                   out[["coefs"]] <- coefs
                   out[[paste(dv, buffer, "full", sep="_")]] <- lm.full
                   out[[paste(dv, buffer, "restricted", sep="_")]] <- lm.restricted
                   
                   return(out)
                   
                 })
  
  r6_coefs <- lapply(r6, function(l){
    l$coefs
  }) %>%
    bind_rows()
  
  r6_models <- lapply(r6, function(l){
    l_model <- l
    l_model$coefs <- NULL
    l_model
  }) %>%
    do.call(what = "c")
  
  # Rounds 2-5: US/UK Models ---------------------------------------------------
  r2_5usuk <- lapply(c("lib_dem_val_index", 
                       "blvs_mult_parties_good",
                       "blvs_mult_parties_create_choice",
                       "blvs_ctzn_should_join_any_cso",
                       "blvs_democ_best_system",
                       "blvs_elec_good"), function(dv){
                         print(dv)
                         
                         lm.restricted.usuk <- felm(as.formula(paste0(dv, " ~               completed_near_ukaid.",buffer,"km.bin + planned_near_ukaid.",buffer,"km.bin + completed_near_usaid.",buffer,"km.bin + planned_near_usaid.",buffer,"km.bin + ", IVs_china_usaid," | ",FEs," | 0 | ", CLUSTER_VAR)), data=df[(df$sample_restricted_uk %in% T) & (df$afro.round %in% 2:5),]) 
                         
                         coefs <- lm.restricted.usuk %>% 
                           extract_coefs() %>% 
                           mutate(subset = "restrictedusuk",
                                  dv = all_of(dv),
                                  round = "2-5")
                         
                         out <- list()
                         out[["coefs"]] <- coefs
                         out[[paste(dv, buffer, "restrictedusuk", sep="_")]] <- lm.restricted.usuk
                         
                         return(out)
                         
                       })
  
  r2_5usuk_coefs <- lapply(r2_5usuk, function(l){
    l$coefs
  }) %>%
    bind_rows()
  
  r2_5usuk_models <- lapply(r2_5usuk, function(l){
    l_model <- l
    l_model$coefs <- NULL
    l_model
  }) %>%
    do.call(what = "c")
  
  # Append All Together --------------------------------------------------------
  coefs_all <- bind_rows(
    r2_5usuk_coefs,
    r6_coefs, 
    r4_coefs, 
    r2_5_coefs 
  ) 
  coefs_all$var <- coefs_all$var %>% str_replace_all(paste0(buffer, "km"), "km")
  coefs_all$buffer <- buffer
  
  models_all <- c(
    r2_5usuk_models,
    r6_models, 
    r4_models, 
    r2_5_models
  )
  
  saveRDS(models_all, file.path(results_file_path, paste0("models_",buffer,"km.Rds")))
  
  return(coefs_all)
}) %>%
  bind_rows()

# Restrict Variables -----------------------------------------------------------
coefs_all_df <- coefs_all_df %>%
  mutate(var = var %>% 
           str_replace("completed_near_china.pl10.km.bin", 
                       "Chinese Aid Completed") %>%
           str_replace("planned_near_china.pl10.km.bin", 
                       "Chinese Aid Planned") %>%
           str_replace("completed_near_usaid.km.bin", 
                       "USA Aid Completed") %>%
           str_replace("planned_near_usaid.km.bin", 
                       "USA Aid Planned") %>%
           str_replace("completed_near_ukaid.km.bin", 
                       "UK Aid Completed") %>%
           str_replace("planned_near_ukaid.km.bin", 
                       "UK Aid Planned")) %>%
  filter(var %in% c("Chinese Aid Completed",
                    "Chinese Aid Planned",
                    "USA Aid Completed",
                    "USA Aid Planned",
                    "UK Aid Completed",
                    "UK Aid Planned"))

# Name DVs ---------------------------------------------------------------------
coefs_all_df$dv_clean <- NA
coefs_all_df$dv_clean[coefs_all_df$dv %in% "lib_dem_val_index"] <- "Liberal\ndemocratic\nvalues\n(index)"
coefs_all_df$dv_clean[coefs_all_df$dv %in% "blvs_mult_parties_good"] <- "Believes\nmultiple\nparties\nare good"
coefs_all_df$dv_clean[coefs_all_df$dv %in% "blvs_mult_parties_create_choice"] <- "Believes\nmultiple\nparties\ncreate choice"
coefs_all_df$dv_clean[coefs_all_df$dv %in% "blvs_ctzn_should_join_any_cso"] <- "Believes\ncitizens\nshould join\nany CSO"
coefs_all_df$dv_clean[coefs_all_df$dv %in% "blvs_democ_best_system"] <- "Believes\ndemocracy\nis best\nsystem"
coefs_all_df$dv_clean[coefs_all_df$dv %in% "blvs_elec_good"] <- "Believes\nelections\nare good"
coefs_all_df$dv_clean[coefs_all_df$dv %in% "china_influential_index"] <- "Believes\nChina is\ninfluencial\n(index)"
coefs_all_df$dv_clean[coefs_all_df$dv %in% "china_positive_influence_index"] <- "Belives\nChinese presence\nis positive\n(index)"
coefs_all_df$dv_clean[coefs_all_df$dv %in% "formcolnpower.most.influence"] <- "Believes\nformer colonial\npower is most\ninfluential"
coefs_all_df$dv_clean[coefs_all_df$dv %in% "formcolnpower.best.dev.model"] <- "Believes\nformer colonial\npower is best\nmodel"
coefs_all_df$dv_clean[coefs_all_df$dv %in% "posimage_chinesepeople"] <- "Positive Image:\nChinese\npeople and\nculture"
coefs_all_df$dv_clean[coefs_all_df$dv %in% "posimage_businessinvetment"] <- "Positive Image:\nChinese\nbusiness\ninvestment"
coefs_all_df$dv_clean[coefs_all_df$dv %in% "posimage_infordevinvetment"] <- "Positive Image:\nChinese\ninfrastructure\ninvestment"
coefs_all_df$dv_clean[coefs_all_df$dv %in% "posimage_noninterference"] <- "Positive Image:\nChinese\npolice of non-interference"
coefs_all_df$dv_clean[coefs_all_df$dv %in% "posimage_supportinintlaffiars"] <- "Positive Image:\nChinese\nsupport in\ninternational\naffairs"
coefs_all_df$dv_clean[coefs_all_df$dv %in% "posimage_productcost"] <- "Positive Image:\nCost of\nChinese\nproducts"
coefs_all_df$dv_clean[coefs_all_df$dv %in% "negimage_cooperateundemocratic"] <- "Negative Image:\nChinese\ncooperation\nw/ undemocratic\nleaders"
coefs_all_df$dv_clean[coefs_all_df$dv %in% "negimage_chinesecitizenbehavior"] <- "Negative Image:\nBehaviours of\nChinese\ncitizens"
coefs_all_df$dv_clean[coefs_all_df$dv %in% "negimage_resourceextraction"] <- "Negative Image:\nChinese\nextraction of\nnatural\nresources"
coefs_all_df$dv_clean[coefs_all_df$dv %in% "negimage_takingjobsbusiness"] <- "Negative Image:\nChinese\nfirms taking\nlocal jobs\nand businesses"
coefs_all_df$dv_clean[coefs_all_df$dv %in% "negimage_landgrabbing"] <- "Negative Image:\nChinese\nland\ngrabbing"
coefs_all_df$dv_clean[coefs_all_df$dv %in% "negimage_productquality"] <- "Negative Image:\nQuality of\nChinese\nproducts"
coefs_all_df$dv_clean[coefs_all_df$dv %in% "china_dontknow_index"] <- "China Questions\nDon't Know Index"
coefs_all_df$dv_clean[coefs_all_df$dv %in% "china.help.country"] <- "Believes China\nHelps Country"
coefs_all_df$dv_clean[coefs_all_df$dv %in% "usa.help.country"] <- "Believes US\nHelps Country"
coefs_all_df$dv_clean[coefs_all_df$dv %in% "china.most.influence"] <- "Believes\nChinese model\nis most\ninfluential"
coefs_all_df$dv_clean[coefs_all_df$dv %in% "usa.most.influence"] <- "Believes\nUS model\nis most\ninfluential"
coefs_all_df$dv_clean[coefs_all_df$dv %in% "china.best.dev.model"] <- "Believes\nChinese model\nis best"
coefs_all_df$dv_clean[coefs_all_df$dv %in% "usa.best.dev.model"] <- "Believes\nUS model\nis best"

# Restricted/Full Clean Variable -----------------------------------------------
coefs_all_df$subset_clean <- NA
coefs_all_df$subset_clean[coefs_all_df$subset %in% "full"] <- "Full Sample"
coefs_all_df$subset_clean[coefs_all_df$subset %in% c("restricted", "restrictedusuk")] <- "Restricted Sample"

# Export -----------------------------------------------------------------------
saveRDS(coefs_all_df,    file.path(results_file_path, "coefficients.Rds"))

