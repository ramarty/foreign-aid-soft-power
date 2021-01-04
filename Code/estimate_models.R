# Figure 2

save_model <- T

# Load Data --------------------------------------------------------------------
df <- readRDS(file.path(data_file_path, "afro_china_data.Rds"))

for(model_type in 1:4){
  
  results_file_path <- file.path(data_results_file_path, 
                                 paste0("models_type_", model_type),
                                 "models")
  
  ## Main Models
  if(model_type %in% 1){
    include_splag <- F
    IVs_china_r2_5       <- IVs_china
    IVs_china_usaid_r2_5 <- IVs_china_usaid
    IVs_china_r6         <- IVs_china
    IVs_china_usaid_r6   <- IVs_china_usaid
    
    FEs <- "iso + afro.round"
  }
  
  ## Include splag
  if(model_type %in% 2){
    include_splag <- T
    IVs_china_r2_5       <- IVs_china
    IVs_china_usaid_r2_5 <- IVs_china_usaid
    IVs_china_r6         <- IVs_china
    IVs_china_usaid_r6   <- IVs_china_usaid
    
    FEs <- "iso + afro.round"
  }
  
  ## Different covariates
  if(model_type %in% 3){
    include_splag <- F
    
    r2_5_ivs <- paste(" + pstYearGoneWithout_index + have_job_pays_cash_income + educ_any_formal + leaderMember_religiousOrg ")
    r6_ivs   <- paste(" + pstYearGoneWithout_index + have_job_pays_cash_income + educ_any_formal + leaderMember_index        + ownership_index + howOftenUse_index ")
    
    IVs_china_r2_5       <- paste(IVs_china,       r2_5_ivs)
    IVs_china_usaid_r2_5 <- paste(IVs_china_usaid, r2_5_ivs)
    IVs_china_r6         <- paste(IVs_china,       r6_ivs)
    IVs_china_usaid_r6   <- paste(IVs_china_usaid, r6_ivs)
    
    FEs <- "iso + afro.round"
  }
  
  ## Main Models
  if(model_type %in% 4){
    include_splag <- F
    IVs_china_r2_5       <- IVs_china
    IVs_china_usaid_r2_5 <- IVs_china_usaid
    IVs_china_r6         <- IVs_china
    IVs_china_usaid_r6   <- IVs_china_usaid
    
    FEs <- "NAME_1 + afro.round"
  }
  
  # Regressions ------------------------------------------------------------------
  # seq(from = 5, to = 50, by = 5)
  coefs_all_df <- lapply(seq(from = 5, to = 50, by = 5), function(buffer){
    
    print(paste(buffer, "------------------------------------------------------"))
    
    # Rounds 2-5 Models ----------------------------------------------------------
    r2_5_coefs <- lapply(c("lib_dem_val_index",
                           "blvs_mult_parties_good",
                           "blvs_mult_parties_create_choice",
                           "blvs_ctzn_should_join_any_cso",
                           "blvs_democ_best_system",
                           "blvs_elec_good"),
                         run_r2_5,
                         df = df,
                         include_splag = include_splag,
                         IVs_china = IVs_china_r2_5, 
                         IVs_china_usaid = IVs_china_usaid_r2_5, 
                         FEs = FEs, 
                         CLUSTER_VAR = CLUSTER_VAR,
                         buffer = buffer) %>%
      bind_rows()
    
    # Rounds 4 Models ------------------------------------------------------------
    # "china.help.country_DONTKNOW"
    r4_coefs <- lapply(c("china.help.country", 
                         "usa.help.country"),
                       run_r4,
                       df = df,
                       include_splag = include_splag,
                       IVs_china = IVs_china_r2_5, 
                       FEs = FEs, 
                       CLUSTER_VAR = CLUSTER_VAR,
                       buffer = buffer) %>%
      bind_rows()
    
    # Rounds 6 Models ------------------------------------------------------------
    r6_coefs <- lapply(c("china_influential_index",
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
                         "china.influence.econ.activity_DONTKNOW",
                         "china.econpol.influence.positive_DONTKNOW",
                         "china.aid.good.job.meet.country.needs_DONTKNOW",
                         "china_dontknow_index"),
                       run_r6,
                       df = df,
                       include_splag = include_splag,
                       IVs_china = IVs_china_r6, 
                       IVs_china_usaid = IVs_china_usaid_r6, 
                       FEs = FEs, 
                       CLUSTER_VAR = CLUSTER_VAR,
                       buffer = buffer) %>%
      bind_rows()
    
    # Rounds 2-5: US/UK Models ---------------------------------------------------
    r2_5usuk_coefs <- lapply(c("lib_dem_val_index", 
                               "blvs_mult_parties_good",
                               "blvs_mult_parties_create_choice",
                               "blvs_ctzn_should_join_any_cso",
                               "blvs_democ_best_system",
                               "blvs_elec_good"),
                             run_r2_5_usuk,
                             df = df,
                             include_splag = include_splag,
                             IVs_china_usaid = IVs_china_usaid_r2_5, 
                             FEs = FEs, 
                             CLUSTER_VAR = CLUSTER_VAR,
                             buffer = buffer) %>%
      bind_rows()
    
    # Append All Together --------------------------------------------------------
    coefs_all <- bind_rows(
      r2_5usuk_coefs,
      r6_coefs, 
      r4_coefs, 
      r2_5_coefs 
    ) 
    coefs_all$var <- coefs_all$var %>% str_replace_all(paste0(buffer, "km"), "km")
    coefs_all$buffer <- buffer
    
    return(coefs_all)
  }) %>%
    bind_rows()
  
  # Restrict Variables -----------------------------------------------------------
  coefs_all_df <- coefs_all_df %>%
    mutate(var = var %>% 
             str_replace("completed_near_china.pl10.km.construct.bin", 
                         "Chinese Aid Completed [Infrastructure]") %>%
             str_replace("planned_near_china.pl10.km.construct.bin", 
                         "Chinese Aid Planned [Infrastructure]") %>%
             
             str_replace("completed_near_china.pl10.km.noconstruct.bin", 
                         "Chinese Aid Completed [Non-Infrastructure]") %>%
             str_replace("planned_near_china.pl10.km.noconstruct.bin", 
                         "Chinese Aid Planned [Non-Infrastructure]") %>%
             
             str_replace("completed_near_china.pl10.km.bin", 
                         "Chinese Aid Completed") %>%
             str_replace("planned_near_china.pl10.km.bin", 
                         "Chinese Aid Planned") %>%
             str_replace("completed_near_china.pl09.km.bin", 
                         "Chinese Aid Completed") %>%
             str_replace("planned_near_china.pl09.km.bin", 
                         "Chinese Aid Planned") %>%
             str_replace("completed_near_china.pl08.km.bin", 
                         "Chinese Aid Completed") %>%
             str_replace("planned_near_china.pl08.km.bin", 
                         "Chinese Aid Planned") %>%
             str_replace("completed_near_china.plNA.km.bin", 
                         "Chinese Aid Completed") %>%
             str_replace("planned_near_china.plNA.km.bin", 
                         "Chinese Aid Planned") %>%
             str_replace("completed_near_usaid.km.bin", 
                         "USA Aid Completed") %>%
             str_replace("planned_near_usaid.km.bin", 
                         "USA Aid Planned") %>%
             str_replace("completed_near_ukaid.km.bin", 
                         "UK Aid Completed") %>%
             str_replace("planned_near_ukaid.km.bin", 
                         "UK Aid Planned") %>%
             str_replace("planned_near_china.plNAcmpltd.km.bin", 
                         "Chinese Aid Planned")) %>%
    filter(var %in% c("Chinese Aid Completed",
                      "Chinese Aid Planned",
                      "Chinese Aid Completed [Infrastructure]",
                      "Chinese Aid Planned [Infrastructure]",
                      "Chinese Aid Completed [Non-Infrastructure]",
                      "Chinese Aid Planned [Non-Infrastructure]",
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
  coefs_all_df$dv_clean[coefs_all_df$dv %in% "posimage_noninterference"] <- "Positive Image:\nChinese\npolicy of\nnon-interference"
  coefs_all_df$dv_clean[coefs_all_df$dv %in% "posimage_supportinintlaffiars"] <- "Positive Image:\nChinese\nsupport in\ninternational\naffairs"
  coefs_all_df$dv_clean[coefs_all_df$dv %in% "posimage_productcost"] <- "Positive Image:\nCost of\nChinese\nproducts"
  coefs_all_df$dv_clean[coefs_all_df$dv %in% "negimage_cooperateundemocratic"] <- "Negative Image:\nChinese\ncooperation\nw/ undemocratic\nleaders"
  coefs_all_df$dv_clean[coefs_all_df$dv %in% "negimage_chinesecitizenbehavior"] <- "Negative Image:\nBehavior of\nChinese\ncitizens"
  coefs_all_df$dv_clean[coefs_all_df$dv %in% "negimage_resourceextraction"] <- "Negative Image:\nChinese\nextraction of\nnatural\nresources"
  coefs_all_df$dv_clean[coefs_all_df$dv %in% "negimage_takingjobsbusiness"] <- "Negative Image:\nChinese\nfirms taking\nlocal jobs\nand businesses"
  coefs_all_df$dv_clean[coefs_all_df$dv %in% "negimage_landgrabbing"] <- "Negative Image:\nChinese\nland\ngrabbing"
  coefs_all_df$dv_clean[coefs_all_df$dv %in% "negimage_productquality"] <- "Negative Image:\nQuality of\nChinese\nproducts"
  coefs_all_df$dv_clean[coefs_all_df$dv %in% "china_dontknow_index"] <- "China Questions\nDon't Know Index"
  coefs_all_df$dv_clean[coefs_all_df$dv %in% "china.influence.econ.activity_DONTKNOW"] <- "How much influence\ndoes China economic\nactivities have\non economy\n[Don't Know]"
  coefs_all_df$dv_clean[coefs_all_df$dv %in% "china.econpol.influence.positive_DONTKNOW"] <- "China has positive\nor negative economic\nand political influence\n[Don't Know]"
  coefs_all_df$dv_clean[coefs_all_df$dv %in% "china.aid.good.job.meet.country.needs_DONTKNOW"] <- "Chinese aid does\ngood or bad job\nto meet country's needs\n[Don't Know]"
  coefs_all_df$dv_clean[coefs_all_df$dv %in% "china.help.country_DONTKNOW"] <- "How much does\nChina help\nthe country\n[Don't Know]"
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
  saveRDS(coefs_all_df,    
          file.path(data_results_file_path, 
                    paste0("models_type_", model_type),
                    "coefficients",
                    "coefficients.Rds"))
  
  write.csv(coefs_all_df,    
            file.path(data_results_file_path, 
                      paste0("models_type_", model_type),
                      "coefficients",
                      "coefficients.csv"), row.names = F)
}


