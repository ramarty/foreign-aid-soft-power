small_felm <- function(m){
  m$fe <- NULL
  #m$residuals <- NULL
  m$r.residuals <- NULL
  m$fitted.values <- NULL
  m$response <- NULL
  m$cfactor <- NULL
  m$inv <- NULL
  m$STATS$promotion <- NULL
  #m$clustervar <- NULL
  
  m$c.fitted.values <- NULL
  m$na.action <- NULL
  
  m$keepCX <- NULL
  m$keepX <- NULL
  
  # If we don't do this, felm thinks the dependent variable name includes
  # "paste0(dv...". Doing this removes that.
  m$call[2] <- "tmp"
  
  m
}

run_r2_5 <- function(dv, 
                     df,
                     include_splag,
                     IVs_china, 
                     IVs_china_usaid, 
                     FEs, 
                     CLUSTER_VAR,
                     buffer){
  
  print(dv)
  
  if(include_splag %in% T){
    dv_splag <- paste0(dv, "_splag + ")
  } else{
    dv_splag <- "" 
  }
  
  lm.full       <- felm(as.formula(paste0(dv, "  ~ ",dv_splag," completed_near_china.plNA.",buffer,"km.bin + planned_near_china.plNA.",buffer,"km.bin + ",                                                                               IVs_china,      " | ",FEs," | 0 | ", CLUSTER_VAR)), data=df[(df$sample_full %in% T) &       (df$afro.round %in% 2:5),], keepModel = F, keepX = FALSE, keepCX = FALSE) %>% small_felm() 
  lm.restricted <- felm(as.formula(paste0(dv, " ~ ",dv_splag," completed_near_china.plNA.",buffer,"km.bin + planned_near_china.plNA.",buffer,"km.bin + completed_near_usaid.",buffer,"km.bin + planned_near_usaid.",buffer,"km.bin + ", IVs_china_usaid," | ",FEs," | 0 | ", CLUSTER_VAR)), data=df[(df$sample_restricted %in% T) & (df$afro.round %in% 2:5),], keepModel = F) %>% small_felm()  
  
  lm.full.plcmpltd       <- felm(as.formula(paste0(dv, " ~ ",dv_splag," completed_near_china.plNA.",buffer,"km.bin + planned_near_china.plNAcmpltd.",buffer,"km.bin + ",                                                                               IVs_china,      " | ",FEs," | 0 | ", CLUSTER_VAR)), data=df[(df$sample_full %in% T) &       (df$afro.round %in% 2:5),], keepModel = F, keepX = FALSE, keepCX = FALSE) %>% small_felm()  
  lm.restricted.plcmpltd <- felm(as.formula(paste0(dv, " ~ ",dv_splag," completed_near_china.plNA.",buffer,"km.bin + planned_near_china.plNAcmpltd.",buffer,"km.bin + completed_near_usaid.",buffer,"km.bin + planned_near_usaid.",buffer,"km.bin + ", IVs_china_usaid," | ",FEs," | 0 | ", CLUSTER_VAR)), data=df[(df$sample_restricted %in% T) & (df$afro.round %in% 2:5),], keepModel = F, keepX = FALSE, keepCX = FALSE) %>% small_felm() 
  
  coefs <- bind_rows(
    lm.full                %>% extract_coefs() %>% mutate(subset = "full",       plcompltd = F),
    lm.restricted          %>% extract_coefs() %>% mutate(subset = "restricted", plcompltd = F),
    lm.full.plcmpltd       %>% extract_coefs() %>% mutate(subset = "full",       plcompltd = T),
    lm.restricted.plcmpltd %>% extract_coefs() %>% mutate(subset = "restricted", plcompltd = T)
  ) %>%
    mutate(dv = all_of(dv),
           round = "2-5")
  
  if(buffer %in% 30 & save_model){
    saveRDS(lm.full, file.path(results_file_path,       paste0(dv, "_full", ".Rds")))
    saveRDS(lm.restricted, file.path(results_file_path, paste0(dv, "_restricted", ".Rds")))
    saveRDS(lm.full.plcmpltd, file.path(results_file_path,       paste0(dv, "_full", "_plcmpltd", ".Rds")))
    saveRDS(lm.restricted.plcmpltd, file.path(results_file_path, paste0(dv, "_restricted", "_plcmpltd", ".Rds")))
  }
  
  return(coefs)
  
}

run_r2_5_usuk <- function(dv, 
                          df,
                          include_splag,
                          IVs_china_usaid, 
                          FEs, 
                          CLUSTER_VAR,
                          buffer){
  
  print(dv)
  
  if(include_splag %in% T){
    dv_splag <- paste0(dv, "_splag + ")
  } else{
    dv_splag <- "" 
  }
  
  lm.restricted.usuk <- felm(as.formula(paste0(dv, " ~ ",dv_splag," completed_near_ukaid.",buffer,"km.bin + planned_near_ukaid.",buffer,"km.bin + completed_near_usaid.",buffer,"km.bin + planned_near_usaid.",buffer,"km.bin + ", IVs_china_usaid," | ",FEs," | 0 | ", CLUSTER_VAR)), data=df[(df$sample_restricted_uk %in% T) & (df$afro.round %in% 2:5),], keepModel = F, keepX = FALSE, keepCX = FALSE) %>% small_felm()  
  
  coefs <- lm.restricted.usuk %>% 
    extract_coefs() %>% 
    mutate(subset = "restrictedusuk",
           dv = all_of(dv),
           round = "2-5")
  
  if(buffer %in% 30 & save_model){
    saveRDS(lm.restricted.usuk, file.path(results_file_path, paste0(dv, "_restrictedusuk", ".Rds")))
  }
  
  return(coefs)
  
}

run_r4 <- function(dv,
                   df,
                   include_splag,
                   IVs_china, 
                   FEs, 
                   CLUSTER_VAR,
                   buffer){
  print(dv)
  
  if(include_splag %in% T){
    dv_splag <- paste0(dv, "_splag + ")
  } else{
    dv_splag <- "" 
  }
  
  lm.full <- felm(as.formula(paste0(dv, " ~  ",dv_splag," completed_near_china.plNA.",buffer,"km.bin + planned_near_china.plNA.",buffer,"km.bin + ", IVs_china," | ",FEs," | 0 | ", CLUSTER_VAR)), data=df[(df$sample_full %in% T) & (df$afro.round %in% 4),], keepModel = F, keepX = FALSE, keepCX = FALSE) %>% small_felm()  
  lm.full.plcmpltd <- felm(as.formula(paste0(dv, " ~  ",dv_splag," completed_near_china.plNA.",buffer,"km.bin + planned_near_china.plNAcmpltd.",buffer,"km.bin + ", IVs_china," | ",FEs," | 0 | ", CLUSTER_VAR)), data=df[(df$sample_full %in% T) & (df$afro.round %in% 4),], keepModel = F, keepX = FALSE, keepCX = FALSE) %>% small_felm()  
  
  coefs <- bind_rows(
    lm.full %>% 
      extract_coefs() %>% mutate(plcompltd = F),
    lm.full.plcmpltd %>% 
      extract_coefs() %>% mutate(plcompltd = T)
  ) %>%
    mutate(subset = "full",
           dv = all_of(dv),
           round = "4")
  
  if(buffer %in% 30 & save_model){
    saveRDS(lm.full, file.path(results_file_path, paste0(dv, "_full", ".Rds")))
    saveRDS(lm.full.plcmpltd, file.path(results_file_path, paste0(dv, "_full", "_plcmpltd", ".Rds")))
  }
  
  return(coefs)
  
}

run_r6 <- function(dv, 
                   df,
                   include_splag,
                   IVs_china, 
                   IVs_china_usaid, 
                   FEs, 
                   CLUSTER_VAR,
                   buffer){
  
  print(dv)
  
  if(include_splag %in% T){
    dv_splag <- paste0(dv, "_splag + ")
  } else{
    dv_splag <- "" 
  }
  
  ## Planned - 2010
  lm.full.2010       <- felm(as.formula(paste0(dv, " ~ ",dv_splag," completed_near_china.pl10.",buffer,"km.bin + planned_near_china.pl10.",buffer,"km.bin + ",                                                                               IVs_china,      " | ",FEs," | 0 | ", CLUSTER_VAR)), data=df[(df$sample_full %in% T)       & (df$afro.round %in% 6),], keepModel = T, keepX = FALSE, keepCX = FALSE) %>% small_felm()  
  lm.restricted.2010 <- felm(as.formula(paste0(dv, " ~ ",dv_splag," completed_near_china.pl10.",buffer,"km.bin + planned_near_china.pl10.",buffer,"km.bin + completed_near_usaid.",buffer,"km.bin + planned_near_usaid.",buffer,"km.bin + ", IVs_china_usaid," | ",FEs," | 0 | ", CLUSTER_VAR)), data=df[(df$sample_restricted %in% T) & (df$afro.round %in% 6),], keepModel = F, keepX = FALSE, keepCX = FALSE) %>% small_felm()  
  
  lm.full.2009       <- felm(as.formula(paste0(dv, " ~ ",dv_splag," completed_near_china.pl09.",buffer,"km.bin + planned_near_china.pl09.",buffer,"km.bin + ",                                                                               IVs_china,      " | ",FEs," | 0 | ", CLUSTER_VAR)), data=df[(df$sample_full %in% T)       & (df$afro.round %in% 6),], keepModel = T, keepX = FALSE, keepCX = FALSE) %>% small_felm()  
  lm.restricted.2009 <- felm(as.formula(paste0(dv, " ~ ",dv_splag," completed_near_china.pl09.",buffer,"km.bin + planned_near_china.pl09.",buffer,"km.bin + completed_near_usaid.",buffer,"km.bin + planned_near_usaid.",buffer,"km.bin + ", IVs_china_usaid," | ",FEs," | 0 | ", CLUSTER_VAR)), data=df[(df$sample_restricted %in% T) & (df$afro.round %in% 6),], keepModel = F, keepX = FALSE, keepCX = FALSE) %>% small_felm()  
  
  lm.full.2008       <- felm(as.formula(paste0(dv, " ~ ",dv_splag," completed_near_china.pl08.",buffer,"km.bin + planned_near_china.pl08.",buffer,"km.bin + ",                                                                               IVs_china,      " | ",FEs," | 0 | ", CLUSTER_VAR)), data=df[(df$sample_full %in% T)       & (df$afro.round %in% 6),], keepModel = T, keepX = FALSE, keepCX = FALSE) %>% small_felm()  
  lm.restricted.2008 <- felm(as.formula(paste0(dv, " ~ ",dv_splag," completed_near_china.pl08.",buffer,"km.bin + planned_near_china.pl08.",buffer,"km.bin + completed_near_usaid.",buffer,"km.bin + planned_near_usaid.",buffer,"km.bin + ", IVs_china_usaid," | ",FEs," | 0 | ", CLUSTER_VAR)), data=df[(df$sample_restricted %in% T) & (df$afro.round %in% 6),], keepModel = F, keepX = FALSE, keepCX = FALSE) %>% small_felm()  
  
  if(buffer %in% 30){
    lm.full.infra       <- felm(as.formula(paste0(dv, " ~ ",dv_splag," completed_near_china.pl10.",buffer,"km.construct.bin + planned_near_china.pl10.",buffer,"km.construct.bin + completed_near_china.pl10.",buffer,"km.noconstruct.bin + planned_near_china.pl10.",buffer,"km.noconstruct.bin + ", IVs_china,      " | ",FEs," | 0 | ", CLUSTER_VAR)), data=df[(df$sample_full %in% T)       & (df$afro.round %in% 6),], keepModel = F, keepX = FALSE, keepCX = FALSE) %>% 
      small_felm()  %>% 
      extract_coefs() %>% 
      mutate(subset = "full", planned_year = 2010, infrastructure = T)
    
  } else{
    lm.full.infra <- data.frame(NULL)
  }
  
  coefs <- bind_rows(
    lm.full.infra,
    
    lm.full.2010       %>% extract_coefs() %>% mutate(subset = "full", planned_year = 2010),
    lm.restricted.2010 %>% extract_coefs() %>% mutate(subset = "restricted", planned_year = 2010),
    lm.full.2009       %>% extract_coefs() %>% mutate(subset = "full", planned_year = 2009),
    lm.restricted.2009 %>% extract_coefs() %>% mutate(subset = "restricted", planned_year = 2009),
    lm.full.2008       %>% extract_coefs() %>% mutate(subset = "full", planned_year = 2008),
    lm.restricted.2008 %>% extract_coefs() %>% mutate(subset = "restricted", planned_year = 2008)
  ) %>%
    mutate(dv = all_of(dv),
           round = "6")
  
  if(buffer %in% 30 & save_model){
    
    ## Infrastructure
    lm.full.infra       <- felm(as.formula(paste0(dv, " ~ ",dv_splag," completed_near_china.pl10.",buffer,"km.construct.bin + planned_near_china.pl10.",buffer,"km.construct.bin + completed_near_china.pl10.",buffer,"km.noconstruct.bin + planned_near_china.pl10.",buffer,"km.noconstruct.bin + ", IVs_china,      " | ",FEs," | 0 | ", CLUSTER_VAR)), data=df[(df$sample_full %in% T)       & (df$afro.round %in% 6),], keepModel = F, keepX = FALSE, keepCX = FALSE) %>% small_felm() 
    
    ## Planned - 2009
    lm.full.2009       <- felm(as.formula(paste0(dv, " ~ ",dv_splag," completed_near_china.pl09.",buffer,"km.bin + planned_near_china.pl09.",buffer,"km.bin + ",                                                                               IVs_china,      " | ",FEs," | 0 | ", CLUSTER_VAR)), data=df[(df$sample_full %in% T)       & (df$afro.round %in% 6),], keepModel = F, keepX = FALSE, keepCX = FALSE) %>% small_felm() 
    lm.restricted.2009 <- felm(as.formula(paste0(dv, " ~ ",dv_splag," completed_near_china.pl09.",buffer,"km.bin + planned_near_china.pl09.",buffer,"km.bin + completed_near_usaid.",buffer,"km.bin + planned_near_usaid.",buffer,"km.bin + ", IVs_china_usaid," | ",FEs," | 0 | ", CLUSTER_VAR)), data=df[(df$sample_restricted %in% T) & (df$afro.round %in% 6),], keepModel = F, keepX = FALSE, keepCX = FALSE) %>% small_felm() 
    
    ## Planned - 2008
    lm.full.2008       <- felm(as.formula(paste0(dv, " ~ ",dv_splag," completed_near_china.pl08.",buffer,"km.bin + planned_near_china.pl08.",buffer,"km.bin + ",                                                                               IVs_china,      " | ",FEs," | 0 | ", CLUSTER_VAR)), data=df[(df$sample_full %in% T)       & (df$afro.round %in% 6),], keepModel = F, keepX = FALSE, keepCX = FALSE) %>% small_felm() 
    lm.restricted.2008 <- felm(as.formula(paste0(dv, " ~ ",dv_splag," completed_near_china.pl08.",buffer,"km.bin + planned_near_china.pl08.",buffer,"km.bin + completed_near_usaid.",buffer,"km.bin + planned_near_usaid.",buffer,"km.bin + ", IVs_china_usaid," | ",FEs," | 0 | ", CLUSTER_VAR)), data=df[(df$sample_restricted %in% T) & (df$afro.round %in% 6),], keepModel = F, keepX = FALSE, keepCX = FALSE) %>% small_felm() 
    
    saveRDS(lm.full.2010,  file.path(results_file_path, paste0(dv, "_full", ".Rds")))
    saveRDS(lm.full.2009,  file.path(results_file_path, paste0(dv, "_full_2009", ".Rds")))
    saveRDS(lm.full.2008,  file.path(results_file_path, paste0(dv, "_full_2008", ".Rds")))
    
    if(!grepl("^negimage|^posimage", dv)){
      saveRDS(lm.full.infra,      file.path(results_file_path, paste0(dv, "_full_infrastructure", ".Rds")))
      saveRDS(lm.restricted.2010, file.path(results_file_path, paste0(dv, "_restricted", ".Rds")))
      saveRDS(lm.restricted.2009, file.path(results_file_path, paste0(dv, "_restricted_2009", ".Rds")))
      saveRDS(lm.restricted.2008, file.path(results_file_path, paste0(dv, "_restricted_2008", ".Rds")))
    } 
  }
  
  return(coefs)
  
}

