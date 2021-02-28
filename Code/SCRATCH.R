# Scratch Paper

# Load Data --------------------------------------------------------------------
df <- readRDS(file.path(data_file_path, "afro_china_data.Rds"))

lm.full.2010       <- lm(china_influential_index ~ completed_near_china.plNAcmpltd.30km.bin + planned_near_china.plNAcmpltd.30km.bin, data = df)
  
  
  felm(as.formula(paste0(dv, " ~ ",dv_splag," completed_near_china.pl10.",buffer,"km.bin + planned_near_china.pl10.",buffer,"km.bin + ",                                                                               IVs_china,      " | ",FEs," | 0 | ", CLUSTER_VAR)), data=df[(df$sample_full %in% T)       & (df$afro.round %in% 6),], keepModel = T, keepX = FALSE, keepCX = FALSE) %>% small_felm()  

completed_near_china.plNAcmpltd.30km.bin


lm.full.2010       <- felm(as.formula(paste0(dv, " ~ ",dv_splag," completed_near_china.pl10.",buffer,"km.bin + planned_near_china.pl10.",buffer,"km.bin + ",                                                                               IVs_china,      " | ",FEs," | 0 | ", CLUSTER_VAR)), data=df[(df$sample_full %in% T)       & (df$afro.round %in% 6),], keepModel = T, keepX = FALSE, keepCX = FALSE) %>% small_felm()  
lm.restricted.2010 <- felm(as.formula(paste0(dv, " ~ ",dv_splag," completed_near_china.pl10.",buffer,"km.bin + planned_near_china.pl10.",buffer,"km.bin + completed_near_usaid.",buffer,"km.bin + planned_near_usaid.",buffer,"km.bin + ", IVs_china_usaid," | ",FEs," | 0 | ", CLUSTER_VAR)), data=df[(df$sample_restricted %in% T) & (df$afro.round %in% 6),], keepModel = F, keepX = FALSE, keepCX = FALSE) %>% small_felm()  
