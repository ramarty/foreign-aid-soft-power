# Figure 2

# Load Data --------------------------------------------------------------------
df <- readRDS(file.path(data_file_path, "afro_china_data.Rds"))

# Regressions ------------------------------------------------------------------
#### Full Sample
df_results <- lapply(seq(from = 5, to = 50, by = 5), function(buffer){
  
  print(buffer)
  
  ## Full
  lib_dem_val_index.full.lm              <- felm(as.formula(paste0("lib_dem_val_index ~ completed_near_china.pl10.30km.bin + planned_near_china.pl10.30km.bin + ",              IVs_china," | ",FEs," | 0 | ", CLUSTER_VAR)), data=df[df$sample_full %in% T,]) 
  #china_influential_index.full.lm        <- felm(as.formula(paste0("china_influential_index ~ completed_near_china.pl10.30km.bin + planned_near_china.pl10.30km.bin + ",              IVs_china," | ",FEs," | 0 | ", CLUSTER_VAR)), data=df[df$sample_full %in% T,]) 
  #china_positive_influence_index.full.lm <- felm(as.formula(paste0("china_positive_influence_index ~ completed_near_china.pl10.30km.bin + planned_near_china.pl10.30km.bin + ",              IVs_china," | ",FEs," | 0 | ", CLUSTER_VAR)), data=df[df$sample_full %in% T,]) 
  #formcolnpower.most.influence.full.lm   <- felm(as.formula(paste0("formcolnpower.most.influence ~ completed_near_china.pl10.30km.bin + planned_near_china.pl10.30km.bin + ",              IVs_china," | ",FEs," | 0 | ", CLUSTER_VAR)), data=df[df$sample_full %in% T,]) 
  #formcolnpower.best.dev.model.full.lm   <- felm(as.formula(paste0("formcolnpower.best.dev.model ~ completed_near_china.pl10.30km.bin + planned_near_china.pl10.30km.bin + ",              IVs_china," | ",FEs," | 0 | ", CLUSTER_VAR)), data=df[df$sample_full %in% T,]) 
  #posimage_chinesepeople.full.lm   <- felm(as.formula(paste0("posimage_chinesepeople ~ completed_near_china.pl10.30km.bin + planned_near_china.pl10.30km.bin + ",              IVs_china," | ",FEs," | 0 | ", CLUSTER_VAR)), data=df[df$sample_full %in% T,]) 
  #posimage_businessinvetment.full.lm   <- felm(as.formula(paste0("posimage_businessinvetment ~ completed_near_china.pl10.30km.bin + planned_near_china.pl10.30km.bin + ",              IVs_china," | ",FEs," | 0 | ", CLUSTER_VAR)), data=df[df$sample_full %in% T,]) 
  #posimage_infordevinvetment.full.lm   <- felm(as.formula(paste0("posimage_infordevinvetment ~ completed_near_china.pl10.30km.bin + planned_near_china.pl10.30km.bin + ",              IVs_china," | ",FEs," | 0 | ", CLUSTER_VAR)), data=df[df$sample_full %in% T,]) 
  #posimage_noninterference.full.lm   <- felm(as.formula(paste0("posimage_noninterference ~ completed_near_china.pl10.30km.bin + planned_near_china.pl10.30km.bin + ",              IVs_china," | ",FEs," | 0 | ", CLUSTER_VAR)), data=df[df$sample_full %in% T,]) 
  #posimage_supportinintlaffiars.full.lm   <- felm(as.formula(paste0("posimage_supportinintlaffiars ~ completed_near_china.pl10.30km.bin + planned_near_china.pl10.30km.bin + ",              IVs_china," | ",FEs," | 0 | ", CLUSTER_VAR)), data=df[df$sample_full %in% T,]) 
  #posimage_productcost.full.lm   <- felm(as.formula(paste0("posimage_productcost ~ completed_near_china.pl10.30km.bin + planned_near_china.pl10.30km.bin + ",              IVs_china," | ",FEs," | 0 | ", CLUSTER_VAR)), data=df[df$sample_full %in% T,]) 
  #negimage_cooperateundemocratic.full.lm   <- felm(as.formula(paste0("negimage_cooperateundemocratic ~ completed_near_china.pl10.30km.bin + planned_near_china.pl10.30km.bin + ",              IVs_china," | ",FEs," | 0 | ", CLUSTER_VAR)), data=df[df$sample_full %in% T,]) 
  #negimage_chinesecitizenbehavior.full.lm   <- felm(as.formula(paste0("negimage_chinesecitizenbehavior ~ completed_near_china.pl10.30km.bin + planned_near_china.pl10.30km.bin + ",              IVs_china," | ",FEs," | 0 | ", CLUSTER_VAR)), data=df[df$sample_full %in% T,]) 
  #negimage_resourceextraction.full.lm   <- felm(as.formula(paste0("negimage_resourceextraction ~ completed_near_china.pl10.30km.bin + planned_near_china.pl10.30km.bin + ",              IVs_china," | ",FEs," | 0 | ", CLUSTER_VAR)), data=df[df$sample_full %in% T,]) 
  #negimage_takingjobsbusiness.full.lm   <- felm(as.formula(paste0("negimage_takingjobsbusiness ~ completed_near_china.pl10.30km.bin + planned_near_china.pl10.30km.bin + ",              IVs_china," | ",FEs," | 0 | ", CLUSTER_VAR)), data=df[df$sample_full %in% T,]) 
  #negimage_landgrabbing.full.lm   <- felm(as.formula(paste0("negimage_landgrabbing ~ completed_near_china.pl10.30km.bin + planned_near_china.pl10.30km.bin + ",              IVs_china," | ",FEs," | 0 | ", CLUSTER_VAR)), data=df[df$sample_full %in% T,]) 
  #negimage_productquality.full.lm   <- felm(as.formula(paste0("negimage_productquality ~ completed_near_china.pl10.30km.bin + planned_near_china.pl10.30km.bin + ",              IVs_china," | ",FEs," | 0 | ", CLUSTER_VAR)), data=df[df$sample_full %in% T,]) 
  china_dontknow_index.full.lm   <- felm(as.formula(paste0("china_dontknow_index ~ completed_near_china.pl10.30km.bin + planned_near_china.pl10.30km.bin + ",              IVs_china," | ",FEs," | 0 | ", CLUSTER_VAR)), data=df[df$sample_full %in% T,]) 
  
  ## Restricted
  lib_dem_val_index.restricted.lm <- felm(as.formula(paste0("lib_dem_val_index ~ completed_near_china.pl10.30km.bin + planned_near_china.pl10.30km.bin + completed_near_usaid.30km.bin + planned_near_usaid.30km.bin + ",               IVs_china_usaid," | ",FEs," | 0 | ", CLUSTER_VAR)), data=df[df$sample_restricted %in% T,]) 
  #china_influential_index.restricted.lm <- felm(as.formula(paste0("china_influential_index ~ completed_near_china.pl10.30km.bin + planned_near_china.pl10.30km.bin + completed_near_usaid.30km.bin + planned_near_usaid.30km.bin + ",               IVs_china_usaid," | ",FEs," | 0 | ", CLUSTER_VAR)), data=df[df$sample_restricted %in% T,]) 
  #china_positive_influence_index.restricted.lm <- felm(as.formula(paste0("china_positive_influence_index ~ completed_near_china.pl10.30km.bin + planned_near_china.pl10.30km.bin + completed_near_usaid.30km.bin + planned_near_usaid.30km.bin + ",               IVs_china_usaid," | ",FEs," | 0 | ", CLUSTER_VAR)), data=df[df$sample_restricted %in% T,]) 
  #formcolnpower.most.influence.restricted.lm <- felm(as.formula(paste0("formcolnpower.most.influence ~ completed_near_china.pl10.30km.bin + planned_near_china.pl10.30km.bin + completed_near_usaid.30km.bin + planned_near_usaid.30km.bin + ",               IVs_china_usaid," | ",FEs," | 0 | ", CLUSTER_VAR)), data=df[df$sample_restricted %in% T,]) 
  #formcolnpower.best.dev.model.restricted.lm <- felm(as.formula(paste0("formcolnpower.best.dev.model ~ completed_near_china.pl10.30km.bin + planned_near_china.pl10.30km.bin + completed_near_usaid.30km.bin + planned_near_usaid.30km.bin + ",               IVs_china_usaid," | ",FEs," | 0 | ", CLUSTER_VAR)), data=df[df$sample_restricted %in% T,]) 
  
  ## Restricted - US/UK
  lib_dem_val_index.restrictedusuk.lm <- felm(as.formula(paste0("lib_dem_val_index ~ completed_near_ukaid.30km.bin + planned_near_ukaid.30km.bin + completed_near_usaid.30km.bin + planned_near_usaid.30km.bin + ",               IVs_china_usaid," | ",FEs," | 0 | ", CLUSTER_VAR)), data=df[df$sample_restricted_uk %in% T,]) 
  
  coef_full_df <- bind_rows(
    extract_coefs(lib_dem_val_index.full.lm) %>% mutate(model = "Liberal\ndemocratic\nvalues\n(index)"),
    extract_coefs(china_influential_index.full.lm) %>% mutate(model = "Believes\nChina is\ninfluencial\n(index)"),
    extract_coefs(china_positive_influence_index.full.lm) %>% mutate(model = "Belives\nChinese presence\nis positive\n(index)"),
    extract_coefs(formcolnpower.most.influence.full.lm) %>% mutate(model = "Believes\nformer colonial\npower is most\ninfluential") ,
    extract_coefs(formcolnpower.best.dev.model.full.lm) %>% mutate(model = "Believes\nformer colonial\npower is best\nmodel"),
    extract_coefs(posimage_chinesepeople.full.lm) %>% mutate(model = "Positive Image:\nChinese\npeople and\nculture"),
    extract_coefs(posimage_businessinvetment.full.lm) %>% mutate(model = "Positive Image:\nChinese\nbusiness\ninvestment"),
    extract_coefs(posimage_infordevinvetment.full.lm) %>% mutate(model = "Positive Image:\nChinese\ninfrastructure\ninvestment"),
    extract_coefs(posimage_noninterference.full.lm) %>% mutate(model = "Positive Image:\nChinese\npolice of non-interference"),
    extract_coefs(posimage_supportinintlaffiars.full.lm) %>% mutate(model = "Positive Image:\nChinese\nsupport in\ninternational\naffairs"),
    extract_coefs(posimage_productcost.full.lm) %>% mutate(model = "Positive Image:\nCost of\nChinese\nproducts"),
    extract_coefs(negimage_cooperateundemocratic.full.lm) %>% mutate(model = "Negative Image:\nChinese\ncooperation\nw/ undemocratic\nleaders"),
    extract_coefs(negimage_chinesecitizenbehavior.full.lm) %>% mutate(model = "Negative Image:\nBehaviours of\nChinese\ncitizens"),
    extract_coefs(negimage_resourceextraction.full.lm) %>% mutate(model = "Negative Image:\nChinese\nextraction of\nnatural\nresources"),
    extract_coefs(negimage_takingjobsbusiness.full.lm) %>% mutate(model = "Negative Image:\nChinese\nfirms taking\nlocal jobs\nand businesses"),
    extract_coefs(negimage_landgrabbing.full.lm) %>% mutate(model = "Negative Image:\nChinese\nland\ngrabbing"),
    extract_coefs(negimage_productquality.full.lm) %>% mutate(model = "Negative Image:\nQuality of\nChinese\nproducts"),
    extract_coefs(china_dontknow_index.full.lm) %>% mutate(model = "China Questions\nDon't Know Index")
  ) %>%
    mutate(subset = "Full Sample") %>%
    mutate(buffer = buffer) %>%
    mutate(var = var %>% str_replace_all(paste0(buffer, "km"), "km"))
  
  coef_restricted_df <- bind_rows(
    extract_coefs(lib_dem_val_index.restricted.lm) %>% mutate(model = "Liberal\ndemocratic\nvalues\n(index)"),
    extract_coefs(china_influential_index.restricted.lm) %>% mutate(model = "Believes\nChina is\ninfluencial\n(index)"),
    extract_coefs(china_positive_influence_index.restricted.lm) %>% mutate(model = "Belives\nChinese presence\nis positive\n(index)"),
    extract_coefs(formcolnpower.most.influence.restricted.lm) %>% mutate(model = "Believes\nformer colonial\npower is most\ninfluential"),
    extract_coefs(formcolnpower.best.dev.model.restricted.lm) %>% mutate(model = "Believes\nformer colonial\npower is best\nmodel")  ) %>%
    mutate(subset = "Restricted Sample") %>%
    mutate(buffer = buffer) %>%
    mutate(var = var %>% str_replace_all(paste0(buffer, "km"), "km"))
  
  coef_restricted_usuk_df <- bind_rows(
    extract_coefs(lib_dem_val_index.restrictedusuk.lm) %>% mutate(model = "Liberal\ndemocratic\nvalues\n(index)")
  ) %>%
    mutate(subset = "Restricted Sample - US/UK") %>%
    mutate(buffer = buffer) %>%
    mutate(var = var %>% str_replace_all(paste0(buffer, "km"), "km"))

  coef_all <- bind_rows(
    coef_full_df,
    coef_restricted_df,
    coef_restricted_usuk_df
  )
  
  return(coef_all)
  
}) %>%
  bind_rows()

df_results <- df_results %>%
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

# Figures : Full ---------------------------------------------------------------
p <- df_results %>%
  filter(subset %in% "Full Sample") %>%
  ggplot(aes(x = buffer, y = coef, ymin = ci2_5, ymax = ci97_5,
             group = var, color = var, shape = var, linetype = var)) +
  geom_hline(yintercept = 0, color = "gray50") +
  geom_linerange(position = position_dodge(width=2.5)) +
  geom_point(position = position_dodge(width=2.5)) +
  coord_flip() +
  facet_wrap(~subset,
             scales = "free_x") +
  labs(x = "Buffer\nSize\n(km)",
       y="Coef (+/- 95% CI)",
       linetype = "",
       shape = "",
       color = "") +
  theme_ipsum() + 
  theme(plot.title = element_text(family="Times",size=12,hjust=0.5,face="bold"),
        axis.title.x = element_text(family="Times",size=13),
        axis.title.y = element_text(family="Times",size=13, angle = 0),
        axis.text.y = element_text(family="Times",size=13),
        axis.text.x = element_text(family="Times",size=13),
        legend.title = element_text(family="Times",size=13),
        legend.text = element_text(family="Times",size=13),
        strip.text = element_text(family="Times",size=13, hjust = 0.5, face = "bold")) +
  guides(colour = guide_legend(reverse=T),
         linetype = guide_legend(reverse=T),
         pch = guide_legend(reverse=T)) +
  scale_linetype_manual(values=c("solid","dashed","solid","dashed")) +
  scale_shape_manual(values=c(16,17,16,17)) +
  scale_color_manual(values=c("darkorange3", "darkorange","dodgerblue4","dodgerblue2")) +
  theme(legend.position="bottom") +
  scale_x_reverse() + 
  facet_wrap(~model,
             scales = "free_x")
ggsave(p, filename = file.path(figures_file_path, "rm_rounds_full.png"),
       height = 18, width = 12)

# Figures: Restricted ----------------------------------------------------------
p <- df_results %>%
  filter(subset %in% "Restricted Sample") %>%
  ggplot(aes(x = buffer, y = coef, ymin = ci2_5, ymax = ci97_5,
             group = var, color = var, shape = var, linetype = var)) +
  geom_hline(yintercept = 0, color = "gray50") +
  geom_linerange(position = position_dodge(width=2.75)) +
  geom_point(position = position_dodge(width=2.75)) +
  coord_flip() +
  facet_wrap(~subset,
             scales = "free_x") +
  labs(x = "Buffer\nSize\n(km)",
       y="Coef (+/- 95% CI)",
       linetype = "",
       shape = "",
       color = "") +
  theme_ipsum() + 
  theme(plot.title = element_text(family="Times",size=12,hjust=0.5,face="bold"),
        axis.title.x = element_text(family="Times",size=13),
        axis.title.y = element_text(family="Times",size=13, angle = 0),
        axis.text.y = element_text(family="Times",size=13),
        axis.text.x = element_text(family="Times",size=13),
        legend.title = element_text(family="Times",size=13),
        legend.text = element_text(family="Times",size=13),
        strip.text = element_text(family="Times",size=13, hjust = 0.5, face = "bold")) +
  guides(colour = guide_legend(reverse=T),
         linetype = guide_legend(reverse=T),
         pch = guide_legend(reverse=T)) +
  scale_linetype_manual(values=c("solid","dashed","solid","dashed")) +
  scale_shape_manual(values=c(16,17,16,17)) +
  scale_color_manual(values=c("darkorange3", "darkorange","dodgerblue4","dodgerblue2")) +
  theme(legend.position="bottom") +
  scale_x_reverse() + 
  facet_wrap(~model,
             scales = "free_x")
ggsave(p, filename = file.path(figures_file_path, "rm_rounds_restricted.png"),
       height = 13, width = 10)

# Figures: Restricted ----------------------------------------------------------
p <- df_results %>%
  filter(subset %in% "Restricted Sample - US/UK") %>%
  ggplot(aes(x = buffer, y = coef, ymin = ci2_5, ymax = ci97_5,
             group = var, color = var, shape = var, linetype = var)) +
  geom_hline(yintercept = 0, color = "gray50") +
  geom_linerange(position = position_dodge(width=2.75)) +
  geom_point(position = position_dodge(width=2.75)) +
  coord_flip() +
  facet_wrap(~subset,
             scales = "free_x") +
  labs(x = "Buffer\nSize\n(km)",
       y="Coef (+/- 95% CI)",
       linetype = "",
       shape = "",
       color = "") +
  theme_ipsum() + 
  theme(plot.title = element_text(family="Times",size=12,hjust=0.5,face="bold"),
        axis.title.x = element_text(family="Times",size=13),
        axis.title.y = element_text(family="Times",size=13, angle = 0),
        axis.text.y = element_text(family="Times",size=13),
        axis.text.x = element_text(family="Times",size=13),
        legend.title = element_text(family="Times",size=13),
        legend.text = element_text(family="Times",size=13),
        strip.text = element_text(family="Times",size=13, hjust = 0.5, face = "bold")) +
  guides(colour = guide_legend(reverse=T),
         linetype = guide_legend(reverse=T),
         pch = guide_legend(reverse=T)) +
  scale_linetype_manual(values=c("solid","dashed","solid","dashed")) +
  scale_shape_manual(values=c(16,17,16,17)) +
  scale_color_manual(values=c("darkorange3", "darkorange","dodgerblue4","dodgerblue2")) +
  scale_x_reverse() 
ggsave(p, filename = file.path(figures_file_path, "rm_rounds_restrictedusuk.png"),
       height = 6.5, width = 6)


