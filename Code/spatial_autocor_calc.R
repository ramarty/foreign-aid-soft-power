# Calculate Moran's I

# Load Data --------------------------------------------------------------------
df <- readRDS(file.path(project_file_path, "Data", "afro_china_data.Rds"))

df$townvill_latlon <- paste(df$latitude, df$longitude)

# Calc Moran i Function --------------------------------------------------------
calc_moran_i <- function(var, df){
  print(var) # Show progress
  
  label <- var_label(df[[var]])
  
  df$var_temp <- df[[var]]
  df_sum <- df %>%
    filter(!is.na(var_temp)) %>%
    group_by(afro.round, townvill_latlon) %>%
    summarise(var_temp = mean(var_temp, na.rm=T),
              longitude = mean(longitude, na.rm=T),
              latitude = mean(latitude, na.rm=T))
  
  mi <- moranfast(df_sum$var_temp, df_sum$longitude, df_sum$latitude)
  
  df_all <- data.frame(var = var,
                       var_label = label,
                       observed = mi$observed,
                       expected = mi$expected,
                       sd = mi$sd,
                       p.value = mi$p.value)
  
  # df_all <- lapply(unique(df_sum$afro.round), function(round_i){
  #   
  #   df_sum_i <- df_sum[df_sum$afro.round %in% round_i,]
  #   
  #   mi <- moranfast(df_sum_i$var_temp, df_sum_i$longitude, df_sum_i$latitude)
  #   
  #   df <- data.frame(var = var,
  #                    var_label = label,
  #                    observed = mi$observed,
  #                    expected = mi$expected,
  #                    sd = mi$sd,
  #                    p.value = mi$p.value,
  #                    round_i = round_i)
  #   
  #   return(df)
  # }) %>%
  #   bind_rows()
  
  return(df_all)
}

# Apply function, looping over variables ---------------------------------------
dep_vars <- c("china_influential_index", 
              "china_positive_influence_index",
              
              "china.most.influence", 
              "usa.most.influence", 
              "china.best.dev.model", 
              "usa.best.dev.model",
              
              "lib_dem_val_index",
              "blvs_mult_parties_good", 
              "blvs_mult_parties_create_choice", 
              "blvs_ctzn_should_join_any_cso", 
              "blvs_democ_best_system", 
              "blvs_elec_good",
              
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
              "negimage_productquality")

mi_df <- lapply(dep_vars, calc_moran_i,  df) %>%
  bind_rows()

# Export -----------------------------------------------------------------------
write.csv(mi_df, file.path(data_file_path, "data_for_select_figures", "morans_i_data.csv"), row.names = F)
