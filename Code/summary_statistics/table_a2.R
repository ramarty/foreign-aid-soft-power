# Descriptive Stats 

# Load Data --------------------------------------------------------------------
df <- readRDS(file.path(data_file_path, "afro_china_data.Rds"))
df <- df[df$afro.round != 1,]

# Subset Data ------------------------------------------------------------------
# Subset to observations in regressions
df <- df %>%
  filter(!is.na(age),
         !is.na(muslim),
         !is.na(urban),
         !is.na(male),
         !is.na(distance_capital),
         !is.na(in_leader_adm1)) %>%
  filter(sample_full %in% T)

sum_stat <- function(var, name, df){
  
  rounds_with_var <- df$afro.round[!is.na(df[[var]])]
  
  latex <- paste(
    name, " & ",
    length(rounds_with_var) %>% prettyNum(big.mark=",",scientific=FALSE), " & ",
    mean(df[[var]], na.rm=T) %>% round(2), " & ",
    sd(df[[var]], na.rm=T) %>% round(2), " & ",
    min(df[[var]], na.rm=T), " & ",
    max(df[[var]], na.rm=T), " & ",
    min(rounds_with_var), " & ",
    max(rounds_with_var), " \\\\ "
  )
  
  return(cat(latex))
}


sink(file.path(tables_file_path, "table_a2.tex"))

cat(" \\begin{tabular}{l ccccccc} \n")
cat(" &   &      &      &     &     & First     & Last     \\\\ \n ")
cat(" &   &      &      &     &     & round     & round    \\\\ \n ")
cat(" & N & Mean & S.D. & Min & Max & available & available \\\\ \n ")
cat("\\hline ")

cat(" \\multicolumn{8}{l}{\\bf Perceptions of Former Colonial Powers} \\\\ \n ")
sum_stat("formcolnpower.most.influence",        "Believes former colonial power is most influential", df)
sum_stat("formcolnpower.best.dev.model",        "Believes former colonial power is best model", df)

cat(" & & & & & & & \\\\ \n ")
cat(" \\multicolumn{8}{l}{\\bf Factors Contributing to Positive Image of China} \\\\ \n ")
sum_stat("posimage_chinesepeople",     "Chinese people and culture", df)
sum_stat("posimage_businessinvetment", "Chinese business investment", df)
sum_stat("posimage_infordevinvetment", "Chinese infrastructure investment", df)
sum_stat("posimage_noninterference", "Chinese police of non-interference", df)
sum_stat("posimage_supportinintlaffiars", "Chinese support in international affairs", df)
sum_stat("posimage_productcost", "Cost of Chinese products", df)

cat(" & & & & & & & \\\\ \n ")
cat(" \\multicolumn{8}{l}{\\bf Factors Contributing to Negative Image of China} \\\\ \n ")
sum_stat("negimage_cooperateundemocratic", "Chinese cooperation w/ undemocratic leaders", df)
sum_stat("negimage_chinesecitizenbehavior", "Behaviors of Chinese citizens", df)
sum_stat("negimage_resourceextraction", "Chinese extraction of natural resources", df)
sum_stat("negimage_takingjobsbusiness", "Chinese firms taking local jobs and businesses", df)
sum_stat("negimage_landgrabbing", "Chinese land grabbing", df)
sum_stat("negimage_productquality", "Quality of Chinese products", df)

cat(" & & & & & & & \\\\ \n ")
cat(" \\multicolumn{8}{l}{\\bf Perceptions of China and US} \\\\ \n ")
sum_stat("china_influential_index",        "Belives China is Influential (Index)", df)
sum_stat("china_positive_influence_index", "Belives Chinese presence is positive (index)", df)
sum_stat("china.most.influence",           "Belives Chinese model is most influential", df)
sum_stat("usa.most.influence",             "Belives US model is most influential", df)
sum_stat("china.best.dev.model",           "Belives Chinese model is best", df)
sum_stat("usa.best.dev.model",             "Belives US model is best", df)

cat(" & & & & & & & \\\\ \n ")
cat(" \\multicolumn{8}{l}{\\bf Liberal democractic values} \\\\ \n ")
sum_stat("lib_dem_val_index",               "Liberal values (index)", df[df$afro.round %in% 2:5,])
sum_stat("blvs_mult_parties_good",          "Belives multiple parties are good", df[df$afro.round %in% 2:5,])
sum_stat("blvs_mult_parties_create_choice", "Believes multiple parties create choice", df[df$afro.round %in% 2:5,])
sum_stat("blvs_ctzn_should_join_any_cso",   "Believes citizens should join any CSO", df[df$afro.round %in% 2:5,])
sum_stat("blvs_democ_best_system",          "Believes democracy is best system", df[df$afro.round %in% 2:5,])
sum_stat("blvs_elec_good",                  "Believes elections are good", df[df$afro.round %in% 2:5,])

cat("\\hline ")
cat(" \\end{tabular}")
sink()


