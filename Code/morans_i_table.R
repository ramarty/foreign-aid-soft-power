# Figure of Moran's I

morani_df <- file.path(data_file_path, "morans_i") %>%
  list.files(pattern = "*.csv",
             full.names = T) %>%
  map_df(read.csv)

morani_df$dv[morani_df$dv %in% "china.most.influence"] <- "Believes Chinese model is most influential"
morani_df$dv[morani_df$dv %in% "usa.most.influence"] <- "Believes US model is most influential"
morani_df$dv[morani_df$dv %in% "china.best.dev.model"] <- "Believes Chinese model is best"
morani_df$dv[morani_df$dv %in% "usa.best.dev.model"] <- "Believes US model is best"
morani_df$dv[morani_df$dv %in% "china_influential_index"] <- "Believes China is influencial (index)"
morani_df$dv[morani_df$dv %in% "china_positive_influence_index"] <- "Belives Chinese presence is positive (index)"
morani_df$dv[morani_df$dv %in% "blvs_mult_parties_good"] <- "Believes multiple parties are good"
morani_df$dv[morani_df$dv %in% "blvs_mult_parties_create_choice"] <- "Believes multiple parties create choice"
morani_df$dv[morani_df$dv %in% "blvs_ctzn_should_join_any_cso"] <- "Believes citizens should join any CSO"
morani_df$dv[morani_df$dv %in% "blvs_democ_best_system"] <- "Believes democracy is best system"
morani_df$dv[morani_df$dv %in% "blvs_elec_good"] <- "Believes elections are good"
morani_df$dv[morani_df$dv %in% "lib_dem_val_index"] <- "Liberal democratic values (index)"
morani_df$dv[morani_df$dv %in% "formcolnpower.most.influence"] <- "Believes former colonial power is most influential"
morani_df$dv[morani_df$dv %in% "formcolnpower.best.dev.model"] <- "Believes former colonial power is best model"
morani_df$dv[morani_df$dv %in% "blvs_mult_parties_good"] <- "Believes multiple parties are good"
morani_df$dv[morani_df$dv %in% "blvs_mult_parties_create_choice"] <- "Believes multiple parties create choice"
morani_df$dv[morani_df$dv %in% "blvs_ctzn_should_join_any_cso"] <- "Believes citizens should join any CSO"
morani_df$dv[morani_df$dv %in% "blvs_democ_best_system"] <- "Believes democracy is best system"
morani_df$dv[morani_df$dv %in% "blvs_elec_good"] <- "Believes elections are good"
morani_df$dv[morani_df$dv %in% "posimage_chinesepeople"] <- "Positive Image: Chinese people and culture"
morani_df$dv[morani_df$dv %in% "posimage_businessinvetment"] <- "Positive Image: Chinese business investment"
morani_df$dv[morani_df$dv %in% "posimage_infordevinvetment"] <- "Positive Image: Chinese infrastructure investment"
morani_df$dv[morani_df$dv %in% "posimage_noninterference"] <- "Positive Image: Chinese police of non-interference"
morani_df$dv[morani_df$dv %in% "posimage_supportinintlaffiars"] <- "Positive Image: Chinese support in international affairs"
morani_df$dv[morani_df$dv %in% "posimage_productcost"] <- "Positive Image: Cost of Chinese products"
morani_df$dv[morani_df$dv %in% "negimage_cooperateundemocratic"] <- "Negative Image: Chinese cooperation w/ undemocratic leaders"
morani_df$dv[morani_df$dv %in% "negimage_chinesecitizenbehavior"] <- "Negative Image: Behavious of Chinese citizens"
morani_df$dv[morani_df$dv %in% "negimage_resourceextraction"] <- "Negative Image: Chinese extraction of natural resources"
morani_df$dv[morani_df$dv %in% "negimage_takingjobsbusiness"] <- "Negative Image: Chinese firms taking local jobs and businesses"
morani_df$dv[morani_df$dv %in% "negimage_landgrabbing"] <- "Negative Image: Chinese land grabbing"
morani_df$dv[morani_df$dv %in% "negimage_productquality"] <- "Negative Image: Quality of Chinese products"
morani_df$dv[morani_df$dv %in% "china_dontknow_index"] <- "China Questions Don't Know Index"
morani_df$dv[morani_df$dv %in% "china.influence.econ.activity_DONTKNOW"] <- "How much influence does China economic activities have on economy [Don't Know]"
morani_df$dv[morani_df$dv %in% "china.econpol.influence.positive_DONTKNOW"] <- "China has positive or negative economic and political influence [Don't Know]"
morani_df$dv[morani_df$dv %in% "china.aid.good.job.meet.country.needs_DONTKNOW"] <- "Chinese aid does good or bad job to meet country's needs [Don't Know]"
morani_df$dv[morani_df$dv %in% "china.help.country_DONTKNOW.lm"] <- "How much does China help the country [Don't Know]"

morani_df <- morani_df %>%
  mutate(latex = paste(dv, " & ", sample, " & ", observed %>% round(3), " & ", p.value %>% round(3), " & ", figure %>%
                         str_replace_all("fig_", "") %>% str_replace_all("_", " "),  " \\\\ \n")) %>%
  arrange(p.value)

sink(file.path(tables_file_path, "morans_i_table.tex"))
cat("\\begin{tabular}{lllll} \n")
cat("\\hline \n")
cat("Dep Variable & Sample & Moran's I & P-Value & Figure \\\\ \n")
cat("\\hline \n")
for(i in 1:nrow(morani_df)) cat(morani_df$latex[i])
cat("\\hline \n")
cat("\\end{tabular} ")
sink()
  



