# Tables: Appendix 2

## Load
model_type <- 2

buffer <- 30
ROUND_NUM <- 2
splag <- "Y"
df <- readRDS(file.path(data_file_path, "afro_china_data.Rds"))

## Paths
models_path <- file.path(data_results_file_path, paste0("models_type_", model_type), "models")

## Function to load models
load_models <- function(models_vec){
  l <- list()
  for(model_i in models_vec){
    print(model_i)
    model <- readRDS(file.path(models_path, paste0(model_i, ".Rds")))
    l[[model_i]] <- model
  }
  return(l)
}

# Table A9 ---------------------------------------------------------------------
ml <- load_models(c("china_positive_influence_index_full",
                    "china.best.dev.model_full",
                    "usa.best.dev.model_full"))

stargazer(ml$china_positive_influence_index_full,
          ml$china.best.dev.model_full,
          ml$usa.best.dev.model_full,
          dep.var.labels = c("China Positive", "China Best","US Best"),
          column.labels   = c("[Index]", "Model", "Model"),
          keep=c("completed_near_china.pl10.30km.bin", "planned_near_china.pl10.30km.bin"),
          covariate.labels = c("China Completed","China Planned"),
          dep.var.labels.include = T,
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
              tryCatch(round(linearHypothesis(ml$china_positive_influence_index_full, "completed_near_china.pl10.30km.bin = planned_near_china.pl10.30km.bin")[2,4],3), error = function(e) print("NA")),
              tryCatch(round(linearHypothesis(ml$china.best.dev.model_full, "completed_near_china.pl10.30km.bin = planned_near_china.pl10.30km.bin")[2,4],3), error = function(e) print("NA")),
              tryCatch(round(linearHypothesis(ml$usa.best.dev.model_full, "completed_near_china.pl10.30km.bin = planned_near_china.pl10.30km.bin")[2,4],3), error = function(e) print("NA")) ),
            c("Planned Year Cut Off",2010,2010,2010,2010,2010,2010),
            c("Morans I P-Value",
              calc_morans_i(ml$china_positive_influence_index_full)$p.value %>% round(ROUND_NUM),
              calc_morans_i(ml$china.best.dev.model_full)$p.value %>% round(ROUND_NUM),
              calc_morans_i(ml$usa.best.dev.model_full)$p.value %>% round(ROUND_NUM)
            ),
            c("Spatial Lag of Dep Var Included", splag, splag, splag, splag, splag, splag),
            c("Country Fixed Effects", "Y", "Y", "Y","Y", "Y", "Y"),
            c("Buffer",buffer,buffer,buffer,buffer,buffer,buffer)),
          out=file.path(tables_file_path, "table_a9_full.tex")) 

rm(ml)
gc(); gc()

# Table A10 --------------------------------------------------------------------
ml <- load_models(c("china_positive_influence_index_restricted",
                    "china.best.dev.model_restricted",
                    "usa.best.dev.model_restricted"))

stargazer(ml$china_positive_influence_index_restricted,
          ml$china.best.dev.model_restricted,
          ml$usa.best.dev.model_restricted,
          dep.var.labels.include = T,
          dep.var.labels = c("China Positive", "China Best","US Best"),
          column.labels   = c("[Index]", "Model", "Model"),
          keep=c("completed_near_china.pl10.30km.bin", "planned_near_china.pl10.30km.bin",
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
              tryCatch(round(linearHypothesis(ml$china_positive_influence_index_restricted, "completed_near_china.pl10.30km.bin = planned_near_china.pl10.30km.bin")[2,4],3), error = function(e) print("NA")),
              tryCatch(round(linearHypothesis(ml$china.best.dev.model_restricted, "completed_near_china.pl10.30km.bin = planned_near_china.pl10.30km.bin")[2,4],3), error = function(e) print("NA")),
              tryCatch(round(linearHypothesis(ml$usa.best.dev.model_restricted, "completed_near_china.pl10.30km.bin = planned_near_china.pl10.30km.bin")[2,4],3), error = function(e) print("NA")) ),
            c("Completed vs. planned $p$-value [US]", 
              tryCatch(round(linearHypothesis(ml$china_positive_influence_index_restricted, "completed_near_usaid.30km.bin = planned_near_usaid.30km.bin")[2,4],3), error = function(e) print("NA")),
              tryCatch(round(linearHypothesis(ml$china.best.dev.model_restricted, "completed_near_usaid.30km.bin = planned_near_usaid.30km.bin")[2,4],3), error = function(e) print("NA")),
              tryCatch(round(linearHypothesis(ml$usa.best.dev.model_restricted, "completed_near_usaid.30km.bin = planned_near_usaid.30km.bin")[2,4],3), error = function(e) print("NA")) ),
            c("Planned Year Cut Off",2010,2010,2010,2010,2010,2010),
            c("Morans I P-Value",
              calc_morans_i(ml$china_positive_influence_index_restricted)$p.value %>% round(ROUND_NUM),
              calc_morans_i(ml$china.best.dev.model_restricted)$p.value %>% round(ROUND_NUM),
              calc_morans_i(ml$usa.best.dev.model_restricted)$p.value %>% round(ROUND_NUM)
            ),
            c("Spatial Lag of Dep Var Included", splag, splag, splag, splag, splag, splag),
            c("Country Fixed Effects", "Y", "Y", "Y","Y", "Y", "Y"),
            c("Buffer",buffer,buffer,buffer,buffer,buffer,buffer)),
          out=file.path(tables_file_path, "table_a9_restricted.tex")) 

rm(ml)
gc(); gc()

# Table A11 --------------------------------------------------------------------
ml <- load_models(c("lib_dem_val_index_full"))

stargazer(ml$lib_dem_val_index_full,
          dep.var.labels.include = T,
          dep.var.labels = c("Liberal Democratic"),
          column.labels   = c("Values Index"),
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
              tryCatch(round(linearHypothesis(ml$lib_dem_val_index_full, "completed_near_china.plNA.30km.bin = planned_near_china.plNA.30km.bin")[2,4],3), error = function(e) print("NA"))
              ),
            c("Planned Year Cut Off","N/A","N/A","N/A","N/A","N/A","N/A"),
            c("Morans I P-Value",
              calc_morans_i(ml$lib_dem_val_index_full)$p.value %>% round(ROUND_NUM)
            ),
            c("Spatial Lag of Dep Var Included", splag, splag, splag, splag, splag, splag),
            c("Country Fixed Effects", "Y", "Y", "Y", "Y","Y", "Y"),
            c("Buffer",buffer,buffer,buffer,buffer, buffer, buffer)),
          out=file.path(tables_file_path, "table_a10_full.tex")) 

rm(ml)
gc(); gc()

# Table A12 --------------------------------------------------------------------
ml <- load_models(c("lib_dem_val_index_restricted"))

stargazer(ml$lib_dem_val_index_restricted,
          dep.var.labels.include = T,
          dep.var.labels = c("Liberal Democratic"),
          column.labels   = c("Values Index"),
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
              tryCatch(round(linearHypothesis(ml$lib_dem_val_index_restricted, "completed_near_china.plNA.30km.bin = planned_near_china.plNA.30km.bin")[2,4],3), error = function(e) print("NA"))
              ),
            c("Completed vs. planned $p$-value [US]", 
              tryCatch(round(linearHypothesis(ml$lib_dem_val_index_restricted, "completed_near_usaid.30km.bin = planned_near_usaid.30km.bin")[2,4],3), error = function(e) print("NA"))
              ),
            c("Planned Year Cut Off","N/A","N/A","N/A","N/A","N/A","N/A"),
            c("Morans I P-Value",
              calc_morans_i(ml$lib_dem_val_index_restricted)$p.value %>% round(ROUND_NUM)
            ),
            c("Spatial Lag of Dep Var Included", splag, splag, splag, splag, splag, splag),
            c("Country Fixed Effects", "Y", "Y", "Y", "Y","Y", "Y"),
            c("Buffer",buffer,buffer,buffer,buffer, buffer,buffer)),
          out=file.path(tables_file_path, "table_a10_restricted.tex")) 

rm(ml)
gc(); gc()

# Table A13 --------------------------------------------------------------------
ml <- load_models(c("formcolnpower.best.dev.model_full"))

stargazer(ml$formcolnpower.best.dev.model_full,
          dep.var.labels.include = T,
          dep.var.labels = c("Former Colonial Power"),
          column.labels   = c("Best Model"),
          keep=c("completed_near_china.pl10.30km.bin", "planned_near_china.pl10.30km.bin"),
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
              tryCatch(round(linearHypothesis(ml$formcolnpower.best.dev.model_full, "completed_near_china.pl10.30km.bin = planned_near_china.pl10.30km.bin")[2,4],3), error = function(e) print("NA")) ),
            c("Planned Year Cut Off",2010,2010),
            c("Morans I P-Value",
              calc_morans_i(ml$formcolnpower.best.dev.model_full)$p.value %>% round(ROUND_NUM)
            ),
            c("Spatial Lag of Dep Var Included", splag, splag),
            c("Country Fixed Effects", "Y", "Y"),
            c("Buffer",buffer,buffer)),
          out=file.path(tables_file_path, "table_a11_full.tex")) 

rm(ml)
gc(); gc()

# Table A14 --------------------------------------------------------------------
ml <- load_models(c("formcolnpower.best.dev.model_restricted"))

stargazer(ml$formcolnpower.best.dev.model_restricted,
          dep.var.labels.include = T,
          dep.var.labels = c("Former Colonial Power"),
          column.labels   = c("Best Model"),
          keep=c("completed_near_china.pl10.30km.bin", "planned_near_china.pl10.30km.bin",
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
              tryCatch(round(linearHypothesis(ml$formcolnpower.best.dev.model_restricted, "completed_near_china.pl10.30km.bin = planned_near_china.pl10.30km.bin")[2,4],3), error = function(e) print("NA")) ),
            c("Completed vs. planned $p$-value [US]", 
              tryCatch(round(linearHypothesis(ml$formcolnpower.best.dev.model_restricted, "completed_near_usaid.30km.bin = planned_near_usaid.30km.bin")[2,4],3), error = function(e) print("NA")) ),
            c("Planned Year Cut Off",2010,2010),
            c("Morans I P-Value",
              calc_morans_i(ml$formcolnpower.best.dev.model_restricted)$p.value %>% round(ROUND_NUM)
            ),
            c("Spatial Lag of Dep Var Included", splag, splag),
            c("Country Fixed Effects", "Y", "Y"),
            c("Buffer",buffer,buffer)),
          out=file.path(tables_file_path, "table_a11_restricted.tex")) 

rm(ml)
gc(); gc()

