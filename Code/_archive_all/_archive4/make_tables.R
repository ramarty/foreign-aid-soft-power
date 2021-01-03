# Make Tables

## Load
buffer <- 30
ROUND_NUM <- 2
df <- readRDS(file.path(data_file_path, "afro_china_data.Rds"))

for(model_type in 1:4){
  print(paste(model_type, "=================================================="))

  ## Define differences
  if(model_type %in% 1){
    splag <- "F"
  }
  
  if(model_type %in% 2){
    splag <- "T"
  }
  
  if(model_type %in% 3){
    splag <- "F"
  }
  
  if(model_type %in% 4){
    splag <- "F"
  }
  
  ## Paths
  models_path <- file.path(dropbox_file_path, paste0("models_type_", model_type), "models")
  tables_file_path <- file.path(dropbox_file_path, paste0("models_type_", model_type), "tables")
  
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
  
  # ROUND 6 MODELS ===============================================================
  
  # Table 1 - 2010: Full ---------------------------------------------------------
  ml <- load_models(c("china_influential_index_full",
                      "china_positive_influence_index_full",
                      "china.most.influence_full",
                      "usa.most.influence_full",
                      "china.best.dev.model_full",
                      "usa.best.dev.model_full"))
  
  stargazer(ml$china_influential_index_full,
            ml$china_positive_influence_index_full,
            ml$china.most.influence_full,
            ml$usa.most.influence_full,
            ml$china.best.dev.model_full,
            ml$usa.best.dev.model_full,
            dep.var.labels = c("China Influential ","China Positive", "China Most","US Most","China Best","US Best"),
            column.labels   = c("[Index]", "[Index]", "Influence", "Influence",  "Model", "Model"),
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
                tryCatch(round(linearHypothesis(ml$china_influential_index_full, "completed_near_china.pl10.30km.bin = planned_near_china.pl10.30km.bin")[2,4],3), error = function(e) print("NA")),
                tryCatch(round(linearHypothesis(ml$china_positive_influence_index_full, "completed_near_china.pl10.30km.bin = planned_near_china.pl10.30km.bin")[2,4],3), error = function(e) print("NA")),
                tryCatch(round(linearHypothesis(ml$china.most.influence_full, "completed_near_china.pl10.30km.bin = planned_near_china.pl10.30km.bin")[2,4],3), error = function(e) print("NA")),
                tryCatch(round(linearHypothesis(ml$usa.most.influence_full, "completed_near_china.pl10.30km.bin = planned_near_china.pl10.30km.bin")[2,4],3), error = function(e) print("NA")),
                tryCatch(round(linearHypothesis(ml$china.best.dev.model_full, "completed_near_china.pl10.30km.bin = planned_near_china.pl10.30km.bin")[2,4],3), error = function(e) print("NA")),
                tryCatch(round(linearHypothesis(ml$usa.best.dev.model_full, "completed_near_china.pl10.30km.bin = planned_near_china.pl10.30km.bin")[2,4],3), error = function(e) print("NA")) ),
              c("Planned Year Cut Off",2010,2010,2010,2010,2010,2010),
              c("Morans I P-Value",
                calc_morans_i(ml$china_influential_index_full)$p.value %>% round(ROUND_NUM),
                calc_morans_i(ml$china_positive_influence_index_full)$p.value %>% round(ROUND_NUM),
                calc_morans_i(ml$china.most.influence_full)$p.value %>% round(ROUND_NUM),
                calc_morans_i(ml$usa.most.influence_full)$p.value %>% round(ROUND_NUM),
                calc_morans_i(ml$china.best.dev.model_full)$p.value %>% round(ROUND_NUM),
                calc_morans_i(ml$usa.best.dev.model_full)$p.value %>% round(ROUND_NUM)
              ),
              c("Spatial Lag of Dep Var Included", splag, splag, splag, splag, splag, splag),
              c("Country Fixed Effects", "Y", "Y", "Y","Y", "Y", "Y"),
              c("Buffer",buffer,buffer,buffer,buffer,buffer,buffer)),
            out=file.path(tables_file_path, "table_01_full.tex"))
  
  rm(ml)
  gc(); gc()
  
  # Table 1 - 2010: Restricted ---------------------------------------------------
  ml <- load_models(c("china_influential_index_restricted",
                      "china_positive_influence_index_restricted",
                      "china.most.influence_restricted",
                      "usa.most.influence_restricted",
                      "china.best.dev.model_restricted",
                      "usa.best.dev.model_restricted"))
  
  stargazer(ml$china_influential_index_restricted,
            ml$china_positive_influence_index_restricted,
            ml$china.most.influence_restricted,
            ml$usa.most.influence_restricted,
            ml$china.best.dev.model_restricted,
            ml$usa.best.dev.model_restricted,
            dep.var.labels.include = T,
            dep.var.labels = c("China Influential ","China Positive", "China Most","US Most","China Best","US Best"),
            column.labels   = c("[Index]", "[Index]", "Influence", "Influence",  "Model", "Model"),
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
                tryCatch(round(linearHypothesis(ml$china_influential_index_restricted, "completed_near_china.pl10.30km.bin = planned_near_china.pl10.30km.bin")[2,4],3), error = function(e) print("NA")),
                tryCatch(round(linearHypothesis(ml$china_positive_influence_index_restricted, "completed_near_china.pl10.30km.bin = planned_near_china.pl10.30km.bin")[2,4],3), error = function(e) print("NA")),
                tryCatch(round(linearHypothesis(ml$china.most.influence_restricted, "completed_near_china.pl10.30km.bin = planned_near_china.pl10.30km.bin")[2,4],3), error = function(e) print("NA")),
                tryCatch(round(linearHypothesis(ml$usa.most.influence_restricted, "completed_near_china.pl10.30km.bin = planned_near_china.pl10.30km.bin")[2,4],3), error = function(e) print("NA")),
                tryCatch(round(linearHypothesis(ml$china.best.dev.model_restricted, "completed_near_china.pl10.30km.bin = planned_near_china.pl10.30km.bin")[2,4],3), error = function(e) print("NA")),
                tryCatch(round(linearHypothesis(ml$usa.best.dev.model_restricted, "completed_near_china.pl10.30km.bin = planned_near_china.pl10.30km.bin")[2,4],3), error = function(e) print("NA")) ),
              c("Completed vs. planned $p$-value [US]", 
                tryCatch(round(linearHypothesis(ml$china_influential_index_restricted, "completed_near_usaid.30km.bin = planned_near_usaid.30km.bin")[2,4],3), error = function(e) print("NA")),
                tryCatch(round(linearHypothesis(ml$china_positive_influence_index_restricted, "completed_near_usaid.30km.bin = planned_near_usaid.30km.bin")[2,4],3), error = function(e) print("NA")),
                tryCatch(round(linearHypothesis(ml$china.most.influence_restricted, "completed_near_usaid.30km.bin = planned_near_usaid.30km.bin")[2,4],3), error = function(e) print("NA")),
                tryCatch(round(linearHypothesis(ml$usa.most.influence_restricted, "completed_near_usaid.30km.bin = planned_near_usaid.30km.bin")[2,4],3), error = function(e) print("NA")),
                tryCatch(round(linearHypothesis(ml$china.best.dev.model_restricted, "completed_near_usaid.30km.bin = planned_near_usaid.30km.bin")[2,4],3), error = function(e) print("NA")),
                tryCatch(round(linearHypothesis(ml$usa.best.dev.model_restricted, "completed_near_usaid.30km.bin = planned_near_usaid.30km.bin")[2,4],3), error = function(e) print("NA")) ),
              c("Planned Year Cut Off",2010,2010,2010,2010,2010,2010),
              c("Morans I P-Value",
                calc_morans_i(ml$china_influential_index_restricted)$p.value %>% round(ROUND_NUM),
                calc_morans_i(ml$china_positive_influence_index_restricted)$p.value %>% round(ROUND_NUM),
                calc_morans_i(ml$china.most.influence_restricted)$p.value %>% round(ROUND_NUM),
                calc_morans_i(ml$usa.most.influence_restricted)$p.value %>% round(ROUND_NUM),
                calc_morans_i(ml$china.best.dev.model_restricted)$p.value %>% round(ROUND_NUM),
                calc_morans_i(ml$usa.best.dev.model_restricted)$p.value %>% round(ROUND_NUM)
              ),
              c("Spatial Lag of Dep Var Included", splag, splag, splag, splag, splag, splag),
              c("Country Fixed Effects", "Y", "Y", "Y","Y", "Y", "Y"),
              c("Buffer",buffer,buffer,buffer,buffer,buffer,buffer)),
            out=file.path(tables_file_path, "table_01_restricted.tex"))
  
  rm(ml)
  gc(); gc()
  
  # Table 1 - 2009: Full ---------------------------------------------------------
  ml <- load_models(c("china_influential_index_full_2009",
                      "china_positive_influence_index_full_2009",
                      "china.most.influence_full_2009",
                      "usa.most.influence_full_2009",
                      "china.best.dev.model_full_2009",
                      "usa.best.dev.model_full_2009"))
  
  stargazer(ml$china_influential_index_full_2009,
            ml$china_positive_influence_index_full_2009,
            ml$china.most.influence_full_2009,
            ml$usa.most.influence_full_2009,
            ml$china.best.dev.model_full_2009,
            ml$usa.best.dev.model_full_2009,
            dep.var.labels.include = T,
            dep.var.labels = c("China Influential ","China Positive", "China Most","US Most","China Best","US Best"),
            column.labels   = c("[Index]", "[Index]", "Influence", "Influence",  "Model", "Model"),
            keep=c("completed_near_china.pl09.30km.bin", "planned_near_china.pl09.30km.bin"),
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
                tryCatch(round(linearHypothesis(ml$china_influential_index_full_2009, "completed_near_china.pl09.30km.bin = planned_near_china.pl09.30km.bin")[2,4],3), error = function(e) print("NA")),
                tryCatch(round(linearHypothesis(ml$china_positive_influence_index_full_2009, "completed_near_china.pl09.30km.bin = planned_near_china.pl09.30km.bin")[2,4],3), error = function(e) print("NA")),
                tryCatch(round(linearHypothesis(ml$china.most.influence_full_2009, "completed_near_china.pl09.30km.bin = planned_near_china.pl09.30km.bin")[2,4],3), error = function(e) print("NA")),
                tryCatch(round(linearHypothesis(ml$usa.most.influence_full_2009, "completed_near_china.pl09.30km.bin = planned_near_china.pl09.30km.bin")[2,4],3), error = function(e) print("NA")),
                tryCatch(round(linearHypothesis(ml$china.best.dev.model_full_2009, "completed_near_china.pl09.30km.bin = planned_near_china.pl09.30km.bin")[2,4],3), error = function(e) print("NA")),
                tryCatch(round(linearHypothesis(ml$usa.best.dev.model_full_2009, "completed_near_china.pl09.30km.bin = planned_near_china.pl09.30km.bin")[2,4],3), error = function(e) print("NA")) ),
              c("Planned Year Cut Off",2009,2009,2009,2009,2009,2009),
              c("Morans I P-Value",
                calc_morans_i(ml$china_influential_index_full_2009)$p.value %>% round(ROUND_NUM),
                calc_morans_i(ml$china_positive_influence_index_full_2009)$p.value %>% round(ROUND_NUM),
                calc_morans_i(ml$china.most.influence_full_2009)$p.value %>% round(ROUND_NUM),
                calc_morans_i(ml$usa.most.influence_full_2009)$p.value %>% round(ROUND_NUM),
                calc_morans_i(ml$china.best.dev.model_full_2009)$p.value %>% round(ROUND_NUM),
                calc_morans_i(ml$usa.best.dev.model_full_2009)$p.value %>% round(ROUND_NUM)
              ),
              c("Spatial Lag of Dep Var Included", splag, splag, splag, splag, splag, splag),
              c("Country Fixed Effects", "Y", "Y", "Y","Y", "Y", "Y"),
              c("Buffer",buffer,buffer,buffer,buffer,buffer,buffer)),
            out=file.path(tables_file_path, "table_01_full_2009.tex"))
  
  rm(ml)
  gc(); gc()
  
  # Table 1 - 2009: Restricted ---------------------------------------------------------
  ml <- load_models(c("china_influential_index_restricted_2009",
                      "china_positive_influence_index_restricted_2009",
                      "china.most.influence_restricted_2009",
                      "usa.most.influence_restricted_2009",
                      "china.best.dev.model_restricted_2009",
                      "usa.best.dev.model_restricted_2009"))
  
  stargazer(ml$china_influential_index_restricted_2009,
            ml$china_positive_influence_index_restricted_2009,
            ml$china.most.influence_restricted_2009,
            ml$usa.most.influence_restricted_2009,
            ml$china.best.dev.model_restricted_2009,
            ml$usa.best.dev.model_restricted_2009,
            dep.var.labels.include = T,
            dep.var.labels = c("China Influential ","China Positive", "China Most","US Most","China Best","US Best"),
            column.labels   = c("[Index]", "[Index]", "Influence", "Influence",  "Model", "Model"),
            keep=c("completed_near_china.pl08.30km.bin", "planned_near_china.pl08.30km.bin",
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
                tryCatch(round(linearHypothesis(ml$china_influential_index_restricted_2009, "completed_near_china.pl09.30km.bin = planned_near_china.pl09.30km.bin")[2,4],3), error = function(e) print("NA")),
                tryCatch(round(linearHypothesis(ml$china_positive_influence_index_restricted_2009, "completed_near_china.pl09.30km.bin = planned_near_china.pl09.30km.bin")[2,4],3), error = function(e) print("NA")),
                tryCatch(round(linearHypothesis(ml$china.most.influence_restricted_2009, "completed_near_china.pl09.30km.bin = planned_near_china.pl09.30km.bin")[2,4],3), error = function(e) print("NA")),
                tryCatch(round(linearHypothesis(ml$usa.most.influence_restricted_2009, "completed_near_china.pl09.30km.bin = planned_near_china.pl09.30km.bin")[2,4],3), error = function(e) print("NA")),
                tryCatch(round(linearHypothesis(ml$china.best.dev.model_restricted_2009, "completed_near_china.pl09.30km.bin = planned_near_china.pl09.30km.bin")[2,4],3), error = function(e) print("NA")),
                tryCatch(round(linearHypothesis(ml$usa.best.dev.model_restricted_2009, "completed_near_china.pl09.30km.bin = planned_near_china.pl09.30km.bin")[2,4],3), error = function(e) print("NA")) ),
              c("Completed vs. planned $p$-value [US]", 
                tryCatch(round(linearHypothesis(ml$china_influential_index_restricted_2009, "completed_near_usaid.30km.bin = planned_near_usaid.30km.bin")[2,4],3), error = function(e) print("NA")),
                tryCatch(round(linearHypothesis(ml$china_positive_influence_index_restricted_2009, "completed_near_usaid.30km.bin = planned_near_usaid.30km.bin")[2,4],3), error = function(e) print("NA")),
                tryCatch(round(linearHypothesis(ml$china.most.influence_restricted_2009, "completed_near_usaid.30km.bin = planned_near_usaid.30km.bin")[2,4],3), error = function(e) print("NA")),
                tryCatch(round(linearHypothesis(ml$usa.most.influence_restricted_2009, "completed_near_usaid.30km.bin = planned_near_usaid.30km.bin")[2,4],3), error = function(e) print("NA")),
                tryCatch(round(linearHypothesis(ml$china.best.dev.model_restricted_2009, "completed_near_usaid.30km.bin = planned_near_usaid.30km.bin")[2,4],3), error = function(e) print("NA")),
                tryCatch(round(linearHypothesis(ml$usa.best.dev.model_restricted_2009, "completed_near_usaid.30km.bin = planned_near_usaid.30km.bin")[2,4],3), error = function(e) print("NA")) ),
              c("Planned Year Cut Off",2009,2009,2009,2009,2009,2009),
              c("Morans I P-Value",
                calc_morans_i(ml$china_influential_index_restricted_2009)$p.value %>% round(ROUND_NUM),
                calc_morans_i(ml$china_positive_influence_index_restricted_2009)$p.value %>% round(ROUND_NUM),
                calc_morans_i(ml$china.most.influence_restricted_2009)$p.value %>% round(ROUND_NUM),
                calc_morans_i(ml$usa.most.influence_restricted_2009)$p.value %>% round(ROUND_NUM),
                calc_morans_i(ml$china.best.dev.model_restricted_2009)$p.value %>% round(ROUND_NUM),
                calc_morans_i(ml$usa.best.dev.model_restricted_2009)$p.value %>% round(ROUND_NUM)
              ),
              c("Spatial Lag of Dep Var Included", splag, splag, splag, splag, splag, splag),
              c("Country Fixed Effects", "Y", "Y", "Y","Y", "Y", "Y"),
              c("Buffer",buffer,buffer,buffer,buffer,buffer,buffer)),
            out=file.path(tables_file_path, "table_01_restricted_2009.tex"))
  
  rm(ml)
  gc(); gc()
  
  # Table 1 - 2008: Full ---------------------------------------------------------
  ml <- load_models(c("china_influential_index_full_2008",
                      "china_positive_influence_index_full_2008",
                      "china.most.influence_full_2008",
                      "usa.most.influence_full_2008",
                      "china.best.dev.model_full_2008",
                      "usa.best.dev.model_full_2008"))
  
  stargazer(ml$china_influential_index_full_2008,
            ml$china_positive_influence_index_full_2008,
            ml$china.most.influence_full_2008,
            ml$usa.most.influence_full_2008,
            ml$china.best.dev.model_full_2008,
            ml$usa.best.dev.model_full_2008,
            dep.var.labels.include = T,
            dep.var.labels = c("China Influential ","China Positive", "China Most","US Most","China Best","US Best"),
            column.labels   = c("[Index]", "[Index]", "Influence", "Influence",  "Model", "Model"),
            keep=c("completed_near_china.pl08.30km.bin", "planned_near_china.pl08.30km.bin"),
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
                tryCatch(round(linearHypothesis(ml$china_influential_index_full_2008, "completed_near_china.pl08.30km.bin = planned_near_china.pl08.30km.bin")[2,4],3), error = function(e) print("NA")),
                tryCatch(round(linearHypothesis(ml$china_positive_influence_index_full_2008, "completed_near_china.pl08.30km.bin = planned_near_china.pl08.30km.bin")[2,4],3), error = function(e) print("NA")),
                tryCatch(round(linearHypothesis(ml$china.most.influence_full_2008, "completed_near_china.pl08.30km.bin = planned_near_china.pl08.30km.bin")[2,4],3), error = function(e) print("NA")),
                tryCatch(round(linearHypothesis(ml$usa.most.influence_full_2008, "completed_near_china.pl08.30km.bin = planned_near_china.pl08.30km.bin")[2,4],3), error = function(e) print("NA")),
                tryCatch(round(linearHypothesis(ml$china.best.dev.model_full_2008, "completed_near_china.pl08.30km.bin = planned_near_china.pl08.30km.bin")[2,4],3), error = function(e) print("NA")),
                tryCatch(round(linearHypothesis(ml$usa.best.dev.model_full_2008, "completed_near_china.pl08.30km.bin = planned_near_china.pl08.30km.bin")[2,4],3), error = function(e) print("NA")) ),
              c("Planned Year Cut Off",2008,2008,2008,2008,2008,2008),
              c("Morans I P-Value",
                calc_morans_i(ml$china_influential_index_full_2008)$p.value %>% round(ROUND_NUM),
                calc_morans_i(ml$china_positive_influence_index_full_2008)$p.value %>% round(ROUND_NUM),
                calc_morans_i(ml$china.most.influence_full_2008)$p.value %>% round(ROUND_NUM),
                calc_morans_i(ml$usa.most.influence_full_2008)$p.value %>% round(ROUND_NUM),
                calc_morans_i(ml$china.best.dev.model_full_2008)$p.value %>% round(ROUND_NUM),
                calc_morans_i(ml$usa.best.dev.model_full_2008)$p.value %>% round(ROUND_NUM)
              ),
              c("Spatial Lag of Dep Var Included", splag, splag, splag, splag, splag, splag),
              c("Country Fixed Effects", "Y", "Y", "Y","Y", "Y", "Y"),
              c("Buffer",buffer,buffer,buffer,buffer,buffer,buffer)),
            out=file.path(tables_file_path, "table_01_full_2008.tex"))
  
  rm(ml)
  gc(); gc()
  
  # Table 1 - 2008: Restricted ---------------------------------------------------------
  ml <- load_models(c("china_influential_index_restricted_2008",
                      "china_positive_influence_index_restricted_2008",
                      "china.most.influence_restricted_2008",
                      "usa.most.influence_restricted_2008",
                      "china.best.dev.model_restricted_2008",
                      "usa.best.dev.model_restricted_2008"))
  
  stargazer(ml$china_influential_index_restricted_2008,
            ml$china_positive_influence_index_restricted_2008,
            ml$china.most.influence_restricted_2008,
            ml$usa.most.influence_restricted_2008,
            ml$china.best.dev.model_restricted_2008,
            ml$usa.best.dev.model_restricted_2008,
            dep.var.labels.include = T,
            dep.var.labels = c("China Influential ","China Positive", "China Most","US Most","China Best","US Best"),
            column.labels   = c("[Index]", "[Index]", "Influence", "Influence",  "Model", "Model"),
            keep=c("completed_near_china.pl08.30km.bin", "planned_near_china.pl08.30km.bin",
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
                tryCatch(round(linearHypothesis(ml$china_influential_index_restricted_2008, "completed_near_china.pl08.30km.bin = planned_near_china.pl08.30km.bin")[2,4],3), error = function(e) print("NA")),
                tryCatch(round(linearHypothesis(ml$china_positive_influence_index_restricted_2008, "completed_near_china.pl08.30km.bin = planned_near_china.pl08.30km.bin")[2,4],3), error = function(e) print("NA")),
                tryCatch(round(linearHypothesis(ml$china.most.influence_restricted_2008, "completed_near_china.pl08.30km.bin = planned_near_china.pl08.30km.bin")[2,4],3), error = function(e) print("NA")),
                tryCatch(round(linearHypothesis(ml$usa.most.influence_restricted_2008, "completed_near_china.pl08.30km.bin = planned_near_china.pl08.30km.bin")[2,4],3), error = function(e) print("NA")),
                tryCatch(round(linearHypothesis(ml$china.best.dev.model_restricted_2008, "completed_near_china.pl08.30km.bin = planned_near_china.pl08.30km.bin")[2,4],3), error = function(e) print("NA")),
                tryCatch(round(linearHypothesis(ml$usa.best.dev.model_restricted_2008, "completed_near_china.pl08.30km.bin = planned_near_china.pl08.30km.bin")[2,4],3), error = function(e) print("NA")) ),
              c("Completed vs. planned $p$-value [US]", 
                tryCatch(round(linearHypothesis(ml$china_influential_index_restricted_2008, "completed_near_usaid.30km.bin = planned_near_usaid.30km.bin")[2,4],3), error = function(e) print("NA")),
                tryCatch(round(linearHypothesis(ml$china_positive_influence_index_restricted_2008, "completed_near_usaid.30km.bin = planned_near_usaid.30km.bin")[2,4],3), error = function(e) print("NA")),
                tryCatch(round(linearHypothesis(ml$china.most.influence_restricted_2008, "completed_near_usaid.30km.bin = planned_near_usaid.30km.bin")[2,4],3), error = function(e) print("NA")),
                tryCatch(round(linearHypothesis(ml$usa.most.influence_restricted_2008, "completed_near_usaid.30km.bin = planned_near_usaid.30km.bin")[2,4],3), error = function(e) print("NA")),
                tryCatch(round(linearHypothesis(ml$china.best.dev.model_restricted_2008, "completed_near_usaid.30km.bin = planned_near_usaid.30km.bin")[2,4],3), error = function(e) print("NA")),
                tryCatch(round(linearHypothesis(ml$usa.best.dev.model_restricted_2008, "completed_near_usaid.30km.bin = planned_near_usaid.30km.bin")[2,4],3), error = function(e) print("NA")) ),
              c("Planned Year Cut Off",2008,2008,2008,2008,2008,2008),
              c("Morans I P-Value",
                calc_morans_i(ml$china_influential_index_restricted_2008)$p.value %>% round(ROUND_NUM),
                calc_morans_i(ml$china_positive_influence_index_restricted_2008)$p.value %>% round(ROUND_NUM),
                calc_morans_i(ml$china.most.influence_restricted_2008)$p.value %>% round(ROUND_NUM),
                calc_morans_i(ml$usa.most.influence_restricted_2008)$p.value %>% round(ROUND_NUM),
                calc_morans_i(ml$china.best.dev.model_restricted_2008)$p.value %>% round(ROUND_NUM),
                calc_morans_i(ml$usa.best.dev.model_restricted_2008)$p.value %>% round(ROUND_NUM)
              ),
              c("Spatial Lag of Dep Var Included", splag, splag, splag, splag, splag, splag),
              c("Country Fixed Effects", "Y", "Y", "Y","Y", "Y", "Y"),
              c("Buffer",buffer,buffer,buffer,buffer,buffer,buffer)),
            out=file.path(tables_file_path, "table_01_restricted_2008.tex"))
  
  rm(ml)
  gc(); gc()
  
  # Table 1 - 2010: Full - Infrastructure --------------------------------------
  ml <- load_models(c("china_influential_index_full_infrastructure",
                      "china_positive_influence_index_full_infrastructure",
                      "china.most.influence_full_infrastructure",
                      "usa.most.influence_full_infrastructure",
                      "china.best.dev.model_full_infrastructure",
                      "usa.best.dev.model_full_infrastructure"))
  
  stargazer(ml$china_influential_index_full_infrastructure,
            ml$china_positive_influence_index_full_infrastructure,
            ml$china.most.influence_full_infrastructure,
            ml$usa.most.influence_full_infrastructure,
            ml$china.best.dev.model_full_infrastructure,
            ml$usa.best.dev.model_full_infrastructure,
            dep.var.labels.include = T,
            dep.var.labels = c("China Influential ","China Positive", "China Most","US Most","China Best","US Best"),
            column.labels   = c("[Index]", "[Index]", "Influence", "Influence",  "Model", "Model"),
            keep=c("completed_near_china.pl10.30km.construct.bin", "planned_near_china.pl10.30km.construct.bin",
                   "completed_near_china.pl10.30km.noconstruct.bin", "planned_near_china.pl10.30km.noconstruct.bin"),
            covariate.labels = c("China Completed, Infras.","China Planned, Infras.",
                                 "USA Completed, Non-Infras.","USA Planned, Non-Infras."),
            dep.var.caption = "",
            omit.stat = c("f","ser"), 
            align=TRUE,
            no.space=TRUE,
            float=FALSE,
            column.sep.width = "8pt",
            report="vcs*",
            digits = 2,
            add.lines = list(
              c("Completed vs. planned $p$-value [Infrast.]", 
                tryCatch(round(linearHypothesis(ml$china_influential_index_full_infrastructure, "completed_near_china.pl10.30km.construct.bin = planned_near_china.pl10.30km.construct.bin")[2,4],3), error = function(e) print("NA")),
                tryCatch(round(linearHypothesis(ml$china_positive_influence_index_full_infrastructure, "completed_near_china.pl10.30km.construct.bin = planned_near_china.pl10.30km.construct.bin")[2,4],3), error = function(e) print("NA")),
                tryCatch(round(linearHypothesis(ml$china.most.influence_full_infrastructure, "completed_near_china.pl10.30km.construct.bin = planned_near_china.pl10.30km.construct.bin")[2,4],3), error = function(e) print("NA")),
                tryCatch(round(linearHypothesis(ml$usa.most.influence_full_infrastructure, "completed_near_china.pl10.30km.construct.bin = planned_near_china.pl10.30km.construct.bin")[2,4],3), error = function(e) print("NA")),
                tryCatch(round(linearHypothesis(ml$china.best.dev.model_full_infrastructure, "completed_near_china.pl10.30km.construct.bin = planned_near_china.pl10.30km.construct.bin")[2,4],3), error = function(e) print("NA")),
                tryCatch(round(linearHypothesis(ml$usa.best.dev.model_full_infrastructure, "completed_near_china.pl10.30km.construct.bin = planned_near_china.pl10.30km.construct.bin")[2,4],3), error = function(e) print("NA")) 
                ),
              c("Completed vs. planned $p$-value [Non-Infrast.]", 
                tryCatch(round(linearHypothesis(ml$china_influential_index_full_infrastructure, "completed_near_china.pl10.30km.noconstruct.bin = planned_near_china.pl10.30km.noconstruct.bin")[2,4],3), error = function(e) print("NA")),
                tryCatch(round(linearHypothesis(ml$china_positive_influence_index_full_infrastructure, "completed_near_china.pl10.30km.noconstruct.bin = planned_near_china.pl10.30km.noconstruct.bin")[2,4],3), error = function(e) print("NA")),
                tryCatch(round(linearHypothesis(ml$china.most.influence_full_infrastructure, "completed_near_china.pl10.30km.noconstruct.bin = planned_near_china.pl10.30km.noconstruct.bin")[2,4],3), error = function(e) print("NA")),
                tryCatch(round(linearHypothesis(ml$usa.most.influence_full_infrastructure, "completed_near_china.pl10.30km.noconstruct.bin = planned_near_china.pl10.30km.noconstruct.bin")[2,4],3), error = function(e) print("NA")),
                tryCatch(round(linearHypothesis(ml$china.best.dev.model_full_infrastructure, "completed_near_china.pl10.30km.noconstruct.bin = planned_near_china.pl10.30km.noconstruct.bin")[2,4],3), error = function(e) print("NA")),
                tryCatch(round(linearHypothesis(ml$usa.best.dev.model_full_infrastructure, "completed_near_china.pl10.30km.noconstruct.bin = planned_near_china.pl10.30km.noconstruct.bin")[2,4],3), error = function(e) print("NA")) 
                ),
              c("Planned Year Cut Off",2010,2010,2010,2010,2010,2010),
              c("Morans I P-Value",
                calc_morans_i(ml$china_influential_index_full_infrastructure)$p.value %>% round(ROUND_NUM),
                calc_morans_i(ml$china_positive_influence_index_full_infrastructure)$p.value %>% round(ROUND_NUM),
                calc_morans_i(ml$china.most.influence_full_infrastructure)$p.value %>% round(ROUND_NUM),
                calc_morans_i(ml$usa.most.influence_full_infrastructure)$p.value %>% round(ROUND_NUM),
                calc_morans_i(ml$china.best.dev.model_full_infrastructure)$p.value %>% round(ROUND_NUM),
                calc_morans_i(ml$usa.best.dev.model_full_infrastructure)$p.value %>% round(ROUND_NUM)
              ),
              c("Spatial Lag of Dep Var Included", splag, splag, splag, splag, splag, splag),
              c("Country Fixed Effects", "Y", "Y", "Y","Y", "Y", "Y"),
              c("Buffer",buffer,buffer,buffer,buffer,buffer,buffer)),
            out=file.path(tables_file_path, "table_01_full_infrastructure.tex"))
  
  rm(ml)
  gc(); gc()
  
  # Table 3 - Full -------------------------------------------------------------------
  ml <- load_models(c("formcolnpower.most.influence_full",
                      "formcolnpower.best.dev.model_full"))
  
  stargazer(ml$formcolnpower.most.influence_full,
            ml$formcolnpower.best.dev.model_full,
            dep.var.labels.include = T,
            dep.var.labels = c("Former Colonial Power", "Former Colonial Power"),
            column.labels   = c("Most Influential", "Best Model"),
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
                tryCatch(round(linearHypothesis(ml$formcolnpower.most.influence_full, "completed_near_china.pl10.30km.bin = planned_near_china.pl10.30km.bin")[2,4],3), error = function(e) print("NA")),
                tryCatch(round(linearHypothesis(ml$formcolnpower.best.dev.model_full, "completed_near_china.pl10.30km.bin = planned_near_china.pl10.30km.bin")[2,4],3), error = function(e) print("NA")) ),
              c("Planned Year Cut Off",2010,2010),
              c("Morans I P-Value",
                calc_morans_i(ml$formcolnpower.most.influence_full)$p.value %>% round(ROUND_NUM),
                calc_morans_i(ml$formcolnpower.best.dev.model_full)$p.value %>% round(ROUND_NUM)
              ),
              c("Spatial Lag of Dep Var Included", splag, splag),
              c("Country Fixed Effects", "Y", "Y"),
              c("Buffer",buffer,buffer)),
            out=file.path(tables_file_path, "table_03_full.tex"))
  
  rm(ml)
  gc(); gc()
  
  # Table 3 - Restricted -------------------------------------------------------------
  ml <- load_models(c("formcolnpower.most.influence_restricted",
                      "formcolnpower.best.dev.model_restricted"))
  
  stargazer(ml$formcolnpower.most.influence_restricted,
            ml$formcolnpower.best.dev.model_restricted,
            dep.var.labels.include = T,
            dep.var.labels = c("Former Colonial Power", "Former Colonial Power"),
            column.labels   = c("Most Influential", "Best Model"),
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
                tryCatch(round(linearHypothesis(ml$formcolnpower.most.influence_restricted, "completed_near_china.pl10.30km.bin = planned_near_china.pl10.30km.bin")[2,4],3), error = function(e) print("NA")),
                tryCatch(round(linearHypothesis(ml$formcolnpower.best.dev.model_restricted, "completed_near_china.pl10.30km.bin = planned_near_china.pl10.30km.bin")[2,4],3), error = function(e) print("NA")) ),
              c("Completed vs. planned $p$-value [US]", 
                tryCatch(round(linearHypothesis(ml$formcolnpower.most.influence_restricted, "completed_near_usaid.30km.bin = planned_near_usaid.30km.bin")[2,4],3), error = function(e) print("NA")),
                tryCatch(round(linearHypothesis(ml$formcolnpower.best.dev.model_restricted, "completed_near_usaid.30km.bin = planned_near_usaid.30km.bin")[2,4],3), error = function(e) print("NA")) ),
              c("Planned Year Cut Off",2010,2010),
              c("Morans I P-Value",
                calc_morans_i(ml$formcolnpower.most.influence_restricted)$p.value %>% round(ROUND_NUM),
                calc_morans_i(ml$formcolnpower.best.dev.model_restricted)$p.value %>% round(ROUND_NUM)
              ),
              c("Spatial Lag of Dep Var Included", splag, splag),
              c("Country Fixed Effects", "Y", "Y"),
              c("Buffer",buffer,buffer)),
            out=file.path(tables_file_path, "table_03_restricted.tex"))
  
  rm(ml)
  gc(); gc()
  
  # Table 5 ----------------------------------------------------------------------
  ml <- load_models(c("posimage_chinesepeople_full",
                      "posimage_businessinvetment_full",
                      "posimage_infordevinvetment_full",
                      "posimage_noninterference_full",
                      "posimage_supportinintlaffiars_full",
                      "posimage_productcost_full"))
  
  stargazer(ml$posimage_chinesepeople_full,
            ml$posimage_businessinvetment_full,
            ml$posimage_infordevinvetment_full,
            ml$posimage_noninterference_full,
            ml$posimage_supportinintlaffiars_full,
            ml$posimage_productcost_full,
            dep.var.labels.include = T,
            dep.var.labels = c("Chinese People ","Business","Inf / Development","Non-Interference","Support in","Cost of"),
            column.labels   = c("and Culture", "Investment", "Investment",      "Policy",           "Intl Affairs", "Chinese Products"),
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
                tryCatch(round(linearHypothesis(ml$posimage_chinesepeople_full, "completed_near_china.pl10.30km.bin = planned_near_china.pl10.30km.bin")[2,4],3), error = function(e) print("NA")),
                tryCatch(round(linearHypothesis(ml$posimage_businessinvetment_full, "completed_near_china.pl10.30km.bin = planned_near_china.pl10.30km.bin")[2,4],3), error = function(e) print("NA")),
                tryCatch(round(linearHypothesis(ml$posimage_infordevinvetment_full, "completed_near_china.pl10.30km.bin = planned_near_china.pl10.30km.bin")[2,4],3), error = function(e) print("NA")),
                tryCatch(round(linearHypothesis(ml$posimage_noninterference_full, "completed_near_china.pl10.30km.bin = planned_near_china.pl10.30km.bin")[2,4],3), error = function(e) print("NA")),
                tryCatch(round(linearHypothesis(ml$posimage_supportinintlaffiars_full, "completed_near_china.pl10.30km.bin = planned_near_china.pl10.30km.bin")[2,4],3), error = function(e) print("NA")),
                tryCatch(round(linearHypothesis(ml$posimage_productcost, "completed_near_china.pl10.30km.bin = planned_near_china.pl10.30km.bin")[2,4],3), error = function(e) print("NA")) ),
              c("Planned Year Cut Off",2010,2010,2010,2010,2010,2010),
              c("Morans I P-Value",
                calc_morans_i(ml$posimage_chinesepeople_full)$p.value %>% round(ROUND_NUM),
                calc_morans_i(ml$posimage_businessinvetment_full)$p.value %>% round(ROUND_NUM),
                calc_morans_i(ml$posimage_infordevinvetment_full)$p.value %>% round(ROUND_NUM),
                calc_morans_i(ml$posimage_noninterference_full)$p.value %>% round(ROUND_NUM),
                calc_morans_i(ml$posimage_supportinintlaffiars_full)$p.value %>% round(ROUND_NUM),
                calc_morans_i(ml$posimage_productcost_full)$p.value %>% round(ROUND_NUM)
              ),
              c("Spatial Lag of Dep Var Included", splag, splag, splag, splag, splag, splag),
              c("Country Fixed Effects", "Y", "Y", "Y","Y", "Y","Y"),
              c("Buffer",buffer,buffer,buffer,buffer, buffer,buffer)),
            out=file.path(tables_file_path, "table_05_full.tex"))
  
  rm(ml)
  gc(); gc()
  
  # Table 6 ----------------------------------------------------------------------
  ml <- load_models(c("negimage_cooperateundemocratic_full",
                      "negimage_chinesecitizenbehavior_full",
                      "negimage_resourceextraction_full",
                      "negimage_takingjobsbusiness_full",
                      "negimage_landgrabbing_full",
                      "negimage_productquality_full"))
  
  stargazer(ml$negimage_cooperateundemocratic_full,
            ml$negimage_chinesecitizenbehavior_full,
            ml$negimage_resourceextraction_full,
            ml$negimage_takingjobsbusiness_full,
            ml$negimage_landgrabbing_full,
            ml$negimage_productquality_full,
            dep.var.labels.include = T,
            dep.var.labels = c("Coop With",     "Behavior of ",    "Resource",   "Taking Jobs",   "Land",     "Product"),
            column.labels   = c("Undem Leaders", "Chinese Citzns", "Extraction", "and Business",  "Grabbing", "Quality"),
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
                tryCatch(round(linearHypothesis(ml$negimage_cooperateundemocratic_full, "completed_near_china.pl10.30km.bin = planned_near_china.pl10.30km.bin")[2,4],3), error = function(e) print("NA")),
                tryCatch(round(linearHypothesis(ml$negimage_chinesecitizenbehavior_full, "completed_near_china.pl10.30km.bin = planned_near_china.pl10.30km.bin")[2,4],3), error = function(e) print("NA")),
                tryCatch(round(linearHypothesis(ml$negimage_resourceextraction_full, "completed_near_china.pl10.30km.bin = planned_near_china.pl10.30km.bin")[2,4],3), error = function(e) print("NA")),
                tryCatch(round(linearHypothesis(ml$negimage_takingjobsbusiness_full, "completed_near_china.pl10.30km.bin = planned_near_china.pl10.30km.bin")[2,4],3), error = function(e) print("NA")),
                tryCatch(round(linearHypothesis(ml$negimage_landgrabbing_full, "completed_near_china.pl10.30km.bin = planned_near_china.pl10.30km.bin")[2,4],3), error = function(e) print("NA")),
                tryCatch(round(linearHypothesis(ml$negimage_productquality_full, "completed_near_china.pl10.30km.bin = planned_near_china.pl10.30km.bin")[2,4],3), error = function(e) print("NA")) ),
              c("Planned Year Cut Off",2010,2010,2010,2010,2010,2010),
              c("Morans I P-Value",
                calc_morans_i(ml$negimage_cooperateundemocratic_full)$p.value %>% round(ROUND_NUM),
                calc_morans_i(ml$negimage_chinesecitizenbehavior_full)$p.value %>% round(ROUND_NUM),
                calc_morans_i(ml$negimage_resourceextraction_full)$p.value %>% round(ROUND_NUM),
                calc_morans_i(ml$negimage_takingjobsbusiness_full)$p.value %>% round(ROUND_NUM),
                calc_morans_i(ml$negimage_landgrabbing_full)$p.value %>% round(ROUND_NUM),
                calc_morans_i(ml$negimage_productquality_full)$p.value %>% round(ROUND_NUM)
              ),
              c("Spatial Lag of Dep Var Included", splag, splag, splag, splag, splag, splag),
              c("Country Fixed Effects", "Y", "Y", "Y","Y", "Y","Y"),
              c("Buffer",buffer,buffer,buffer,buffer, buffer,buffer)),
            out=file.path(tables_file_path, "table_06_full.tex"))
  
  rm(ml)
  gc(); gc()
  
  # Table 7 ----------------------------------------------------------------------
  ml <- load_models(c("china_dontknow_index_full",
                      "china.influence.econ.activity_DONTKNOW_full",
                      "china.econpol.influence.positive_DONTKNOW_full",
                      "china.aid.good.job.meet.country.needs_DONTKNOW_full"))
  
  stargazer(ml$china_dontknow_index_full,
            ml$china.influence.econ.activity_DONTKNOW_full,
            ml$china.econpol.influence.positive_DONTKNOW_full,
            ml$china.aid.good.job.meet.country.needs_DONTKNOW_full,
            dep.var.labels.include = T,
            dep.var.labels = c("Chinese Q Don't", "Chinese Econ Activity",  "Econ/Pol Influ.",   "Chinese Aid Meets",     "China Helps"),
            column.labels   = c("Know [Index]",   "Influ. Economy [DK]",    "Pos. or Neg. [DK]", "Country's Needs [DK]",  "Country [DK]"),
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
                tryCatch(round(linearHypothesis(ml$china_dontknow_index_full, "completed_near_china.pl10.30km.bin = planned_near_china.pl10.30km.bin")[2,4],3), error = function(e) print("NA")),
                tryCatch(round(linearHypothesis(ml$china.influence.econ.activity_DONTKNOW_full, "completed_near_china.pl10.30km.bin = planned_near_china.pl10.30km.bin")[2,4],3), error = function(e) print("NA")),
                tryCatch(round(linearHypothesis(ml$china.econpol.influence.positive_DONTKNOW_full, "completed_near_china.pl10.30km.bin = planned_near_china.pl10.30km.bin")[2,4],3), error = function(e) print("NA")),
                tryCatch(round(linearHypothesis(ml$china.aid.good.job.meet.country.needs_DONTKNOW_full, "completed_near_china.pl10.30km.bin = planned_near_china.pl10.30km.bin")[2,4],3), error = function(e) print("NA"))
              ),
              c("Planned Year Cut Off",2010,2010,2010,2010),
              c("Morans I P-Value",
                calc_morans_i(ml$china_dontknow_index_full)$p.value %>% round(ROUND_NUM),
                calc_morans_i(ml$china.influence.econ.activity_DONTKNOW_full)$p.value %>% round(ROUND_NUM),
                calc_morans_i(ml$china.econpol.influence.positive_DONTKNOW_full)$p.value %>% round(ROUND_NUM),
                calc_morans_i(ml$china.aid.good.job.meet.country.needs_DONTKNOW_full)$p.value %>% round(ROUND_NUM)
              ),
              c("Spatial Lag of Dep Var Included", splag, splag, splag, splag),
              c("Country Fixed Effects", "Y", "Y", "Y","Y"),
              c("Buffer",buffer,buffer,buffer,buffer)),
            out=file.path(tables_file_path, "table_07_full.tex"))
  
  rm(ml)
  gc(); gc()
  
  # ROUND 4 MODELS ===============================================================
  
  # Table x ----------------------------------------------------------------------
  ml <- load_models(c("china.help.country_full",
                      "usa.help.country_full"))
  
  stargazer(ml$china.help.country,
            ml$usa.help.country,
            dep.var.labels.include = T,
            dep.var.labels = c("Believes","Believes"),
            column.labels   = c("China Helps", "US Helps"),
            keep=c("completed_near_china.plNA.30km.bin",   "planned_near_china.plNA.30km.bin"),
            covariate.labels = c("China Completed", "China Planned"),
            dep.var.caption = "",
            omit.stat = c("f","ser"), 
            align=TRUE,
            no.space=TRUE,
            float=FALSE,
            column.sep.width = "8pt",
            report="vcs*",
            digits = 2,
            add.lines = list(
              c("Completed vs. planned infrastructure $p$-value", 
                tryCatch(round(linearHypothesis(ml$china.help.country, "completed_near_china.plNA.30km.bin = planned_near_china.plNA.30km.bin")[2,4],3), error = function(e) print("NA")),
                tryCatch(round(linearHypothesis(ml$usa.help.country, "completed_near_china.plNA.30km.bin = planned_near_china.plNA.30km.bin")[2,4],3), error = function(e) print("NA")) ),
              c("Planned Year Cut Off","N/A","N/A"),
              c("Morans I P-Value",
                calc_morans_i(ml$china.help.country)$p.value %>% round(ROUND_NUM),
                calc_morans_i(ml$usa.help.country)$p.value %>% round(ROUND_NUM)
              ),
              c("Spatial Lag of Dep Var Included", splag, splag),
              c("Country Fixed Effects", "Y", "Y"),
              c("Buffer",buffer,buffer)),
            out=file.path(tables_file_path, "round4_table.tex"))
  
  rm(ml)
  gc(); gc()
  
  # Table x - Planned Eventually Completed ---------------------------------------
  ml <- load_models(c("china.help.country_full_plcmpltd",
                      "usa.help.country_full_plcmpltd"))
  
  stargazer(ml$china.help.country_full_plcmpltd,
            ml$usa.help.country_full_plcmpltd,
            dep.var.labels.include = T,
            dep.var.labels = c("Believes","Believes"),
            column.labels   = c("China Helps", "US Helps"),
            keep=c("completed_near_china.plNAcmpltd.30km.bin",   "planned_near_china.plNAcmpltd.30km.bin"),
            covariate.labels = c("China Completed", "China Planned"),
            dep.var.caption = "",
            omit.stat = c("f","ser"), 
            align=TRUE,
            no.space=TRUE,
            float=FALSE,
            column.sep.width = "8pt",
            report="vcs*",
            digits = 2,
            add.lines = list(
              c("Completed vs. planned infrastructure $p$-value", 
                tryCatch(round(linearHypothesis(ml$china.help.country_full_plcmpltd, "completed_near_china.plNA.30km.bin = planned_near_china.plNAcmpltd.30km.bin")[2,4],3), error = function(e) print("NA")),
                tryCatch(round(linearHypothesis(ml$usa.help.country_full_plcmpltd, "completed_near_china.plNA.30km.bin = planned_near_china.plNAcmpltd.30km.bin")[2,4],3), error = function(e) print("NA")) ),
              c("Planned Year Cut Off","N/A","N/A"),
              c("Morans I P-Value",
                calc_morans_i(ml$china.help.country_full_plcmpltd)$p.value %>% round(ROUND_NUM),
                calc_morans_i(ml$usa.help.country_full_plcmpltd)$p.value %>% round(ROUND_NUM)
              ),
              c("Spatial Lag of Dep Var Included", splag, splag),
              c("Country Fixed Effects", "Y", "Y"),
              c("Buffer",buffer,buffer)),
            out=file.path(tables_file_path, "round4_table_plannedCompleted.tex"))
  
  rm(ml)
  gc(); gc()
  
  # ROUND 2-5 MODELS =============================================================
  
  # Table 2 - Full ---------------------------------------------------------------
  ml <- load_models(c("lib_dem_val_index_full",
                      "blvs_mult_parties_good_full",
                      "blvs_mult_parties_create_choice_full",
                      "blvs_ctzn_should_join_any_cso_full",
                      "blvs_democ_best_system_full",
                      "blvs_elec_good_full"))
  
  stargazer(ml$lib_dem_val_index_full,
            ml$blvs_mult_parties_good_full,
            ml$blvs_mult_parties_create_choice_full,
            ml$blvs_ctzn_should_join_any_cso_full,
            ml$blvs_democ_best_system_full,
            ml$blvs_elec_good_full,
            dep.var.labels.include = T,
            dep.var.labels = c("Liberal Democratic", "Multiple Pol.", "Multiple Pol.",          "Can Join","Democracy", "Elections"),
            column.labels   = c("Values Index", "Parties Good", "Parties Create Choice",  "Any CSO", "Best System", "Good"),
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
                tryCatch(round(linearHypothesis(ml$lib_dem_val_index_full, "completed_near_china.plNA.30km.bin = planned_near_china.plNA.30km.bin")[2,4],3), error = function(e) print("NA")),
                tryCatch(round(linearHypothesis(ml$blvs_mult_parties_good_full, "completed_near_china.plNA.30km.bin = planned_near_china.plNA.30km.bin")[2,4],3), error = function(e) print("NA")),
                tryCatch(round(linearHypothesis(ml$blvs_mult_parties_create_choice_full, "completed_near_china.plNA.30km.bin = planned_near_china.plNA.30km.bin")[2,4],3), error = function(e) print("NA")),
                tryCatch(round(linearHypothesis(ml$blvs_ctzn_should_join_any_cso_full, "completed_near_china.plNA.30km.bin = planned_near_china.plNA.30km.bin")[2,4],3), error = function(e) print("NA")),
                tryCatch(round(linearHypothesis(ml$blvs_democ_best_system_full, "completed_near_china.plNA.30km.bin = planned_near_china.plNA.30km.bin")[2,4],3), error = function(e) print("NA")),
                tryCatch(round(linearHypothesis(ml$blvs_elec_good_full, "completed_near_china.plNA.30km.bin = planned_near_china.plNA.30km.bin")[2,4],3), error = function(e) print("NA")) ),
              c("Planned Year Cut Off","N/A","N/A","N/A","N/A","N/A","N/A"),
              c("Morans I P-Value",
                calc_morans_i(ml$lib_dem_val_index_full)$p.value %>% round(ROUND_NUM),
                calc_morans_i(ml$blvs_mult_parties_good_full)$p.value %>% round(ROUND_NUM),
                calc_morans_i(ml$blvs_mult_parties_create_choice_full)$p.value %>% round(ROUND_NUM),
                calc_morans_i(ml$blvs_ctzn_should_join_any_cso_full)$p.value %>% round(ROUND_NUM),
                calc_morans_i(ml$blvs_democ_best_system_full)$p.value %>% round(ROUND_NUM),
                calc_morans_i(ml$blvs_elec_good_full)$p.value %>% round(ROUND_NUM)
              ),
              c("Spatial Lag of Dep Var Included", splag, splag, splag, splag, splag, splag),
              c("Country Fixed Effects", "Y", "Y", "Y", "Y","Y", "Y"),
              c("Buffer",buffer,buffer,buffer,buffer, buffer, buffer)),
            out=file.path(tables_file_path, "table_02_full.tex"))
  
  rm(ml)
  gc(); gc()
  
  # Table 2 - Restricted ---------------------------------------------------------
  ml <- load_models(c("lib_dem_val_index_restricted",
                      "blvs_mult_parties_good_restricted",
                      "blvs_mult_parties_create_choice_restricted",
                      "blvs_ctzn_should_join_any_cso_restricted",
                      "blvs_democ_best_system_restricted",
                      "blvs_elec_good_restricted"))
  
  stargazer(ml$lib_dem_val_index_restricted,
            ml$blvs_mult_parties_good_restricted,
            ml$blvs_mult_parties_create_choice_restricted,
            ml$blvs_ctzn_should_join_any_cso_restricted,
            ml$blvs_democ_best_system_restricted,
            ml$blvs_elec_good_restricted,
            dep.var.labels.include = T,
            dep.var.labels = c("Liberal Democratic", "Multiple Pol.", "Multiple Pol.",          "Can Join","Democracy", "Elections"),
            column.labels   = c("Values Index", "Parties Good", "Parties Create Choice",  "Any CSO", "Best System", "Good"),
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
                tryCatch(round(linearHypothesis(ml$lib_dem_val_index_restricted, "completed_near_china.plNA.30km.bin = planned_near_china.plNA.30km.bin")[2,4],3), error = function(e) print("NA")),
                tryCatch(round(linearHypothesis(ml$blvs_mult_parties_good_restricted, "completed_near_china.plNA.30km.bin = planned_near_china.plNA.30km.bin")[2,4],3), error = function(e) print("NA")),
                tryCatch(round(linearHypothesis(ml$blvs_mult_parties_create_choice_restricted, "completed_near_china.plNA.30km.bin = planned_near_china.plNA.30km.bin")[2,4],3), error = function(e) print("NA")),
                tryCatch(round(linearHypothesis(ml$blvs_ctzn_should_join_any_cso_restricted, "completed_near_china.plNA.30km.bin = planned_near_china.plNA.30km.bin")[2,4],3), error = function(e) print("NA")),
                tryCatch(round(linearHypothesis(ml$blvs_democ_best_system_restricted, "completed_near_china.plNA.30km.bin = planned_near_china.plNA.30km.bin")[2,4],3), error = function(e) print("NA")),
                tryCatch(round(linearHypothesis(ml$blvs_elec_good_restricted, "completed_near_china.plNA.30km.bin = planned_near_china.plNA.30km.bin")[2,4],3), error = function(e) print("NA")) ),
              c("Completed vs. planned $p$-value [US]", 
                tryCatch(round(linearHypothesis(ml$lib_dem_val_index_restricted, "completed_near_usaid.30km.bin = planned_near_usaid.30km.bin")[2,4],3), error = function(e) print("NA")),
                tryCatch(round(linearHypothesis(ml$blvs_mult_parties_good_restricted, "completed_near_usaid.30km.bin = planned_near_usaid.30km.bin")[2,4],3), error = function(e) print("NA")),
                tryCatch(round(linearHypothesis(ml$blvs_mult_parties_create_choice_restricted, "completed_near_usaid.30km.bin = planned_near_usaid.30km.bin")[2,4],3), error = function(e) print("NA")),
                tryCatch(round(linearHypothesis(ml$blvs_ctzn_should_join_any_cso_restricted, "completed_near_usaid.30km.bin = planned_near_usaid.30km.bin")[2,4],3), error = function(e) print("NA")),
                tryCatch(round(linearHypothesis(ml$blvs_democ_best_system_restricted, "completed_near_usaid.30km.bin = planned_near_usaid.30km.bin")[2,4],3), error = function(e) print("NA")),
                tryCatch(round(linearHypothesis(ml$blvs_elec_good_restricted, "completed_near_usaid.30km.bin = planned_near_usaid.30km.bin")[2,4],3), error = function(e) print("NA")) ),
              c("Planned Year Cut Off","N/A","N/A","N/A","N/A","N/A","N/A"),
              c("Morans I P-Value",
                calc_morans_i(ml$lib_dem_val_index_restricted)$p.value %>% round(ROUND_NUM),
                calc_morans_i(ml$blvs_mult_parties_good_restricted)$p.value %>% round(ROUND_NUM),
                calc_morans_i(ml$blvs_mult_parties_create_choice_restricted)$p.value %>% round(ROUND_NUM),
                calc_morans_i(ml$blvs_ctzn_should_join_any_cso_restricted)$p.value %>% round(ROUND_NUM),
                calc_morans_i(ml$blvs_democ_best_system_restricted)$p.value %>% round(ROUND_NUM),
                calc_morans_i(ml$blvs_elec_good_restricted)$p.value %>% round(ROUND_NUM)
              ),
              c("Spatial Lag of Dep Var Included", splag, splag, splag, splag, splag, splag),
              c("Country Fixed Effects", "Y", "Y", "Y", "Y","Y", "Y"),
              c("Buffer",buffer,buffer,buffer,buffer, buffer,buffer)),
            out=file.path(tables_file_path, "table_02_restricted.tex"))
  
  rm(ml)
  gc(); gc()
  
  # Table 2 - Planned Eventually Completed: Full ---------------------------------
  # TODO: Fix!
  ml <- load_models(c("lib_dem_val_index_full_plcmpltd",
                      "blvs_mult_parties_good_full_plcmpltd",
                      "blvs_mult_parties_create_choice_full_plcmpltd",
                      "blvs_ctzn_should_join_any_cso_full_plcmpltd",
                      "blvs_democ_best_system_full_plcmpltd",
                      "blvs_elec_good_full_plcmpltd"))
  
  stargazer(ml$lib_dem_val_index_full_plcmpltd,
            ml$blvs_mult_parties_good_full_plcmpltd,
            ml$blvs_mult_parties_create_choice_full_plcmpltd,
            ml$blvs_ctzn_should_join_any_cso_full_plcmpltd,
            ml$blvs_democ_best_system_full_plcmpltd,
            ml$blvs_elec_good_full_plcmpltd,
            dep.var.labels.include = T,
            dep.var.labels = c("Liberal Democratic", "Multiple Pol.", "Multiple Pol.",          "Can Join","Democracy", "Elections"),
            column.labels   = c("Values Index", "Parties Good", "Parties Create Choice",  "Any CSO", "Best System", "Good"),
            keep=c("completed_near_china.plNA.30km.bin", "planned_near_china.plNAcmpltd.30km.bin"),
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
                tryCatch(round(linearHypothesis(ml$lib_dem_val_index_full_plcmpltd, "completed_near_china.plNA.30km.bin = planned_near_china.plNAcmpltd.30km.bin")[2,4],3), error = function(e) print("NA")),
                tryCatch(round(linearHypothesis(ml$blvs_mult_parties_good_full_plcmpltd, "completed_near_china.plNA.30km.bin = planned_near_china.plNAcmpltd.30km.bin")[2,4],3), error = function(e) print("NA")),
                tryCatch(round(linearHypothesis(ml$blvs_mult_parties_create_choice_full_plcmpltd, "completed_near_china.plNA.30km.bin = planned_near_china.plNAcmpltd.30km.bin")[2,4],3), error = function(e) print("NA")),
                tryCatch(round(linearHypothesis(ml$blvs_ctzn_should_join_any_cso_full_plcmpltd, "completed_near_china.plNA.30km.bin = planned_near_china.plNAcmpltd.30km.bin")[2,4],3), error = function(e) print("NA")),
                tryCatch(round(linearHypothesis(ml$blvs_democ_best_system_full_plcmpltd, "completed_near_china.plNA.30km.bin = planned_near_china.plNAcmpltd.30km.bin")[2,4],3), error = function(e) print("NA")),
                tryCatch(round(linearHypothesis(ml$blvs_elec_good_full_plcmpltd, "completed_near_china.plNA.30km.bin = planned_near_china.plNAcmpltd.30km.bin")[2,4],3), error = function(e) print("NA")) ),
              c("Planned Year Cut Off","N/A","N/A","N/A","N/A","N/A","N/A"),
              c("Morans I P-Value",
                calc_morans_i(ml$lib_dem_val_index_full_plcmpltd)$p.value %>% round(ROUND_NUM),
                calc_morans_i(ml$blvs_mult_parties_good_full_plcmpltd)$p.value %>% round(ROUND_NUM),
                calc_morans_i(ml$blvs_mult_parties_create_choice_full_plcmpltd)$p.value %>% round(ROUND_NUM),
                calc_morans_i(ml$blvs_ctzn_should_join_any_cso_full_plcmpltd)$p.value %>% round(ROUND_NUM),
                calc_morans_i(ml$blvs_democ_best_system_full_plcmpltd)$p.value %>% round(ROUND_NUM),
                calc_morans_i(ml$blvs_elec_good_full_plcmpltd)$p.value %>% round(ROUND_NUM)
              ),
              c("Spatial Lag of Dep Var Included", splag, splag, splag, splag, splag, splag),
              c("Country Fixed Effects", "Y", "Y", "Y", "Y","Y", "Y"),
              c("Buffer",buffer,buffer,buffer,buffer, buffer, buffer)),
            out=file.path(tables_file_path, "table_02_plannedCompleted_full.tex"))
  
  rm(ml)
  gc(); gc()
  
  # Table 2 - Planned Eventually Completed: Restricted ---------------------------
  ml <- load_models(c("lib_dem_val_index_restricted_plcmpltd",
                      "blvs_mult_parties_good_restricted_plcmpltd",
                      "blvs_mult_parties_create_choice_restricted_plcmpltd",
                      "blvs_ctzn_should_join_any_cso_restricted_plcmpltd",
                      "blvs_democ_best_system_restricted_plcmpltd",
                      "blvs_elec_good_restricted_plcmpltd"))
  
  stargazer(ml$lib_dem_val_index_restricted_plcmpltd,
            ml$blvs_mult_parties_good_restricted_plcmpltd,
            ml$blvs_mult_parties_create_choice_restricted_plcmpltd,
            ml$blvs_ctzn_should_join_any_cso_restricted_plcmpltd,
            ml$blvs_democ_best_system_restricted_plcmpltd,
            ml$blvs_elec_good_restricted_plcmpltd,
            dep.var.labels.include = T,
            dep.var.labels = c("Liberal Democratic", "Multiple Pol.", "Multiple Pol.",          "Can Join","Democracy", "Elections"),
            column.labels   = c("Values Index", "Parties Good", "Parties Create Choice",  "Any CSO", "Best System", "Good"),
            keep=c("completed_near_china.plNA.30km.bin", "planned_near_china.plNAcmpltd.30km.bin",
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
                tryCatch(round(linearHypothesis(ml$lib_dem_val_index_restricted_plcmpltd, "completed_near_china.plNA.30km.bin = planned_near_china.plNAcmpltd.30km.bin")[2,4],3), error = function(e) print("NA")),
                tryCatch(round(linearHypothesis(ml$blvs_mult_parties_good_restricted_plcmpltd, "completed_near_china.plNA.30km.bin = planned_near_china.plNAcmpltd.30km.bin")[2,4],3), error = function(e) print("NA")),
                tryCatch(round(linearHypothesis(ml$blvs_mult_parties_create_choice_restricted_plcmpltd, "completed_near_china.plNA.30km.bin = planned_near_china.plNAcmpltd.30km.bin")[2,4],3), error = function(e) print("NA")),
                tryCatch(round(linearHypothesis(ml$blvs_ctzn_should_join_any_cso_restricted_plcmpltd, "completed_near_china.plNA.30km.bin = planned_near_china.plNAcmpltd.30km.bin")[2,4],3), error = function(e) print("NA")),
                tryCatch(round(linearHypothesis(ml$blvs_democ_best_system_restricted_plcmpltd, "completed_near_china.plNA.30km.bin = planned_near_china.plNAcmpltd.30km.bin")[2,4],3), error = function(e) print("NA")),
                tryCatch(round(linearHypothesis(ml$blvs_elec_good_restricted_plcmpltd, "completed_near_china.plNA.30km.bin = planned_near_china.plNAcmpltd.30km.bin")[2,4],3), error = function(e) print("NA")) ),
              c("Completed vs. planned $p$-value [US]", 
                tryCatch(round(linearHypothesis(ml$lib_dem_val_index_restricted_plcmpltd, "completed_near_usaid.30km.bin = planned_near_usaid.30km.bin")[2,4],3), error = function(e) print("NA")),
                tryCatch(round(linearHypothesis(ml$blvs_mult_parties_good_restricted_plcmpltd, "completed_near_usaid.30km.bin = planned_near_usaid.30km.bin")[2,4],3), error = function(e) print("NA")),
                tryCatch(round(linearHypothesis(ml$blvs_mult_parties_create_choice_restricted_plcmpltd, "completed_near_usaid.30km.bin = planned_near_usaid.30km.bin")[2,4],3), error = function(e) print("NA")),
                tryCatch(round(linearHypothesis(ml$blvs_ctzn_should_join_any_cso_restricted_plcmpltd, "completed_near_usaid.30km.bin = planned_near_usaid.30km.bin")[2,4],3), error = function(e) print("NA")),
                tryCatch(round(linearHypothesis(ml$blvs_democ_best_system_restricted_plcmpltd, "completed_near_usaid.30km.bin = planned_near_usaid.30km.bin")[2,4],3), error = function(e) print("NA")),
                tryCatch(round(linearHypothesis(ml$blvs_elec_good_restricted_plcmpltd, "completed_near_usaid.30km.bin = planned_near_usaid.30km.bin")[2,4],3), error = function(e) print("NA")) ),
              c("Planned Year Cut Off","N/A","N/A","N/A","N/A","N/A","N/A"),
              c("Morans I P-Value",
                calc_morans_i(ml$lib_dem_val_index_restricted_plcmpltd)$p.value %>% round(ROUND_NUM),
                calc_morans_i(ml$blvs_mult_parties_good_restricted_plcmpltd)$p.value %>% round(ROUND_NUM),
                calc_morans_i(ml$blvs_mult_parties_create_choice_restricted_plcmpltd)$p.value %>% round(ROUND_NUM),
                calc_morans_i(ml$blvs_ctzn_should_join_any_cso_restricted_plcmpltd)$p.value %>% round(ROUND_NUM),
                calc_morans_i(ml$blvs_democ_best_system_restricted_plcmpltd)$p.value %>% round(ROUND_NUM),
                calc_morans_i(ml$blvs_elec_good_restricted_plcmpltd)$p.value %>% round(ROUND_NUM)
              ),
              c("Spatial Lag of Dep Var Included", splag, splag, splag, splag, splag, splag),
              c("Country Fixed Effects", "Y", "Y", "Y", "Y","Y", "Y"),
              c("Buffer",buffer,buffer,buffer,buffer, buffer,buffer)),
            out=file.path(tables_file_path, "table_02_plannedCompleted_restricted.tex"))
  
  rm(ml)
  gc(); gc()
  
  # Table 4 ----------------------------------------------------------------------
  ml <- load_models(c("lib_dem_val_index_restrictedusuk",
                      "blvs_mult_parties_good_restrictedusuk",
                      "blvs_mult_parties_create_choice_restrictedusuk",
                      "blvs_ctzn_should_join_any_cso_restrictedusuk",
                      "blvs_democ_best_system_restrictedusuk",
                      "blvs_elec_good_restrictedusuk"))
  
  stargazer(ml$lib_dem_val_index_restrictedusuk,
            ml$blvs_mult_parties_good_restrictedusuk,
            ml$blvs_mult_parties_create_choice_restrictedusuk,
            ml$blvs_ctzn_should_join_any_cso_restrictedusuk,
            ml$blvs_democ_best_system_restrictedusuk,
            ml$blvs_elec_good_restrictedusuk,
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
                tryCatch(round(linearHypothesis(ml$lib_dem_val_index_restrictedusuk, "completed_near_usaid.30km.bin = planned_near_usaid.30km.bin")[2,4],3), error = function(e) print("NA")),
                tryCatch(round(linearHypothesis(ml$blvs_mult_parties_good_restrictedusuk, "completed_near_usaid.30km.bin = planned_near_usaid.30km.bin")[2,4],3), error = function(e) print("NA")),
                tryCatch(round(linearHypothesis(ml$blvs_mult_parties_create_choice_restrictedusuk, "completed_near_usaid.30km.bin = planned_near_usaid.30km.bin")[2,4],3), error = function(e) print("NA")),
                tryCatch(round(linearHypothesis(ml$blvs_ctzn_should_join_any_cso_restrictedusuk, "completed_near_usaid.30km.bin = planned_near_usaid.30km.bin")[2,4],3), error = function(e) print("NA")),
                tryCatch(round(linearHypothesis(ml$blvs_democ_best_system_restrictedusuk, "completed_near_usaid.30km.bin = planned_near_usaid.30km.bin")[2,4],3), error = function(e) print("NA")),
                tryCatch(round(linearHypothesis(ml$blvs_elec_good_restrictedusuk, "completed_near_usaid.30km.bin = planned_near_usaid.30km.bin")[2,4],3), error = function(e) print("NA")) ),
              c("Completed vs. planned $p$-value [UK]", 
                tryCatch(round(linearHypothesis(ml$lib_dem_val_index_restrictedusuk, "completed_near_ukaid.30km.bin = planned_near_ukaid.30km.bin")[2,4],3), error = function(e) print("NA")),
                tryCatch(round(linearHypothesis(ml$blvs_mult_parties_good_restrictedusuk, "completed_near_ukaid.30km.bin = planned_near_ukaid.30km.bin")[2,4],3), error = function(e) print("NA")),
                tryCatch(round(linearHypothesis(ml$blvs_mult_parties_create_choice_restrictedusuk, "completed_near_ukaid.30km.bin = planned_near_ukaid.30km.bin")[2,4],3), error = function(e) print("NA")),
                tryCatch(round(linearHypothesis(ml$blvs_ctzn_should_join_any_cso_restrictedusuk, "completed_near_ukaid.30km.bin = planned_near_ukaid.30km.bin")[2,4],3), error = function(e) print("NA")),
                tryCatch(round(linearHypothesis(ml$blvs_democ_best_system_restrictedusuk, "completed_near_ukaid.30km.bin = planned_near_ukaid.30km.bin")[2,4],3), error = function(e) print("NA")),
                tryCatch(round(linearHypothesis(ml$blvs_elec_good_restrictedusuk, "completed_near_ukaid.30km.bin = planned_near_ukaid.30km.bin")[2,4],3), error = function(e) print("NA")) ),
              c("Planned Year Cut Off","N/A","N/A","N/A","N/A","N/A","N/A"),
              c("Morans I P-Value",
                calc_morans_i(ml$lib_dem_val_index_restrictedusuk)$p.value %>% round(ROUND_NUM),
                calc_morans_i(ml$blvs_mult_parties_good_restrictedusuk)$p.value %>% round(ROUND_NUM),
                calc_morans_i(ml$blvs_mult_parties_create_choice_restrictedusuk)$p.value %>% round(ROUND_NUM),
                calc_morans_i(ml$blvs_ctzn_should_join_any_cso_restrictedusuk)$p.value %>% round(ROUND_NUM),
                calc_morans_i(ml$blvs_democ_best_system_restrictedusuk)$p.value %>% round(ROUND_NUM),
                calc_morans_i(ml$blvs_elec_good_restrictedusuk)$p.value %>% round(ROUND_NUM)
              ),
              c("Spatial Lag of Dep Var Included", splag, splag, splag, splag, splag, splag),
              c("Country Fixed Effects", "Y", "Y", "Y","Y", "Y", "Y"),
              c("Buffer",buffer,buffer,buffer,buffer, buffer)),
            out=file.path(tables_file_path, "table_04_restricted.tex"))
  
  rm(ml)
  gc(); gc()
  
  # Table 4 ----------------------------------------------------------------------
  
  
  
}
