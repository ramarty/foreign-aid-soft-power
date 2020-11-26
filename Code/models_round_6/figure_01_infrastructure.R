# Figure 2 - Componenets

# Load Data --------------------------------------------------------------------
df <- readRDS(file.path(data_file_path, "afro_china_data.Rds"))

# Regressions ------------------------------------------------------------------
influential_index.full.lm <- felm(as.formula(paste0("china_influential_index ~               completed_near_china.pl10.30km.construct.bin + planned_near_china.pl10.30km.construct.bin + completed_near_china.pl10.30km.noconstruct.bin + planned_near_china.pl10.30km.noconstruct.bin + ",              IVs_china," | ",FEs," | 0 | ", CLUSTER_VAR)), data=df[df$sample_full %in% T,]) 
positive_influence_index.full.lm <- felm(as.formula(paste0("china_positive_influence_index ~ completed_near_china.pl10.30km.construct.bin + planned_near_china.pl10.30km.construct.bin + completed_near_china.pl10.30km.noconstruct.bin + planned_near_china.pl10.30km.noconstruct.bin + ",IVs_china," | ",FEs," | 0 | ", CLUSTER_VAR)), data=df[df$sample_full %in% T,]) 
china.most.influence.full.lm <- felm(as.formula(paste0("china.most.influence ~               completed_near_china.pl10.30km.construct.bin + planned_near_china.pl10.30km.construct.bin + completed_near_china.pl10.30km.noconstruct.bin + planned_near_china.pl10.30km.noconstruct.bin + ",              IVs_china," | ",FEs," | 0 | ", CLUSTER_VAR)), data=df[df$sample_full %in% T,]) 
usa.most.influence.full.lm <- felm(as.formula(paste0("usa.most.influence ~                   completed_near_china.pl10.30km.construct.bin + planned_near_china.pl10.30km.construct.bin + completed_near_china.pl10.30km.noconstruct.bin + planned_near_china.pl10.30km.noconstruct.bin + ",IVs_china," | ",FEs," | 0 | ", CLUSTER_VAR)), data=df[df$sample_full %in% T,]) 
china.best.dev.model.full.lm <- felm(as.formula(paste0("china.best.dev.model ~               completed_near_china.pl10.30km.construct.bin + planned_near_china.pl10.30km.construct.bin + completed_near_china.pl10.30km.noconstruct.bin + planned_near_china.pl10.30km.noconstruct.bin + ",              IVs_china," | ",FEs," | 0 | ", CLUSTER_VAR)), data=df[df$sample_full %in% T,]) 
usa.best.dev.model.full.lm <- felm(as.formula(paste0("usa.best.dev.model ~                   completed_near_china.pl10.30km.construct.bin + planned_near_china.pl10.30km.construct.bin + completed_near_china.pl10.30km.noconstruct.bin + planned_near_china.pl10.30km.noconstruct.bin + ",IVs_china," | ",FEs," | 0 | ", CLUSTER_VAR)), data=df[df$sample_full %in% T,]) 

# Full Table -------------------------------------------------------------------
buffer <- 30
stargazer(influential_index.full.lm,
          positive_influence_index.full.lm,
          china.most.influence.full.lm,
          usa.most.influence.full.lm,
          china.best.dev.model.full.lm,
          usa.best.dev.model.full.lm,
          dep.var.labels.include = T,
          dep.var.labels = c("China Influential ","China Positive", "China Most","US Most","China Best","US Best"),
          column.labels   = c("[Index]", "[Index]", "Influence", "Influence",  "Model", "Model"),
          keep=c("completed_near_china.pl10.30km.construct.bin",   "planned_near_china.pl10.30km.construct.bin",
                 "completed_near_china.pl10.30km.noconstruct.bin", "planned_near_china.pl10.30km.noconstruct.bin"),
          covariate.labels = c("Near completed Chinese infrastructure project","Near planned Chinese infrastructure project",
                               "Near completed Chinese other project","Near planned Chinese other project"),
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
              tryCatch(round(linearHypothesis(influential_index.full.lm, "completed_near_china.pl10.30km.construct.bin = planned_near_china.pl10.30km.construct.bin")[2,4],3), error = function(e) print("NA")),
              tryCatch(round(linearHypothesis(positive_influence_index.full.lm, "completed_near_china.pl10.30km.construct.bin = planned_near_china.pl10.30km.construct.bin")[2,4],3), error = function(e) print("NA")),
              tryCatch(round(linearHypothesis(china.most.influence.full.lm, "completed_near_china.pl10.30km.construct.bin = planned_near_china.pl10.30km.construct.bin")[2,4],3), error = function(e) print("NA")),
              tryCatch(round(linearHypothesis(usa.most.influence.full.lm, "completed_near_china.pl10.30km.construct.bin = planned_near_china.pl10.30km.construct.bin")[2,4],3), error = function(e) print("NA")),
              tryCatch(round(linearHypothesis(china.best.dev.model.full.lm, "completed_near_china.pl10.30km.construct.bin = planned_near_china.pl10.30km.construct.bin")[2,4],3), error = function(e) print("NA")),
              tryCatch(round(linearHypothesis(usa.best.dev.model.full.lm, "completed_near_china.pl10.30km.construct.bin = planned_near_china.pl10.30km.construct.bin")[2,4],3), error = function(e) print("NA")) ),
            c("Completed vs. planned other $p$-value", 
              tryCatch(round(linearHypothesis(influential_index.full.lm, "completed_near_china.pl10.30km.noconstruct.bin = planned_near_china.pl10.30km.noconstruct.bin")[2,4],3), error = function(e) print("NA")),
              tryCatch(round(linearHypothesis(positive_influence_index.full.lm, "completed_near_china.pl10.30km.noconstruct.bin = planned_near_china.pl10.30km.noconstruct.bin")[2,4],3), error = function(e) print("NA")),
              tryCatch(round(linearHypothesis(china.most.influence.full.lm, "completed_near_china.pl10.30km.noconstruct.bin = planned_near_china.pl10.30km.noconstruct.bin")[2,4],3), error = function(e) print("NA")),
              tryCatch(round(linearHypothesis(usa.most.influence.full.lm, "completed_near_china.pl10.30km.noconstruct.bin = planned_near_china.pl10.30km.noconstruct.bin")[2,4],3), error = function(e) print("NA")),
              tryCatch(round(linearHypothesis(china.best.dev.model.full.lm, "completed_near_china.pl10.30km.noconstruct.bin = planned_near_china.pl10.30km.noconstruct.bin")[2,4],3), error = function(e) print("NA")),
              tryCatch(round(linearHypothesis(usa.best.dev.model.full.lm, "completed_near_china.pl10.30km.noconstruct.bin = planned_near_china.pl10.30km.noconstruct.bin")[2,4],3), error = function(e) print("NA")) ),
            #c("Planned Year Cut Off",planned_cutoff_year,planned_cutoff_year,planned_cutoff_year,planned_cutoff_year),
            c("Morans I P-Value",
              calc_morans_i(influential_index.full.lm)$p.value %>% round(ROUND_NUM),
              calc_morans_i(positive_influence_index.full.lm)$p.value %>% round(ROUND_NUM),
              calc_morans_i(china.most.influence.full.lm)$p.value %>% round(ROUND_NUM),
              calc_morans_i(usa.most.influence.full.lm)$p.value %>% round(ROUND_NUM),
              calc_morans_i(china.best.dev.model.full.lm)$p.value %>% round(ROUND_NUM),
              calc_morans_i(usa.best.dev.model.full.lm)$p.value %>% round(ROUND_NUM)
            ),
            c("Spatial Lag of Dep Var Included", "N", "N", "N", "N", "N", "N"),
            c("Country Fixed Effects", "Y", "Y", "Y","Y", "Y", "Y"),
            c("Buffer",buffer,buffer,buffer,buffer, buffer, buffer)),
          out=file.path(tables_file_path, "table_infrastructure.tex"))

