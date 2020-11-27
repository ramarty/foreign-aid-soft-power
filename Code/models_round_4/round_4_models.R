# Figure 2 - Componenets

# Load Data --------------------------------------------------------------------
df <- readRDS(file.path(data_file_path, "afro_china_data.Rds"))

df <- df %>%
  filter(afro.round %in% 4)

# Regressions ------------------------------------------------------------------
china.help.country.lm <- felm(as.formula(paste0("china.help.country ~ china.help.country_splag + completed_near_china.plNA.30km.bin + planned_near_china.plNA.30km.bin  + ",IVs_china," | ",FEs," | 0 | ", CLUSTER_VAR)), data=df[df$sample_full %in% T,]) 
usa.help.country.lm <- felm(as.formula(paste0("usa.help.country ~ usa.help.country_splag +    completed_near_china.plNA.30km.bin + planned_near_china.plNA.30km.bin  + ", IVs_china," | ",FEs," | 0 | ", CLUSTER_VAR)), data=df[df$sample_full %in% T,]) 

# Full Table -------------------------------------------------------------------
buffer <- 30
planned_cutoff_year <- "N/A"

stargazer(china.help.country.lm,
          usa.help.country.lm,
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
              tryCatch(round(linearHypothesis(china.help.country.lm, "completed_near_china.plNA.30km.bin = planned_near_china.plNA.30km.bin")[2,4],3), error = function(e) print("NA")),
              tryCatch(round(linearHypothesis(usa.help.country.lm, "completed_near_china.plNA.30km.bin = planned_near_china.plNA.30km.bin")[2,4],3), error = function(e) print("NA")) ),
            c("Planned Year Cut Off",planned_cutoff_year,planned_cutoff_year),
            c("Morans I P-Value",
              calc_morans_i(china.help.country.lm)$p.value %>% round(ROUND_NUM),
              calc_morans_i(usa.help.country.lm)$p.value %>% round(ROUND_NUM)
            ),
            c("Spatial Lag of Dep Var Included", "Y", "Y"),
            c("Country Fixed Effects", "Y", "Y"),
            c("Buffer",buffer,buffer)),
          out=file.path(tables_file_path, "round4_table.tex"))

