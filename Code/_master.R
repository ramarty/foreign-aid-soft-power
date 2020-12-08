# Foreign Aid and Soft Power: 
# Great Power Competition in Africa in the Early 21st Century

# Mater R Script 

RUN_SCRIPTS <- F
save_model <- F

# Filepaths --------------------------------------------------------------------
dropbox_file_path <- "~/Dropbox/China in Africa/Paper 2/paper_2_replication_package" 
github_file_path <- "~/Documents/Github/foreign-aid-soft-power"

code_file_path    <- file.path(github_file_path, "Code")
data_file_path    <- file.path(dropbox_file_path, "Data")
results_file_path    <- file.path(dropbox_file_path, "Data_Results")
outputs_file_path <- file.path(dropbox_file_path, "Outputs")
tables_file_path  <- file.path(outputs_file_path, "tables")
figures_file_path <- file.path(outputs_file_path, "figures")

# Packages ---------------------------------------------------------------------
library(dplyr)
library(stargazer)
library(lfe)
library(car)
library(broom)
library(stringr)
library(ggplot2)
library(ggthemes)
library(hrbrthemes)
library(tidyselect)
library(labelled)
#library(devtools)
#install_github('mcooper/moranfast')
library(moranfast)
library(purrr)
library(haven)
library(ggpubr)

source(file.path(code_file_path, "_functions.R"))
source(file.path(code_file_path, "_functions_run_models.R"))

# Parameters -------------------------------------------------------------------

# Vector of iso codes for countries where have both US and Chinese aid data and
# Afrobarometer data
iso_usaid <- c("MWI","NIG","UGA","SEN","BDI","SRL")

# Independent variables
IVs_china <- c("age", "muslim", "urban", "male",
               "distance_capital", "in_leader_adm1", "riotsprotests_china_before1stplanned_30km_average") %>%
  paste(collapse = " + ")

IVs_china_usaid <- c("age", "muslim", "urban", "male",
                     "distance_capital", "in_leader_adm1", "riotsprotests_china_usaid_before1stplanned_30km_average") %>%
  paste(collapse = " + ")

# Fixed effects
# !!!! In 01_clean_data_for_replication, removed end changing location_id
FEs <- "NAME_1 + afro.round" # "iso + afro.round",  uniqueea_afroround 

# Cluster variables
CLUSTER_VAR <- "location_id" # "location_id" # townvill uniqueea_afroround

# Figure Colors
#figure_colors <- c("darkorange3", "darkorange","dodgerblue4","dodgerblue2")
figure_colors <- c("firebrick2", "darkorange","dodgerblue4","green4") # mediumseagreen

# Run Scripts ------------------------------------------------------------------
if(RUN_SCRIPTS){
  
  ## Summary Stats
  sum_stat_file_path <- file.path(code_file_path, "summary_statistics")
  source(file.path(sum_stat_file_path, "table_a1.R"))
  source(file.path(sum_stat_file_path, "table_a2.R"))
  source(file.path(sum_stat_file_path, "table_a3.R"))
  source(file.path(sum_stat_file_path, "table_a4.R"))
  source(file.path(sum_stat_file_path, "table_a9.R"))
  source(file.path(sum_stat_file_path, "indep_var_sum_stat.R"))
  
  ## Models: Rounds 6
  models_r6_file_path <- file.path(code_file_path, "models_round_6")
  source(file.path(models_r6_file_path, "figure_01.R"))
  source(file.path(models_r6_file_path, "figure_01_2009.R"))
  source(file.path(models_r6_file_path, "figure_01_2008.R"))
  source(file.path(models_r6_file_path, "figure_01_infrastructure.R"))
  source(file.path(models_r6_file_path, "figure_03.R"))
  source(file.path(models_r6_file_path, "figure_05.R"))
  source(file.path(models_r6_file_path, "figure_06.R"))
  source(file.path(models_r6_file_path, "figure_07.R"))

  ## Models: Round 2-5
  models_r2_5_file_path <- file.path(code_file_path, "models_rounds_2-5")
  source(file.path(models_r2_5_file_path, "figure_02.R"))
  source(file.path(models_r2_5_file_path, "figure_04.R"))
  source(file.path(models_r2_5_file_path, "figure_02_plannedCompleted.R"))
  
  ## Models: Round 4
  models_r4_file_path <- file.path(code_file_path, "models_round_4")
  source(file.path(models_r4_file_path, "round_4_models.R"))
  
  ## Buffer Sensitivity Analysis
  buffer_sensitivity_file_path <- file.path(code_file_path, "buffer_sensitivity")
  source(file.path(buffer_sensitivity_file_path, "buffer_sensitivity.R"))
  
}





