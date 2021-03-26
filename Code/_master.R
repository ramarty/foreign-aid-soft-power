# Foreign Aid and Soft Power: 
# Great Power Competition in Africa in the Early 21st Century

# Mater R Script 

## Run scripts parameters
ESTIMATE_MODELS <- F
RUN_TABLES_FIGURES <- T

# Filepaths --------------------------------------------------------------------
github_file_path <- "~/Documents/Github/foreign-aid-soft-power"

code_file_path    <- file.path(github_file_path, "code")
data_results_file_path <- file.path(github_file_path, "results_data")
data_file_path    <- file.path(github_file_path, "data") 
tables_file_path  <- file.path(github_file_path, "output", "tables")
figures_file_path <- file.path(github_file_path, "output", "figures")

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
library(tidyr)
library(devtools)
library(moranfast) #install_github('mcooper/moranfast')
library(purrr)
library(haven)
library(ggpubr)
library(rworldmap)
library(rgeos)
library(raster)
library(gt)
library(paletteer)

source(file.path(code_file_path, "functions", "extract_coefs_from_felm.R"))
source(file.path(code_file_path, "functions", "make_figures_all_buffers.R"))
source(file.path(code_file_path, "functions", "make_figures_one_buffer.R"))
source(file.path(code_file_path, "functions", "run_models.R"))

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
FEs <- "iso + afro.round" 

# Cluster variables
CLUSTER_VAR <- "location_id" 

# Figure Colors
figure_colors_bw <- c("black", "black","gray65","gray65") 
figure_colors_c <- c("firebrick2", "darkorange","dodgerblue4","green3") 

# Run Scripts - Estimate Models ------------------------------------------------
if(ESTIMATE_MODELS){
  source(file.path(code_file_path, "estimate_models.R"))
}

# Run Scripts - Results Tables/Figures -----------------------------------------
if(RUN_TABLES_FIGURES){
  source(file.path(code_file_path, "make_figures_body_of_paper.R"))
  
  source(file.path(code_file_path, "make_tables_figures_appendix_1", "figure_a1.R"))
  source(file.path(code_file_path, "make_tables_figures_appendix_1", "figure_a2.R"))
  source(file.path(code_file_path, "make_tables_figures_appendix_1", "figure_a3.R"))
  source(file.path(code_file_path, "make_tables_figures_appendix_1", "table_a1.R"))
  source(file.path(code_file_path, "make_tables_figures_appendix_1", "table_a2.R"))
  source(file.path(code_file_path, "make_tables_figures_appendix_1", "table_a3.R"))
  source(file.path(code_file_path, "make_tables_figures_appendix_1", "table_a4.R"))
  source(file.path(code_file_path, "make_tables_figures_appendix_1", "table_a5.R"))
  source(file.path(code_file_path, "make_tables_figures_appendix_1", "table_a6.R"))
  source(file.path(code_file_path, "make_tables_figures_appendix_1", "table_a7.R"))
  source(file.path(code_file_path, "make_tables_figures_appendix_1", "table_a20.R"))
  
  source(file.path(code_file_path, "make_figures_appendix_2.R"))
  source(file.path(code_file_path, "make_tables_appendix_2.R"))
  
  source(file.path(code_file_path, "make_figures_appendix_3.R"))
  
  source(file.path(code_file_path, "make_tables_appendix_4.R"))
}

