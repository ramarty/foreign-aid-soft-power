# Foreign Aid and Soft Power: 
# Great Power Competition in Africa in the Early 21st Century

# Mater R Script 

# Filepaths --------------------------------------------------------------------
project_file_path <- "~/Dropbox/China in Africa/Paper 2/paper_2_replication_package" 

code_file_path    <- file.path(project_file_path, "Code")
data_file_path    <- file.path(project_file_path, "Data")
outputs_file_path <- file.path(project_file_path, "Outputs")
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

source(file.path(code_file_path, "_functions.R"))

# Parameters -------------------------------------------------------------------
# Define parameters that are used in multiple scripts

# Vector of iso codes for countries where have both US and Chinese aid data and
# Afrobarometer data
iso_usaid <- c("MWI","NIG","UGA","SEN","BDI","SRL")

IVs_china <- c("age", "muslim", "urban", "male",
               "distance_capital", "in_leader_adm1", "riotsprotests_china_before1stplanned_30km_average") %>%
  paste(collapse = " + ")

IVs_china_usaid <- c("age", "muslim", "urban", "male",
                     "distance_capital", "in_leader_adm1", "riotsprotests_china_usaid_before1stplanned_30km_average") %>%
  paste(collapse = " + ")

# Run Scripts ------------------------------------------------------------------
if(F){
  source(file.path(code_file_path, "figure_01.R"))
}
