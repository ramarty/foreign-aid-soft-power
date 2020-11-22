# Foreign Aid and State Legitimacy in Africa: Cross-National and Sub-National 
# Evidence from Surveys, Survey Experiments, and Behavioral Games

# Table A.12: Comparison of US projects in full sample to US projects in our 
# sample by sector

# Load Data --------------------------------------------------------------------
us_aid <- readRDS(file.path(data_file_path, "us_aid_projects.Rds"))

# Make Table -------------------------------------------------------------------
# For full and sub-sample, determine N and prop by sector

us_aid_sum <- us_aid %>%
  group_by(sector) %>%
  dplyr::summarise(N = n()) %>%
  mutate(prop = N / sum(N)) %>%
  dplyr::rename(n_all = N,
                prop_all = prop) 

us_aid_sub_sum <- us_aid %>%
  filter(sample == 1) %>%
  group_by(sector) %>%
  dplyr::summarise(N = n()) %>%
  mutate(prop = N / sum(N)) %>%
  dplyr::rename(n_sample = N,
                prop_sample = prop) 

table_df <- merge(us_aid_sum,
                  us_aid_sub_sum, 
                  by = "sector", 
                  all=T) %>%
  replace(., is.na(.), 0)

table_df$tex <- paste(table_df$sector, " & ",
                      table_df$n_all, " & ",
                      table_df$n_sample, " & ",
                      table_df$prop_all %>% round(3), " & ",
                      table_df$prop_sample %>% round(3), " \\\\ \n ")

# Make Table -------------------------------------------------------------------
sink(file.path(tables_file_path, "table_a2.tex"))

cat(" \\begin{tabular}{l cc cc} ")
cat(" & \\multicolumn{2}{c}{N} & \\multicolumn{2}{c}{Proportion} \\\\ ")
cat(" \\hline ")
cat(" & All & Sample & All & Sample \\\\")
cat(" \\hline ")
for(i in 1:nrow(table_df)){
  
  cat(table_df$tex[i])
  
}
cat(" \\hline ")

cat("Total & ",
    sum(table_df$n_all), " & ",
    sum(table_df$n_sample), " & ",
    "1 & 1 \\\\")

cat(" \\hline ")
cat(" \\end{tabular} ")

sink()

