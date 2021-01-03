# Foreign Aid and State Legitimacy in Africa: Cross-National and Sub-National 
# Evidence from Surveys, Survey Experiments, and Behavioral Games

# Table A.13: Comparison of completed and planned Chinese projects by sector

# Load Data --------------------------------------------------------------------
chinese_aid <- readRDS(file.path(data_file_path, "chinese_aid_projects.Rds"))

# Make Table -------------------------------------------------------------------
# For completed and planned projects, determine N and prop by sector

chinese_aid_completed <- chinese_aid %>%
  filter(completed_near_china.pl10 %in% 1,
         sample %in% 1) %>%
  group_by(sector) %>%
  dplyr::summarise(N = n()) %>%
  mutate(prop = N / sum(N)) %>%
  dplyr::rename(n_completed = N,
                prop_completed = prop) 

chinese_aid_sub_planned <- chinese_aid %>%
  filter(planned_near_china.pl10 %in% 1,
         sample %in% 1) %>%
  group_by(sector) %>%
  dplyr::summarise(N = n()) %>%
  mutate(prop = N / sum(N)) %>%
  dplyr::rename(n_planned = N,
                prop_planned = prop) 

table_df <- merge(chinese_aid_completed,
                  chinese_aid_sub_planned, 
                  by = "sector",
                  all = T) %>%
  replace(., is.na(.), 0)

table_df$tex <- paste(table_df$sector, " & ",
                      table_df$n_completed, " & ",
                      table_df$n_planned, " & ",
                      table_df$prop_completed %>% round(3), " & ",
                      table_df$prop_planned %>% round(3), " \\\\ \n ")


# Make Table -------------------------------------------------------------------
sink(file.path(tables_file_path, "table_a6.tex"))

cat(" \\begin{tabular}{l cc cc} ")
cat(" & \\multicolumn{2}{c}{N} & \\multicolumn{2}{c}{Proportion} \\\\ ")
cat(" \\hline ")
cat(" & Completed & Planned & Completed & Planned \\\\")
cat(" \\hline ")
for(i in 1:nrow(table_df)){
  
  cat(table_df$tex[i])
  
}
cat(" \\hline ")

cat("Total & ",
    sum(table_df$n_completed), " & ",
    sum(table_df$n_planned), " & ",
    "1 & 1 \\\\")

cat(" \\hline ")
cat(" \\end{tabular} ")

sink()

