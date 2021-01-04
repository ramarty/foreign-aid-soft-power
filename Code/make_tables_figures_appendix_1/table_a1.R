# Descriptive Stats 

# Load Data --------------------------------------------------------------------
df <- readRDS(file.path(data_file_path, "afro_china_data.Rds"))

# Subset Data ------------------------------------------------------------------
# Subset to observations in regressions
df <- df %>%
  filter(!is.na(age),
         !is.na(muslim),
         !is.na(urban),
         !is.na(male),
         !is.na(distance_capital),
         !is.na(in_leader_adm1)) 

sum_stat <- function(var, name, df){
  
  rounds_with_var <- df$afro.round[!is.na(df[[var]])]
  
  latex <- paste(
    name, " & ",
    length(rounds_with_var) %>% prettyNum(big.mark=",",scientific=FALSE), " & ",
    mean(df[[var]], na.rm=T) %>% round(2), " \\\\ "
    #sd(df[[var]], na.rm=T) %>% round(2), " & ",
    #min(df[[var]], na.rm=T), " & ",
    #max(df[[var]], na.rm=T), " & ",
    #min(rounds_with_var), " & ",
    #max(rounds_with_var), " \\\\ "
  )
  
  return(cat(latex))
}


sink(file.path(tables_file_path, "table_a1.tex"))

cat(" \\begin{tabular}{l cc} \n")
#cat(" &   &      &      &     &     & First     & Last     \\\\ \n ")
#cat(" &   &      &      &     &     & round     & round    \\\\ \n ")
#cat(" & N & Mean & S.D. & Min & Max & available & available \\\\ \n ")
cat(" & N & Mean \\\\ \n ")
cat("\\hline ")

cat(" {\\bf Full Sample} & & \\\\ \n ")
sum_stat("planned_near_china.pl10.30km.bin",   "\\% of respondents within 30km of a planned Chinese project",   df[df$sample_full %in% T,])
sum_stat("completed_near_china.pl10.30km.bin", "\\% of respondents within 30km of a completed Chinese project", df[df$sample_full %in% T,])

cat("  & & \\\\ \n ")
cat(" {\\bf Restricted Sample} & & \\\\ \n ")
sum_stat("planned_near_china.pl10.30km.bin",   "\\% of respondents within 30km of a planned Chinese project",   df[df$sample_restricted %in% T,])
sum_stat("completed_near_china.pl10.30km.bin", "\\% of respondents within 30km of a completed Chinese project", df[df$sample_restricted %in% T,])

sum_stat("planned_near_usaid.30km.bin",   "\\% of respondents within 30km of a planned US project",   df[df$sample_restricted %in% T,])
sum_stat("completed_near_usaid.30km.bin", "\\% of respondents within 30km of a completed US project", df[df$sample_restricted %in% T,])

sum_stat("planned_near_ukaid.30km.bin",   "\\% of respondents within 30km of a planned UK project",   df[df$sample_restricted %in% T,])
sum_stat("completed_near_ukaid.30km.bin", "\\% of respondents within 30km of a completed UK project", df[df$sample_restricted %in% T,])

cat("\\hline ")
cat(" \\end{tabular}")
sink()


