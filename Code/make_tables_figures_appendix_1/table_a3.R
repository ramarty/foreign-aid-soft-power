# Descriptive Stats 

# Load Data --------------------------------------------------------------------
df <- readRDS(file.path(data_file_path, "afro_china_data.Rds"))
df <- df[df$afro.round != 1,]

# Subset Data ------------------------------------------------------------------
# Subset to observations in regressions
df <- df %>%
  filter(!is.na(age),
         !is.na(muslim),
         !is.na(urban),
         !is.na(male),
         !is.na(distance_capital),
         !is.na(in_leader_adm1)) %>%
  filter(sample_full %in% T)

sum_stat <- function(var, name, df){
  
  rounds_with_var <- df$afro.round[!is.na(df[[var]])]
  
  latex <- paste(
    name, " & ",
    length(rounds_with_var) %>% prettyNum(big.mark=",",scientific=FALSE), " & ",
    mean(df[[var]], na.rm=T) %>% round(2), " & ",
    sd(df[[var]], na.rm=T) %>% round(2), " & ",
    min(df[[var]], na.rm=T) %>% round(2), " & ",
    max(df[[var]], na.rm=T) %>% round(2), " & ",
    min(rounds_with_var), " & ",
    max(rounds_with_var), " \\\\ "
  )
  
  return(cat(latex))
}

sink(file.path(tables_file_path, "table_a3.tex"))

cat(" \\begin{tabular}{l ccccccc} \n")
cat(" &   &      &      &     &     & First     & Last     \\\\ \n ")
cat(" &   &      &      &     &     & round     & round    \\\\ \n ")
cat(" & N & Mean & S.D. & Min & Max & available & available \\\\ \n ")
cat("\\hline ")

#cat(" & & & & & & & \\\\ \n ")
#cat(" \\multicolumn{8}{l}{\\bf Liberal democractic values} \\\\ \n ")
sum_stat("age",     "Age", df)
sum_stat("muslim",     "Muslim", df)
sum_stat("urban",     "Urban", df)
sum_stat("male",     "Male", df)
sum_stat("distance_capital",     "Distance to capital (km)", df)
sum_stat("in_leader_adm1",     "Lives in leader's home region", df)
sum_stat("riotsprotests_china_before1stplanned_30km_average", "Avg. annual riots \\& protests", df)

cat("\\hline ")
cat(" \\end{tabular}")
sink()


