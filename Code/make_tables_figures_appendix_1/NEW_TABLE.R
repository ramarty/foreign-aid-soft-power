# Foreign Aid and State Legitimacy in Africa: Cross-National and Sub-National 
# Evidence from Surveys, Survey Experiments, and Behavioral Games

# Table A.14: Magnitude of Impacts Table

fig1_vars <- c("china_positive_influence_index",
               "china.best.dev.model",
               "usa.best.dev.model")

fig2_vars <- c("lib_dem_val_index")

fig3_vars <- c("formcolnpower.best.dev.model")

fig4_vars <- c("posimage_businessinvetment",
               "posimage_chinesepeople",
               "posimage_infordevinvetment",
               "posimage_noninterference",
               "posimage_productcost",
               "posimage_supportinintlaffiars")

fig5_vars <- c("negimage_chinesecitizenbehavior",
               "negimage_cooperateundemocratic",
               "negimage_landgrabbing",
               "negimage_productquality",
               "negimage_resourceextraction",
               "negimage_takingjobsbusiness")

# Load/Prep Coefficient Results ------------------------------------------------
df_results <- readRDS(file.path(data_results_file_path, 
                                paste0("models_type_", 1), 
                                "coefficients",
                                "coefficients.Rds"))

df_results <- df_results %>%
  filter(buffer == 30) %>%
  filter(is.na(planned_year) | planned_year %in% 2010) %>%
  filter(is.na(infrastructure))

df_results <- df_results %>%
  mutate(completed_planned =
           case_when(grepl("Completed",var) ~ "completed",
                     grepl("Planned",var) ~ "planned"),
         country = var %>% 
           str_replace_all("Completed|Planned", "") %>% 
           str_replace_all(" Aid", "") %>% 
           str_squish() %>%
           tolower()) %>%
  dplyr::select(-var) %>%
  pivot_wider(names_from = completed_planned,
              values_from = c(coef, ci2_5, ci97_5))

df_results <- df_results %>%
  filter(subset %in% c("full", "restricted"),
         dv %in% c(fig1_vars,
                   fig2_vars,
                   fig3_vars,
                   fig4_vars,
                   fig5_vars))

# Average DVs ==================================================================
# 1. Load data and subset to complete observations (with covariates)
# 2. Take average across different aid groups
# -- 2.1 All data
# -- 2.2 Near Planned: usaid
# -- 2.3 Near Planned: china.pl10
# -- 2.4 Near Planned: china.plNA
# 3. Merge and append

# 1. Load ----------------------------------------------------------------------
df <- readRDS(file.path(data_file_path, "afro_china_data.Rds"))

df <- df %>%
  filter(afro.round != 1) %>%
  filter(!is.na(age),
         !is.na(muslim),
         !is.na(urban),
         !is.na(male),
         !is.na(distance_capital),
         !is.na(in_leader_adm1))

# 2 Take average across different aid groups -----------------------------------
# 2.1 All data -----------------------------------------------------------------
df_full <- df %>% 
  filter(sample_full %in% T) %>%
  dplyr::summarise_all(mean, na.rm = T) %>%
  mutate(subset = "full")

df_restricted <- df %>% 
  filter(sample_restricted %in% T) %>%
  dplyr::summarise_all(mean, na.rm = T) %>%
  mutate(subset = "restricted")

dvs_means_all_df <- bind_rows(df_full,
                              df_restricted) %>%
  pivot_longer(cols = -subset) %>%
  #mutate(aid_subset = "all") %>%
  filter(name %in% c(fig1_vars,
                     fig2_vars,
                     fig3_vars,
                     fig4_vars,
                     fig5_vars))

# 2.2 Near Planned: usaid ------------------------------------------------------
df_full <- df %>% 
  filter(sample_full %in% T,
         planned_near_usaid.30km.bin %in% T) %>%
  dplyr::summarise_all(mean, na.rm = T) %>%
  mutate(subset = "full")

df_restricted <- df %>% 
  filter(sample_restricted %in% T,
         planned_near_usaid.30km.bin %in% T) %>%
  dplyr::summarise_all(mean, na.rm = T) %>%
  mutate(subset = "restricted")

dvs_means_usaid_df <- bind_rows(df_full,
                                df_restricted) %>%
  pivot_longer(cols = -subset) %>%
  #mutate(aid_subset = "usaid") %>%
  filter(name %in% c(fig1_vars,
                     fig2_vars,
                     fig3_vars,
                     fig4_vars,
                     fig5_vars))

# 2.3 Near Planned: china.pl10 -------------------------------------------------
df_full <- df %>% 
  filter(sample_full %in% T,
         planned_near_china.pl10.30km.bin %in% T) %>%
  dplyr::summarise_all(mean, na.rm = T) %>%
  mutate(subset = "full")

df_restricted <- df %>% 
  filter(sample_restricted %in% T,
         planned_near_china.pl10.30km.bin %in% T) %>%
  dplyr::summarise_all(mean, na.rm = T) %>%
  mutate(subset = "restricted")

dvs_means_china.pl10_df <- bind_rows(df_full,
                                     df_restricted) %>%
  pivot_longer(cols = -subset) %>%
  #mutate(aid_subset = "china.pl10") %>%
  filter(name %in% c(fig1_vars,
                     fig3_vars,
                     fig4_vars,
                     fig5_vars))

# 2.4 Near Planned: china.plNA -------------------------------------------------
df_full <- df %>% 
  filter(sample_full %in% T,
         planned_near_china.plNA.30km.bin %in% T) %>%
  dplyr::summarise_all(mean, na.rm = T) %>%
  mutate(subset = "full")

df_restricted <- df %>% 
  filter(sample_restricted %in% T,
         planned_near_china.plNA.30km.bin %in% T) %>%
  dplyr::summarise_all(mean, na.rm = T) %>%
  mutate(subset = "restricted")

dvs_means_china.plNA_df <- bind_rows(df_full,
                                     df_restricted) %>%
  pivot_longer(cols = -subset) %>%
  #mutate(aid_subset = "china.plNA") %>%
  filter(name %in% fig2_vars)

# 3. Merge and append ----------------------------------------------------------
dvs_means_usaid_df <- dvs_means_usaid_df %>%
  mutate(country = "usa")

dvs_means_china_df <- bind_rows(dvs_means_china.pl10_df,
                                dvs_means_china.plNA_df) %>%
  mutate(country = "chinese")

dvs_means_plannedcountry_df <- bind_rows(dvs_means_usaid_df,
                                         dvs_means_china_df) %>%
  dplyr::rename(dv_value_planned = value,
                dv = name)

dvs_means_all_df <- dvs_means_all_df %>%
  dplyr::rename(dv_value_all = value,
                dv = name)

df_results <- merge(df_results, dvs_means_all_df, by = c("dv", "subset"), all = T)
df_results <- merge(df_results, dvs_means_plannedcountry_df, by = c("dv", "subset", "country"), all = T)

df_results <- df_results %>%
  filter(!is.na(dv_clean)) # posistive/negative vars -- no usa, so NA here

# TABLE ========================================================================
df_results <- df_results %>%
  mutate(coef_diff       = coef_completed - coef_planned,
         pchange_all     = (coef_diff)/dv_value_all * 100,
         pchange_planned = (coef_diff)/dv_value_planned * 100)

df_results <- df_results[is.na(df_results$plcompltd) | df_results$plcompltd %in% F,]

df_results <- df_results %>%
  dplyr::select(subset,
                dv,
                dv_clean,
                country,
                coef_diff,
                dv_value_all,
                dv_value_planned,
                pchange_all,
                pchange_planned) %>%
  mutate(country = case_when(country %in% "chinese" ~ "China",
                             country %in% "usa" ~ "USA"),
         dv_clean = dv_clean %>% str_replace_all("\\n", "  ") %>% 
           str_replace_all("Positive Image: ", "") %>% 
           str_replace_all("Negative Image: ", "") %>% 
           str_squish() %>% 
           str_squish())

df_results <- df_results %>%
  mutate(tex = paste(dv_clean,
                     country,
                     coef_diff %>% round(3),
                     dv_value_all %>% round(2),
                     pchange_all %>% round(2) %>% paste("\\%"),
                     dv_value_planned %>% round(2),
                     pchange_planned %>% round(2) %>% paste("\\%"),
                     sep = " & ") %>% paste("\\\\ \n "))

pull_tex <- function(dv, subset, country){
  df_results[((df_results$dv %in% dv) & 
                (df_results$subset %in% subset) & 
                (df_results$country %in% country)),] %>% 
    pull(tex) %>% 
    cat()
}

sink("~/Desktop/test/tables/table.tex")
cat("\\begin{tabular}{lcc | cc  | cc} \n ")
cat("\\hline \n ")
cat("Dependent Variable & Aid & Coef $\\Delta$  & \\multicolumn{2}{c}{All} & \\multicolumn{2}{c}{Respondents Near} \\\\ \n ")
cat("                   &     & [Compl. - Pln.] & \\multicolumn{2}{c}{Respondents} & \\multicolumn{2}{c}{Planned Project} \\\\ \n ")
cat("\\hline \n ")
cat("                   &     &                & DV   & \\% $\\Delta$ & DV   & \\% $\\Delta$ \\\\ \n ")
cat("                   &     &                & Mean & from DV       & Mean & from DV \\\\ \n ")
cat("\\hline \n ")

# FIGURE 1 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
cat("\\multicolumn{5}{l|}{\\bf Effects of Chinese and US aid on perceptions of China and the US} & \\multicolumn{2}{l}{} \\\\ \n ")

cat("\\multicolumn{3}{l|}{\\emph{Full Sample}} & \\multicolumn{2}{c|}{} & \\multicolumn{2}{c}{} \\\\ \n")
pull_tex("china_positive_influence_index", "full", "China")
pull_tex("china.best.dev.model",           "full", "China")
pull_tex("usa.best.dev.model",             "full", "China")

cat("\\multicolumn{3}{l|}{} & \\multicolumn{2}{c|}{} & \\multicolumn{2}{c}{} \\\\ \n") # Blank
cat("\\multicolumn{3}{l|}{\\emph{Restricted Sample}} & \\multicolumn{2}{c|}{} & \\multicolumn{2}{c}{} \\\\ \n")

pull_tex("china_positive_influence_index", "restricted", "China")
pull_tex("china.best.dev.model",           "restricted", "China")
pull_tex("usa.best.dev.model",             "restricted", "China")

pull_tex("china_positive_influence_index", "restricted", "USA")
pull_tex("china.best.dev.model",           "restricted", "USA")
pull_tex("usa.best.dev.model",             "restricted", "USA")

# FIGURE 2 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
cat("\\multicolumn{3}{l|}{} & \\multicolumn{2}{c|}{} & \\multicolumn{2}{c}{} \\\\ \n") # Blank
cat("\\multicolumn{3}{l|}{\\bf Effects of Chinese and US aid on liberal democratic values} & \\multicolumn{2}{l|}{} & \\multicolumn{2}{l}{} \\\\ \n ")

cat("\\multicolumn{3}{l|}{\\emph{Full Sample}} & \\multicolumn{2}{c|}{} & \\multicolumn{2}{c}{} \\\\ \n")

pull_tex("lib_dem_val_index", "full", "China")

cat("\\multicolumn{3}{l|}{} & \\multicolumn{2}{c|}{} & \\multicolumn{2}{c}{} \\\\ \n") # Blank
cat("\\multicolumn{3}{l|}{\\emph{Restricted Sample}} & \\multicolumn{2}{c|}{} & \\multicolumn{2}{c}{} \\\\ \n")

pull_tex("lib_dem_val_index", "restricted", "China")
pull_tex("lib_dem_val_index", "restricted", "USA")

# FIGURE 3 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
cat("\\multicolumn{3}{l|}{} & \\multicolumn{2}{c|}{} & \\multicolumn{2}{c}{} \\\\ \n") # Blank
cat("\\multicolumn{5}{l|}{\\bf Effects of Chinese and US aid on perceptions of former colonial powers} & \\multicolumn{2}{l}{} \\\\ \n ")

cat("\\multicolumn{3}{l|}{\\emph{Full Sample}} & \\multicolumn{2}{c|}{} & \\multicolumn{2}{c}{} \\\\ \n")

pull_tex("formcolnpower.best.dev.model", "full", "China")

cat("\\multicolumn{3}{l|}{} & \\multicolumn{2}{c|}{} & \\multicolumn{2}{c}{} \\\\ \n") # Blank
cat("\\multicolumn{3}{l|}{\\emph{Restricted Sample}} & \\multicolumn{2}{c|}{} & \\multicolumn{2}{c}{} \\\\ \n")

pull_tex("formcolnpower.best.dev.model", "restricted", "China")
pull_tex("formcolnpower.best.dev.model", "restricted", "USA")

# FIGURE 4 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
cat("\\multicolumn{3}{l|}{} & \\multicolumn{2}{c|}{} & \\multicolumn{2}{c}{} \\\\ \n") # Blank
cat("\\multicolumn{5}{l|}{\\bf Effects of Chinese aid on factors contributing to positive image of China} & \\multicolumn{2}{l}{} \\\\ \n ")

cat("\\multicolumn{3}{l|}{\\emph{Full Sample}} & \\multicolumn{2}{c|}{} & \\multicolumn{2}{c}{} \\\\ \n")

pull_tex("posimage_businessinvetment", "full", "China")
pull_tex("posimage_chinesepeople", "full", "China")
pull_tex("posimage_infordevinvetment", "full", "China")
pull_tex("posimage_noninterference", "full", "China")
pull_tex("posimage_productcost", "full", "China")
pull_tex("posimage_supportinintlaffiars", "full", "China")

# FIGURE 5 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
cat("\\multicolumn{3}{l|}{} & \\multicolumn{2}{c|}{} & \\multicolumn{2}{c}{} \\\\ \n") # Blank
cat("\\multicolumn{5}{l|}{\\bf Effects of Chinese aid on factors contributing to negative image of China} & \\multicolumn{2}{l}{} \\\\ \n ")

cat("\\multicolumn{3}{l|}{\\emph{Full Sample}} & \\multicolumn{2}{c|}{} & \\multicolumn{2}{c}{} \\\\ \n")

pull_tex("negimage_chinesecitizenbehavior", "full", "China")
pull_tex("negimage_cooperateundemocratic", "full", "China")
pull_tex("negimage_landgrabbing", "full", "China")
pull_tex("negimage_productquality", "full", "China")
pull_tex("negimage_resourceextraction", "full", "China")
pull_tex("negimage_takingjobsbusiness", "full", "China")

cat("\\hline \n ")
cat("\\end{tabular} ")
sink()



