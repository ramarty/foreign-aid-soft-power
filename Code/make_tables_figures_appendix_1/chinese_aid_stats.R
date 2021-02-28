# Chinese Aid Stats

# Load Data --------------------------------------------------------------------
chinese_aid <- readRDS(file.path(data_file_path, "chinese_aid_projects.Rds"))

## Restrict to Sample
chinese_aid <- chinese_aid %>%
  filter(sample == 1)

# Stats ------------------------------------------------------------------------
## If planned, percent with completion date
chinese_aid %>%
  filter(planned_near_china.pl10 %in% 1) %>%
  pull(end_actual_year) %>%
  is.na %>% 
  ifelse(FALSE, TRUE) %>% # flip is.na, so TRUE is has completion date
  mean()


