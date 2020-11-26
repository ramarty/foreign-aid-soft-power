# Chinese Aid Stats

# Load Data --------------------------------------------------------------------
chinese_aid <- readRDS(file.path(data_file_path, "chinese_aid_projects.Rds"))

## Restrict to Sample
chinese_aid <- chinese_aid %>%
  filter(sample == 1)

# Stats ------------------------------------------------------------------------
table(!is.na(chinese_aid$start_actual_year))
table(!is.na(chinese_aid$end_actual_year))
