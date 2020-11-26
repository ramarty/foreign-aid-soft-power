# Chinese Aid Stats

# Load Data --------------------------------------------------------------------
chinese_aid <- readRDS(file.path(data_file_path, "chinese_aid_projects.Rds"))

## Restrict to Sample
chinese_aid <- chinese_aid %>%
  filter(sample == 1)

# Stats ------------------------------------------------------------------------
chinese_aid

