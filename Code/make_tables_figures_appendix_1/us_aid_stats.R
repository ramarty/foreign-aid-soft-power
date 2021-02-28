# Chinese Aid Stats

# Load Data --------------------------------------------------------------------
us_aid <- readRDS(file.path(data_file_path, "us_aid_projects.Rds"))

## Restrict to Sample
us_aid <- us_aid %>%
  filter(sample == 1)

# Stats ------------------------------------------------------------------------
us_aid$end_date_year %>% is.na %>% table()

## If planned, percent with completion date
us_aid %>%
  filter(planned_near_usaid %in% 1) %>%
  pull(end_date_year) %>%
  is.na %>% 
  ifelse(FALSE, TRUE) %>% # flip is.na, so TRUE is has completion date
  mean()



getData('GADM', country='NGA', level=1) %>% nrow()
getData('GADM', country='UGA', level=1) %>% nrow()
