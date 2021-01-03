# Common functions used throughout analysis

extract_coefs <- function(model){
  # Extract dataframe of coefficients and 95% CI from model
  
  ## Extract coefs
  df_coefs   <- tidy(model)
  
  ## Extract 95% CI
  df_confint <- confint(model) %>% as.data.frame() 
  df_confint$term <- row.names(df_confint) 
  
  ## Merge
  df_all <- merge(df_coefs, df_confint, by = "term")
  
  ## Subset and rename
  df_all <- df_all %>%
    dplyr::select("term", "estimate", "2.5 %", "97.5 %") %>%
    dplyr::rename("var" = "term",
                  "coef" = "estimate",
                  "ci2_5" = "2.5 %",
                  "ci97_5" = "97.5 %")
  
  return(df_all)
}

