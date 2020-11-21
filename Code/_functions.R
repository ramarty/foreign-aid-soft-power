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

make_plot_all <- function(df,
                          height,
                          width,
                          file_name){
  
  df %>%
    ggplot(aes(x = model, y = coef, ymin = ci2_5, ymax = ci97_5,
               group = var, color = var, shape = var, linetype = var)) +
    geom_hline(yintercept = 0, color = "gray50") +
    geom_linerange(position = position_dodge(width=.4)) +
    geom_point(position = position_dodge(width=.4)) +
    coord_flip() +
    facet_wrap(~subset,
               scales = "free_x") +
    labs(x = "",
         y="Coef (+/- 95% CI)",
         linetype = "",
         shape = "",
         color = "") +
    theme_ipsum() + 
    theme(plot.title = element_text(family="Times",size=12,hjust=0.5,face="bold"),
          axis.title.x = element_text(family="Times",size=13),
          axis.title.y = element_text(family="Times",size=13),
          axis.text.y = element_text(family="Times",size=13),
          axis.text.x = element_text(family="Times",size=13),
          legend.title = element_text(family="Times",size=13),
          legend.text = element_text(family="Times",size=13),
          strip.text = element_text(family="Times",size=13, hjust = 0.5, face = "bold")) +
    guides(colour = guide_legend(reverse=T),
           linetype = guide_legend(reverse=T),
           pch = guide_legend(reverse=T)) +
    scale_linetype_manual(values=c("solid","dashed","solid","dashed")) +
    scale_shape_manual(values=c(16,17,16,17)) +
    scale_color_manual(values=c("darkorange3", "darkorange","dodgerblue4","dodgerblue2")) +
    ggsave(file.path(figures_file_path, file_name),
           height = height, width = width)
  
}




