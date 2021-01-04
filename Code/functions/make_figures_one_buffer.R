# Common functions used throughout analysis

make_plot_all <- function(df,
                          height,
                          width,
                          file_name,
                          x_axis_breaks = 5){
  
  df %>%
    ggplot(aes(x = dv_clean, y = coef, ymin = ci2_5, ymax = ci97_5,
               group = var, color = var, shape = var, linetype = var)) +
    geom_hline(yintercept = 0, color = "gray50") +
    geom_linerange(position = position_dodge(width=.4)) +
    geom_point(position = position_dodge(width=.4)) +
    coord_flip() +
    facet_wrap(~subset_clean,
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
    scale_color_manual(values=figure_colors) +
    scale_y_continuous(breaks = scales::pretty_breaks(n = x_axis_breaks)) + 
    ggsave(file.path(figures_file_path, file_name),
           height = height, width = width)
  
}


calc_morans_i <- function(model){
  
  resid_df <- data.frame(
    location_id = model$clustervar$location_id, 
    residuals = model$residuals[,1] 
  ) %>%
    group_by(location_id) %>%
    dplyr::summarise(resid_sum = sum(residuals)) %>%
    mutate(location_id = location_id %>% as.character() %>% as.numeric())
  
  
  df_sum <- df %>% 
    group_by(location_id) %>%
    dplyr::summarise(latitude = mean(latitude),
                     longitude = mean(longitude)) %>%
    left_join(resid_df, by = "location_id") %>%
    filter(!is.na(resid_sum))
  
  mi <- moranfast(df_sum$resid_sum, df_sum$longitude, df_sum$latitude)
  
  out <- data.frame(observed = mi$observed,
                    expected =mi$expected,
                    sd = mi$sd,
                    p.value = mi$p.value)
  out$dv <- model$response[1,] %>% names()

  return(out)
}

