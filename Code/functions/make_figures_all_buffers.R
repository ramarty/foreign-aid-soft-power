make_1_figure <- function(df,
                          nrow_figure,
                          legend_pos = "right",
                          x_axis_breaks){
  p <- df %>%
    ggplot(aes(x = buffer, y = coef, ymin = ci2_5, ymax = ci97_5,
               group = var, color = var, fill = var, shape = var, linetype = var)) +
    geom_hline(yintercept = 0, color = "gray50") +
    geom_linerange(position = position_dodge(width=2.9)) +
    geom_point(position = position_dodge(width=2.9)) +
    coord_flip() +
    labs(x = "Buffer\nSize\n(km)",
         y="Coef (+/- 95% CI)",
         title = df$subset_clean[1],
         linetype = "",
         shape = "",
         fill = "",
         color = "") +
    theme_ipsum() + 
    theme(plot.title = element_text(family="Times",size=12,hjust=0.5,face="bold"),
          axis.title.x = element_text(family="Times",size=13),
          axis.title.y = element_text(family="Times",size=13, angle = 0),
          axis.text.y = element_text(family="Times",size=13),
          axis.text.x = element_text(family="Times",size=13),
          legend.title = element_text(family="Times",size=13),
          legend.text = element_text(family="Times",size=13),
          strip.text = element_text(family="Times",size=13, hjust = 0.5, face = "bold")) +
    guides(colour = guide_legend(reverse=T),
           linetype = guide_legend(reverse=T),
           pch = guide_legend(reverse=T),
           fill = guide_legend(reverse=T)) +
    #scale_linetype_manual(values=c("solid","dashed","solid","dashed")) +
    scale_linetype_manual(values=c("solid","solid","solid","solid")) +
    #scale_shape_manual(values=c(16,17,16,17)) +
    scale_shape_manual(values=c(16,24,16,24)) +
    scale_fill_manual(values=c("black", "white", "black", "white")) +
    scale_color_manual(values=figure_colors) +
    theme(legend.position=legend_pos) +
    scale_y_continuous(breaks = scales::pretty_breaks(n = x_axis_breaks)) + 
    scale_x_reverse() + 
    facet_wrap(~dv_clean,
               nrow = nrow_figure,
               scales = "free_x")
  
  return(p)
}

make_fig_full_restr <- function(df,
                                nrow_figure,
                                ncol_arrange,
                                height,
                                width,
                                legend_pos = "right",
                                file_name,
                                x_axis_breaks){
  
  p_full <- df %>%
    filter(subset %in% "full") %>%
    make_1_figure(nrow_figure = nrow_figure,
                  x_axis_breaks = x_axis_breaks) +
    theme(legend.position = "none")
  
  p_restricted <- df %>%
    filter(subset %in% "restricted") %>%
    make_1_figure(nrow_figure = nrow_figure,
                  x_axis_breaks = x_axis_breaks)
  
  p <- ggarrange(p_full,
                 p_restricted,
                 #common.legend = T,
                 legend = legend_pos,
                 legend.grob = ggpubr::get_legend(p_restricted),
                 ncol = ncol_arrange)
  
  ggsave(p, 
         filename = file.path(figures_file_path, file_name),
         height = height, 
         width = width)
  
}

make_fig <- function(df,
                     nrow_figure,
                     height,
                     width,
                     legend_pos = "right",
                     file_name,
                     x_axis_breaks){
  
  p <- df %>%
    make_1_figure(nrow_figure = nrow_figure,
                  legend_pos = legend_pos,
                  x_axis_breaks = x_axis_breaks) 
  
  ggsave(p, 
         filename = file.path(figures_file_path, file_name),
         height = height, 
         width = width)
  
}

