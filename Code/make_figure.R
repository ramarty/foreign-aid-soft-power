# Figure 2

make_1_figure <- function(df,
                          nrow_figure){
  p <- df %>%
    ggplot(aes(x = buffer, y = coef, ymin = ci2_5, ymax = ci97_5,
               group = var, color = var, shape = var, linetype = var)) +
    geom_hline(yintercept = 0, color = "gray50") +
    geom_linerange(position = position_dodge(width=2.5)) +
    geom_point(position = position_dodge(width=2.5)) +
    coord_flip() +
    labs(x = "Buffer\nSize\n(km)",
         y="Coef (+/- 95% CI)",
         title = df$subset_clean[1],
         linetype = "",
         shape = "",
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
           pch = guide_legend(reverse=T)) +
    scale_linetype_manual(values=c("solid","dashed","solid","dashed")) +
    scale_shape_manual(values=c(16,17,16,17)) +
    scale_color_manual(values=figure_colors) +
    #theme(legend.position="bottom") +
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
                                file_name){
  
  p_full <- df %>%
    filter(subset %in% "full") %>%
    make_1_figure(nrow_figure = nrow_figure) +
    theme(legend.position = "none")
  
  p_restricted <- df %>%
    filter(subset %in% "restricted") %>%
    make_1_figure(nrow_figure = nrow_figure)
  
  p <- ggarrange(p_full,
                 p_restricted,
                 #common.legend = T,
                 legend = "right",
                 legend.grob = ggpubr::get_legend(p_restricted),
                 ncol = ncol_arrange)
  
  ggsave(p, 
         filename = file.path(figures_file_path, file_name),
         height = height, 
         width = width)
  
}


# Load Data --------------------------------------------------------------------
df_results <- readRDS(file.path(results_file_path, "coefficients.Rds"))

lookup <- df_results %>%
  dplyr::select(dv, dv_clean) %>%
  distinct()

# Figure 1 ---------------------------------------------------------------------
df_results %>%
  filter(dv %in% c("china_influential_index",
                   "china_positive_influence_index")) %>%
  make_fig_full_restr(nrow_figure = 1,
                      ncol_arrange = 2,
                      height = 5,
                      width = 12,
                      file_name = "figure_01.png")

# Figure 1 - Components --------------------------------------------------------
df_results %>%
  filter(dv %in% c("china.most.influence", 
                   "usa.most.influence", 
                   "china.best.dev.model",
                   "usa.best.dev.model")) %>%
  make_fig_full_restr(nrow_figure = 1,
                      ncol_arrange = 1,
                      height = 10,
                      width = 12,
                      file_name = "figure_01_components.png")

