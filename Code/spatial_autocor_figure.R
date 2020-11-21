# Figure of Moran's I

mi_df <- read.csv(file.path(data_file_path, "data_for_select_figures", "morans_i_data.csv"))

mi_df %>%
  mutate(sig_5 = ifelse(p.value <= 0.05, "< 0.05", " > 0.05")) %>%
  ggplot(aes(x = reorder(var_label, p.value), 
             y = p.value,
             color = sig_5)) +
  geom_point() + 
  coord_flip() +
  labs(x = "", 
       y = "Moran's I: p-value", 
       color = NULL,
       title = "Spatial Autocorrelation of\nDependent Variables") +
  scale_color_manual(values = c("black", "red")) +
  theme_minimal() +
  theme(legend.position = "top",
        plot.title = element_text(family="Times", face = "bold", hjust = 0.5),
        axis.text.x = element_text(family="Times", size=12),
        axis.text.y = element_text(family="Times", size=12, color = "black"),
        axis.title.x = element_text(family="Times", size=12)) +
  ggsave(file.path(figures_file_path, "spatial_auto.png"),
         height = 8, width = 8)





