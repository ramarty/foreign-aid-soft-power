# Map of Data

# Load Data --------------------------------------------------------------------
#### Africa 
africa <- getMap(resolution = "coarse", projection = NA)
africa <- africa[africa$REGION %in% "Africa",]

#### Afrobarometer 
afro_all <- readRDS(file.path(data_file_path, "afro_china_data.Rds"))
afro_all <- afro_all %>%
  mutate(id = 1) %>%
  group_by(latitude, longitude) %>%
  dplyr::summarise(id = mean(id))

#### Chinese Aid
china_aid <- read.csv(file.path(data_file_path, "official_finance_locations.csv"))
china_aid <- china_aid[!grepl("Angola|Congo|Chad|Central African Rep.|South Sudan|Western Sahara|Mauritania", china_aid$all_recipients),]
china_aid <- china_aid[china_aid$precision_code %in% c(1,2),]
china_aid <- china_aid %>%
  group_by(latitude, longitude) %>%
  dplyr::summarise(geonameID = mean(geonameID))

#### US/UK AMP Aid
amp_aid <- read.csv(file.path(data_file_path, "aiddata_amp_allcountries_appended.csv"))
amp_aid <- amp_aid[!(amp_aid$country %in% c("drc","central african republic","somolia")),]
amp_aid <- amp_aid[amp_aid$precision_code %in% c(1,2),]
amp_aid <- amp_aid[!is.na(amp_aid$latitude),]

us_aid <- amp_aid[amp_aid$donors_us,]
uk_aid <- amp_aid[grepl("united kingdom|dfid", tolower(amp_aid$donors)),]

us_aid <- us_aid %>%
  group_by(latitude, longitude) %>%
  dplyr::summarise(even_split_commitments = mean(even_split_commitments))

uk_aid <- uk_aid %>%
  group_by(latitude, longitude) %>%
  dplyr::summarise(even_split_commitments = mean(even_split_commitments))

# Figure -----------------------------------------------------------------------
#afrobarometer_color <- "#ee5f33"
afrobarometer_color <- "moccasin" # "ivory" # "burlywood1"
china_color <- "red"
us_color <- "deepskyblue1"
uk_color <- "olivedrab2"

fig <- ggplot() +
  geom_polygon(data=africa, aes(x=long, y=lat, group=group), 
               fill="gray30", color="black", size=.4) + # .3
  #geom_polygon(data=africa[africa$ADMIN.1 %in% c("Burundi","Malawi", "Nigeria","Senegal","Sierra Leone","Uganda"),], 
  #             aes(x=long, y=lat, group=group, fill="Country with\nUS & UK Aid Data"), 
  #             color="gray40", size=1) +
  geom_point(data=afro_all, aes(x=longitude, y=latitude,color="Afrobarometer\nSurvey Location\n"), alpha=0.2,  size=2) +
  
  # Just for legend
  geom_point(data=uk_aid, aes(x=longitude, y=latitude, color="UK Aid"),         alpha=1, size=0.1) +
  geom_point(data=us_aid, aes(x=longitude, y=latitude, color="US Aid"),         alpha=1, size=0.1) +
  geom_point(data=china_aid, aes(x=longitude, y=latitude, color="Chinese Aid"), alpha=1, size=0.1) +
  
  # 5.5 // 3 // 2.5
  geom_point(data=uk_aid, aes(x=longitude, y=latitude),       alpha=1, size=6, color = "black", fill = uk_color, pch = 21) +
  geom_point(data=us_aid, aes(x=longitude, y=latitude),       alpha=1, size=3.5,   color = "black", fill = us_color, pch = 21) +
  geom_point(data=china_aid, aes(x=longitude, y=latitude),    alpha=1, size=3, color = "black", fill = china_color, pch = 21) +
  
  #geom_point(data=uk_aid, aes(x=longitude, y=latitude),       alpha=1, size=1.5, color = "black", fill = uk_color, pch = 21) +
  #geom_point(data=us_aid, aes(x=longitude, y=latitude),       alpha=1, size=1.5, color = "white", fill = us_color, pch = 21) +
  #geom_point(data=china_aid, aes(x=longitude, y=latitude),    alpha=1, size=1.5, color = "white", fill = china_color, pch = 21) +
  
  #geom_point(data=uk_aid, aes(x=longitude, y=latitude, color="UK Aid"),         alpha=1, size=1) +
  
  #geom_point(data=china_aid, aes(x=longitude, y=latitude), color=china_color,alpha=.3,size=3.5) +
  #geom_polygon(data=africa[africa$ADMIN.1 %in% c("Burundi","Malawi", "Nigeria","Senegal","Sierra Leone","Uganda"),], 
  #             aes(x=long, y=lat, group=group), 
  #             color="gray10", size=.1, fill=NA) +
  scale_color_manual(values=c(afrobarometer_color, china_color, uk_color, us_color)) +
  scale_fill_manual(values="gray80") +
  labs(color="", fill="", size="") +
  coord_quickmap() +
  theme_void() +
  theme(legend.text = element_text(color="white",size=30,family="Times New Roman"),
        legend.box.background = element_rect(fill="black",color="white",size=.5),
        legend.margin = margin(6, 6, 6, 6),
        legend.position = c(0.24, 0.17),
        plot.background = element_rect(color = "black", fill = "black")) +
  guides(size = guide_legend(override.aes = list(size = c(6))),
         color = guide_legend(override.aes = list(size = c(6))))
ggsave(fig, filename=file.path(figures_file_path, "figure_a1.png"), height=18,width=20,dpi=300)


 

