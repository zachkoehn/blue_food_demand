library(tidyverse);library(ggpubr)

work_directory <- "/Volumes/GoogleDrive/My Drive/BFA_Independent/bfa_demand/blue_food_demand"

# add working directory information in quotes here. 
# work_directory <- ""
raw_dat <- read.csv(
  file.path(
    work_directory,
    "data",
    "fig_1_data.csv"
  ),
  header=TRUE
)

fish_cor <- round(cor(raw_dat$GDP.per.capita_Constant.2010.US..2015,raw_dat$fish.consumption),4)
land_cor <- round(cor(raw_dat$GDP.per.capita_Constant.2010.US..2015,raw_dat$terrestrial.animal.consumption),4)


clean_dat <- raw_dat %>%
  select(
    -X
  ) %>%
  rename(
    gdp=GDP.per.capita_Constant.2010.US..2015,
    fish=fish.consumption,
    land=terrestrial.animal.consumption
  )

theme_scatter <- theme(
  panel.border = element_blank(),
  panel.background = element_blank(),
  panel.grid.minor = element_blank(),
  axis.title.y = element_text(size=12,face="bold"),
  axis.line = element_line(colour = "grey"),
  axis.text = element_text(size=12,),
  strip.text = element_text(size = 12,face = "bold"),
  strip.background = element_rect(color="gray94"),
  legend.text = element_text(size=12),legend.key.size = unit(1.5,"line"),
  plot.title =  element_text(size=14,face="bold")
) 

fish_plot <- clean_dat %>%
  ggplot() +
  geom_jitter(
    aes(
      x=gdp,
      y=fish,
    ),
    color="#364F6B",
    size=1.5
  ) +
  annotate(geom="text",label=paste0("Coef: ",fish_cor),size=5,color="black",x=62000,y=5) +
  ylab("") + xlab("") +
  theme_bw() +
  theme_scatter +
  ylim(c(0,280)) +
  ggtitle("a) Seafood")

land_plot <- clean_dat %>%
  ggplot() +
  geom_jitter(
    aes(
      x=gdp,
      y=land,
    ),
    color="#70468C",
    size=1.5
  ) +
  annotate(geom="text",label=paste0("Coef: ",land_cor),size=5,color="black",x=62000,y=5) +
  ylab("") + xlab("") +
  theme_bw() +
  theme_scatter +
  ylim(c(0,280)) +
  ggtitle("b) Terrestrial animals") 
  


multipanel_plot <- ggarrange(
  fish_plot,land_plot, ncol=2
  ) 
  
consumption_plot <- annotate_figure(
  multipanel_plot,
  bottom = text_grob("GDP per capita (US $ 2015)", color = "black",face="bold",
                     size = 12,vjust = 0.5),
  left = text_grob("Consumption (kg/capita/year, live weight)", color = "black", size=12,rot = 90,vjust=0.5,face="bold")
)
consumption_plot

ggsave(
  consumption_plot,
  filename = file.path(
    zach_wk_dir,
    "figures",
    "figure_1_consumption.pdf"
  ),
  width = 11.5,
  height = 7.39
  
)
