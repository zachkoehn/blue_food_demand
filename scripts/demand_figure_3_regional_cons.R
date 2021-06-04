library(tidyverse)

work_directory <- "/Volumes/GoogleDrive/My Drive/BFA_Independent/bfa_demand/blue_food_demand"

# add working directory information in quotes here. 
# work_directory <- ""
raw_dat <- read.csv(
  file.path(
    work_directory,
    "data",
    "fig_3_data.csv"
  ),
  header=TRUE
)

clean_dat <- raw_dat %>%
  pivot_longer(Cephalopods:Pelagic_fish,names_to = "category",values_to = "value") %>%
  mutate(
    category=str_replace_all(category,"_"," "),
    category=str_replace_all(category," .O",", o")
  )

region_edible_plot <- clean_dat %>%
  ggplot() +
  geom_bar(
    aes(
      x=factor(region,levels=c("Asia","Africa","South America","North America","Europe","Oceania")),
      y=value,
      fill=as.factor(category)
    ),
    stat = "identity",position = "stack"
  ) +
  ylab("Consumption (kg/cap/yr, edible)") +
  theme_bw() +
  theme(
    panel.border = element_blank(),
    panel.background = element_blank(),
    panel.grid.minor = element_blank(),
    axis.title.y = element_text(size=12,face="bold"),
    axis.line = element_line(colour = "grey"),
    axis.text = element_text(size=12,),
    strip.text = element_text(size = 12,face = "bold"),
    strip.background = element_rect(color="gray94"),
    legend.text = element_text(size=12),legend.key.size = unit(1.5,"line")
  ) +
  scale_fill_manual(
    values = c(
      'Bivalves' = "#70468C", #blue
      'Cephalopods' = "#B389ED", #light purple
      'Crustaceans' = "#A69569", #gold
      'Demersal fish' = "#A9D158", #green
      'Freshwater fish' = "#FFD947", #pink
      'Marine fish, other' = "#3FC1C9", #blue
      'Pelagic fish' = "#364F6B" #dark blue
    ),
    name=""
  ) +
  # guides(
  #   fill=guide_legend(nrow=3,byrow=TRUE)
  # ) +
  xlab("") 

region_edible_plot_clean <- region_edible_plot +
  geom_hline(yintercept = 14.6, linetype="dotted",size=2,alpha=0.8) +
  annotate("text",label="World average (14.6)",y=15.2,x=2.5,hjust=0.5,vjust=0.5,fontface="bold") +
  annotate("text",label="(70%)",y=16.94,x=1,hjust=0.5,vjust=0.5)+
  annotate("text",label="(7%)",y=9.87,x=2,hjust=0.5,vjust=0.5)+
  annotate("text",label="(5%)",y=8.27,x=3,hjust=0.5,vjust=0.5)+
  annotate("text",label="(6%)",y=14.58,x=4,hjust=0.5,vjust=0.5)+
  annotate("text",label="(11%)",y=16.28,x=5,hjust=0.5,vjust=0.5)+
  annotate("text",label="(1%)",y=20.01,x=6,hjust=0.5,vjust=0.5)



ggsave(
  region_edible_plot_clean,
  filename = file.path(
    zach_wk_dir,
    "figures",
    "figure_3_regional_percap.pdf"
  ),
  width = 11.5,
  height = 7.39
  
)
