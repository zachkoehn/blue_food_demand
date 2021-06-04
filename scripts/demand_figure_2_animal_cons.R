library(tidyverse);library(ggpubr)

work_directory <- "/Volumes/GoogleDrive/My Drive/BFA_Independent/bfa_demand/blue_food_demand"

# add working directory information in quotes here. 
# work_directory <- ""
raw_dat <- read.csv(
  file.path(
    work_directory,
    "data",
    "fig_2a_raw.csv"
  ),
  header=TRUE
)

dat_2b <- read.csv(
  file.path(
    zach_wk_dir,
    "data",
    "fig_2b_normalized.csv"
  ),
  header=TRUE
)

plot_theme <- theme(
  # panel.border = element_blank(),
  panel.background = element_blank(),
  panel.grid.minor = element_blank(),
  axis.title.y = element_text(size=10,face="bold"),
  axis.line = element_line(colour = "grey"),
  axis.text = element_text(size=12,),
  legend.text = element_text(size=12),
  legend.key.size = unit(2,"line"),
  legend.key=element_rect(fill = NA)
  )

plot_2a <- dat_2a %>% 
  pivot_longer(cols=Beef:Seafood,names_to = "category",values_to = "value") %>%
  ggplot() +
  geom_line(
    aes(x=year,y=value,color=category),size=1.5,alpha=0.8
    ) +
  xlab("") + ylab("Consumption (kg/cap/year, edible)") +
  scale_x_continuous(breaks=seq(from=1961, to=2020,by=7)) +
  scale_color_manual(
    values = c(
      Beef = "#FC5185",
      Pork = "#A9D158",
      Poultry = "#FFA647",
      Seafood = "#364F6B"
      ),
    name=""
    ) +
  plot_theme
  


plot_2b <- dat_2b %>% 
  pivot_longer(cols=Beef:Seafood,names_to = "category",values_to = "value") %>%
  ggplot() +
  geom_line(
    aes(x=year,y=value,color=category),size=1.5,alpha=0.8
  )  +
  xlab("") + ylab("Consumption normalized to 1961") +
  scale_x_continuous(breaks=seq(from=1961, to=2020,by=7)) +
  scale_color_manual(
    values = c(
      Beef = "#FC5185",
      Pork = "#A9D158",
      Poultry = "#FFA647",
      Seafood = "#364F6B"
    ),
    name=""
  ) +
  plot_theme

multipanel_plot <- ggarrange(
  plot_2a,plot_2b, ncol=2, labels=c("a","b"),common.legend = TRUE,legend = "bottom"
)

multipanel_plot


ggsave(
  multipanel_plot,
  filename = file.path(
    zach_wk_dir,
    "figures",
    "figure_2_animal_cons_time.pdf"
  ),
  width = 11.5,
  height = 7.39
  
)
