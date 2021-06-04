library(tidyverse);library(drlib)

work_directory <- "/Volumes/GoogleDrive/My Drive/BFA_Independent/bfa_demand/blue_food_demand"

# add working directory information in quotes here. 
# work_directory <- ""
raw_dat <- read.csv(
  file.path(
    work_directory,
    "data",
    "fig_7_data.csv"
  ),
  header=TRUE
)

clean_dat <- raw_dat %>%
  pivot_longer(Beef:Seafood,names_to = "category",values_to = "value") %>%
  mutate(
    category=str_replace_all(category,"_"," "),
    category=str_replace_all(category," .O",", o")
  )

animal_food_cons_plot <- clean_dat %>%
  ggplot() +
  geom_bar(
    aes(
      x=factor(category),
      y=value,
      fill=factor(year)
    ),
    stat = "identity",position = "dodge"
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
      "#0B0405FF",
      "#395D9CFF",
      "#60CEACFF"
    ),
    name=""
  ) +
  xlab("") +
  # guides(
  #   fill=guide_legend(nrow=3,byrow=TRUE)
  # ) +
  facet_wrap(
    ~factor(country,levels = c("China","India","Nigeria","Chile"))
  )
animal_food_cons_plot_free_y
animal_food_cons_plot

ggsave(
  animal_food_cons_plot_free_y,
  filename = file.path(
    zach_wk_dir,
    "figures",
    "figure_7_consump_proj_freeyaxis.pdf"
  ),
  width = 11.5,
  height = 7.39
)
ggsave(
  animal_food_cons_plot,
  filename = file.path(
    zach_wk_dir,
    "figures",
    "figure_7_consump_proj_sameyaxis.pdf"
  ),
  width = 11.5,
  height = 7.39
)
