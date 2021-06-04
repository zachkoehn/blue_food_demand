library(tidyverse);library(drlib)

work_directory <- "/Volumes/GoogleDrive/My Drive/BFA_Independent/bfa_demand/blue_food_demand"

# add working directory information in quotes here. 
# work_directory <- ""
raw_dat <- read.csv(
  file.path(
    work_directory,
    "data",
    "fig_4_edible.csv"
  ),
  header=TRUE
)

clean_dat <- raw_dat %>%
  pivot_longer(Cephalopods:Pelagic_fish,names_to = "category",values_to = "value") %>%
  mutate(
    category=str_replace_all(category,"_"," "),
    category=str_replace_all(category," .O",", o")
  )

edible_percap_plot <- clean_dat %>%
  ggplot() +
  geom_bar(
    aes(
      x=factor(year),
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
  xlab("") +
  guides(
    fill=guide_legend(nrow=3,byrow=TRUE)
    ) +
  facet_wrap(
    ~factor(country,levels = c("China","India","Ghana","Nigeria","Peru","Brazil","United States","Mexico","Spain","France"))
    )

legend_object <- get_legend(edible_percap_plot)
edible_percap_plot <- edible_percap_plot + theme(legend.position = "none")

edible_percap_plot_clean <- ggdraw() +
  draw_plot(edible_percap_plot) +
  draw_plot(legend_object,x=0.25,y=-.33)

ggsave(
  edible_percap_plot_clean,
  filename = file.path(
    zach_wk_dir,
    "figures",
    "figure_4_consumption_percap.pdf"
  ),
  width = 11.5,
  height = 7.39
  
)
