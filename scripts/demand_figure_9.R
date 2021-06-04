library(tidyverse);library(drlib)

work_directory <- "/Volumes/GoogleDrive/My Drive/BFA_Independent/bfa_demand/blue_food_demand"


# add working directory information in quotes here. 
# work_directory <- ""
raw_dat <- read.csv(
  file.path(
    work_directory,
    "data",
    "fig_9_total consumption 2015 vs 2050.csv"
  ),
  header=TRUE
)

clean_dat <- raw_dat %>%
  mutate(
    country=factor(country,levels = c("China","India","United States","Mexico","Nigeria","Brazil","France","Spain","Peru","Ghana"))
  )

consumption_plot <- clean_dat %>%
  ggplot() +
  geom_bar(
    aes(
      x=factor(country,levels = c("China","India","Ghana","Nigeria","Peru","Brazil","United States","Mexico","Spain","France")),
      y=(consumption)/1000,
      fill=as.factor(year)
    ),
    stat = "identity",position = "dodge"
  ) +
  ylab("Consumption (total live weight million tonnes) ") +
  theme_bw() +
  theme(
    axis.text.x = element_text(
      size=12
    ),
    axis.text.y = element_text(
      size=12
    )
  ) +
  scale_fill_manual(
    values = c(
      '2015' = "#3FC1C9", #blue
      '2050' = "#364F6B" #purple
    ),
    name = ""
  ) +
  scale_y_continuous(breaks = c(10, 30, 50, 70, 90, 110)) +
  xlab("") 


ggsave(
  consumption_plot,
  filename = file.path(
    zach_wk_dir,
    "figures",
    "figure_9_consumption_total.pdf"
  ),
  width = 11.5,
  height = 7.39
  
)
