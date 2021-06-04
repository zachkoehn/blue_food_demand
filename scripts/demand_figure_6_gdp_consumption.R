library(tidyverse);library(ggpubr);library(tidytext)

work_directory <- "/Volumes/GoogleDrive/My Drive/BFA_Independent/bfa_demand/blue_food_demand"

# add working directory information in quotes here. 
# work_directory <- ""
raw_dat <- read.csv(
  file.path(
    work_directory,
    "data",
    "fig_6_data.csv"
  ),
  header=TRUE
)


library(tidytext)
multipanel_plot <- raw_dat %>%
  ggplot() +
  geom_jitter(
    aes(
      fill=year,
      x=gdp_percap_normalized,
      y=consumption_percap_normalized,
    ),
    color="black",
    size=2,
    pch=21,stroke=0.25
  ) +
  scale_fill_viridis(option = "mako", name="Year") +
  geom_abline(slope=1,linetype=2,color="red4") +
  # annotate(geom="text",label=paste0("Coef: ",fish_cor),size=6,color="black",x=60000,y=4.5) +
  ylab("Consumption (normalized)") + xlab("GDP (normalized)") +
  theme_bw() +
  theme(
    axis.text.x = element_text(
      size=11
    ),
    axis.text.y = element_text(
      size=11
    )
  ) +
  theme_bw() +
  theme(
    panel.border = element_blank(),
    panel.background = element_blank(),
    panel.grid.minor = element_blank(),
    axis.title = element_text(size=12,face="bold"),
    axis.line = element_line(colour = "grey"),
    axis.text = element_text(size=12,),
    strip.text = element_text(size = 12,face = "bold"),
    strip.background = element_rect(color="gray94"),
    legend.text = element_text(size=10),
    legend.key.size = unit(1.5,"line")
  ) +
  facet_wrap(~factor(country,levels = c("China","India","Nigeria","Chile")))

text_data <- data.frame(
  label = c("Coef: 0.9947", "Coef: 0.8459", "Coef: 0.8102", "Coef: -0.3451"),
  country= c("China","India","Nigeria","Chile")
  
)

multipanel_plot<-multipanel_plot+ 
  geom_text(
    data    = text_data,
    mapping = aes(x = 55, y = 1, label = label),
    hjust   = 0,
    vjust   = 0,
    size=4
  )

ggsave(
  multipanel_plot,
  filename = file.path(
    zach_wk_dir,
    "figures",
    "figure_6_gdp_consumption.pdf"
  ),
  width = 11.5,
  height = 7.39
  
)
