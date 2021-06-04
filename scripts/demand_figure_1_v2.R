  library(tidyverse);library(ggpubr);library(ggtext)
  
  work_directory <- "/Volumes/GoogleDrive/My Drive/BFA_Independent/bfa_demand/blue_food_demand"
  
  # add working directory information in quotes here. 
  # work_directory <- ""
  raw_dat <- read.csv(
    file.path(
      work_directory,
      "data",
      "fig 1 data_II.csv"
    ),
    header=TRUE
  )
  
  
  df <- raw_dat %>%
    rename(gdp = GDP_percapita_Constan.2010_US_2015,
           animal = percapita_terrestrial_animal_consumption,
           fish = per_capita_fish_consumption)
  
  # Plot data
  ggplot(df, aes(x = gdp, y = animal)) +
    geom_point()
  
  # To fit y = ax^b, replace y with ln(y) and x with ln(x)
  # Fit with linear regression (y = bx + c) to get b and c, then a = e^c
  df <- df %>% 
    mutate(ln_gdp = log(gdp),
           ln_animal = log(animal),
           ln_fish = log(fish))
  
  # Animal regression
  animal_linear_fit <- lm(df$ln_animal ~ df$ln_gdp)
  animal_b <- animal_linear_fit$coefficients[2] # slope
  animal_c <- animal_linear_fit$coefficients[1] # intercept
  animal_a <- exp(animal_c)
  
  df_fit_animal <- data.frame(x = 1:max(df$gdp))
  df_fit_animal <- df_fit_animal %>% 
    mutate(y = animal_a*x^(animal_b))
  
  # Report r and r^2
  animal_r2 <- summary(animal_linear_fit)$r.squared
  animal_r <- sqrt(animal_r2)
  
  
  g_animal <- ggplot() +
    geom_point(data = df, aes(x = gdp, y = animal),color="#70468C") + 
    geom_line(data = df_fit_animal, aes(x = x, y = y),color="#70468C") +
    geom_richtext(
      aes(
        x = 80000, y = 2.5, 
        label = paste("r<sup>2</sup> = ", round(animal_r2, 2)),
        fill=after_scale(alpha("#70468C",0.2))
        ),
      label.size = 0.25,vjust=0
      ) +
    labs(x = "GDP per capita (USD 2015)", y = "Terrestrial animal apparent consumption \n(per capita, kg/yr)",title = "b) Terrestrial animals")
  
  
  # fish regression
  fish_linear_fit <- lm(df$ln_fish ~ df$ln_gdp)
  fish_b <- fish_linear_fit$coefficients[2] # slope
  fish_c <- fish_linear_fit$coefficients[1] # intercept
  fish_a <- exp(fish_c)
  
  df_fit_fish <- data.frame(x = 1:max(df$gdp))
  df_fit_fish <- df_fit_fish %>% 
    mutate(y = fish_a*x^(fish_b))
  
  # Report r and r^2
  fish_r2 <- summary(fish_linear_fit)$r.squared
  fish_r <- sqrt(fish_r2)
  notation <- expression("r"^2)
  g_fish <- ggplot() +
    geom_point(data = df, aes(x = gdp, y = fish),color="#364F6B") + 
    geom_line(data = df_fit_fish, aes(x = x, y = y),color="#364F6B") + 
    geom_richtext(
      aes(
        x = 80000, y = 2.5, 
        label = paste0("r<sup>2</sup> = ", round(fish_r2, 2)),
        fill=after_scale(alpha("#364F6B",0.2))
        ),
      vjust=0
      ) +
    labs(x = "GDP per capita (USD 2015)", y = "Fish apparent consumption \n(per capita, kg/yr",title = "a) Seafood") 
  g_fish
  
  
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
  
  
  land_plot <- g_animal + theme_scatter + ylab("") + xlab("") + ylim(0,280)
  fish_plot <- g_fish + theme_scatter + ylab("") + xlab("") + ylim(0,280)
  
  
  multipanel_plot <- ggarrange(
    fish_plot,land_plot, ncol=2
    ) 
    
  consumption_plot <- annotate_figure(
    multipanel_plot,
    bottom = text_grob("GDP per capita (USD 2015)", color = "black",face="bold",
                       size = 12,vjust = 0.5),
    left = text_grob("Apparent consumption \n(kg/capita/year, live weight)", color = "black", size=12,rot = 90,vjust=0.5,face="bold")
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
