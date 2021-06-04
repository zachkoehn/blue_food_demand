# Figure 1
library(tidyverse)
library(ggthemes)
library(ggpubr)

# Load data
df <- read.csv("fig 1 data_II.csv")

df <- df %>%
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
  geom_point(data = df, aes(x = gdp, y = animal)) + 
  geom_line(data = df_fit_animal, aes(x = x, y = y)) +
  geom_text(aes(x = 80000, y = 20, label = paste("r = ", round(animal_r, 2)))) +
  labs(x = "GDP per capita (USD 2015)", y = "Terrestrial animal apparent consumption \n(per capita, kg/yr)") +
  theme_minimal()


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

g_fish <- ggplot() +
  geom_point(data = df, aes(x = gdp, y = fish)) + 
  geom_line(data = df_fit_fish, aes(x = x, y = y)) + 
  geom_text(aes(x = 80000, y = 10, label = paste("r = ", round(fish_r, 2)))) +
  labs(x = "GDP per capita (USD 2015)", y = "Fish apparent consumption \n(per capita, kg/yr") +
  theme_minimal()

png("fig1_polynomial.png", width = 4, height = 6, units = "in", res = 300)
ggarrange(g_fish, g_animal, ncol = 1, labels = c("a", "b"), vjust = c(1.5, -1.5))
dev.off()
