library(tidyverse);library(drlib)

work_directory <- "/Volumes/GoogleDrive/My Drive/BFA_Independent/bfa_demand/blue_food_demand"

# add working directory information in quotes here. 
# work_directory <- ""
raw_dat <- read.csv(
  file.path(
    work_directory,
    "data",
    "fish_trade_long.csv"
  ),
  header=TRUE
)

clean_dat <- raw_dat %>%
  mutate(
    Import=str_replace_all(Import,",",""),
    Export=str_replace_all(Export,",",""),
    Import=as.numeric(Import),
    Export=as.numeric(Export),
    trade_balance= Export-Import,
    Import=Import*-1,
    category=str_replace_all(category," trade",""),
    category=str_replace_all(category,"All fish","All seafood")
  ) %>%
  pivot_longer(cols=c(Import,Export),names_to = "trade",values_to="values") %>%
  mutate(
    category=as.factor(category),
    country_name=tidytext::reorder_within(
      country_name,
      desc(trade_balance),
      category
    )
  )

library(tidytext)
trade_plot <- clean_dat %>%
  ggplot() +
  geom_bar(
    aes(
      x=country_name,
      y=values,
      fill=trade
      ),
    stat = "identity"
    ) +
  ylab("Export, Import (Tonnes live weight)") +
  theme_bw() +
  theme(
    axis.text.x = element_text(
      size=11,angle = 300, vjust = 0.5, hjust=0
      ),
    axis.text.y = element_text(
      size=10
    ),
    strip.text = element_text(size = 12),
    strip.background = element_rect(color="gray94")
  ) +
  scale_x_reordered()+
  scale_fill_manual(
    values = c(
      Export = "#3FC1C9", #blue
      Import = "#B389ED" #purple
    ),
    name = ""
  ) +
  xlab("") +
  facet_wrap(~category,nrow=2,scales = "free")
trade_plot


ggsave(
  trade_plot,
  filename = file.path(
    zach_wk_dir,
    "figures",
    "figure_4_trade.pdf"
  ),
  width = 11.5,
  height = 7.39
  
)
