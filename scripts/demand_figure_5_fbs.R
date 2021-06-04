# FBS Plots for Demand paper

library(tidyverse)
library(countrycode)
library(ggthemes)
library(cowplot)

# First load function for rebuilding from FishStat ZIP file:
rm(list=ls())


# note: Change outdir to the filepath where you want outputs to go
outdir <- "Outputs"
# for MacOS
# datadir <- "/Volumes/jgephart/FishStatR/Data/FoodBalanceSheets"
datadir <- "/Volumes/GoogleDrive/My Drive/BFA_Independent/bfa_demand/blue_food_demand"
# add working directory information in quotes here. 
# datadir <- ""

df_new <- read.csv(file.path(datadir, "data","FoodBalanceSheets","FoodBalanceSheets_E_All_Data", "FoodBalanceSheets_E_All_Data.csv"))
#df_hist <- read.csv(file.path(datadir, "FoodBalanceSheetsHistoric_E_All_Data", "FoodBalanceSheetsHistoric_E_All_Data.csv"))

# Filter to countries and blue foods 
# (currently excluding "Aquatic Products, Other", "Aquatic Animals, Others", "Aquatic Plants", "Meat, Aquatic Mammals"))
df_new <- df_new %>%
  #filter(Element %in% c("Protein supply quantity (g/capita/day)", "Import Quantity", "Export Quantity"), Area.Code < 1000) %>%
  filter(Item %in% c("Freshwater Fish", "Demersal Fish", "Pelagic Fish", "Marine Fish, Other", "Crustaceans", 
                     "Cephalopods", "Molluscs, Other")) %>%
  select(-ends_with("F")) %>% 
  pivot_longer(Y2014:Y2017, names_to = "year") %>%
  mutate(year = as.numeric(gsub("Y", "", year)))

#df_hist <- df_hist %>%
#  filter(Element %in% c("Protein supply quantity (g/capita/day)", "Import Quantity", "Export Quantity"), Area.Code < 1000) %>%
#  filter(Item %in% c("Freshwater Fish", "Demersal Fish", "Pelagic Fish", "Marine Fish, Other", "Crustaceans", 
#                     "Cephalopods", "Molluscs, Other")) %>%
#  select(-ends_with("F")) %>% 
#  pivot_longer(Y1961:Y2013, names_to = "year") %>%
#  mutate(year = as.numeric(gsub("Y", "", year)))

df <- df_new %>%
  # Excluding for now when we don't need the historical data
  #bind_rows(df_new, df_hist) %>%
  pivot_wider(names_from = Element, values_from = value, names_repair = "universal", values_fill = 0) %>%
  mutate(iso3c = countrycode(Area, origin = "country.name", destination = "iso3c"),
         iso3n = countrycode(Area, origin = "country.name", destination = "iso3n")) %>%
  filter(!(Area == "China")) %>% # Only include China, mainland because territories report separate data 
  mutate(net_import = Import.Quantity - Export.Quantity,
         net_export = Export.Quantity - Import.Quantity)

# Clean codes
df$iso3c[df$Area == "Eswatini"] <- "SWZ"
df$iso3n[df$Area == "Eswatini"] <- "748"

# Plot net import for China, India, Ghana, Nigeria, Peru, Brazil, U.S., Mexico, Spain, and France
df_plot <- df %>%
  filter(iso3c %in% c("CHN", "IND", "GHA", "PER", "NGA", "BRA", "USA", "MEX", "ESP", "FRA"), 
         year == 2015)
df_plot$iso3c <- factor(df_plot$iso3c, levels = c("CHN", "IND", "GHA", "NGA", "PER",  "BRA", "MEX", "USA", "ESP", "FRA"))
df_plot$Item <- factor(df_plot$Item, levels = c("Demersal Fish", "Pelagic Fish", "Marine Fish, Other", "Freshwater Fish", 
                                                 "Crustaceans", "Cephalopods", "Molluscs, Other"))

df_categories <- df_plot %>% 
  filter(Unit=="1000 tonnes") %>%
  select(Area,iso3c,Item,year,Import.Quantity,Export.Quantity) %>%
  rename(
    Import=Import.Quantity,
    Export=Export.Quantity
  ) %>%
  mutate(
    trade_balance= Export-Import,
    Export=Export*-1
  ) %>%
  pivot_longer(cols=c(Import,Export),names_to = "trade",values_to = "value")

df_total <- df_plot %>%
  filter(Unit=="1000 tonnes") %>%
  group_by(Area, iso3c) %>%
  summarise(Import.Quantity = sum(Import.Quantity),
            Export.Quantity = sum(Export.Quantity)) %>%
  mutate(
    # net_import = Import.Quantity - Export.Quantity,
    # net_export = Export.Quantity - Import.Quantity,
    Item = "Total") %>%
  rename(
    Import=Import.Quantity,
    Export=Export.Quantity
  )%>%
  mutate(
    trade_balance= Export-Import,
    Export=Export*-1
  ) %>%
  pivot_longer(cols=c(Import,Export),names_to = "trade",values_to = "value")


trade_plot_func <- function(dat,Item,title) {
  # dat=df_categories
  # Item="Pelagic Fish",
  
  Item=enquo(Item)
  trade_plot <- dat %>%
    filter(Item==!!Item) %>%
    ggplot() +
    geom_bar(
      aes(
        # x=reorder_within(iso3c,trade_balance,!!Item),
        x=factor(iso3c,levels = c("CHN","IND","GHA","NGA","PER","BRA","USA","MEX","ESP","FRA")),
        y=abs(value/1000),
        # y=(value/1000),
        fill=trade
        ),
      stat = "identity",position="dodge"
      # stat = "identity",position="stack"
    ) +
      scale_x_reordered()+
      scale_fill_manual(
        values = c(
          Export = "#3FC1C9", #blue
          Import = "#B389ED" #purple
        ),
        name = NULL
      ) +
      xlab("") + ylab("") +
    theme(
      panel.border = element_blank(),
      panel.background = element_blank(),
      panel.grid.minor = element_blank(),
      axis.line = element_line(colour = "grey"),
      axis.text = element_text(size=8,),
      plot.title = element_text(size=12,face="bold")
      
      ) +
    # theme_classic() +
    ggtitle(title)
  
    trade_plot
}
  
pelagic_plot <- trade_plot_func(dat=df_categories,Item = "Pelagic Fish",title="Pelagic fish") + theme(legend.position = "none",axis.text.x = element_text(size=6.5)) 
freshwater_fish_plot <- trade_plot_func(dat=df_categories,Item = "Freshwater Fish",title="Freshwater fish") + theme(legend.position = "none",axis.text.x = element_text(size=6.5)) #+ ylim(c(-1.3,1.5))
demersal_fish_plot <- trade_plot_func(dat=df_categories,Item = "Demersal Fish",title="Demersal fish") + theme(legend.position = "none",axis.text.x = element_text(size=6.5)) #+ ylim(c(-1.3,1.5))
crustacean_plot <- trade_plot_func(dat=df_categories,Item = "Crustaceans",title="Crustaceans") + theme(legend.position = "none",axis.text.x = element_text(size=6.5)) #+ ylim(c(-1.3,1.5))
cephalopod_plot <- trade_plot_func(dat=df_categories,Item = "Cephalopods",title="Cephalopods") + theme(legend.position = "none",axis.text.x = element_text(size=6.5))
other_molluscs_plot <- trade_plot_func(dat=df_categories,Item = "Molluscs, Other",title="Molluscs, Other") + theme(legend.position = "none",axis.text.x = element_text(size=6.5)) #+ ylim(c(-1.3,1.5))
other_marine_fish_plot <- trade_plot_func(dat=df_categories,Item = "Marine Fish, Other",title="Marine fish, other") + theme(legend.position = "none",axis.text.x = element_text(size=6.5))
total_plot <- trade_plot_func(dat=df_total,Item = "Total",title="Total")

legend_insert <- get_legend(total_plot)


multipanel_grouped <- ggdraw() +
  draw_plot(
    total_plot + 
      theme(
        legend.position = c(.75,.85),legend.text = element_text(size=12),legend.key.size = unit(1.5,"line")
            ) + 
      guides(shape = guide_legend(override.aes = list(size = 10))),
    x=0.01,y=.25, hjust=0,vjust=0,height=.75,width=.75) +
  draw_plot(
    freshwater_fish_plot,
    x=0.01,y=.0, hjust=0,vjust=0,height=.25,width=.25) +
  draw_plot(
    demersal_fish_plot,
    x=0.26,y=0, hjust=0,vjust=0,height=.25,width=.25) +
  draw_plot(
    crustacean_plot,
    x=0.51,y=.0, hjust=0,vjust=0,height=.25,width=.25) +
  draw_plot(
    other_molluscs_plot,
    x=.75,y=.0, hjust=0,vjust=0,height=.25,width=.25) +
  draw_plot(
    cephalopod_plot,
    x=.75,y=.75, hjust=0,vjust=0,height=.25,width=.25) +
  draw_plot(
    other_marine_fish_plot,
    x=.75,y=.5, hjust=0,vjust=0,height=.25,width=.25) +
  draw_plot(
    pelagic_plot,
    x=.75,y=.25, hjust=0,vjust=0,height=.25,width=.25) +
  # draw_plot(
  #   legend_insert,x=.65,y=.8, hjust=0.5,vjust=0.5,scale=2.5,height=1,width=1) +
  draw_text(
    text="Trade volume (million tonnes live weight)",
    x=0.005,y=0.5,hjust=0.5,vjust=1,angle=90,size=12,fontface="bold"
            )




ggsave(
  multipanel_grouped,
  filename = file.path(
    "/Volumes/GoogleDrive/My Drive/BFA_Independent/bfa_demand/",
    "figures",
    "figure_5_trade.pdf"
  ),
  width = 11.5,
  height = 7.39
)



