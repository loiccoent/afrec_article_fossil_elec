library(RColorBrewer)
#install.packages(c("spData", "tmap", "cartogram"))
library(sf)
#library(raster)
library(spData)
#library(tmap)
#library(leaflet)
#library(cartogram)
#install.packages("scales")
library("scales")
#install.packages("ggpubr")
library("ggpubr")

first_year = 2000
last_year = 2017

#output chart
outputpath = "output/"

# Electricity production by source
electricity_prod_source <- Statistics %>%
  filter(
    flow %in% c(
      "Electricity production from Fuel Oil",
      "Electricity production from Gas/Diesel Oil",
      "Electricity production from Natural gas",
      "Electricity production from Hydro",
      "Electricity production from Solar PV",
      "Electricity production from Wind",
      "Electricity production from Other sources",
      "Electricity production from Coal"
    ), 
    product == "Gross electricity production",
    year >= first_year,
    year <= last_year 
  ) %>%
  mutate(flow = str_replace_all(
    flow,
    c(
      "Electricity production from Fuel Oil" = "Fuel oil",
      "Electricity production from Gas/Diesel Oil" = "Gas/diesel oil",
      "Electricity production from Natural gas" = "Natural gas",
      "Electricity production from Hydro" = "Hydro",
      "Electricity production from Solar PV" = "Solar PV",
      "Electricity production from Wind" = "Wind",
      "Electricity production from Other sources" = "Other",
      "Electricity production from Coal" = "Coal"
    )
  )) %>%
  mutate(flow = factor(flow,
                       levels = rev(
                         c(
                           "Fuel oil",
                           "Gas/diesel oil",
                           "Natural gas",
                           "Coal",
                           "Hydro",
                           "Solar PV",
                           "Wind",
                           "Other"
                         )
                       )))

# By source, for Total Africa

filename = "Electricity_prod_source_Africa.jpg"

jpeg(file=paste(outputpath, filename, sep = ""), 
     width=2400, 
     height=1600, 
     res=300)

electricity_prod_source %>%
  filter(country == "AFRICA") %>%
  ggplot(aes(x = year, y = value, fill = flow)) +
  geom_col() +
  theme_classic() +
  #ggtitle("Total electricity production by source in Africa") +
  theme(
    axis.title.x = element_blank(),
    legend.title = element_blank(),
    legend.position = "bottom",
    legend.box = "horizontal",
    plot.title = element_text(hjust = 0.5)
  ) +
  scale_y_continuous(labels = comma) +
  ylab("Total Electricity production (GWh)") +
  scale_fill_manual(
    values = c(
      "Fuel oil" = brewer.pal(8, "Dark2")[7],
      "Gas/diesel oil" = brewer.pal(8, "Dark2")[2],
      "Natural gas" = brewer.pal(8, "Dark2")[1],
      "Coal" = brewer.pal(8, "Dark2")[8],
      "Other" = brewer.pal(8, "Dark2")[5],
      "Hydro" = brewer.pal(8, "Dark2")[3],
      "Wind" = brewer.pal(8, "Dark2")[4],
      "Solar PV" = brewer.pal(8, "Dark2")[6]
    )
  )

dev.off()

# By source, by region

filename = "Electricity_prod_source_regions.jpg"

jpeg(file=paste(outputpath, filename, sep = ""), 
     width=1600, 
     height=2400, 
     res=300)

electricity_prod_source %>%
  filter(
    country %in% c(
      "NORTHERN AFRICA",
      "EASTERN AFRICA",
      "WESTERN AFRICA",
      "CENTRAL AFRICA",
      "SOUTHERN AFRICA"
    )
  ) %>%
  ggplot(aes(x = year, y = value, fill = flow)) +
  geom_col() +
  theme_classic() +
  #ggtitle("Total electricity production by source in African regions") +
  theme(
    axis.title.x = element_blank(),
    legend.title = element_blank(),
    legend.position = c(0.75, 0.15),
    #legend.box = "horizontal",
    plot.title = element_text(hjust = 0.5)
  ) +
  scale_y_continuous(labels = comma) +
  ylab("Total Electricity production (GWh)") +
  scale_fill_manual(
    values = c(
      "Fuel oil" = brewer.pal(8, "Dark2")[7],
      "Gas/diesel oil" = brewer.pal(8, "Dark2")[2],
      "Natural gas" = brewer.pal(8, "Dark2")[1],
      "Coal" = brewer.pal(8, "Dark2")[8],
      "Other" = brewer.pal(8, "Dark2")[5],
      "Hydro" = brewer.pal(8, "Dark2")[3],
      "Wind" = brewer.pal(8, "Dark2")[4],
      "Solar PV" = brewer.pal(8, "Dark2")[6]
    )
  ) +
  facet_wrap(~ country, ncol = 2)

dev.off()

# Total electricity production
electricity_prod_total <- Statistics %>%
  filter(
    # Need to take everything and sum up
    flow == "Total electricity production",
    year >= first_year,
    year <= last_year ) %>%
  group_by(country, official, year, product) %>%
  summarise(value = sum(value)) %>%
  ungroup()

# Total electricity production by Region

filename = "Electricity_prod_total_regions.jpg"

jpeg(file=paste(outputpath, filename, sep = ""), 
     width=2400, 
     height=1600, 
     res=300)

electricity_prod_total %>%
  filter(
    country %in% c(
      "NORTHERN AFRICA",
      "EASTERN AFRICA",
      "WESTERN AFRICA",
      "CENTRAL AFRICA",
      "SOUTHERN AFRICA"
    )
  ) %>%
  ggplot(aes(x = year, y = value, fill = country)) +
  geom_col() +
  theme_classic() +
  #ggtitle("Total electricity production by region in Africa") +
  theme(
    axis.title.x = element_blank(),
    legend.title = element_blank(),
    legend.position = "bottom",
    legend.box = "horizontal",
    plot.title = element_text(hjust = 0.5)
  ) +
  scale_y_continuous(labels = comma) +
  ylab("Total Electricity production (GWh)") +
  scale_fill_manual(
    values = c(
      "CENTRAL AFRICA" = brewer.pal(5, "Dark2")[1],
      "EASTERN AFRICA" = brewer.pal(5, "Dark2")[2],
      "NORTHERN AFRICA" = brewer.pal(5, "Dark2")[3],
      "WESTERN AFRICA" = brewer.pal(5, "Dark2")[4],
      "SOUTHERN AFRICA" = brewer.pal(5, "Dark2")[5]
    )
  )

dev.off()

# Total electricity production variation, by country

country_prod_ranked <- electricity_prod_total %>%
  filter(
    !country %in% c(
      "AFRICA",
      "NORTHERN AFRICA",
      "EASTERN AFRICA",
      "WESTERN AFRICA",
      "CENTRAL AFRICA",
      "SOUTHERN AFRICA"),
    year == last_year,
    product == "Gross electricity production",
  ) %>%
  arrange(desc(value)) %>%
  head(n = 20) %>%
  pull("official")

filename = "Electricity_prod_variation_countries.jpg"

jpeg(file=paste(outputpath, filename, sep = ""), 
     width=2400, 
     height=1600, 
     res=300)

electricity_prod_total %>%
  filter(
    official %in% .env$country_prod_ranked,
    year %in% c(first_year, last_year),
    product == "Gross electricity production",
  ) %>%
  ggplot(aes(x = factor(official, levels = rev(.env$country_prod_ranked)), 
                        y = value)) +
  geom_point(aes(color = factor(year)), size = 3) +
  geom_line(aes(group = country), 
            colour = "grey") +
  coord_flip() +
  theme_classic() +
  #ggtitle("Countries with the highest electricity production in 2018, vs 2000") +
  theme(
    axis.title.y = element_blank(),
    legend.position = c(0.8, 0.2),
    legend.box = "horizontal",
    plot.title = element_text(hjust = 0.5)
  )  +
  scale_y_continuous(labels = comma) +
  ylab("Electricity production (GWh)") +
  labs(color = "year")

dev.off()

# Electricity production shares
electricity_prod_share <- Statistics %>%
  filter(
    # No total prod, need to take everything and sum up
    flow %in% c(
      "Electricity production from Fuel Oil",
      "Electricity production from Gas/Diesel Oil",
      "Electricity production from Natural gas",
      "Electricity production from Coal",
      "Electricity production from Hydro",
      "Electricity production from Solar PV",
      "Electricity production from Wind",
      "Electricity production from Other sources",
      "Total electricity production"
    ),
    product == "Gross electricity production",
    year >= first_year,
    year <= last_year
  ) %>%
  pivot_wider(names_from = "flow", values_from = "value")  %>%
  replace(is.na(.), 0) %>%
  mutate(
    fossil = (.data[["Electricity production from Fuel Oil"]] +
                .data[["Electricity production from Gas/Diesel Oil"]] +
                .data[["Electricity production from Natural gas"]] + 
                .data[["Electricity production from Coal"]]) /
      .data[["Total electricity production"]],
    hydro = .data[["Electricity production from Hydro"]] /
      .data[["Total electricity production"]],
    other_renewables = (.data[["Electricity production from Solar PV"]] +
                          .data[["Electricity production from Wind"]] +
                          .data[["Electricity production from Other sources"]]) /
      .data[["Total electricity production"]],
    fuel_oil = .data[["Electricity production from Fuel Oil"]] /
      .data[["Total electricity production"]],
    diesel_oil = .data[["Electricity production from Gas/Diesel Oil"]] /
      .data[["Total electricity production"]],
    coal = .data[["Electricity production from Coal"]] /
      .data[["Total electricity production"]],
    natural_gas = .data[["Electricity production from Natural gas"]] /
      .data[["Total electricity production"]]
  ) %>%
  pivot_longer(
    cols = -c("country", "official", "year", "product"),
    names_to = "flow",
    values_to = "value"
  ) %>% 
  mutate(flow = str_replace_all(
    flow,
    c("fossil" = "Fossil fuels",
      "hydro" = "Hydroelectricity",
      "other_renewables" = "Other renewables",
      "fuel_oil" = "Fuel oil",
      "diesel_oil" = "Diesel oil",
      "coal" = "Coal",
      "natural_gas" = "Natural gas"))) 

# Electricity production shares by region

filename = "Electricity_prod_share_regions.jpg"

jpeg(file=paste(outputpath, filename, sep = ""), 
     width=1600, 
     height=2400, 
     res=300)

electricity_prod_share %>%
  filter(
    country %in% c(
      "NORTHERN AFRICA",
      "EASTERN AFRICA",
      "WESTERN AFRICA",
      "CENTRAL AFRICA",
      "SOUTHERN AFRICA"
    ),
    flow %in% c("Fossil fuels",
                "Hydroelectricity",
                "Other renewables")
  ) %>%
  ggplot(aes(x = year, y = value, color = flow)) +
  geom_line() +
  theme_classic() +
  #ggtitle("Shares in electricity production by region in Africa") +
  theme(
    axis.title.x = element_blank(),
    legend.title = element_blank(),
    legend.position = c(0.75, 0.15),
    #legend.box = "horizontal",
    plot.title = element_text(hjust = 0.5)
  ) +
  scale_y_continuous(labels =scales::label_percent()) +
  ylab("Share of electricity production") +
  scale_color_manual(
    values = c(
      "Fossil fuels" = brewer.pal(3, "Dark2")[2],
      "Hydroelectricity" = brewer.pal(3, "Dark2")[3],
      "Other renewables" = brewer.pal(3, "Dark2")[1]
    )
  ) +
  facet_wrap(~ country, ncol = 2)

dev.off()

shares_fossil <- electricity_prod_share %>%
  filter(
    !country %in% c(
       "NORTHERN AFRICA",
       "EASTERN AFRICA",
       "WESTERN AFRICA",
       "CENTRAL AFRICA",
       "SOUTHERN AFRICA",
       "AFRICA"
     ),
    flow %in% c("Fuel oil",
                "Diesel oil",
                "Coal",
                "Natural gas"),
    year %in% c(first_year, last_year)
  )

for (prod in c("Fuel",
               "Diesel",
               "Coal",
               "Gas")) {
  
  assign(paste(prod, "ranked", sep = "_"),
         shares_fossil %>%
            filter(case_when(
                       prod == "Fuel" ~ flow == "Fuel oil",
                       prod == "Diesel" ~ flow == "Diesel oil",
                       prod == "Coal" ~ flow == "Coal",
                       prod == "Gas" ~ flow == "Natural gas"),
                   year == last_year) %>%
            arrange(desc(value)) %>%
           head(n = 10) %>%
           pull("official"))
  
}


# # Loop overwrite the plots for some reasons. Done indivdually below.
# for (prod in c("Fuel",
#                "Diesel",
#                "Coal",
#                "Gas")) {
#   
#   # prod <- "Fuel"
#   # prod <- "Diesel"
#   # prod <- "Coal"
#   # prod <- "Gas"
# 
#   if (prod == "Fuel") {
#     countries <- Fuel_ranked
#     product <- "Fuel oil"
#     } else if (prod == "Diesel"){
#     countries <- Diesel_ranked
#     product <- "Diesel oil"
#     } else if (prod == "Coal") {
#     countries <- Coal_ranked
#     product <- "Coal"
#     } else if (prod == "Gas"){
#     countries <- Gas_ranked
#     product <- "Natural gas"}
# 
#   
#   assign(
#     paste(prod, "chart", sep = "_"),
#     shares_fossil %>%
#       filter(official %in% .env$countries,
#              flow == .env$product) %>%
#       ggplot(aes(x = factor(official, levels = rev(.env$countries)),
#                  y = value * 100)) +
#       geom_point(aes(color = factor(year)), size = 3) +
#       geom_line(aes(group = official), colour = "grey") +
#       coord_flip() +
#       theme_classic() +
#       #ggtitle("Share of electricity production from fossil fuels in 2018") +
#       theme(
#         axis.title.y = element_blank(),
#         axis.title.x = element_blank(),
#         #legend.position = c(0.75, 0.15),
#         #legend.title = element_blank(),
#         #legend.position = "bottom",
#         legend.box = "horizontal",
#         #plot.title = element_text(hjust = 0.50)
#         #axis.text.y = element_text(size = 5)
#       ) +
#       #ylab("Share in total electricity production (%)") +
#       labs(color = "year") +
#       ylim(c(-0.1, 100.1))
#   )
#   dev.off()
# 
# }

# start of loop replacement

# Fuel
Fuel_chart <- shares_fossil %>%
  filter(official %in% Fuel_ranked,
         flow == "Fuel oil") %>%
  ggplot(aes(x = factor(official, levels = rev(Fuel_ranked)),
             y = value * 100)) +
  geom_point(aes(color = factor(year)), size = 3) +
  geom_line(aes(group = official), colour = "grey") +
  coord_flip() +
  theme_classic() +
  #ggtitle("Share of electricity production from fossil fuels in 2018") +
  theme(
    axis.title.y = element_blank(),
    axis.title.x = element_blank(),
    #legend.position = c(0.75, 0.15),
    #legend.title = element_blank(),
    #legend.position = "bottom",
    legend.box = "horizontal",
    #plot.title = element_text(hjust = 0.50)
    #axis.text.y = element_text(size = 5)
  ) +
  #ylab("Share in total electricity production (%)") +
  labs(color = "year") +
  ylim(c(-0.1, 100.1))

# Diesel
Diesel_chart <- shares_fossil %>%
  filter(official %in% Diesel_ranked,
         flow == "Diesel oil") %>%
  ggplot(aes(x = factor(official, levels = rev(Diesel_ranked)),
             y = value * 100)) +
  geom_point(aes(color = factor(year)), size = 3) +
  geom_line(aes(group = official), colour = "grey") +
  coord_flip() +
  theme_classic() +
  #ggtitle("Share of electricity production from fossil fuels in 2018") +
  theme(
    axis.title.y = element_blank(),
    axis.title.x = element_blank(),
    #legend.position = c(0.75, 0.15),
    #legend.title = element_blank(),
    #legend.position = "bottom",
    legend.box = "horizontal",
    #plot.title = element_text(hjust = 0.50)
    #axis.text.y = element_text(size = 5)
  ) +
  #ylab("Share in total electricity production (%)") +
  labs(color = "year") +
  ylim(c(-0.1, 100.1))


# Coal
Coal_chart <- shares_fossil %>%
  filter(official %in% Coal_ranked,
         flow == "Coal") %>%
  ggplot(aes(x = factor(official, levels = rev(Coal_ranked)),
             y = value * 100)) +
  geom_point(aes(color = factor(year)), size = 3) +
  geom_line(aes(group = official), colour = "grey") +
  coord_flip() +
  theme_classic() +
  #ggtitle("Share of electricity production from fossil fuels in 2018") +
  theme(
    axis.title.y = element_blank(),
    axis.title.x = element_blank(),
    #legend.position = c(0.75, 0.15),
    #legend.title = element_blank(),
    #legend.position = "bottom",
    legend.box = "horizontal",
    #plot.title = element_text(hjust = 0.50)
    #axis.text.y = element_text(size = 5)
  ) +
  #ylab("Share in total electricity production (%)") +
  labs(color = "year") +
  ylim(c(-0.1, 100.1))

# Gas
Gas_chart <- shares_fossil %>%
  filter(official %in% Gas_ranked,
         flow == "Natural gas") %>%
  ggplot(aes(x = factor(official, levels = rev(Gas_ranked)),
             y = value * 100)) +
  geom_point(aes(color = factor(year)), size = 3) +
  geom_line(aes(group = official), colour = "grey") +
  coord_flip() +
  theme_classic() +
  #ggtitle("Share of electricity production from fossil fuels in 2018") +
  theme(
    axis.title.y = element_blank(),
    axis.title.x = element_blank(),
    #legend.position = c(0.75, 0.15),
    #legend.title = element_blank(),
    #legend.position = "bottom",
    legend.box = "horizontal",
    #plot.title = element_text(hjust = 0.50)
    #axis.text.y = element_text(size = 5)
  ) +
  #ylab("Share in total electricity production (%)") +
  labs(color = "year") +
  ylim(c(-0.1, 100.1))

# end of loop replacement

filename = "FF_share_ranked_country_2.jpg"

jpeg(file=paste(outputpath, filename, sep = ""), 
     width=2400, 
     height=3200, 
     res=300)

ggarrange(
   Coal_chart, 
   Diesel_chart, 
   Fuel_chart, 
   Gas_chart,
          ncol = 2, 
          nrow = 2,
          labels = c("Coal", "Diesel oil", "Fuel oil", "Natural gas"),
          label.x = 0.5,
          align = "hv",
          common.legend = TRUE, 
          legend = "bottom"
          )

dev.off()


# Fossil production shares heatmap

# Geographical element
AfricaShareFossilElectricity = world %>%
  filter(continent == "Africa") %>%
  mutate(
    name_long = case_when(
      #Correct the typo in Eswatini name
      name_long == "eSwatini" ~ "Eswatini",
      # Simplify the accent that causes issues
      name_long == "Côte d'Ivoire" ~ "Cote d'Ivoire",
      TRUE ~ name_long
    )
  ) %>%
  st_transform("+proj=aea +lat_1=20 +lat_2=-23 +lat_0=0 +lon_0=25") %>%
  merge(
    electricity_prod_share,
    by.x = "name_long",
    by.y = "official",
    all.x = TRUE
  )

# Not used but not bad
#plot(africa["ShareBiomass"])
#tm_shape(africa) +
#  tm_polygons("ShareBiomass")

filename = "Fossil_share_heat_map.jpg"

jpeg(file=paste(outputpath, filename, sep = ""), 
     width=2400, 
     height=1600, 
     res=300)

AfricaShareFossilElectricity %>%
  filter(flow == "Fossil fuels",
         year == 2017) %>%
  ggplot() +
  geom_sf(aes(geometry = geometry, fill = value)) +
  #ggtitle("Share of fossil fuel in electricity generation, 2017") +
  theme_classic() +
  theme(
    plot.title = element_text(hjust = 0.5),
    panel.grid = element_blank(),
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    line = element_blank(),
    legend.position = c(0.25, 0.30)
  ) +
  labs(fill = "Share") +
  scale_fill_gradient(
    # it was a pain to show the 100% correctly
    limits = c(1, 0),
    breaks = scales::pretty_breaks(n = 4)(0:1),
    labels = scales::percent,
    trans = 'reverse'
  )

dev.off()

# Efficiency by fuel
efficiency_prod_source <- Balances %>%
  filter(flow == "Inputs to electricity production", 
         product %in% c("Gas/diesel oil", "Fuel oil", "Natural gas", "Coal"),
         year >= first_year,
         year <= last_year) %>% 
  mutate(input=-value * 11.63) %>% 
  select(c(country, official, product, year, input)) %>% 
  merge(electricity_prod_source %>%
          filter(flow %in% c("Gas/diesel oil", "Fuel oil", "Natural gas", "Coal")) %>%
          mutate(output = value)%>%
          select(c(country, official, flow, year, value)) %>%
          rename(product = flow,
                 output=value),
        by=c("country", "official", "product", "year"),
        all=TRUE) %>% 
  drop_na("input") %>%
  drop_na("output") %>%
  filter(input != 0,
         output != 0) %>%
  mutate(efficiency = output / input)

# Efficiency by fuel by region

filename = "Production_efficiency_region.jpg"

jpeg(file=paste(outputpath, filename, sep = ""), 
     width=1600, 
     height=2400, 
     res=300)

efficiency_prod_source %>% 
  filter(country %in% c(
           "AFRICA",
           "NORTHERN AFRICA",
           "EASTERN AFRICA",
           "WESTERN AFRICA",
           "CENTRAL AFRICA",
           "SOUTHERN AFRICA"),
         year>first_year,
         year<=last_year) %>%
  ggplot(aes(x = year, y = efficiency, color = country)) +
  geom_line() +
  theme_classic() +
  ggtitle("Efficiency in electricity production by country in Africa") +
  theme(
    axis.title.x = element_blank(),
    legend.title = element_blank(),
    legend.position = c(0.75, 0.15),
    #legend.box = "horizontal",
    plot.title = element_text(hjust = 0.5)
  ) +
  ylab("Efficiency of electricity production") +
  #ylim(0, 1) +
  facet_wrap(~ product, ncol = 2)

dev.off()

# Efficiency by fuel by country

filename = "Production_efficiency_country.jpg"

jpeg(file=paste(outputpath, filename, sep = ""), 
     width=2400, 
     height=3200, 
     res=300)

efficiency_prod_source %>% 
  select(country, official, product, year, efficiency) %>%
  mutate(official = factor(official), 
         official = factor(official, levels = rev(levels(official)))) %>%
  filter(!country %in% c(
           "AFRICA",
           "NORTHERN AFRICA",
           "EASTERN AFRICA",
           "WESTERN AFRICA",
           "CENTRAL AFRICA",
           "SOUTHERN AFRICA"),
         year %in% c(as.character(last_year))) %>%
  pivot_wider(names_from = "year", values_from = "efficiency") %>%
  arrange(desc(.data[[as.character(last_year)]])) %>%
  pivot_longer(
    col = -c("country", "official", "product"),
    names_to = "year",
    values_to = "value"
  ) %>%
  ggplot(aes(x = official, y = value * 100)) +
  geom_point(aes(color = factor(year)), size = 3) +
  geom_line(aes(group = official), colour = "grey") +
  coord_flip() +
  theme_classic() +
  #ggtitle("Calculated efficiency in electricity production from fossil fuels in 2018") +
  theme(
    axis.title.y = element_blank(),
    legend.title = element_blank(),
    legend.position = "bottom",
    #legend.position = c(0.75, 0.15),
    legend.box = "horizontal",
    plot.title = element_text(hjust = 0.50)
    #axis.text.y = element_text(size = 5)
  )  +
  ylab("Efficiency of electricity production (%)") +
  #labs(color = "year") + 
  ylim(c(0.1,150)) +
  geom_hline(yintercept=100,lwd=1,lty=2,colour="red") + 
  # geom_rect(aes(ymin=20, ymax=50, xmin=0, xmax=Inf), alpha = 0.2, fill = "blue") +
  annotate("rect", xmin = 0, xmax = Inf, ymin = 20, ymax = 50, alpha = .2) +
  facet_wrap(~ product)

dev.off()


