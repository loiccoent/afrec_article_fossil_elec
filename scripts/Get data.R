#install.packages("tidyverse")
library(tidyverse)

# Read the data
ISOfile <- "references/ISOname.csv"
ISOmapping <- read.csv(ISOfile)

Officialfile <- "references/officialname.csv"
Officialmapping <- read.csv(Officialfile) %>%
  rename("country"="AFREC")

balancefile <- "data/Balances.csv"
Balances_raw <- read.csv(balancefile, sep = ";")

statsfile <- "data/Statistics.csv"
Stats_raw <- read.csv(statsfile, sep = ";")

# Rename and reshape

Balances <- Balances_raw %>%
  # merge ISO codes to AFREC names
  merge(ISOmapping,
        by.x="zone_code", 
        by.y="ISO") %>%
  rename("country"="ï..AFREC",
         "flow"="Flow.Trad",
         "product"="Product.Trad") %>%
  # format names
  mutate(year=as.numeric(substr(date, nchar(date)-4+1, nchar(date))),
         value=as.numeric(sub(",", ".", value, fixed = TRUE)),
         flow=trimws(flow, which = c("left"))) %>%
filter(flow %in% c(
  # for combustibles input
  "Inputs to electricity production", 
  # for nuclear, hydro, wind, PV, other inputs
  "TRANSFORMATIONS: INPUTS (-) AND OUTPUTS (+)")) %>%
  select(c("country", "flow", "product", "year", "value"))

unique(Balances_raw %>% 
         filter(Flow.Trad == "   Inputs to electricity production") %>%
         select(Product.Trad))

# aggregate products  
Balances_expanded <- Balances %>% 
  pivot_wider(names_from = "product", values_from = "value") %>%
  mutate("Fuel oil" = rowSums(.[c("Fuel oil", 
                                  # For some reason low sulfur content is not part of total...
                                  "Fuel oil with low sulfur content (<1%)",
                                  # Adding crude here
                                  "Crude oil")], 
                              na.rm=TRUE), 
         "Gas/diesel oil" = rowSums(.[c("Gas/diesel oil", 
                                  "Gasoline",
                                  "LPG",
                                  "Jet Kerosene",
                                  "Other Kerosene",
                                  "Non-specified oil products"
                                  )], 
                                  na.rm=TRUE), 
         "Coal" = rowSums(.[c("Hard Coal", 
                              "Maufactured and recovered gases", 
                              "Coke oven coke", 
                              "Coal Products", 
                              "Blast furnace gas", 
                              "Other bituminous coal", 
                              "BKB (Brown coal briquettes)", 
                              "Brown Coal", 
                              "Lignite", 
                              "Sub-bituminous coal")], 
                          na.rm=TRUE),
         "Biofuels" = rowSums(.[c("Biogases", 
                                  "Other vegetal and agricultural waste",
                                  "Other solid biofuels unspecified",
                                  "Liquid biofuels",
                                  "Biogasoline")], 
                              na.rm=TRUE)) %>%
  pivot_longer(cols = - c("country", "flow", "year"), 
               names_to = "product", 
               values_to = "value") %>%
  drop_na(value)

# Recalculate the regions
Balances_regions <- Balances_expanded %>%
  mutate(region=case_when(
    country %in% c("Algeria",
                   "Egypt",
                   "Libya",
                   "Morocco",
                   "Sudan",
                   "Tunisia",
                   "Sahrawi Republic") ~ "North",
    country %in% c(
      "Burundi",
      "Comoros",
      "Djibouti",
      "Eritrea",
      "Ethiopia",
      "Kenya",
      "Madagascar",
      "Malawi",
      "Mauritius",
      "Rwanda",
      "Seychelles",
      "Somalia",
      "South sudan",
      "Tanzania",
      "Uganda") ~ "East",
    country %in% c(
      "Cameroon",
      "Central Africa Republic",
      "Chad",
      "Congo",
      "Democratic Republic of Congo",
      "Equatorial Guinea",
      "Gabon",
      "Sao Tome and Principe") ~ "Central",
    country %in% c(
      "Angola",
      #sic
      "Bostwana",
      "Eswatini",
      "Lesotho",
      "Mozambique",
      "Namibia",
      "South Africa",
      "Zambia",
      "Zimbabwe") ~ "South", 
    country %in% c(
      "Benin",
      "Burkina Faso",
      "Cape Verde",
      #sic
      "Cote d Ivoire",
      "Gambia",
      "Ghana",
      "Guinea",
      "Guinea Bissau",
      "Liberia",
      "Mali",
      "Mauritania",
      "Niger",
      "Nigeria",
      "Senegal",
      "Sierra Leone",
      "Togo") ~ "West")
  ) %>%
  select(c("country", "region", "flow", "product", "year", "value"))

north_countries <- Balances_regions %>%
  filter(region == "North") %>%
  group_by(product, flow, year) %>%
  summarize(value=sum(value, na.rm = TRUE)) %>%
  mutate(country="NORTHERN AFRICA")

south_countries <- Balances_regions %>%
  filter(region == "South") %>%
  group_by(product, flow, year) %>%
  summarize(value=sum(value, na.rm = TRUE)) %>%
  mutate(country="SOUTHERN AFRICA")

west_countries <- Balances_regions %>%
  filter(region == "West") %>%
  group_by(product, flow, year) %>%
  summarize(value=sum(value, na.rm = TRUE)) %>%
  mutate(country="WESTERN AFRICA")

east_countries <- Balances_regions %>%
  filter(region == "East") %>%
  group_by(product, flow, year) %>%
  summarize(value=sum(value, na.rm = TRUE)) %>%
  mutate(country="EASTERN AFRICA")

central_countries <- Balances_regions %>%
  filter(region == "Central") %>%
  group_by(product, flow, year) %>%
  summarize(value=sum(value, na.rm = TRUE)) %>%
  mutate(country="CENTRAL AFRICA")

all_countries <- Balances_regions %>%
  group_by(product, flow, year) %>%
  summarize(value=sum(value, na.rm = TRUE)) %>%
  mutate(country="AFRICA")

Balances_regions <- Balances_regions %>% 
  select(c("country", "flow", "product", "year", "value")) %>% 
  rbind(north_countries) %>% 
  rbind(south_countries) %>% 
  rbind(west_countries) %>% 
  rbind(east_countries) %>% 
  rbind(central_countries) %>% 
  rbind(all_countries)

# merge AFREC names with official names
Balances_regions <- Balances_regions %>% 
  merge(Officialmapping,
        by=c("country"),
        all = TRUE)

Balances <- Balances_regions

unique(Balances$country)
unique(Balances$year)
unique(Balances$flow)
unique(Balances$product)

Statistics <- Stats_raw %>%
  rename("country"="ï..Zone.Trad",
         "flow"="Flow.Trad",
         "product"="Product.Trad..browser.",
         "unit"="Unit.Trad",
         "year"="AnnÃ.e.de.date") %>%
  mutate(value=as.numeric(sub(",", ".", value, fixed = TRUE)),
         flow=trimws(flow, which = c("left")),
         country = case_when(
           country == "Ivory Coast" ~ "Cote d Ivoire",
           TRUE ~ country
         )) %>%
  filter(flow %in% c("Electricity production from Fuel Oil",
                     "Electricity production from Gas/Diesel Oil",
                     "Electricity production from Hydro", 
                     "Electricity production from Natural gas", 
                     "Electricity production from Other sources",   
                     "Electricity production from Solar PV", 
                     "Electricity production from Wind",
                     "TRANSFORMATIONS: INPUTS (-) AND OUTPUTS (+)",
                     "Inputs to electricity production")) %>%
  select(c("country", "flow", "product", "unit", "year", "value")) %>%
  mutate(value = case_when(
    #correct South Africa electricity from fuel oil
    (country == "South Africa" &
       year == "2000" &
       flow == "Electricity production from Fuel Oil" &
       product == "Gross electricity production") ~ 4,
    #correct Zimbabwe electricity from gas-diesel in 2017
    (country == "Zimbabwe" &
       year == "2017" &
       flow == "Electricity production from Gas/Diesel Oil" &
       product == "Gross electricity production") ~ value / 100,
    #remove Botswana electricity from fuel oil and from gas diesel oil for the all time series
    (country == "Bostwana" &
       flow %in% c("Electricity production from Fuel Oil",
                   "Electricity production from Gas/Diesel Oil") &
       product == "Gross electricity production") ~ 0,
    TRUE ~ value))

# Get the total electricity output from Balance, in order to calculate the output from coal plants
Elec_output <- Balances %>%
  filter(flow == "TRANSFORMATIONS: INPUTS (-) AND OUTPUTS (+)", 
         product %in% c("Nuclear", "Hydroelectricity", "Wind", "Solar", "Other sources", "Electricity"),
         !country %in% c(
           "AFRICA",
           "NORTHERN AFRICA",
           "EASTERN AFRICA",
           "WESTERN AFRICA",
           "CENTRAL AFRICA",
           "SOUTHERN AFRICA")) %>%
  select("country", "product", "flow", "year", "value") %>%
  mutate(
    value = case_when(
      #correct Morroco total electricity in 2014
      (country == "Morocco" &
         year == "2014" &
         flow == "TRANSFORMATIONS: INPUTS (-) AND OUTPUTS (+)" &
         product %in% c("Electricity", "Hydroelectricity", "Wind")) ~ value / 10, 
      #correct Mali total electricity in 2015
      (country == "Mali" &
         year == "2015" &
         flow == "TRANSFORMATIONS: INPUTS (-) AND OUTPUTS (+)" &
         product == "Electricity") ~ value / 100,
      TRUE ~ value)) %>%
  pivot_wider(names_from = "product", values_from = "value") %>%
  # inputs are negative, electricity output is positive
  mutate("Combustible" = rowSums(.[,c("Nuclear", "Hydroelectricity", "Wind", "Solar", "Other sources", "Electricity")], na.rm=TRUE)) %>%
  select(-c("Nuclear", "Hydroelectricity", "Wind", "Solar", "Other sources")) %>%
  pivot_longer(cols=-c("country", "flow", "year"), names_to = "product", values_to = "value") %>%
  drop_na(value)  %>%
  mutate(flow=case_when(product=="Combustible"~"Electricity production from combustible fuels",
                        product=="Electricity"~"Total electricity production"),
         product="Gross electricity production",
         value = value * 11.63,
         unit="GWh") %>%
  drop_na(value) 

# append the electricity output total to calculate the output from coal
Statistics_expanded <- Statistics %>%
  rbind(Elec_output) %>%
  pivot_wider(names_from = "flow", 
              values_from = "value") %>%
  mutate(negcombustible = - .data[["Electricity production from combustible fuels"]]) %>%
  # here we are assuming electricity production from biofuels is negligible, and 
  # all the remaining production from combustible is allocated to coal 
  mutate(coal = rowSums(.[,c("negcombustible", 
                                  "Electricity production from Fuel Oil",
                                  "Electricity production from Gas/Diesel Oil",
                                  "Electricity production from Natural gas")], 
                        na.rm = TRUE),
         coal = - coal) %>%
  #rename to allow calculation
  rename("other" = "Electricity production from Other sources") %>%
  #correct Uganda here (missing other sources production, resulting in wrong coal calculation
  mutate(
    other = case_when(
      country == "Uganda" ~ coal, 
      TRUE ~ other),
    coal = case_when(
      country == "Uganda" ~ 0, 
      TRUE ~ coal)) %>%
  select(-negcombustible) %>%
  rename("Electricity production from Coal" = coal,
         "Electricity production from Other sources" = other) %>%
  pivot_longer(cols=-c("country", "product", "unit", "year"), 
               names_to = "flow", 
               values_to = "value")
  
# Recalculate the regions
Statistics_region <- Statistics_expanded %>%
  filter(!country %in% c(
    "AFRICA",
    "NORTHERN AFRICA",
    "EASTERN AFRICA",
    "WESTERN AFRICA",
    "CENTRAL AFRICA",
    "SOUTHERN AFRICA")) %>%
  mutate(region=case_when(
           country %in% c("Algeria",
                          "Egypt",
                          "Libya",
                          "Morocco",
                          "Sudan",
                          "Tunisia",
                          "Sahrawi Republic") ~ "North",
           country %in% c(
             "Burundi",
             "Comoros",
             "Djibouti",
             "Eritrea",
             "Ethiopia",
             "Kenya",
             "Madagascar",
             "Malawi",
             "Mauritius",
             "Rwanda",
             "Seychelles",
             "Somalia",
             "South sudan",
             "Tanzania",
             "Uganda") ~ "East",
           country %in% c(
             "Cameroon",
             "Central Africa Republic",
             "Chad",
             "Congo",
             "Democratic Republic of Congo",
             "Equatorial Guinea",
             "Gabon",
             "Sao Tome and Principe") ~ "Central",
           country %in% c(
             "Angola",
             #sic
             "Bostwana",
             "Eswatini",
             "Lesotho",
             "Mozambique",
             "Namibia",
             "South Africa",
             "Zambia",
             "Zimbabwe") ~ "South", 
           country %in% c(
             "Benin",
             "Burkina Faso",
             "Cape Verde",
             #sic
             "Cote d Ivoire",
             "Gambia",
             "Ghana",
             "Guinea",
             "Guinea Bissau",
             "Liberia",
             "Mali",
             "Mauritania",
             "Niger",
             "Nigeria",
             "Senegal",
             "Sierra Leone",
             "Togo") ~ "West")
  ) %>%
  select(c("country", "region", "flow", "product", "year", "value"))

north_countries <- Statistics_region %>%
  filter(region == "North") %>%
  group_by(product, flow, year) %>%
  summarize(value=sum(value, na.rm = TRUE)) %>%
  mutate(country="NORTHERN AFRICA")

south_countries <- Statistics_region %>%
  filter(region == "South") %>%
  group_by(product, flow, year) %>%
  summarize(value=sum(value, na.rm = TRUE)) %>%
  mutate(country="SOUTHERN AFRICA")

west_countries <- Statistics_region %>%
  filter(region == "West") %>%
  group_by(product, flow, year) %>%
  summarize(value=sum(value, na.rm = TRUE)) %>%
  mutate(country="WESTERN AFRICA")

east_countries <- Statistics_region %>%
  filter(region == "East") %>%
  group_by(product, flow, year) %>%
  summarize(value=sum(value, na.rm = TRUE)) %>%
  mutate(country="EASTERN AFRICA")

central_countries <- Statistics_region %>%
  filter(region == "Central") %>%
  group_by(product, flow, year) %>%
  summarize(value=sum(value, na.rm = TRUE)) %>%
  mutate(country="CENTRAL AFRICA")

all_countries <- Statistics_region %>%
  group_by(product, flow, year) %>%
  summarize(value=sum(value, na.rm = TRUE)) %>%
  mutate(country="AFRICA")

Statistics_region <- Statistics_region %>% 
  filter(!country %in% c(
    "AFRICA",
    "NORTHERN AFRICA",
    "EASTERN AFRICA",
    "WESTERN AFRICA",
    "CENTRAL AFRICA",
    "SOUTHERN AFRICA"
  )) %>%
  select(c("country", "flow", "product", "year", "value")) %>% 
  rbind(north_countries) %>% 
  rbind(south_countries) %>% 
  rbind(west_countries) %>% 
  rbind(east_countries) %>% 
  rbind(central_countries) %>% 
  rbind(all_countries)

# merge AFREC names with official names
Statistics_region <- Statistics_region %>% 
  merge(Officialmapping,
        by=c("country"),
        all = TRUE)
  
Statistics<- Statistics_region

unique(Statistics$country)
unique(Statistics$year)
unique(Statistics$flow)
unique(Statistics$product)

