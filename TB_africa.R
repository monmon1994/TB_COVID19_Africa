# TB data 

library(getTBinR)
library(tidyverse)
library(extrafont)
loadfonts(device = "win")

map_tb_burden(metric = "e_inc_100k")

plot_tb_burden_overview(metric = "e_inc_100k",
                        countries = "South Africa",
                        compare_to_region = TRUE,
                        interactive = FALSE)

plot_tb_burden(metric = "e_inc_100k",
               countries = "South Africa",
               interactive = FALSE)

tb_df <- get_tb_burden(additional_datasets = "all", verbose = FALSE)

tb_budget <- get_tb_burden(additional_datasets = "Budget", verbose = F)


tb_africa <- tb_df %>% 
  filter(iso3 %in% c("DZA", "AGO", "BEN", "BWA", "BFA", "BDI", "CV", "CMR", "CAF", "TCD", "COM", "COD",
                     "COG", "CIV", "DJI", "EGY", "ERI", "SWZ", "ETH", "GAB", "GMB", "GHA", "GIN", "GNB",
                     "KEN", "LSO", "LBR", "LBY", "MDG", "MWI", "MLI", "MRT", "MUS", "MAR", "MOZ", "NAM",
                     "NER", "NGA", "RWA", "STP", "SEN", "SYC", "SLE", "SOM", "ZAF", "SSD", "SDN", "TZA",
                     "TGO", "TUN", "UGA", "ZMB", "ZWE"))


tb_africa %>% 
  filter(year == 2018) %>% 
  arrange(desc(e_inc_100k)) %>% 
  select(country, e_inc_100k, e_mort_num, year) %>% 
  head(50)

top_5 <- tb_africa %>% 
  filter(country %in% c("South Africa", "Nigeria", "Democratic Republic of Congo", "Mozambique", "United Republic of Tanzania")) %>% 
  select(country, e_inc_100k, e_mort_num, year)

breaks <- c(2000, 2002, 2004, 2006, 2008, 2010, 2012, 2014, 2016, 2018)

top_5 %>% 
  filter(year >= 2000) %>% 
ggplot(aes(x = year)) +
  geom_line(aes(y = e_mort_num, color = country), size = 1) +
  scale_x_continuous(breaks = breaks, labels = breaks) +
  labs(x = "", y = "Number of TB deaths", title = "Tuberculosis Deaths between 2000 and 2018",
       caption = "Source: World Health Organisation | @Mon_Bennett8") +
  theme_light(base_size = 12, base_family = "Calibri")


top_inc <- tb_africa %>% 
  filter(country %in% c("South Africa", "Nigeria", "Democratic Republic of Congo", "Mozambique", "Ethiopia")) %>% 
  select(country, e_inc_100k, e_mort_num, year)

top_inc %>% 
  filter(year >= 2000) %>% 
  ggplot(aes(x = year)) +
  geom_line(aes(y = e_mort_num, color = country), size = 1) +
  scale_x_continuous(breaks = breaks, labels = breaks) +
  labs(x = "", y = "Number of TB incidences per 100 000 of population", 
       title = "Estimated incidences of all forms of TB between 2000 and 2018",
       caption = "Source: World Health Organisation | @Mon_Bennett8") +
  theme_light(base_size = 12, base_family = "Calibri")

