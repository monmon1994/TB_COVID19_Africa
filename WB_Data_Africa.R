# COVID Data and World Bank Data for Africa

library(wbstats)
library(tidyverse)
library(tidycovid19)
library(scales)
library(ggrepel)

# World Bank Data
# Load the data from the stats
wb_africa <- wb(country = c("DZA", "AGO", "BEN", "BWA", "BFA", "BDI", "CV", "CMR", "CAF", "TCD", "COM", "COD",
                            "COG", "CIV", "DJI", "EGY", "ERI", "SWZ", "ETH", "GAB", "GMB", "GHA", "GIN", "GNB",
                            "KEN", "LSO", "LBR", "LBY", "MDG", "MWI", "MLI", "MRT", "MUS", "MAR", "MOZ", "NAM",
                            "NER", "NGA", "RWA", "STP", "SEN", "SYC", "SLE", "SOM", "ZAF", "SSD", "SDN", "TZA",
                            "TGO", "TUN", "UGA", "ZMB", "ZWE"),
                indicator = c("SH.MED.PHYS.ZS", "SH.XPD.CHEX.GD.ZS", "SH.XPD.OOPC.CH.ZS",
                              "SH.STA.DIAB.ZS", "SH.PRV.SMOK", "SH.DYN.NCOM.ZS", "SH.STA.BASS.ZS",
                              "SP.POP.65UP.TO.ZS", "NY.GDP.MKTP.CD", "EN.POP.DNST", "SH.MED.BEDS.ZS",
                              "SH.STA.HYGN.ZS", "NY.GDP.PCAP.CD"), 
                startdate = 2015,
                enddate = 2018, POSIXct = T, return_wide = T, removeNA = T)

## COVID DATA AFRICA
df <- download_jhu_csse_covid19_data(silent = T, cached = T)
df <- df[['country']]

df_africa <- df %>% 
    filter(iso3c %in% c("DZA", "AGO", "BEN", "BWA", "BFA", "BDI", "CV", "CMR", "CAF", "TCD", "COM", "COD",
                        "COG", "CIV", "DJI", "EGY", "ERI", "SWZ", "ETH", "GAB", "GMB", "GHA", "GIN", "GNB",
                        "KEN", "LSO", "LBR", "LBY", "MDG", "MWI", "MLI", "MRT", "MUS", "MAR", "MOZ", "NAM",
                        "NER", "NGA", "RWA", "STP", "SEN", "SYC", "SLE", "SOM", "ZAF", "SSD", "SDN", "TZA",
                        "TGO", "TUN", "UGA", "ZMB", "ZWE")) %>% 
    group_by(country) %>% 
    mutate(
        total_confirmed = max(confirmed),
        total_deaths = max(deaths)
    ) %>% 
    select(country, total_confirmed, total_deaths, iso3c) %>% 
    distinct() %>% 
    ungroup() %>% 
    arrange(-total_deaths)

merged <- merge(wb_africa, df_africa, by = "iso3c") # merge the df's

save(merged, file = "data/merged.RData")

########### plots 

pal <- wesanderson::wes_palette("BottleRocket2", n = 51, type = "continuous")

# AGE 65 above 

merged %>% 
    filter(date == 2018) %>% 
ggplot() +
    geom_point(aes(x = SP.POP.65UP.TO.ZS, y = total_confirmed, colour = country.x, size = 3),
               show.legend = FALSE) +
    scale_fill_gradientn(colours = pal) +
    scale_y_log10() +
    geom_label_repel(aes(x = SP.POP.65UP.TO.ZS, y = total_confirmed, label = country.x)) +
    guides(fill=FALSE) +
    xlab("Population ages 65 and above (% of total)") +
    ylab("COVID-19 Confirmed Cases") +
    theme_classic() 

# SMOKING SH.PRV.SMOK

ggplot(merged) +
    geom_point(aes(x = SH.PRV.SMOK, y = total_confirmed, colour = country.x, size = 3),
               show.legend = FALSE) +
    scale_fill_gradientn(colours = pal) +
    scale_y_log10() +
    geom_label_repel(aes(x = SH.PRV.SMOK, y = total_confirmed, label = country.x)) +
    guides(fill=FALSE) +
    xlab("Smoking prevalence, total, ages 15+") +
    ylab("COVID-19 Confirmed Cases") +
    theme_classic() 

# Physicians per 1,000 SH.MED.PHYS.ZS

ggplot(merged) +
    geom_point(aes(x = SH.MED.PHYS.ZS, y = total_confirmed, colour = country.x, size = 3),
               show.legend = FALSE) +
    scale_fill_gradientn(colours = pal) +
    scale_y_log10() +
    geom_label_repel(aes(x = SH.MED.PHYS.ZS, y = total_confirmed, label = country.x)) +
    guides(fill=FALSE) +
    xlab("Physicians (1,000 per people)") +
    ylab("COVID-19 Confirmed Cases") +
    theme_classic()

# Non-communicable diseases SH.DTH.NCOM.ZS

ggplot(merged) +
    geom_point(aes(x = SH.DYN.NCOM.ZS, y = total_confirmed, colour = country.x, size = 3),
               show.legend = FALSE) +
    scale_fill_gradientn(colours = pal) +
    scale_y_log10() +
    geom_label_repel(aes(x = SH.DYN.NCOM.ZS, y = total_confirmed, label = country.x)) +
    guides(fill=FALSE) +
    xlab("Mortality from CVD, cancer, diabetes or 
         CRD between exact ages 30 and 70 (%)") +
    ylab("COVID-19 Confirmed Cases") +
    theme_classic()

# SH.STA.DIAB.ZS no data 


# SH.STA.HYGN.ZS basic hygiene services

merged %>% 
    filter(date == 2017) %>% 
ggplot() +
    geom_point(aes(x = SH.STA.HYGN.ZS, y = total_confirmed, colour = country.x, size = 3),
               show.legend = FALSE) +
    scale_fill_gradientn(colours = pal) +
    scale_y_log10() +
    guides(fill=FALSE, size = F) +
    gghighlight::gghighlight(use_direct_label = T) +
    xlab("People with basic handwashing facilities including soap and water (% of population)") +
    ylab("COVID-19 Confirmed Cases") +
    theme_classic()

# NY.GDP.MKTP.CD 

merged %>% 
    filter(date == 2018) %>% 
    ggplot() +
    geom_point(aes(x = NY.GDP.MKTP.CD, y = total_confirmed, colour = country.x, size = 3),
               show.legend = FALSE) +
    scale_fill_gradientn(colours = pal) +
    scale_y_log10() +
    scale_x_log10() +
    guides(fill=FALSE, size = F) +
    gghighlight::gghighlight(use_direct_label = T) +
    xlab("GDP (current USD)") +
    ylab("COVID-19 Confirmed Cases") +
    theme_classic()

# NY.GDP.PCAP.CD GDP per capita

merged %>% 
    filter(date == 2018) %>% 
ggplot() +
    geom_point(aes(x = NY.GDP.PCAP.CD, y = total_confirmed, colour = country.x, size = 3),
               show.legend = FALSE) +
    scale_fill_gradientn(colours = pal) +
    scale_y_log10() +
    scale_x_log10() +
    guides(fill=FALSE, size = F) +
    gghighlight::gghighlight(use_direct_label = T) +
    xlab("GDP (current USD)") +
    ylab("COVID-19 Confirmed Cases") +
    theme_classic()

# Health expenditure "SH.XPD.CHEX.GD.ZS"

merged %>% 
    filter(date == 2017) %>% 
    ggplot() +
    geom_point(aes(x = SH.XPD.CHEX.GD.ZS, y = total_confirmed, colour = country.x, size = 3),
               show.legend = FALSE) +
    scale_fill_gradientn(colours = pal) +
    scale_y_log10() +
    guides(fill=FALSE, size = F) +
    gghighlight::gghighlight(use_direct_label = T) +
    xlab("Current health expenditure (% of GDP)") +
    ylab("COVID-19 Confirmed Cases") +
    theme_classic()

# SH.XPD.OOPC.CH.ZS  "Out-of-pocket expenditure (% of current health expenditure)"

merged %>% 
    filter(date == 2017) %>% 
    ggplot() +
    geom_point(aes(x = SH.XPD.OOPC.CH.ZS, y = total_confirmed, colour = country.x, size = 3),
               show.legend = FALSE) +
    scale_fill_gradientn(colours = pal) +
    scale_y_log10() +
    guides(fill=FALSE, size = F) +
    gghighlight::gghighlight(use_direct_label = T) +
    xlab("Out-of-pocket expenditure (% of current health expenditure)") +
    ylab("COVID-19 Confirmed Cases") +
    theme_classic()

