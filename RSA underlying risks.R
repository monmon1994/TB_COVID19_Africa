library(nCov2019)
library(tidyverse)
library(readr)
library(lubridate)
library(wbstats)
library(ggrepel)
library(countrycode)
library(readr)
 # COVID data 

df_current <- get_nCov2019(lang = 'en') # totals as of today

df <- load_nCov2019() # historial data

df_A <- df['global']

df_A %>% 
    as_tibble %>% 
    filter(country != "China") %>% 
    rename(confirm=cum_confirm) %>%
    group_by(country)

df_A$iso3c <- countrycode(df_A$country, origin = 'country.name', destination = "iso3c")

df_africa <- df_A %>% 
    as_tibble %>%
    rename(confirm=cum_confirm) %>%
    filter(iso3c %in% c("DZA", "AGO", "BEN", "BWA", "BFA", "BDI", "CV", "CMR", "CAF", "TCD", "COM", "COD",
                        "COG", "CIV", "DJI", "EGY", "ERI", "SWZ", "ETH", "GAB", "GMB", "GHA", "GIN", "GNB",
                        "KEN", "LSO", "LBR", "LBY", "MDG", "MWI", "MLI", "MRT", "MUS", "MAR", "MOZ", "NAM",
                        "NER", "NGA", "RWA", "STP", "SEN", "SYC", "SLE", "SOM", "ZAF", "SSD", "SDN", "TZA",
                        "TGO", "TUN", "UGA", "ZMB", "ZWE")) %>%
    group_by(country) %>%
    mutate(days_since_100 = as.numeric(time - min(time))) %>%
    ungroup 

pal <- wes_palette("BottleRocket2", n = 35, type = "continuous")

# Health of RSA population and COVID-19
# People most at risk of COVID = Older Adults (65 and over), People with Asthma, HIV, Heart conditions, Obesity, Diabetes, 
# Chronic kidney disease, liver disease, TB 

# age indicator = SP.POP.65UP.TO.ZS
# Incidence of HIV (per 1,000 uninfected pop age 15-49) = SH.HIV.INCD.ZS
# SH.DYN.AIDS.ZS = Prevalence of HIV, total (% of population ages 15-49)
# SH.DYN.AIDS = Adults (ages 15+) living with HIV
    
HIV <- wbsearch(pattern = "HIV", fields = "indicator")

HIV_RSA <- wb(country = "ZAF", indicator = c("SH.HIV.INCD.ZS", "SH.DYN.AIDS.ZS", "SH.DYN.AIDS"), startdate = 2015,
              enddate = 2019, POSIXct = T, return_wide = T, removeNA = T, include_lastUpdated = T)


covid_rsa <- subset(df_current['global'], name == 'South Africa')
covid_rsa <- gather(RSA, curve, count, -time, -country)

rsa_merged <- merge(covid_rsa, HIV_RSA, by.x = "country", by.y = "country")

rsa_merged_current <- rsa_merged[256,]

breaks=c(100, 200, 500, 1000, 2000, 5000, 10000, 20000, 50000, 100000)



#probability-of-dying-between-exact-ages-30-and-70-from-any-of-cardiovascular-disease-cancer-diabetes-or-chronic-respiratoryRSA

non_commun_2016_rsa <- data.frame("total" = 26.2, "men" = 32.2, "women" = 21.2)
non_commun_2016_africa <- data.frame("total" = 20.56, "men" = 21.13, "women" = 20.07)

# Obesity in RSA

obesity_over_rsa <- data.frame("men" = 31.0, "women" = 68.0) # one in 5 women in RSA have a BMI of >35.0 (severe obesity)
hyperten_rsa <- data.frame("men" = 44.0, "women" = 46.0) # hypertensive based on their systolic blood pressure being above
# 140mmHg, their diastolic blood pressure being above 90 mmHg or taking antihypertensive medication

# smoking in RSA

smokers_rsa <- data.frame("men" = 28.0, "women" = 05.0)

# cause of death RSA 2017

death_TB <- data.frame("men" = 07.6, "women" = 05.1, "total" = 06.4)
death_diabetes <- data.frame("men" = 04.2, "women" = 07.3, "total" = 05.7)
death_HIV <- data.frame("men" = 04.7,"women" = 04.9, "total" = 04.8)
death_TB_age <- data.frame("1_14" = 03.9, "15_44" = 11.3, "45_64" = 07.9, ">65" = 02.5)

# TB global statistics

tb_global <- read_csv("data/TB_WHO_data.csv")

tb_global$Year <- strptime(tb_global$Year, "%Y")
tb_global$Year <- format(tb_global$Year, "%Y")


tb_rsa <- tb_global %>% 
    filter(Country == "South Africa") 


ggplot(data = tb_rsa, aes(x = Year)) +
    geom_col(aes(y = `New cases tested for RR-/MDR-TB (%)`)) 
    geom_col(aes(y = `Previously treated cases tested for RR-/MDR-TB (%)`))

# Health Workers 

h_workers <- read_csv("HWF_0024.csv")

# PLOTS for RISKS

who <- read_csv("fusion_GLOBAL_DATAFLOW_UNICEF_1.0_all.csv")
