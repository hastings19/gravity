############ R CODE!!!! ############

# lib
library(readr)
library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(ggrepel)
library(fixest)
library(broom)
library(haven)

# wd
setwd("~/Documents/OneDrive/Documents/MSc Econ/Sem 5 SS/MASTER THESIS/destat data")
destat <- read_excel("destat.xlsx")


############################# CLEAN #########################################
destat <- rename (destat, partner = Column1)

destat <- destat %>% fill(partner, .direction = "down")

unique_partners <- destat %>% distinct(partner)
write.csv(unique_partners, "unique_partners.csv", row.names = FALSE) #verify

unwanted_partners <- c(
  "American Oceania (until 2000)", 
  "Australian Oceania (until 2000)", 
  "Belgium and Luxembourg (until 1998)", 
  "Czechoslovakia (until 1992)", 
  "Netherlands Antilles (until 2012)",
  "Polar Regions (until 2000)",
  "Reunion (until 1996)", 
  "French Guiana (until 1996)", 
  "Svalbard (until 1996)",
  "Soviet Union (until 04/1992)",
  "Canary Islands (until 1996)",
  "Yugoslavia (until 04/1992)",
  "Yugoslavia (05/1992 - 12/1992)",
  "Serbia and Montenegro (01/1993 - 05/2005)",
  "Ceuta and Mellila (until 1998)",
  "Confidential countries",
  "Countries and territories not specified",
  "Countries not specified (intra-Community trade)",
  "Martinique (until 1996)",
  "Stores and provisions",
  "High seas (since 2013)",
  "Mayotte (until 2013)",
  "New Zealand Oceania (until 2000)"
)
destat <- destat[!(destat$partner %in% unwanted_partners), ]


destat$`Exports: Value` <- destat$`Exports: Value` * 1000
destat$`Exports: Value (US-Dollar)` <- destat$`Exports: Value (US-Dollar)` * 1000

destat$`Imports: Value` <- as.numeric(gsub(",", "", destat$`Imports: Value`))
destat$`Imports: Value (US-Dollar)` <- as.numeric(gsub(",", "", destat$`Imports: Value (US-Dollar)`))

destat$`Imports: Value` <- destat$`Imports: Value` * 1000
destat$`Imports: Value (US-Dollar)` <- destat$`Imports: Value (US-Dollar)` * 1000 #deal with real values

write.csv(destat, "destat02.csv", row.names = FALSE)

#ISO REP

#iso2
iso2_ref <- read.csv2("iso2.csv")

destat <- destat %>%
  left_join(iso2_ref, by = "partner")

destat %>% filter(is.na(partner_iso2)) %>% distinct(partner)

destat <- destat %>%
  mutate(partner_iso2 = if_else(partner == "Namibia", "NA", partner_iso2))

unique_partners <- destat %>% distinct(partner_iso2)

#iso3
iso3_ref <- read.csv2("iso.csv")

destat <- destat %>%
  left_join(iso3_ref %>% select(partner_iso2, partner_iso3), by = "partner_iso2")

destat %>% filter(is.na(partner_iso3)) %>% distinct(partner)

destat <- destat %>%
  mutate(partner_iso3 = case_when(
    partner == "Kosovo" ~ "XKX",
    partner == "Kosovo (since 06/2005)" ~ "XKX",
    partner == "Namibia" ~ "NAM",
    TRUE ~ partner_iso3  # keep existing values
  ))

#### GDP from the world bank group
gdp <- read_excel("gdp.xlsx") 

gdp <- gdp %>%
  mutate(period = as.numeric(period))

destat <- destat %>%
  left_join(gdp, by = c("partner_iso3", "period"))

#### HIGH IMPACT SECTORS (CSDDD)

high_impact_codes <- c(
  # Textiles, leather, footwear
  "WA41", "WA42", "WA43", "WA50", "WA51", "WA52", "WA53", "WA54", "WA55", "WA56", "WA57", "WA58", "WA59", "WA60", "WA61", "WA62", "WA63", "WA64",
  
  # Agriculture, forestry, fisheries, food, beverages
  "WA01", "WA02", "WA03", "WA04", "WA05", "WA06", "WA07", "WA08", "WA09", "WA10", "WA11", "WA12", "WA13", "WA14", "WA15", "WA16", "WA17", "WA18", "WA19", "WA20", "WA21", "WA22", "WA23", "WA24", "WA44",
  
  # Extraction of mineral resources
  "WA25", "WA26", "WA27", "WA71", "WA72", "WA73", "WA74", "WA75", "WA76", "WA78", "WA79", "WA80", "WA81",
  
  # Manufacture of mineral and metal products
  "WA68", "WA69", "WA70", "WA82", "WA83",
  
  # Intermediate products, fuels, chemicals
  "WA28", "WA29", "WA30", "WA31", "WA32", "WA33", "WA34", "WA39", "WA40"
)

destat$high_impact <- ifelse(destat$`product code eu` %in% high_impact_codes, 1, 0)

##### CORRUPTION PERCEPTION INDEX

cpi <- read_excel("cpi2.xlsx") 
destat <- left_join(destat, cpi, by = c("period", "partner_iso3"))

#### WORKERS RIGHTS INDEX
wi <- read.csv2("workers_rights_index.csv")

wi_clean <- wi %>%
  distinct(partner_iso3, period, .keep_all = TRUE)

destat <- left_join(destat, wi_clean %>% select(partner_iso3, period, level),
                    by = c("partner_iso3", "period"))

#### LKSG

destat$lksg1 <- ifelse(destat$period == 2023, 1, 0)
destat$lksg2 <- ifelse(destat$period == 2024, 1, 0)
destat$lksg <- ifelse(destat$period %in% c(2023, 2024), 1, 0)


##### LDC STATUS
ldc_iso2 <- c(
  "AF", "AO", "BD", "BJ", "BF", "BI", "KH", "CF", "TD",
  "KM", "CD", "DJ", "ER",
  "ET", "GM", "GN", "GW", "HT", "KI",
  "LA", "LS", "LR", "MW", "ML", "MR", "MG",
  "MZ", "MM", "NP", "TL", "TG",
  "NE", "RW", "SN", "SL", "SB", "SO", "SS", "SD",
  "TV", "UG", "TZ", "YE", "ZM"
)

destat$ldc_dummy <- ifelse(destat$partner_iso2 %in% ldc_iso2, 1, 0)

destat$ldc_dummy <- ifelse(destat$partner_iso2 == "VU" & destat$period <= 2020, 1, destat$ldc_dummy)
destat$ldc_dummy <- ifelse(destat$partner_iso2 == "BT" & destat$period <= 2023, 1, destat$ldc_dummy)
destat$ldc_dummy <- ifelse(destat$partner_iso2 == "ST" & destat$period <= 2024, 1, destat$ldc_dummy) #recent graduations


### PERIOD SELECTION
destat <- destat[destat$period >= 2020, ]


### DE GPD
gdp_values <- data.frame(
  period = 2020:2024,
  gdp_de = c(
    3940142541354.1,
    4348297440387.53,
    4163596357879.39,
    4525703903627.53,
    4659929336890.62
  )
)
destat <- destat %>%
  left_join(gdp_values, by = "period")
###################################################################
write.csv(destat, "~/Documents/OneDrive/Documents/MSc Econ/Sem 5 SS/MASTER THESIS/destat data/destat2020.csv", row.names = FALSE)
rm(list = ls())
cat("\014")

destat2020 <- read_dta("~/Documents/OneDrive/Documents/MSc Econ/Sem 5 SS/Master thesis/destat data/destat2020.dta")

####### COUNTRY LEVEL SUBDATASET 
destat_small <- destat2020 %>%
  group_by(period, partner_iso3) %>%
  summarise(
    importsvalue = sum(importsvalue, na.rm = TRUE),
    lksg1 = first(lksg1),
    lksg2 = first(lksg2),
    cpi = first(cpi),
    level = first(level),
    ldc_dummy = first(ldc_dummy),
    gdp = first(gdp),
    gdp_de = first(gdp_de),
    .groups = "drop"
  )
destat_small$lksg <- ifelse(destat_small$period %in% c(2023, 2024), 1, 0)






############## SIMPLE STATS #########################################

##### FIGURE 1

itpde <- read_csv("Downloads/ITPDE_R03.csv")

itpde22 <- itpde %>% 
  filter(year >= 2016 & year <= 2022)
iso_keep <- c(
  "AUT","BEL","BGR","HRV","CYP","CZE","DNK","EST","FIN","FRA","DEU",
  "GRC","HUN","IRL","ITA","LVA","LTU","LUX","MLT","NLD","POL","PRT",
  "ROU","SVK","SVN","ESP","SWE","USA","CHN")
itpde22a <- itpde22 %>%
  filter(importer_iso3 %in% iso_keep)
write_csv(itpde22a, "Desktop/itpde22a.csv")
itpde22a <- read_csv("Desktop/itpde22a.csv")
itpde22ab <- itpde22a %>%
  group_by(importer_iso3, year) %>%
  summarise(trade = sum(trade, na.rm = TRUE), .groups = "drop")
eu_iso <- c(
  "AUT","BEL","BGR","HRV","CYP","CZE","DNK","EST","FIN","FRA","DEU",
  "GRC","HUN","IRL","ITA","LVA","LTU","LUX","MLT","NLD","POL","PRT",
  "ROU","SVK","SVN","ESP","SWE")
itpde22abc <- itpde22ab %>%
  mutate(importer_iso3 = if_else(importer_iso3 %in% eu_iso, "EUE", importer_iso3)) %>%
  filter(importer_iso3 %in% c("EUE", "USA", "CHN")) %>%
  group_by(importer_iso3, year) %>%
  summarise(trade = sum(trade, na.rm = TRUE), .groups = "drop")
itpde22abc <- itpde22abc %>% 
  filter(year >= 2016 & year <= 2021)
ggplot(itpde22abc, aes(x = year, y = trade / 1e7, color = importer_iso3, group = importer_iso3)) +
  geom_line(size = 1.2) +
  scale_y_continuous(labels = scales::comma, limits = c(0, NA)) +
  labs(
    title = "Import Value 2016 - 2021",
    x = "Year",
    y = "import value in trillions",
    color = "Importer"
  ) +
  theme_minimal(base_size = 14)





####### FIGURE 3
destat <- read_dta("Documents/OneDrive/Documents/MSc Econ/Sem 5 SS/MASTER THESIS/destat data/destat2020.dta")
setwd("~/Documents/OneDrive/Documents/MSc Econ/Sem 5 SS/Master thesis/destat data/Post regression")
saveRDS(destat, file="destat2020_for_summary.rds")
destat2020$wri_dummy <- ifelse(destat$level %in% c(5, 6), 1, 0)
#wri dummy = 0 for countries with good worker rights, = 1 for countries with bad worker rights. 

line_data <- destat2020 %>% 
  group_by(period, wri_dummy) %>% 
  summarise (total_imports = sum(importsvalue, na.rm = TRUE), .groups = "drop")

ggplot(line_data, aes(x=period, y=total_imports / 1e11 , color=factor(wri_dummy)))+
  geom_line(size=1.2)+
  labs(title = "",
       x= "period",
       y= "Imports in hundred billions",
       color = "WRI")+
  theme_minimal()+
  expand_limits(y = 0)


### FIGURE 4
line_data2 <- destat2020 %>% 
  group_by(period, ldc_dummy) %>% 
  summarise (total_imports = sum(importsvalue, na.rm = TRUE), .groups = "drop")

base_period <- min(line_data2$period)

line_data2 <- line_data2 %>%
  group_by(ldc_dummy) %>%
  mutate(log_imports = log(total_imports),
         log_diff = log_imports - log_imports[period == base_period])

ggplot(line_data2, aes(x = period, y = log_diff, color = factor(ldc_dummy))) +
  geom_line(size = 1.2) +
  labs(title = "Log-Difference of Imports Over Time (Relative to Base Period)",
       x = "Period", y = "Log-Difference (relative to base period)",
       color = "LDC Status") +
  theme_minimal()


#### FIGURE 5
line_data_hig <- destat %>%
  group_by(period, high_impact) %>%
  summarise(total_imports = sum(importsvalue, na.rm = TRUE), .groups = "drop") %>%
  group_by(high_impact) %>%
  mutate(log_imports = log(total_imports),
         log_diff = log_imports - log_imports[period == base_period])

ggplot(line_data_hig, aes(x = period, y = log_diff, color = factor (high_impact))) +
  geom_line(size = 1.2) +
  labs (title = "",
        x = "Period",
        y = "log difference",
        color = "High Impact") +
  theme_minimal() +
  expand_limits(y = 0)

#### FIGURE 6

line_data_hi <- destat %>%
  filter(ldc_dummy == 1) %>% 
  group_by(period, high_impact) %>%
  summarise(total_imports = sum(importsvalue, na.rm = TRUE), .groups = "drop") %>%
  group_by(high_impact) %>%
  mutate(log_imports = log(total_imports),
         log_diff = log_imports - log_imports[period == base_period])

ggplot(line_data_hi, aes(x = period, y = log_diff, color = factor (high_impact))) +
  geom_line(size = 1.2) +
  labs (title = "",
        x = "Period",
        y = "log difference",
        color = "High Impact") +
  theme_minimal() +
  expand_limits(y = 0)











