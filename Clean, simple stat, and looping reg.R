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

# wd
setwd("~/Documents/OneDrive/Documents/MSc Econ/Sem 5 SS/MASTER THESIS/destat data")
destat <- read_excel("destat.xlsx")


#### CLEAN
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
  "WA41", "WA42", "WA43", "WA50", "WA51", "WA52", "WA53", "WA54",
  "WA55", "WA56", "WA57", "WA58", "WA59", "WA60", "WA61", "WA62", "WA63", "WA64",
  
  # Agriculture, forestry, fisheries, food, beverages
  "WA01", "WA02", "WA03", "WA04", "WA05", "WA06", "WA07", "WA08", "WA09",
  "WA10", "WA11", "WA12", "WA13", "WA14", "WA15", "WA16", "WA17", "WA18",
  "WA19", "WA20", "WA21", "WA22", "WA23", "WA24", "WA44",
  
  # Extraction of mineral resources
  "WA25", "WA26", "WA27", "WA71", "WA72", "WA73", "WA74", "WA75", "WA76",
  "WA78", "WA79", "WA80", "WA81",
  
  # Manufacture of mineral and metal products
  "WA68", "WA69", "WA70", "WA82", "WA83",
  
  # Intermediate products, fuels, chemicals
  "WA28", "WA29", "WA30", "WA31", "WA32", "WA33", "WA34",
  "WA39", "WA40"
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

##### SIMPLE STATS

# Total Imports Value, by LDC
line_data <- destat %>%
  group_by(period, ldc_dummy) %>%
  summarise(total_imports = sum(importsvalue, na.rm = TRUE), .groups = "drop")

ggplot(line_data, aes(x = period, y = total_imports, color = factor(ldc_dummy))) +
  geom_line(size = 1.2) +
  labs(title = "Total Imports Over Time by LDC Status",
       x = "Period",
       y = "Total Imports Value",
       color = "LDC Status") +
  theme_minimal()

# ldc import log difference 
base_period <- min(line_data$period)
line_data <- line_data %>%
  group_by(ldc_dummy) %>%
  mutate(log_imports = log(total_imports),
         log_diff = log_imports - log_imports[period == base_period])

ggplot(line_data, aes(x = period, y = log_diff, color = factor(ldc_dummy))) +
  geom_line(size = 1.2) +
  labs(title = "Log-Difference of Imports Over Time (Relative to Base Period)",
       x = "Period", y = "Log-Difference (relative to base period)",
       color = "LDC Status") +
  theme_minimal()


# high impact
ggplot(line_data, aes(x = period, y = total_imports, color = factor(high_impact))) +
  geom_line(size = 1.2) +
  labs(title = "Total Imports Over Time (LDCs Only), by High-Impact Status",
       x = "Period",
       y = "Total Imports Value",
       color = "High Impact") +
  theme_minimal()


# import value ordering
ldc_weight <- destat %>%
  filter(ldc_dummy == 1) %>%
  group_by(product_category) %>%
  summarise(importsvalue = sum(importsvalue, na.rm = TRUE)) %>%
  mutate(
    above_4m = as.integer(importsvalue >= 4e6),
    above_40m = as.integer(importsvalue >= 4e7)
  ) %>%
  arrange(desc(importsvalue))

####### COUNTRY LEVEL SUBDATASET 
destat_small <- destat %>%
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


####### LOOPING REGRESSION

destat <- destat %>%
  mutate(
    product_category = as.factor(product_category),
    partner__iso3 = as.factor(partner__iso3)
  )
results_list <- list()
for (cat in unique(destat$product_category)) {
  data_subset <- destat %>% filter(product_category == cat)
    if (nrow(data_subset) > 50) { #Central limit theorem
    try({
      model <- feglm(
        importsvalue ~ gdp_de + gdp + lksg1 + lksg2 + ldc_lksg1 + ldc_lksg2 + ldc_dummy | partner__iso3,
        data = data_subset,
        family = poisson()
      )
      
      tidy_res <- tidy(model) %>%
        mutate(
          product_category = cat,
          stars = case_when(
            p.value < 0.001 ~ "***",
            p.value < 0.01 ~ "**",
            p.value < 0.05 ~ "*",
            p.value < 0.1 ~ ".",
            TRUE ~ ""
          ),
          estimate = paste0(round(estimate, 4), stars)
        ) %>%
        select(product_category, term, estimate)
      
      results_list[[as.character(cat)]] <- tidy_res
    }, silent = TRUE)
  }
}
final_table <- bind_rows(results_list)

# print(final_table)

