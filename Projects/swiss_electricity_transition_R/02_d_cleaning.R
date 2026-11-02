library(tidyverse)
library(janitor)
library(data.table)
library(readxl)
library(stringr)
library(lubridate)

# import data----

# starting with the small data set 
            # - by month and contains types as variables
            # - all the numbers are in GWh - this needs to be converted 
            # - will not only partial column names and will rename columns after 
            #    (removed rows in in yellow)
                              
## energ Source overview ----

d.monthly_prod_1 <- read_excel(
                          "raw_data/5634-Zeitreihe_Elektrizitätsbilanz_Schweiz_Monatswerte.xlsx",
                          sheet = "Tabelle1",
                          range = "A13:P438") %>% 
  clean_names()

str(d.monthly_prod_1)


d.monthly_prod <- d.monthly_prod_1 %>% 
    rename(hydroelectric = hydrauliques,
           nuclear = nucleaires,
           thermic_clasique = thermiques_classique,
           thermic_renewable = thermiques,
           wind_thurbine = revise_des_2020,
           photovoltaic = photovoltaiques,
           acumulation_pumping = dacccumulation,
           net_production = nette,
           import = des_2000_physique_11,
           export = des_2000_physique_12,
           country_consumtion = du_pays,
           losses = pertes,
           final_consumtion = finale,
           export_import = importateur) %>% 
  mutate(across(
                c(thermic_renewable, wind_thurbine, photovoltaic),
                ~ as.numeric(na_if(., "n.a.")))) %>% 
  mutate(
    year = str_extract(date, "\\d{4}"),
    date_month = case_when(
                        str_detect(date, "Januar / Janvier") ~ paste0(year, "-01-01"),
                        str_detect(date, "Februar / Février") ~ paste0(year, "-02-01"),
                        str_detect(date, "März / Mars") ~ paste0(year, "-03-01"),
                        str_detect(date, "April / Avril") ~ paste0(year, "-04-01"),
                        str_detect(date, "Mai / Mai") ~ paste0(year, "-05-01"),
                        str_detect(date, "Juni / Juin") ~ paste0(year, "-06-01"),
                        str_detect(date, "Juli / Juillet") ~ paste0(year, "-07-01"),
                        str_detect(date, "August / Août") ~ paste0(year, "-08-01"),
                        str_detect(date, "September / Septembre") ~ paste0(year, "-09-01"),
                        str_detect(date, "Oktober / Octobre") ~ paste0(year, "-10-01"),
                        str_detect(date, "November / Novembre") ~ paste0(year, "-11-01"),
                        str_detect(date, "Dezember / Décembre") ~ paste0(year, "-12-01"),
                        TRUE ~ NA_character_
                      ),
    date_month = ymd(date_month)) %>% 
  mutate(date_label = format(date_month, "%b-%Y")) %>% 
  relocate(date_month, .after = date) %>% 
  relocate(date_label, .after = date_month) %>% 
  select(-date, -year)

# we keep date_month and date_label as I do not know how/if I'm doing the join 
    

#glimpse(d.monthly_prod) 
# used to view the structure of the data every time i make another change to the data set

str(d.monthly_prod)

saveRDS(d.monthly_prod, "RDS/d.monthly_prod.rds")
# we will load the rds file into EDA and Querto files.


monthly_prod_metadata <- tibble::tibble(
  variable_name = c(
    "date_month",
    "date_label",
    "hydroelectric",
    "nuclear", 
    "thermic_clasique", 
    "thermic_renewable",
    "wind_thurbine", 
    "photovoltaic",
    "total",
    "acumulation_pumping",
    "net_production",
    "import",
    "export", 
    "country_consumtion", 
    "losses",
    "final_consumtion",
    "export_import"
  ),
  description = c(
    "Monthly timestamp (first day of each month, parsed from multilingual date string)",
    "Month label in short format (e.g., Jan-2020) for plotting or reporting",
    
    "Monthly electricity production from hydroelectric sources (in GWh)",
    "Monthly electricity production from nuclear power plants (in GWh)",
    "Electricity production from classic thermal (fossil fuel-based) sources (in GWh)",
    "Electricity production from renewable thermal sources (e.g., biogas, waste) (in GWh)",
    
    "Electricity generated from wind turbines (in GWh)",
    "Electricity produced from photovoltaic (solar) sources (in GWh)",
    "Total electricity production from all sources (hydro, nuclear, thermal, renewables) (in GWh)",
    
    "Electricity used for pumping water into accumulation lakes (in GWh, i.e., storage)",
    "Net electricity production (total production - pumping/storage use) (in GWh)",
    
    "Gross imports of electricity from neighboring countries (in GWh)",
    "Gross exports of electricity to neighboring countries (in GWh)",
    "Total electricity used domestically, including grid and industrial use (in GWh)",
    "Grid losses (distribution/transmission) (in GWh)",
    "Final consumption by Swiss end users (households, services, industry) (in GWh)",
    
    "Net importer/exporter status (1 = net importer, 0 = net exporter)"
  ),
  source = "Swiss Federal Office of Energy (SFOE)",
  unit = c(
    "YYYY-MM-DD", "string",
    rep("GWh", 14),  # Updated to reflect 14 GWh-based variables
    "binary (0/1)"
  )
)

write_rds(monthly_prod_metadata, "RDS/monthly_prod_metadata.rds")



##weather_data---- 

#Links:
# https://data.geo.admin.ch/ch.meteoschweiz.ogd-nbcn/alt/ogd-nbcn_alt_m.csv
# https://data.geo.admin.ch/ch.meteoschweiz.ogd-nbcn/stg/ogd-nbcn_stg_m.csv


#just importing metadata to read about what columns we need 
meta_meteo_data <- read.csv("raw_data/weather_data/ogd-nbcn_meta_parameters.csv", sep = ";")

#basel_meteo <- read.csv("raw_data/weather_data/ogd-nbcn_bas_m.csv", sep = ";")

# importing from thewe
# alt <- read_delim("https://data.geo.admin.ch/ch.meteoschweiz.ogd-nbcn/alt/ogd-nbcn_alt_m.csv",
#                   delim = ";",
#                   locale = locale(encoding = "UTF-8"),
#                   show_col_types = FALSE)
# names(alt)
# 
# alt <- alt %>% 
#   select(station_abbr, reference_timestamp, ghs000m0) %>% 
#   mutate(date = as.Date(reference_timestamp, format = "%d.%m.%Y %H:%M")) %>% 
#   select(- reference_timestamp)
# 
# 
# bas1 <- read_delim("https://data.geo.admin.ch/ch.meteoschweiz.ogd-nbcn/bas/ogd-nbcn_bas_m.csv",
#                   delim = ";",
#                   locale = locale(encoding = "UTF-8"),
#                   show_col_types = FALSE)
# names(bas)
# 
# bas <- bas1 %>% 
#   select(station_abbr, reference_timestamp, ghs000m0) %>% 
#   mutate(date = as.Date(reference_timestamp, format = "%d.%m.%Y %H:%M")) %>% 
#   filter(date >= as.Date("2020-01-01")) %>% 
#   select(- reference_timestamp) %>% 
#   sort(date)


# Dowloaded allfiles 
# creating a function to dowload all the stations, biding the data, 
# since data will be in long format will group by year and summarize 


stations <- c("alt", "bas", "ber", "cdf", "eng",
              "grh", "gsb", "gve", "jun", "lug",
              "luz", "neu", "otl", "pay", "sae",
              "sam", "sbe", "sio", "sma", "stg")




fetch_station <- function(station) {
  url <- paste0("https://data.geo.admin.ch/ch.meteoschweiz.ogd-nbcn/",
                station, "/ogd-nbcn_", station, "_m.csv")
  
  weather <- read_delim(url,
                   delim = ";",
                   locale = locale(encoding = "UTF-8"),
                   show_col_types = FALSE)
  
  weather <- weather %>%
    select(station_abbr, reference_timestamp, ghs000m0) %>%
    mutate(date = as.Date(reference_timestamp, format = "%d.%m.%Y %H:%M")) %>%
    filter(date >= as.Date("2020-01-01")) %>%
    select(-reference_timestamp) %>%
    arrange(date)
  
  return(weather)
}



weather_all <- map_dfr(stations, fetch_station)

d.radiation_ch <- weather_all %>%
  group_by(date) %>%
  summarise(global_radiation = mean(ghs000m0, na.rm = TRUE), .groups = "drop") %>% 
  rename(date_month = date)


write_rds(d.radiation_ch, "RDS/d.radiation_ch.rds")

str(d.radiation_ch)



radiation_metadata <- tibble::tibble(
  variable_name = c("date_month", "global_radiation"),
  description = c(
    "First day of each month (used as timestamp for monthly aggregated data)",
    "Monthly average global solar radiation across all Swiss MeteoSwiss stations (in MJ/m²)"
  ),
  source = c(
    "meteoswiss.ch",
    "meteoswiss.ch"
  ),
  processing_steps = c(
    "Dates represent the aggregation level (monthly); derived from daily values",
    "Downloaded monthly global radiation values from multiple stations; averaged across stations per month"
  ),
  unit = c("YYYY-MM-DD", "MJ/m²")
)

saveRDS(radiation_metadata, "RDS/radiation_metadata.rds")


# nuclear plants ----


d.nuclear_raw <- read_excel("raw_data/9606-Zeitreihe_Kernkraftwerke_der_Schweiz.xlsx",
                            sheet = "Tabelle1",
                            range = "A10:X66")


d.nuclear_raw <- d.nuclear_raw[, -c(5, 9, 13, 17, 21)]

colnames(d.nuclear_raw) <- c(
  "year",
  
  "beznau1_production_gwh", "beznau1_capacity_mwe", "beznau1_util_pct",
  "beznau2_production_gwh", "beznau2_capacity_mwe", "beznau2_util_pct",
  "muhleberg_production_gwh", "muhleberg_capacity_mwe", "muhleberg_util_pct",
  "gosgen_production_gwh", "gosgen_capacity_mwe", "gosgen_util_pct",
  "leibstadt_production_gwh", "leibstadt_capacity_mwe", "leibstadt_util_pct",
  "total_production_gwh", "total_capacity_mwe", "total_util_pct"
)

str(d.nuclear_raw)


d.nuclear <- d.nuclear_raw %>% 
  mutate(across(where(is.character), ~ na_if(., "-"))) %>%
  mutate(year = as.integer(year)) %>%
  mutate(across(-year, ~ as.numeric(.))) %>% 
  mutate(across(-year, ~ na_if(., 0)))

str(d.nuclear)

write_rds(d.nuclear, "RDS/d.nuclear.rds")


nuclear_metadata <- tibble::tribble(
  ~Variable_Name,               ~Description,
  "year",                      "Calendar year",
  "beznau1_production_gwh",    "Beznau I: Annual energy production (GWh)",
  "beznau1_capacity_mwe",      "Beznau I: Installed capacity (MWe)",
  "beznau1_util_pct",          "Beznau I: Capacity utilization rate (%)",
  "beznau2_production_gwh",    "Beznau II: Production (GWh)",
  "beznau2_capacity_mwe",      "Beznau II: Capacity (MWe)",
  "beznau2_util_pct",          "Beznau II: Utilization (%)",
  "muhleberg_production_gwh",  "Mühleberg: Production (GWh)",
  "muhleberg_capacity_mwe",    "Mühleberg: Capacity (MWe)",
  "muhleberg_util_pct",        "Mühleberg: Utilization (%)",
  "gosgen_production_gwh",     "Gösgen: Production (GWh)",
  "gosgen_capacity_mwe",       "Gösgen: Capacity (MWe)",
  "gosgen_util_pct",           "Gösgen: Utilization (%)",
  "leibstadt_production_gwh",  "Leibstadt: Production (GWh)",
  "leibstadt_capacity_mwe",    "Leibstadt: Capacity (MWe)",
  "leibstadt_util_pct",        "Leibstadt: Utilization (%)",
  "total_production_gwh",      "Sum of all reactors’ production (GWh)",
  "total_capacity_mwe",        "Sum of installed capacities (MWe)",
  "total_util_pct",            "Aggregate capacity utilization (%, rough)"
)

write_rds(nuclear_metadata, "RDS/nuclear_metadata.rds")




# This document we decided not to work with as we found the monthly breakdown;
#however, we kept the code just in case
# RDS is saved in the RDS folder

## energ_overview ----
# - the file is in KWh, at a 15 min interval, once it clean aggregate per day and transform to Gwh

###2020----

# 
# d.energ_overw_20.1 <- read_excel("raw_data/EnergieUebersichtCH-2020.xlsx", sheet = "Zeitreihen0h15")
# d.energ_overw_20.1 <- d.energ_overw_20.1[-1, ]  %>% # removing the 1st row - won't be using it as is just the measurement name
#   clean_names()
# 
# # here we store the new column names in a vector,
# # when importing the data set for the next years will be more cleaner than to paste this big chunk 5-6 times
# 
# 
# energie_overview_new_cols <- c(
#   date = "x1",
#   end_user_usage = "summe_endverbrauchte_energie_regelblock_schweiz_total_energy_consumed_by_end_users_in_the_swiss_controlblock",
#   energy_production = "summe_produzierte_energie_regelblock_schweiz_total_energy_production_swiss_controlblock",
#   energy_consumption = "summe_verbrauchte_energie_regelblock_schweiz_total_energy_consumption_swiss_controlblock",
#   net_transmission_outflow = "netto_ausspeisung_aus_dem_ubertragungsnetz_schweiz_net_outflow_of_the_swiss_transmission_grid",
#   transmission_grid_feed_in = "vertikale_einspeisung_ins_ubertragungsnetz_schweiz_grid_feed_in_swiss_transmission_grid",
#   positive_secondary_control_energy = "positive_sekundar_regelenergie_positive_secundary_control_energy",
#   negative_secondary_control_energy = "negative_sekundar_regelenergie_negative_secundary_control_energy",
#   positive_tertiary_control_energy  = "positive_tertiar_regelenergie_positive_tertiary_control_energy",
#   negative_tertiary_control_energy = "negative_tertiar_regelenergie_negative_tertiary_control_energy",
#   x_ch_at = "verbundaustausch_ch_at_cross_border_exchange_ch_at",
#   x_at_ch = "verbundaustausch_at_ch_cross_border_exchange_at_ch",
#   x_ch_de = "verbundaustausch_ch_de_cross_border_exchange_ch_de",
#   x_de_ch = "verbundaustausch_de_ch_cross_border_exchange_de_ch",
#   x_ch_fr = "verbundaustausch_ch_fr_cross_border_exchange_ch_fr",
#   x_fr_ch = "verbundaustausch_fr_ch_cross_border_exchange_fr_ch",
#   x_ch_it = "verbundaustausch_ch_it_cross_border_exchange_ch_it",
#   x_it_ch = "verbundaustausch_it_ch_cross_border_exchange_it_ch",
#   prod_ag = "produktion_kanton_ag_production_canton_ag",
#   verbr_ag = "verbrauch_kanton_ag_consumption_canton_ag",
#   prod_fr = "produktion_kanton_fr_production_canton_fr",
#   verbr_fr = "verbrauch_kanton_fr_consumption_canton_fr",
#   prod_gl = "produktion_kanton_gl_production_canton_gl",
#   verbr_gl = "verbrauch_kanton_gl_consumption_canton_gl",
#   prod_gr = "produktion_kanton_gr_production_canton_gr",
#   verbr_gr = "verbrauch_kanton_gr_consumption_canton_gr",
#   prod_lu = "produktion_kanton_lu_production_canton_lu",
#   verbr_lu = "verbrauch_kanton_lu_consumption_canton_lu",
#   prod_ne = "produktion_kanton_ne_production_canton_ne",
#   verbr_ne = "verbrauch_kanton_ne_consumption_canton_ne",
#   prod_so = "produktion_kanton_so_production_canton_so",
#   verbr_so = "verbrauch_kanton_so_consumption_canton_so",
#   prod_sg = "produktion_kanton_sg_production_canton_sg",
#   verbr_sg = "verbrauch_kanton_sg_consumption_canton_sg",
#   prod_tg = "produktion_kanton_tg_production_canton_tg",
#   verbr_tg = "verbrauch_kanton_tg_consumption_canton_tg",
#   prod_ti = "produktion_kanton_ti_production_canton_ti",
#   verbr_ti = "verbrauch_kanton_ti_consumption_canton_ti",
#   price_secondary_reserve_pos = "durchschnittliche_positive_sekundar_regelenergie_preise_average_positive_secondary_control_energy_prices",
#   price_secondary_reserve_neg = "durchschnittliche_negative_sekundar_regelenergie_preise_average_negative_secondary_control_energy_prices",
#   price_tertiary_reserve_pos = "durchschnittliche_positive_tertiar_regelenergie_preise_average_positive_tertiary_control_energy_prices",
#   price_tertiary_reserve_neg = "durchschnittliche_negative_tertiar_regelenergie_preise_average_negative_tertiary_control_energy_prices",
#   prod_across_cantons = "produktion_kantonsubergreifend_production_across_cantons",
#   verbr_across_cantons = "verbrauch_kantonsubergreifend_consumption_across_cantons",
#   prod_control_area_ch_foreign = "produktion_regelzone_ch_auslandische_gebiete_production_control_area_ch_foreign_territories",
#   verb_control_area_ch_foreign = "verbrauch_regelzone_ch_auslandische_gebiete_consumption_control_area_ch_foreign_territories"
# )
# 
# d.energ_overw_20 <- d.energ_overw_20.1 %>%
#   rename(!!!energie_overview_new_cols)
# 
# 
# 
# glimpse(d.energ_overw_20) # using it to check the column names, the output is more cleaner than str()
# 
# 
# 
# 
# 
# ###metadata----
# 
# data_dictionary <- data.frame(
#   variable_name = names(energie_overview_new_cols),
#   description = c(
#     "The calendar date for the daily aggregated record.",
#     "Total electrical energy consumed by final end-users (homes, businesses, industry) within the Swiss control block.",
#     "Total electrical energy produced within the Swiss control block.",
#     "Total electrical energy consumed within the Swiss control block.",
#     "Net balance of energy exchanged with neighboring countries. Positive value: Net exporter. Negative value: Net importer.",
#     "Total energy produced and fed directly into the Swiss high-voltage transmission grid.",
#     "Energy from automatic reserves to add power to the grid to correct a frequency drop (activated within 5 mins).",
#     "Energy from automatic reserves to reduce power in the grid to correct a frequency surge (activated within 5 mins).",
#     "Energy from manual reserves to add power to the grid to relieve the secondary reserve (activated within 15 mins).",
#     "Energy from manual reserves to reduce power in the grid to relieve the secondary reserve (activated within 15 mins).",
#     "Net energy flow on the CH-AT interconnector. Positive: Export to Austria. Negative: Import from Austria.",
#     "Net energy flow on the AT-CH interconnector. Positive: Import from Austria. Negative: Export to Austria.",
#     "Net energy flow on the CH-DE interconnector. Positive: Export to Germany. Negative: Import from Germany.",
#     "Net energy flow on the DE-CH interconnector. Positive: Import from Germany. Negative: Export to Germany.",
#     "Net energy flow on the CH-FR interconnector. Positive: Export to France. Negative: Import from France.",
#     "Net energy flow on the FR-CH interconnector. Positive: Import from France. Negative: Export to France.",
#     "Net energy flow on the CH-IT interconnector. Positive: Export to Italy. Negative: Import from Italy.",
#     "Net energy flow on the IT-CH interconnector. Positive: Import from Italy. Negative: Export to Italy.",
#     "Energy production within the canton of Aargau (AG).",
#     "Energy consumption within the canton of Aargau (AG).",
#     "Energy production within the canton of Fribourg (FR).",
#     "Energy consumption within the canton of Fribourg (FR).",
#     "Energy production within the canton of Glarus (GL).",
#     "Energy consumption within the canton of Glarus (GL).",
#     "Energy production within the canton of Grisons (GR).",
#     "Energy consumption within the canton of Grisons (GR).",
#     "Energy production within the canton of Lucerne (LU).",
#     "Energy consumption within the canton of Lucerne (LU).",
#     "Energy production within the canton of Neuchâtel (NE).",
#     "Energy consumption within the canton of Neuchâtel (NE).",
#     "Energy production within the canton of Solothurn (SO).",
#     "Energy consumption within the canton of Solothurn (SO).",
#     "Energy production within the canton of St. Gallen (SG).",
#     "Energy consumption within the canton of St. Gallen (SG).",
#     "Energy production within the canton of Thurgau (TG).",
#     "Energy consumption within the canton of Thurgau (TG).",
#     "Energy production within the canton of Ticino (TI).",
#     "Energy consumption within the canton of Ticino (TI).",
#     "Average price (€/kWh) paid for activated positive secondary control energy.",
#     "Average price (€/kWh) paid for activated negative secondary control energy.",
#     "Average price (€/kWh) paid for activated positive tertiary control energy.",
#     "Average price (€/kWh) paid for activated negative tertiary control energy.",
#     "Energy production that cannot be attributed to a single canton (e.g., large hydro or nuclear).",
#     "Energy consumption that cannot be attributed to a single canton (e.g., grid losses).",
#     "Energy produced within the Swiss control area but physically located in foreign territories (e.g., Büsingen).",
#     "Energy consumed within the Swiss control area but physically located in foreign territories."
#   )
# )
# 
# print(data_dictionary)
# 
# write.csv(data_dictionary, "energie_overview_data_dictionary.csv", row.names = FALSE)
# 
# 
# 
# ###2021-25----
# 
# 
# d.energ_overw_21.1 <- read_excel("raw_data/EnergieUebersichtCH-2021.xlsx", sheet = "Zeitreihen0h15")
# d.energ_overw_21.1 <- d.energ_overw_21.1[-1, ]  %>% # removing the 1st row - won't be using it as is just the measurement name
#   clean_names()
# 
# 
# d.energ_overw_21 <- d.energ_overw_21.1 %>%
#   rename(!!!energie_overview_new_cols)
# 
# 
# 
# 
# glimpse(d.energ_overw_21)
# 
# #2022
# 
# d.energ_overw_22.1 <- read_excel("raw_data/EnergieUebersichtCH-2022.xlsx", sheet = "Zeitreihen0h15")
# d.energ_overw_22.1 <- d.energ_overw_22.1[-1, ]  %>% # removing the 1st row - won't be using it as is just the measurement name
#   clean_names()
# 
# 
# d.energ_overw_22 <- d.energ_overw_22.1 %>%
#   rename(!!!energie_overview_new_cols)
# 
# #2023
# 
# d.energ_overw_23.1 <- read_excel("raw_data/EnergieUebersichtCH-2023.xlsx", sheet = "Zeitreihen0h15")
# d.energ_overw_23.1 <- d.energ_overw_23.1[-1, ]  %>% # removing the 1st row - won't be using it as is just the measurement name
#   clean_names()
# 
# 
# d.energ_overw_23 <- d.energ_overw_23.1 %>%
#   rename(!!!energie_overview_new_cols)
# 
# 
# #2024
# 
# 
# d.energ_overw_24.1 <- read_excel("raw_data/EnergieUebersichtCH-2024.xlsx", sheet = "Zeitreihen0h15")
# d.energ_overw_24.1 <- d.energ_overw_24.1[-1, ]  %>% # removing the 1st row - won't be using it as is just the measurement name
#   clean_names()
# 
# 
# d.energ_overw_24 <- d.energ_overw_24.1 %>%
#   rename(!!!energie_overview_new_cols)
# 
# 
# #2025
# 
# d.energ_overw_25.1 <- read_excel("raw_data/EnergieUebersichtCH-2025.xlsx", sheet = "Zeitreihen0h15")
# d.energ_overw_25.1 <- d.energ_overw_25.1[-1, ]  %>% # removing the 1st row - won't be using it as is just the measurement name
#   clean_names()
# 
# 
# d.energ_overw_25 <- d.energ_overw_25.1 %>%
#   rename(!!!energie_overview_new_cols)
# 
# 
# 
# 
# d.energ_overview.1 <- bind_rows(d.energ_overw_20,
#                               d.energ_overw_21,
#                               d.energ_overw_22,
#                               d.energ_overw_23,
#                               d.energ_overw_24,
#                               d.energ_overw_25)
# 
# #glimpse(d.energ_overview.1)
# 
# 
# # fixing the date and transform all columns from character to numeric
# 
# d.energ_overview <- d.energ_overview.1 %>%
#   mutate(date = sub(" .*", "", date)) %>%
#   mutate(date = as.Date(date, format = "%d.%m.%Y")) %>%
#   mutate(across(
#     .cols = -date,                          # all columns except 'date'
#     .fns = ~ as.numeric(.x)                 # convert to numeric
#   ))
# 
# 
# ####summarize----
# 
# #by day and transfor into GWh, 1Gwh = 1 mil kwh
# 
# d.daily_energy_overview <- d.energ_overview %>%
#   group_by(date) %>%
#   summarise(
#     across(where(is.numeric), ~ sum(.x, na.rm = TRUE)),
#     .groups = "drop"
#   ) %>%
#   mutate(across(where(is.numeric), ~ .x / 1e6))
# 
# 
# write_rds(d.daily_energy_overview, "RDS/d.daily_energy_overview.rds")
# #probably will not use
# 
# #by month
# 
# d.monthly_energy_overview <- d.energ_overview %>%
#   mutate(month = floor_date(date, "month")) %>%   # create a month column
#   group_by(month) %>%
#   summarise(
#     across(where(is.numeric), ~ sum(.x, na.rm = TRUE)),
#     .groups = "drop"
#   )%>%
#   mutate(across(where(is.numeric), ~ .x / 1e6))
# 
# 
# write_rds(d.monthly_energy_overview, "RDS/d.monthly_energy_overview.rds")
# most likely this file will not be used as it does not provide energy production for all cantons
# 
# 
# 






