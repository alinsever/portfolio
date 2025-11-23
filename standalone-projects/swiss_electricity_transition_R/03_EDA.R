library(tidyverse)
library(janitor)
#library(data.table)
library(readxl)
library(stringr)
library(lubridate)
library(scales)
library(leaflet)
library(sf)
library(anomalize)
library(rnaturalearth)
library(rnaturalearthdata)
library(fable)
library(tsibble)
library(feasts)
library(tseries)


# by energy source----

#saveRDS(d.monthly_prod, "RDS/d.monthly_prod.rds")
d.monthly_prod <- readRDS("RDS/d.monthly_prod.rds")

d.energy_source <- d.monthly_prod %>% 
  select(- c(date_label, import, export,
             export_import, losses,
             country_consumtion, final_consumtion)) %>%  # <- removed `total` from here
  filter(date_month >= as.Date("2020-01-01")) %>%
  mutate(date_month = as.Date(date_month))

glimpse(d.energy_source)


d.energy_long <- d.energy_source %>% 
  pivot_longer(
    cols = c(hydroelectric, nuclear, thermic_clasique, thermic_renewable, wind_thurbine, photovoltaic),
    names_to = "production_type",
    values_to = "energy_mwh"
  ) %>%
  mutate(production_type = recode(production_type,
                                  hydroelectric = "Hydroelectric",
                                  nuclear = "Nuclear",
                                  thermic_clasique = "Thermal (Classic)",
                                  thermic_renewable = "Thermal (Renewable)",
                                  wind_thurbine = "Wind",
                                  photovoltaic = "Solar"
  ))

# Swiss Monthly Energy Production by Type--


ggplot() +
  geom_col(
    data = d.energy_long,
    aes(x = date_month, y = energy_mwh, fill = production_type),
    position = "stack"
  ) +
  geom_col(
    data = d.energy_source,
    aes(x = date_month, y = -acumulation_pumping),  
    fill = "black",
    alpha = 0.5,
    width = 20
  ) +
  geom_line(
    data = d.energy_source,
    aes(x = date_month, y = net_production),
    color = "#BB4430",
    linewidth = 1.2   
  ) +
  scale_y_continuous(labels = scales::comma) +
  scale_x_date(date_labels = "%b\n%Y", date_breaks = "2 months") +
  scale_fill_brewer(palette = "Set2") +
  labs(
    title = "Swiss Monthly Energy Production by Type",
    subtitle = "Includes energy consumed by pumped storage (black bars, negative)",
    x = NULL,
    y = "Energy (MWh)",
    fill = "Energy Type"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(face = "bold", size = 16)
  )


# i would do an import export as waterfall? -- import on one side export on the other ,
# not sure exactly what is called

# 
# #facet wrap - wont use
# 
# ggplot(d.energy_long, aes(x = date_month, y = energy_mwh)) +
#   geom_col(fill = "#0072B2") +
#   facet_wrap(~ production_type,
#              scales = "free_y") +
#   scale_x_date(date_breaks = "6 months",
#                date_labels = "%b\n%Y") +
#   labs(title = "Monthly Energy Production by Type (Faceted)",
#        y = "MWh",
#        x = NULL) +
#   theme_minimal()
# 


# percentage - prduction share

d.energy_pct <- d.energy_long %>%
  group_by(date_month) %>%
  mutate(total = sum(energy_mwh, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(share = 100 * energy_mwh / total)



# Line plot 
ggplot(d.energy_pct, aes(x = date_month, y = share, color = production_type)) +
  geom_line(linewidth = 1) +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  scale_x_date(date_breaks = "3 months", date_labels = "%b\n%Y") +
  labs(
    title = "Monthly Share of Energy Production by Type",
    y = "Share of Total Production (%)",
    x = NULL,
    color = "Energy Type"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(face = "bold", size = 16),
    legend.position = "bottom"
  )


# correlation analysis that we can make-evident? 


d.wide_pct <- d.energy_pct %>%
  select(date_month, production_type, share) %>%
  pivot_wider(names_from = production_type, values_from = share)

cor(d.wide_pct$Hydroelectric, d.wide_pct$Nuclear, use = "complete.obs")

# strong negative correlation
# As hydro production increases, nuclear production tends to decrease significantly, and vice versa.

cor(d.wide_pct$Solar, d.wide_pct$Nuclear, use = "complete.obs")

# moderate negative correlation, as solar rises, nuclear tends to fall. 
# Likely because solar, while growing, is still a smaller part of Switzerland’s energy mix than hydro.



# summarize the data - average over multiple years?

d.share_by_year <- d.energy_long %>%
  mutate(year = year(date_month)) %>%
  group_by(year, production_type) %>%
  summarise(yearly_mwh = sum(energy_mwh, na.rm = TRUE)) %>%
  ungroup()


d.share_by_year <- d.share_by_year %>%
  group_by(year) %>%
  mutate(total_mwh = sum(yearly_mwh),
         share = 100 * yearly_mwh / total_mwh) %>%
  ungroup()


d.share_2020_2024 <- d.share_by_year %>%
  filter(year >= 2020, year <= 2024) %>%
  group_by(production_type) %>%
  summarise(avg_share = mean(share, na.rm = TRUE)) %>%
  arrange(desc(avg_share))

#maybe put in a table in quarto on the right side with a description on the left.... 
#under the monthly share plot


# DON'T USE - ugly - plot percentages but using the table instead....
#
# ggplot(d.share_2020_2024, aes(x = reorder(production_type, avg_share), y = avg_share)) +
#   geom_col(fill = "#0072B2") +
#   coord_flip() +
#   scale_y_continuous(labels = scales::percent_format(scale = 1)) +
#   labs(
#     title = "Average Share of Energy Production by Type (2020–2024)",
#     x = NULL,
#     y = "Share of Total Production (%)"
#   ) +
#   theme_minimal()




# imp. d.radiation_ch----

#write_rds(d.radiation_ch, "RDS/d.radiation_ch.rds")

d.radiation_ch <- readRDS("RDS/d.radiation_ch.rds")


str(d.radiation_ch)

## join solar & radiation on month column----
# drops may to august months from radiation because it joins on Monthly_prod

d.solar_combined <- d.monthly_prod %>%
  select(date_month, photovoltaic) %>%
  inner_join(d.radiation_ch, by = "date_month") %>%
  select(date_month, global_radiation, photovoltaic)




# #DON'T USE - geo_pooint radiation vs PV but 
# ---
# 
# ggplot(d.solar_combined, aes(x = global_radiation, y = photovoltaic)) +
#   geom_point(color = "#0D5C63", size = 3) +
#   geom_smooth(method = "lm", se = FALSE, color = "#BB4430", linewidth = 1) +
#   labs(
#     title = "Relationship Between Solar Radiation and Photovoltaic Production",
#     x = "Monthly Global Radiation (MJ/m²)",
#     y = "Solar Electricity Production (MWh)"
#   ) +
#   theme_minimal()
# ---

# cor(d.solar_combined$global_radiation, d.solar_combined$photovoltaic, use = "complete.obs")

# lag, but not sure - not gonna use

d.radiation_ch <- d.radiation_ch %>%
  arrange(date_month) %>%
  mutate(
    radiation_lag1 = lag(global_radiation, 1)
  )
  

#all the powers sources combined



d.energy_solar <- d.monthly_prod %>%
  select(date_month, photovoltaic, wind_thurbine, hydroelectric, nuclear) %>%
  inner_join(d.radiation_ch, by = "date_month") %>%
  select(
    date_month,
    photovoltaic,
    wind_thurbine,
    hydroelectric,
    nuclear,
    radiation_current = global_radiation,
    radiation_lag1
  )

# correlation for lag? - not gonna use

# cor_t   <- cor(d.energy_solar$photovoltaic, d.energy_solar$radiation_current, use = "complete.obs")
# cor_lag <- cor(d.energy_solar$photovoltaic, d.energy_solar$radiation_lag1, use = "complete.obs")
# 
# tibble(
#   Correlation_Type = c("Same Month", "Lag 1 Month"),
#   Correlation = c(cor_t, cor_lag)
# ) %>%
#   gt::gt() %>%
#   gt::fmt_number(columns = Correlation, decimals = 3)


# Calculate scale factor to match the axis

# scale_factor <- max(d.energy_solar$photovoltaic, na.rm = TRUE) / 
#   max(d.energy_solar$radiation_current, na.rm = TRUE)

# this shows the relationship but it includes also the instalation ofnew systems trend
# ggplot(d.energy_solar, aes(x = date_month)) +
#   geom_col(aes(y = photovoltaic), fill = "#0D5C63", alpha = 0.8) +
#   geom_line(aes(y = radiation_current * scale_factor), color = "#BB4430", linewidth = 1.2) +
#   scale_y_continuous(
#     name = "Photovoltaic Production (MWh)",
#     sec.axis = sec_axis(~ . / scale_factor, name = "Solar Radiation (MJ/m²)")
#   ) +
#   scale_x_date(date_labels = "%b %Y", date_breaks = "3 months") +
#   labs(
#     title = "Photovoltaic vs Solar Radiation Over Time",
#     x = NULL
#   ) +
#   theme_minimal() +
#   theme(
#     axis.text.x = element_text(angle = 45, hjust = 1),
#     plot.title = element_text(face = "bold", size = 14)
#   )

#STL decomsition to remove

solar_stl <- d.energy_solar %>%
  select(date_month, photovoltaic) %>%
  time_decompose(photovoltaic, method = "stl", frequency = "auto", trend = "auto")


solar_stl <- solar_stl %>%
  mutate(normalized_photovoltaic = season + remainder)


solar_stl %>%
  select(date_month, observed, trend, season, remainder) %>%
  pivot_longer(cols = -date_month, names_to = "component", values_to = "value") %>%
  ggplot(aes(x = date_month, y = value, color = component)) +
  geom_line() +
  facet_wrap(~component, scales = "free_y", ncol = 1) +
  labs(
    title = "STL Decomposition of Photovoltaic Production",
    x = NULL,
    y = NULL
  ) +
  theme_minimal()


#compare with radiation


solar_compare <- solar_stl %>%
  left_join(d.radiation_ch, by = "date_month") %>%
  select(date_month, normalized_photovoltaic, global_radiation)

glimpse(solar_compare)


scale_factor <- max(solar_compare$normalized_photovoltaic, na.rm = TRUE) /
                max(solar_compare$global_radiation, na.rm = TRUE)



# ggplot(solar_compare, aes(x = date_month)) +
#   geom_line(aes(y = normalized_photovoltaic), color = "#0D5C63", linewidth = 1.2) +
#   geom_line(aes(y = global_radiation * scale_factor), color = "#BB4430", linetype = "dashed", linewidth = 1.1) +
#   scale_y_continuous(
#     name = "Normalized Solar Production (MWh)",
#     sec.axis = sec_axis(~ . / scale_factor, name = "Solar Radiation (MJ/m²)")
#   ) +
#   labs(
#     title = "Normalized Solar Production vs Solar Radiation",
#     subtitle = "Trend-removed solar production vs monthly solar radiation",
#     x = NULL
#   ) +
#   theme_minimal() +
#   theme(
#     axis.text.x = element_text(angle = 45, hjust = 1),
#     plot.title = element_text(face = "bold", size = 14)
#   )


# long-pivot data to do a legend
solar_long <- solar_compare %>%
  mutate(radiation_scaled = global_radiation * scale_factor) %>%
  pivot_longer(
    cols = c(normalized_photovoltaic, radiation_scaled),
    names_to = "variable",
    values_to = "value"
  )

# Plot with legend
ggplot(solar_long, aes(x = date_month, y = value, color = variable, linetype = variable)) +
  geom_line(linewidth = 1.2) +
  scale_y_continuous(
    name = "Normalized Solar Production (MWh)",
    sec.axis = sec_axis(~ . / scale_factor, name = "Solar Radiation (MJ/m²)")
  ) +
  scale_color_manual(
    values = c(
      normalized_photovoltaic = "#0D5C63",  # dark teal
      radiation_scaled = "#BB4430"          # red
    ),
    labels = c(
      normalized_photovoltaic = "Normalized PV Output",
      radiation_scaled = "Solar Radiation"
    )
  ) +
  scale_linetype_manual(
    values = c(
      normalized_photovoltaic = "solid",
      radiation_scaled = "dashed"
    ),
    labels = c(
      normalized_photovoltaic = "Normalized PV Output",
      radiation_scaled = "Solar Radiation"
    )
  ) +
  labs(
    title = "Normalized Solar Production vs Solar Radiation",
    subtitle = "Trend-removed solar production vs monthly solar radiation",
    x = NULL,
    color = "Legend",
    linetype = "Legend"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(face = "bold", size = 14),
    legend.position = "top"
  )

# even though trend is removed it is stillshowing the an increase over the years, 
#         - instalations of better PV?
#         - remove the remaining 



cor_result <- cor(
  solar_compare$global_radiation,
  solar_compare$normalized_photovoltaic,
  use = "complete.obs"  # ensures missing values don't break it
)

cor_result

# corelation is good 


# trying just with the season here
# after ploting it shows that the first season + remainder it actually follows the radiation very close
# 

# solar_stl2 <- solar_stl %>%
#   mutate(normalized_photovoltaic = season)
# 
# 
# solar_compare2 <- solar_stl2 %>%
#   left_join(d.radiation_ch, by = "date_month") %>%
#   select(date_month, normalized_photovoltaic, global_radiation)
# 
# 
# ggplot(solar_compare2, aes(x = date_month)) +
#   geom_line(aes(y = normalized_photovoltaic), color = "#0D5C63", linewidth = 1.2) +
#   geom_line(aes(y = global_radiation * scale_factor), color = "#BB4430", linetype = "dashed", linewidth = 1.1) +
#   scale_y_continuous(
#     name = "Normalized Solar Production (MWh)",
#     sec.axis = sec_axis(~ . / scale_factor, name = "Solar Radiation (MJ/m²)")
#   ) +
#   labs(
#     title = "Normalized Solar Production vs Solar Radiation",
#     subtitle = "Trend-removed solar production vs monthly solar radiation",
#     x = NULL
#   ) +
#   theme_minimal() +
#   theme(
#     axis.text.x = element_text(angle = 45, hjust = 1),
#     plot.title = element_text(face = "bold", size = 14)
#   )



solar_anomalies <- solar_compare %>%
  select(date_month, normalized_photovoltaic) %>%
  time_decompose(normalized_photovoltaic, method = "stl", frequency = "auto") %>%
  anomalize(remainder, method = "iqr") %>%
  time_recompose()
  

solar_anomalies %>%
  filter(anomaly == "Yes") %>%
  select(date_month, observed, anomaly)


solar_anomalies %>%
  plot_anomalies(ncol = 1, alpha_dots = 0.7) +
  labs(
    title = "Anomalies in Normalized Solar Production",
    subtitle = "Detected using STL decomposition and IQR method"
  )


# #write_rds(d.monthly_energy_overview, "RDS/d.monthly_energy_overview.rds")
# #imp. energy overwie----
# 
# d.monthly_energy_overview <- readRDS("RDS/d.monthly_energy_overview.rds")
# 
# 
# #for later----
# d.monthly_energy_overview %>%
#   select(month, energy_production, energy_consumption) %>%
#   pivot_longer(-month, names_to = "type", values_to = "GWh") %>%
#   ggplot(aes(x = month, y = GWh, color = type)) +
#   geom_line(size = 1.2) +
#   labs(title = "National Energy Trends", x = NULL, y = "Energy (GWh)", color = "Metric") +
#   theme_minimal()
# # how can i modify this to show + and - 
# # maybe difference and column with + and columns with - .....
# 
# 


# 
# d.monthly_energy_overview %>%
#   select(month, energy_production, energy_consumption, end_user_usage) %>%
#   pivot_longer(-month, names_to = "metric", values_to = "GWh") %>%
#   ggplot(aes(x = month, y = GWh, color = metric)) +
#   geom_line(size = 1.2) +
#   facet_wrap(~ metric, ncol = 1, scales = "free_y") +
#   labs(title = "Swiss Energy Overview: Production, Consumption, End-Use",
#        x = NULL, y = "Energy (GWh)") +
#   theme_minimal()
# 

# cantons----
# we do not have data for all cantons so we are going to skip and drop dataset....

# prod_long <- d.monthly_energy_overview %>%
#   select(month, starts_with("prod_")) %>%
#   pivot_longer(cols = -month, names_to = "canton", values_to = "production_gwh") %>%
#   mutate(canton = str_remove(canton, "prod_"))
# 
# 
# cons_long <- d.monthly_energy_overview %>%
#   select(month, starts_with("verbr_")) %>%
#   pivot_longer(cols = -month, names_to = "canton", values_to = "consumption_gwh") %>%
#   mutate(canton = str_remove(canton, "verbr_"))
# 
# canton_energy_summary <- left_join(prod_long, cons_long, by = c("month", "canton")) %>%
#   group_by(canton) %>%
#   summarise(
#     total_production = sum(production_gwh, na.rm = TRUE),
#     total_consumption = sum(consumption_gwh, na.rm = TRUE),
#     .groups = "drop"
#   )
# 
# 
# canton_energy_summary %>%
#   arrange(desc(total_production)) %>%
#   slice_head(n = 10) %>%
#   ggplot(aes(x = reorder(canton, total_production), y = total_production)) +
#   geom_col(fill = "#0D5C63") +
#   coord_flip() +
#   labs(title = "Top 10 Producing Cantons", y = "Total Production (GWh)", x = "Canton") +
#   theme_minimal()
# 
# 
# canton_energy_summary %>%
#   arrange(desc(total_consumption)) %>%
#   slice_head(n = 10) %>%
#   ggplot(aes(x = reorder(canton, total_consumption), y = total_consumption)) +
#   geom_col(fill = "#BB4430") +
#   coord_flip() +
#   labs(title = "Top 10 Consuming Cantons", y = "Total Consumption (GWh)", x = "Canton") +
#   theme_minimal()
# 


# nuclear----


d.nuclear_share <- d.energy_source %>%
  mutate(nuclear_share = (nuclear / total) * 100)
d.nuclear_share


nuclear_yearly <- d.energy_source %>%
  filter(year(date_month) %in% 2020:2024) %>%
  mutate(year = year(date_month)) %>%
  group_by(year) %>%
  summarise(
    nuclear_total = sum(nuclear, na.rm = TRUE),
    total_energy  = sum(total, na.rm = TRUE)
  ) %>%
  mutate(nuclear_share_pct = round(100 * nuclear_total / total_energy, 1))


min_nuclear <- nuclear_yearly %>%
  filter(nuclear_share_pct == min(nuclear_share_pct)) %>%
  select(min_pct = nuclear_share_pct, min_year = year)

min_nuclear

max_nuclear <- nuclear_yearly %>%
  filter(nuclear_share_pct == max(nuclear_share_pct)) %>%
  select(max_pct = nuclear_share_pct, max_year = year)

max_nuclear

min_max_string <- paste0(min_nuclear$min_pct, "% (", min_nuclear$min_year, ") and ",
                      max_nuclear$max_pct, "% (", max_nuclear$max_year, ")")

min_max_string
#Over the last five years (2020–2024), nuclear energy contributed between `r min_max_str` of Switzerland’s total electricity production.
write_rds(min_max_string, "RDS/min_max_string.rds")


#write_rds(d.nuclear, "RDS/d.nuclear.rds")
d.nuclear <- readRDS("RDS/d.nuclear.rds")


nuclear_long <- d.nuclear %>%
  filter(year >= 2020 & year <= 2024) %>%
  select(year, ends_with("production_gwh")) %>%
  pivot_longer(
    cols = -year,
    names_to = "plant",
    values_to = "production_gwh"
  ) %>%
  mutate(
    plant = gsub("_production_gwh", "", plant),
    plant = stringr::str_to_title(plant)
  )
# 
# ggplot(nuclear_long, aes(x = year, y = production_gwh, fill = plant)) +
#   geom_col(position = "stack") +
#   scale_fill_brewer(palette = "Set2") +
#   labs(
#     title = "Swiss Nuclear Production (2020–2024)",
#     x = NULL,
#     y = "Production (GWh)",
#     fill = "Reactor"
#   ) +
#   theme_minimal() +
#   theme(legend.position = "bottom")



avg_nuclear_prod <- d.nuclear %>%
  filter(year >= 2020 & year <= 2024) %>%
  summarise(avg_gwh = mean(total_production_gwh, na.rm = TRUE)) %>%
  pull(avg_gwh)

avg_nuclear_prod
write_rds(avg_nuclear_prod, "RDS/avg_nuclear_prod.rds")


avg_nuclear_plant <- d.nuclear %>% 
  filter(year >= 2020 & year <= 2024) %>%
  select(year,
         beznau1_production_gwh,
         beznau2_production_gwh,
         gosgen_production_gwh,
         leibstadt_production_gwh) %>% 
  summarise(
    Beznau_I     = mean(beznau1_production_gwh, na.rm = TRUE),
    Beznau_II    = mean(beznau2_production_gwh, na.rm = TRUE),
    Gösgen       = mean(gosgen_production_gwh, na.rm = TRUE),
    Leibstadt    = mean(leibstadt_production_gwh, na.rm = TRUE)
  ) %>%
  tidyr::pivot_longer(
    cols = everything(),
    names_to = "plant",
    values_to = "avg_gwh"
  )


nuclear_plants <- tibble(
  plant = c("Beznau_I", "Beznau_II", "Gösgen", "Leibstadt", "Mühleberg"),
  display_name = c("Beznau I", "Beznau II", "Gösgen", "Leibstadt", "Mühleberg"),
  canton = c("AG", "AG", "SO", "AG", "BE"),
  status = c("Operational", "Operational", "Operational", "Operational", "Decommissioned"),
  note   = c(
    "Oldest commercial reactor worldwide (1969).",
    "Twin reactor of Beznau I (1971).",
    "In operation since 1979.",
    "Largest Swiss reactor, in operation since 1984.",
    "Decommissioned in December 2019."
  ),
  lat = c(47.552107, 47.552107, 47.3656, 47.6012, 46.9689),
  lon = c(8.228492, 8.228492, 7.9671, 8.1845, 7.2684)
)


# Download Switzerland geometry
switzerland <- ne_countries(country = "Switzerland", returnclass = "sf")


ggplot() +
  geom_sf(data = switzerland, fill = "grey95", color = "grey40") +
  geom_point(data = nuclear_plants, aes(x = lon, y = lat, color = status), size = 4) +
  scale_color_manual(values = c("Operational" = "darkgreen", "Decommissioned" = "red")) +
  labs(title = "Nuclear Power Plants in Switzerland", x = NULL, y = NULL, color = "Status") +
  theme_minimal()





leaflet(nuclear_plants) %>%
  addTiles() %>%
  addCircleMarkers(
    ~lon, ~lat,
    popup = ~paste0("<strong>", plant, "</strong><br>Status: ", status),
    radius = 4,
    color = ~ifelse(status == "Operational", "green", "red"),
    fillOpacity = 0.8
  ) %>%
  addLegend(
    "bottomright",
    colors = c("green", "red"),
    labels = c("Operational", "Decommissioned"),
    title = "Nuclear Plant Status"
  )





#Pv- share----

pv_yearly <- d.energy_source %>%
  filter(year(date_month) %in% 2020:2024) %>%
  mutate(year = year(date_month)) %>%
  group_by(year) %>%
  summarise(
    pv_total = sum(photovoltaic, na.rm = TRUE),
    total_energy = sum(total, na.rm = TRUE)
  ) %>%
  mutate(pv_share_pct = round(100 * pv_total / total_energy, 1))

write_rds(pv_yearly, "RDS/pv_yearly.rds")

min_pv <- pv_yearly %>%
  filter(pv_share_pct == min(pv_share_pct)) %>%
  select(min_pct = pv_share_pct, min_year = year)

max_pv <- pv_yearly %>%
  filter(pv_share_pct == max(pv_share_pct)) %>%
  select(max_pct = pv_share_pct, max_year = year)


pv_min_max_string <- paste0(min_pv$min_pct, "% (", min_pv$min_year, ") and ",
                         max_pv$max_pct, "% (", max_pv$max_year, ")")

pv_min_max_string

#pv-seasonal

d.pv_seasonal <- d.energy_source %>%
  filter(year(date_month) %in% 2020:2024) %>%
  mutate(
    year = year(date_month),
    month = month(date_month)
  )

winter_months <- c(12, 1, 2)
spring_months <- c(3, 4, 5)
summer_months <- c(6, 7, 8)
autumn_months <- c(9, 10, 11)

# Step 3: Summarise photovoltaic production per season and year
pv_seasonal_summary <- d.pv_seasonal %>%
  group_by(year) %>%
  summarise(
    pv_annual = sum(photovoltaic, na.rm = TRUE),
    pv_winter = sum(photovoltaic[month %in% winter_months], na.rm = TRUE),
    pv_spring = sum(photovoltaic[month %in% spring_months], na.rm = TRUE),
    pv_summer = sum(photovoltaic[month %in% summer_months], na.rm = TRUE),
    pv_autumn = sum(photovoltaic[month %in% autumn_months], na.rm = TRUE)
  ) %>%
  mutate(
    winter_pct = round(100 * pv_winter / pv_annual, 1),
    spring_pct = round(100 * pv_spring / pv_annual, 1),
    summer_pct = round(100 * pv_summer / pv_annual, 1),
    autumn_pct = round(100 * pv_autumn / pv_annual, 1)
  )

# Step 4: Average across years
pv_avg_seasonal <- pv_seasonal_summary %>%
  summarise(
    avg_winter_pct = round(mean(winter_pct), 1),
    avg_spring_pct = round(mean(spring_pct), 1),
    avg_summer_pct = round(mean(summer_pct), 1),
    avg_autumn_pct = round(mean(autumn_pct), 1)
  )

# Output
print(pv_seasonal_summary)
print(pv_avg_seasonal)

saveRDS(pv_avg_seasonal, "RDS/pv_avg_seasonal.rds")


    
plot_data <- pv_yearly %>%
  select(year, photovoltaic = pv_total, total_energy) %>%
  pivot_longer(cols = c(photovoltaic, total_energy), 
               names_to = "type", 
               values_to = "energy_gwh") %>%
  mutate(type = recode(type,
                       photovoltaic = "Photovoltaic",
                       total_energy = "Total Energy"))

# Plot
ggplot(plot_data, aes(x = factor(year), y = energy_gwh, fill = type)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7) +
  geom_text(aes(label = round(energy_gwh)),
            position = position_dodge(width = 0.8),
            vjust = -0.3,
            size = 3.5) +
  scale_fill_manual(values = c("Photovoltaic" = "#F9A03F", "Total Energy" = "#457B9D")) +
  labs(
    title = "Photovoltaic vs Total Energy Production (2020–2024)",
    x = "Year",
    y = "Energy (GWh)",
    fill = "Energy Type"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
    axis.title.y = element_text(margin = margin(r = 10)),
    legend.position = "top")


pv_yearly %>%
  ggplot(aes(x = factor(year), y = pv_total)) +
  geom_col(fill = "#2E8B57", width = 0.6) +  # Seagreen
  geom_text(
    aes(label = round(pv_total)),
    vjust = -0.3,
    size = 3.5,
    color = "black"
  ) +
  labs(
    title = "Photovoltaic Production in Switzerland (2020–2024)",
    x = "Year",
    y = "Energy Produced (GWh)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
    axis.title.y = element_text(margin = margin(r = 10)),
    axis.text.y = element_text(size = 10),
    axis.text.x = element_text(size = 10)
  )



#deficit/surplus----


d.energy_source_all <- d.monthly_prod %>% 
  select(- c(date_label)) %>%  # <- removed `total` from here
 filter(date_month >= as.Date("2020-01-01")) %>%
  mutate(date_month = as.Date(date_month))

write_rds(d.energy_source_all, "RDS/d.energy_source_all.rds")

str(d.energy_source_all)

#is the power grid balanced ?

d.energy_source_all_balance <- d.energy_source_all %>%
  mutate(
    energy_balance = (net_production + import) - export - country_consumtion,
    balance_status = case_when(
      energy_balance > 0 ~ "Surplus",
      energy_balance < 0 ~ "Deficit",
      TRUE ~ "Balanced"
    )
  )

d.energy_source_all_balance %>%
  select(date_month,
         net_production,
         import,
         export,
         country_consumtion,
         energy_balance,
         balance_status) %>%
  head(10)

# cool - absolutely balanced 

ggplot(d.energy_source_all_balance, aes(x = date_month, y = energy_balance, fill = balance_status)) +
  geom_col() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_fill_manual(values = c(Surplus = "#2ca02c", Deficit = "#d62728", Balanced = "#1f77b4")) +
  labs(
    title = "Monthly Energy Balance in Switzerland",
    x = NULL,
    y = "Energy Balance (GWh)",
    fill = "Status"
  ) +
  theme_minimal()

# import/ export----

d.import_export_monthly <- d.energy_source_all %>%
  mutate(
    net_import = import - export,
    month = lubridate::month(date_month, label = TRUE, abbr = TRUE)  # ensure proper function
  )


# Average net import by month (across years)

monthly_avg_import <- d.import_export_monthly %>%
  group_by(month) %>%
  summarise(
    avg_net_import = round(mean(net_import, na.rm = TRUE), 1)
  ) %>%
  arrange(match(month, month.abb))



ggplot(monthly_avg_import, aes(x = month, y = avg_net_import, fill = avg_net_import > 0)) +
  geom_col(show.legend = FALSE) +
  scale_fill_manual(values = c("TRUE" = "#BB4430", "FALSE" = "#0D5C63")) +
  labs(
    title = "Average Net Electricity Import by Month (2020–2024)",
    subtitle = "Positive = Net Import, Negative = Net Export",
    x = "Month",
    y = "Average Net Import (GWh)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    axis.text.x = element_text(size = 11),
    axis.title.y = element_text(size = 12)
  )



# replace nuclear with solar----
# will try to simulate as solar power will replace nuclear. 
#we are considering that on top of the 2024 solar prod.
#cca.6K in 2024 and we add the 22k avg from nuclear a year
#we will have approx 28K in solar production per year but we will use the average production distribution
# apprx 8 percent in winter,  and 40 % in summer (will try to do by month)


#calculate avg nuclear prod by year
avg_nuclear_2020_2024 <- d.energy_source %>%
  filter(year(date_month) %in% 2020:2024) %>%
  group_by(year = year(date_month)) %>%
  summarise(annual_nuclear_gwh = sum(nuclear, na.rm = TRUE)) %>%
  summarise(avg_nuclear_gwh = mean(annual_nuclear_gwh)) %>%
  pull(avg_nuclear_gwh)

avg_nuclear_2020_2024

avg_PV_2020_2024 <- d.energy_source %>%
  filter(year(date_month) %in% 2020:2024) %>%
  group_by(year = year(date_month)) %>%
  summarise(annual_PV_gwh = sum(photovoltaic, na.rm = TRUE)) %>%
  summarise(avg_photovoltaic_gwh = mean(annual_PV_gwh)) %>%
  pull(avg_photovoltaic_gwh)

avg_PV_2020_2024

#1. Average PV by month 2020 -2024
monthly_pv_avg <- d.energy_source %>%
  filter(year(date_month) %in% 2020:2024) %>%
  mutate(month = month(date_month)) %>%
  group_by(month) %>%
  summarise(avg_pv = mean(photovoltaic, na.rm = TRUE), .groups = "drop")

# monthly_pv_avg <- d.energy_source %>%
#   filter(year(date_month) == 2024) %>%
#   mutate(month = month(date_month)) %>%
#   group_by(month) %>%
#   summarise(avg_pv = mean(photovoltaic, na.rm = TRUE), .groups = "drop")




#2. monthly distribution percentages of 

monthly_pv_avg <- monthly_pv_avg %>%
  mutate(
    monthly_pct = avg_pv / sum(avg_pv),
    scaled_pv_gwh = round(monthly_pct * (avg_nuclear_2020_2024 + avg_PV_2020_2024), 0) )

# scaled_pv_gwh <-  round(monthly_pv_avg$monthly_pct * 28000, 1) 
# scaled_pv_gwh

#3. calculated scaled PV production, and will join with the other energy sources (-nuclear)
monthly_pv_avg <- monthly_pv_avg %>%
  mutate(month_label = month.abb[month]) %>%
  select(month_label, scaled_pv_gwh)


#4. other sources average including country_consumption

monthly_power_other_avg <- d.energy_source_all %>%
  filter(year(date_month) %in% 2020:2024) %>%
  mutate(month = month(date_month)) %>%
  group_by(month) %>%
  summarise(avg_hydroelectric = mean((hydroelectric - acumulation_pumping), na.rm = TRUE),
            avg_thermic_clasique = mean(thermic_clasique, na.rm = TRUE),
            avg_thermic_renewable = mean(thermic_renewable, na.rm = TRUE),
            avg_wind_thurbine = mean(wind_thurbine, na.rm = TRUE),
           #avg_accumulation_pumping = mean(acumulation_pumping, na.rm = TRUE),
            avg_country_consumption = mean(country_consumtion, na.rm = TRUE),
            avg_import = mean(import, na.rm = TRUE),
            .groups = "drop") %>% 
  mutate(month_label = month.abb[month])

# I removed accumulation pumping for now since this is a negative energy, 
# I think the plot will be hard to read
# and will add import (export does not matter bcz we only export on surplus)
  
d.monthly_averages <- monthly_pv_avg %>% 
  inner_join(monthly_power_other_avg, by = "month_label")

write_rds(d.monthly_averages, "RDS/d.monthly_averages")


#factor - orders the column bars
d.monthly_averages$month_label <- factor(
  d.monthly_averages$month_label,
  levels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun",
             "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
)




# First, pivot the production sources for stacked bars
d.energy_avg_long <- d.monthly_averages %>%
  select(month_label,
         scaled_pv_gwh,
         avg_hydroelectric,
         avg_thermic_clasique,
         avg_thermic_renewable,
         avg_wind_thurbine,
         avg_import) %>%
  pivot_longer(
    cols = -month_label,
    names_to = "source",
    values_to = "gwh"
  )

d.energy_avg_long$month_label <- factor(
  d.energy_avg_long$month_label,
  levels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun",
             "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
)

# Plot 1
ggplot() +
  geom_col(
    data = d.energy_avg_long,
    aes(x = month_label, y = gwh, fill = source),
    position = "stack"
  ) +

  # geom_col(
  #   data = d.monthly_averages,
  #   aes(x = month_label, y = -avg_accumulation_pumping),
  #   fill = "black",
  #   alpha = 0.5,
  #   width = 0.6
  # ) +
  
  geom_line(
    data = d.monthly_averages,
    aes(x = month_label, y = avg_country_consumption, group = 1),
    color = "#BB4430",
    linewidth = 1.4
  ) +
  
  scale_fill_brewer(palette = "Set2", name = "Energy Source",
                    labels = c(
                      "scaled_pv_gwh" = "Solar (Scaled)",
                      "avg_hydroelectric" = "Hydroelectric",
                      "avg_thermic_clasique" = "Thermic Classic",
                      "avg_thermic_renewable" = "Thermic Renewable",
                      "avg_wind_thurbine" = "Wind",
                      "avg_import" = "Import"
                    )) +
  labs(
    title = "Monthly Average Electricity Production vs. Consumption",
    subtitle = "Simulated scenario: Replacing nuclear with 28 TWh of solar (scaled monthly)",
    x = NULL,
    y = "Energy (GWh)",
    #caption = "Negative black bars = pumping storage use\nRed line = average country consumption (2020–2024)"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    axis.text.x = element_text(face = "bold"),
    plot.title = element_text(face = "bold", size = 15),
    legend.position = "top"
  )


#WINTER GAP

d.monthly_averages <- d.monthly_averages %>%
  mutate(total_production = scaled_pv_gwh +
           avg_hydroelectric +
           avg_thermic_clasique +
           avg_thermic_renewable +
           avg_wind_thurbine)

#  monthly gap (production - consumption)
# calculate what is the gap percentage of total 
monthly_gap <- d.monthly_averages %>%
  mutate(
    gap_gwh = total_production - avg_country_consumption,
    gap_pct = 100 * gap_gwh / sum(avg_country_consumption)
  ) %>%
  select(month_label, avg_country_consumption, total_production, gap_gwh, gap_pct)

monthly_gap


ggplot(monthly_gap, aes(x = month_label, y = gap_gwh, fill = gap_gwh > 0)) +
  geom_col(show.legend = FALSE) +
  scale_fill_manual(values = c("TRUE" = "#0D5C63", "FALSE" = "#BB4430")) +
  geom_hline(yintercept = 0, color = "black") +
  labs(
    title = "Monthly Electricity Gap (Production – Consumption)",
    subtitle = "Nuclear-free scenario with 28 TWh of solar (no imports)",
    x = NULL,
    y = "Gap (GWh)"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    axis.text.x = element_text(face = "bold"),
    plot.title = element_text(face = "bold", size = 15)
  )






#Foreacasting PV growth 2025-2030
#Since this isa time series I will try to use an ARIMA model
#first filter the data set and then do an adf test for stationarity

d.pv_forecast <- d.energy_solar %>%
  select(date_month, photovoltaic) %>%
  filter(!is.na(photovoltaic)) %>%
  as_tsibble(index = date_month)

# create the vector with the intergers
pv_vector <- d.pv_forecast$photovoltaic


#run the adf test
adf.test(pv_vector)

# we are testing if the vector is stationary 
#null- hypothesis is not- stationary, alternative it is stationary 
#when the test is negative = -8,74 => is good for stationarity
#pvalue is 0.01
#no differencingis required for an ARIMA model







# Fit ARIMA and ETS models
pv_models <- d.pv_forecast %>%
  model(
    ARIMA = ARIMA(photovoltaic))

#Warning message:
# there  are no gaps in the time columns 
# fix the monthts 


v_tsibble <- d.pv_forecast %>%
  mutate(month_index = yearmonth(date_month)) %>%   # new yearmonth column
  select(month_index, photovoltaic) %>%
  as_tsibble(index = month_index) %>%
  fill_gaps()


model_arima <- v_tsibble %>%
  model(ARIMA = ARIMA(photovoltaic))


# Forecast 5 years  (60 months)

forecast_arima <- model_arima %>%
  forecast(h = "60 months")


forecast_arima %>%
  autoplot(v_tsibble, level = c(80, 95)) +
  labs(
    title = "ARIMA Forecast of Monthly Solar PV Production (2025–2030)",
    x = "Date",
    y = "Photovoltaic Output (GWh)"
  ) +
  theme_minimal(base_size = 13)




