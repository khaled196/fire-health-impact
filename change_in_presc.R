library(sf)
library(OpenStreetMap)
library(tidyverse)
library(tmap)
library(rosm)
library(readr)
library(dplyr)
library(ggplot2)

CCG_boundaries <- geojsonsf::geojson_sf("https://openprescribing.net/api/1.0/org_location/?org_type=ccg") |> st_transform(27700)


bradford_code <- "36J"



# Load built-up area boundaries
builtup_bounds <- st_read("os_open_built_up_areas.gpkg",
                          layer = "os_open_built_up_areas")

bradford_zones <- builtup_bounds[CCG_boundaries[CCG_boundaries$code==bradford_code,],] |> slice_max(geometry_area_m)

Bradford_Practices <- geojsonsf::geojson_sf(
  paste0("https://openprescribing.net/api/1.0/org_location/?q=",
         bradford_code)
) |> st_transform(27700)

tmap_mode("view")
qtm(Bradford_Practices |> st_make_valid())



lpzomnibus <- read_csv(paste0(
  "https://openprescribing.net/api/1.0/measure_by_practice/?format=csv&org=",
  bradford_code,
  "&parent_org_type=ccg&measure=lpzomnibus")
)

head(lpzomnibus)

lpzomnibus |> 
  ggplot(aes(x = date,
             y = calc_value,
             groups = org_id))+
  geom_line(alpha = 0.15, col = "dodgerblue2",linewidth = 0.65)+
  stat_smooth(geom = "line",method = "lm",alpha = 0.2, col = "dodgerblue4",linewidth = 0.7)+ 
  theme_minimal()+
  labs(title = "Ratio of Prescribed SABA over inhaled corticosteroid inhalers + SABA",
       y = "value"
  )


lpzomnibus |>
  drop_na() |>
  summarise(n_reports = n(),
            .by = org_id) |>
  arrange(n_reports) 


ids_to_include <- lpzomnibus |>
  drop_na() |>
  summarise(n_reports = n(),
            .by = org_id) |>
  arrange(n_reports) |> 
  filter(n_reports>10) |> 
  pull(org_id)


start_month <- min(lpzomnibus$date)
diff_month <- function(start, end){
  length(seq(from=start, to=end, by='month')) - 1
}
lpzomnibus$month <- vapply(lpzomnibus$date,\(x){
  diff_month(start_month,x)},numeric(1))


city_practices <- Bradford_Practices[bradford_zones,] |> pull(code)


clean_data <- lpzomnibus |>
  filter(org_id %in% ids_to_include,org_id %in% city_practices)


lpzomnibus_processed <- clean_data |> 
  nest(data = -org_id) |> 
  mutate(lm.model = map(.x = data,
                        \(x) {
                          lm(calc_value ~ month, data = x)
                        }),
         coef = map_dbl(lm.model,\(x){coef(x)[2]})
  )

bradford_trends <- Bradford_Practices |>
  inner_join(
    lpzomnibus_processed |>
      select(org_id,coef),
    by = c("code"="org_id"))


tm_shape(bradford_trends |> 
           mutate(abs.size = abs(coef))) +
  tm_basemap("OpenStreetMap") +  # Add OSM as the basemap
  tm_dots(
    col = "coef",
    midpoint = 0,
    palette = "Spectral",
    size = "abs.size",
    style = "fisher"
  ) +
  tm_layout(bg.color = "gray")



library(dplyr)
library(lubridate)
library(tmap)

# Step 1: Calculate monthly averages up to November 2021
lpzomnibus_monthly_avg <- lpzomnibus |>
  filter(date <= as.Date("2021-11-30")) |>  # Include only data up to November 2021
  mutate(year_month = floor_date(date, "month")) |>  # Extract year and month
  group_by(org_id, year_month) |>
  summarise(monthly_avg = mean(calc_value, na.rm = TRUE), .groups = "drop")

# Step 2: Compute overall average of monthly averages for each practice
lpzomnibus_practice_avg <- lpzomnibus_monthly_avg |>
  group_by(org_id) |>
  summarise(overall_avg = mean(monthly_avg, na.rm = TRUE), .groups = "drop")

# Step 3: Filter for November 2021 values
lpzomnibus_nov2021 <- lpzomnibus |>
  filter(date == as.Date("2021-11-01")) |>
  select(org_id, nov2021_value = calc_value)

# Step 4: Calculate the difference
lpzomnibus_diff <- lpzomnibus_nov2021 |>
  inner_join(lpzomnibus_practice_avg, by = "org_id") |>
  mutate(diff_value = nov2021_value - overall_avg)

# Step 5: Join the difference with spatial data
bradford_trends_diff <- Bradford_Practices |>
  inner_join(lpzomnibus_diff, by = c("code" = "org_id"))

# Step 6: Visualize the differences on the map
tm_shape(bradford_trends_diff |> 
           mutate(abs_diff = abs(diff_value))) +
  tm_basemap("OpenStreetMap") +
  tm_dots(
    col = "diff_value",
    midpoint = 0,
    palette = "Spectral",
    size = "abs_diff",
    style = "fisher"
  ) +
  tm_layout(bg.color = "gray")




# Step 1: Calculate monthly averages up to December 2021
lpzomnibus_monthly_avg <- lpzomnibus |>
  filter(date <= as.Date("2021-12-31")) |>  # Include only data up to November 2021
  mutate(year_month = floor_date(date, "month")) |>  # Extract year and month
  group_by(org_id, year_month) |>
  summarise(monthly_avg = mean(calc_value, na.rm = TRUE), .groups = "drop")

# Step 2: Compute overall average of monthly averages for each practice
lpzomnibus_practice_avg <- lpzomnibus_monthly_avg |>
  group_by(org_id) |>
  summarise(overall_avg = mean(monthly_avg, na.rm = TRUE), .groups = "drop")

# Step 3: Filter for December 2021 values
lpzomnibus_nov2021 <- lpzomnibus |>
  filter(date == as.Date("2021-12-01")) |>
  select(org_id, nov2021_value = calc_value)

# Step 4: Calculate the difference
lpzomnibus_diff <- lpzomnibus_nov2021 |>
  inner_join(lpzomnibus_practice_avg, by = "org_id") |>
  mutate(diff_value = nov2021_value - overall_avg)

# Step 5: Join the difference with spatial data
bradford_trends_diff <- Bradford_Practices |>
  inner_join(lpzomnibus_diff, by = c("code" = "org_id"))

# Step 6: Visualize the differences on the map
tm_shape(bradford_trends_diff |> 
           mutate(abs_diff = abs(diff_value))) +
  tm_basemap("OpenStreetMap") +
  tm_dots(
    col = "diff_value",
    midpoint = 0,
    palette = "Spectral",
    size = "abs_diff",
    style = "fisher"
  ) +
  tm_layout(bg.color = "gray")





# Step 1: Calculate monthly averages up to January 2022
lpzomnibus_monthly_avg <- lpzomnibus |>
  filter(date <= as.Date("2022-01-30")) |>  # Include only data up to November 2021
  mutate(year_month = floor_date(date, "month")) |>  # Extract year and month
  group_by(org_id, year_month) |>
  summarise(monthly_avg = mean(calc_value, na.rm = TRUE), .groups = "drop")

# Step 2: Compute overall average of monthly averages for each practice
lpzomnibus_practice_avg <- lpzomnibus_monthly_avg |>
  group_by(org_id) |>
  summarise(overall_avg = mean(monthly_avg, na.rm = TRUE), .groups = "drop")

# Step 3: Filter for January 2022 values
lpzomnibus_nov2021 <- lpzomnibus |>
  filter(date == as.Date("2022-01-01")) |>
  select(org_id, nov2021_value = calc_value)

# Step 4: Calculate the difference
lpzomnibus_diff <- lpzomnibus_nov2021 |>
  inner_join(lpzomnibus_practice_avg, by = "org_id") |>
  mutate(diff_value = nov2021_value - overall_avg)

# Step 5: Join the difference with spatial data
bradford_trends_diff <- Bradford_Practices |>
  inner_join(lpzomnibus_diff, by = c("code" = "org_id"))

# Step 6: Visualize the differences on the map
tm_shape(bradford_trends_diff |> 
           mutate(abs_diff = abs(diff_value))) +
  tm_basemap("OpenStreetMap") +
  tm_dots(
    col = "diff_value",
    midpoint = 0,
    palette = "Spectral",
    size = "abs_diff",
    style = "fisher"
  ) +
  tm_layout(bg.color = "gray")

