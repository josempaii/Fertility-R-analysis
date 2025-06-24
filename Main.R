# Ensure all necessary packages are loaded
# If you haven't installed them, run:
# install.packages(c("tidyverse", "sf", "rnaturalearth", "rnaturalearthdata"))
library(tidyverse)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)

# --- Step 1: Import Data, Clean Numeric Values, and Divide into Subgroups ---

# Load the datasets
age_first_child_raw <- read_csv("agefirstchild.csv")
total_fertility_raw <- read_csv("totfertility.csv")

# --- Diagnostic: Check raw data for France (FR) ---
message("\n--- Raw Data Check for France (FR) ---")
message(paste("Is 'FR' present in raw age_first_child data (geo column)?", "FR" %in% unique(age_first_child_raw$geo)))
message(paste("Is 'FR' present in raw total_fertility data (geo column)?", "FR" %in% unique(total_fertility_raw$geo)))

# Function to clean and convert OBS_VALUE to numeric, handling Eurostat flags
clean_obs_value <- function(df, value_col_name) {
  df[[value_col_name]] <- df[[value_col_name]] %>%
    as.character() %>%
    str_replace_all("[pebuiczs]", "") %>%
    str_trim() %>%
    na_if(":") %>%
    as.numeric()
  return(df)
}

# Process age at first child data
age_first_child_processed <- age_first_child_raw %>%
  select(
    Country = geo,
    Year = TIME_PERIOD,
    Age_First_Child = OBS_VALUE
  ) %>%
  clean_obs_value("Age_First_Child")

# Process total fertility data
total_fertility_processed <- total_fertility_raw %>%
  select(
    Country = geo,
    Year = TIME_PERIOD,
    Total_Fertility_Rate = OBS_VALUE
  ) %>%
  clean_obs_value("Total_Fertility_Rate")

# Ensure Year is an integer
age_first_child_processed <- age_first_child_processed %>%
  mutate(Year = as.integer(Year))
total_fertility_processed <- total_fertility_processed %>%
  mutate(Year = as.integer(Year))

# --- Diagnostic: Check processed data for France (FR) ---
message("\n--- Processed Data Check for France (FR) ---")
message("Age at first child for France (FR):")
print(age_first_child_processed %>% filter(Country == "FR"))
message("Total fertility rate for France (FR):")
print(total_fertility_processed %>% filter(Country == "FR"))


# --- Define Country Groupings (UPDATED: FR moved to Southern Europe, Excluding UK, Including IS, CY) ---
northern_europe <- c("DK", "EE", "FI", "IS", "IE", "LV", "LT", "NO", "SE") # Iceland (IS) is here
southern_europe <- c("AL", "BA", "HR", "CY", "EL", "IT", "MT", "ME", "MK", "PT", "RS", "SI", "ES", "FR") # France (FR) is now here
central_eastern_europe <- c("AT", "BE", "BG", "CZ", "DE", "HU", "LU", "NL", "PL", "RO", "SK", "CH") # FR removed from here

# Combine all lists for filtering purposes (EXCLUDING UK)
all_euro_countries <- unique(c(northern_europe, southern_europe, central_eastern_europe))

# Function to assign regions
assign_region <- function(country_code) {
  if (country_code %in% northern_europe) {
    return("Northern Europe")
  } else if (country_code %in% southern_europe) {
    return("Southern Europe")
  } else if (country_code %in% central_eastern_europe) {
    return("Central/Eastern Europe")
  } else {
    return(NA_character_)
  }
}

# Apply region assignment and filter for selected European countries (EXCLUDING UK)
age_first_child_filtered <- age_first_child_processed %>%
  filter(Country %in% all_euro_countries) %>%
  mutate(Region = sapply(Country, assign_region)) %>%
  filter(!is.na(Region))

total_fertility_filtered <- total_fertility_processed %>%
  filter(Country %in% all_euro_countries) %>%
  mutate(Region = sapply(Country, assign_region)) %>%
  filter(!is.na(Region))

# Merge the two datasets
euro_demographics <- inner_join(
  age_first_child_filtered,
  total_fertility_filtered,
  by = c("Country", "Year", "Region")
)

message("\n--- Merged Data (euro_demographics) Check for France (FR) ---")
print(euro_demographics %>% filter(Country == "FR"))
message("Unique countries in euro_demographics after region assignment:")
message(paste(unique(euro_demographics$Country), collapse = ", "))


# --- Step 2: Calculate Average Total Fertility for the Three Subgroups ---
regional_averages <- euro_demographics %>%
  group_by(Region, Year) %>%
  summarise(
    Avg_Total_Fertility = mean(Total_Fertility_Rate, na.rm = TRUE),
    Avg_Age_First_Child = mean(Age_First_Child, na.rm = TRUE),
    .groups = 'drop'
  )

# --- Step 3: Create Boolean Variable for Above/Below European Average (Total Fertility Only) ---
europe_yearly_average <- euro_demographics %>%
  group_by(Year) %>%
  summarise(
    Europe_Avg_Total_Fertility = mean(Total_Fertility_Rate, na.rm = TRUE),
    Europe_Avg_Age_First_Child = mean(Age_First_Child, na.rm = TRUE),
    .groups = 'drop'
  )

euro_demographics_final <- euro_demographics %>%
  left_join(europe_yearly_average, by = "Year") %>%
  mutate(
    Above_Avg_Fertility = Total_Fertility_Rate >= Europe_Avg_Total_Fertility
  )

message("\n--- Final Data (euro_demographics_final) Check for France (FR) ---")
message("Data for France (FR) in euro_demographics_final for 2018 and 2023:")
print(euro_demographics_final %>% filter(Country == "FR", Year %in% c(2018, 2023)))
message("Unique countries in euro_demographics_final:")
message(paste(unique(euro_demographics_final$Country), collapse = ", "))


# --- Step 4 (Revised): Create Three Separate Bar Charts - Percentage Above/Below EU Average Total Fertility ---

simple_bar_chart_data <- euro_demographics_final %>%
  group_by(Region, Above_Avg_Fertility) %>%
  summarise(
    Num_Observations = n(),
    .groups = 'drop_last'
  ) %>%
  group_by(Region) %>%
  mutate(
    Percentage = (Num_Observations / sum(Num_Observations, na.rm = TRUE)) * 100
  ) %>%
  ungroup() %>%
  mutate(Above_Avg_Fertility_Label = ifelse(Above_Avg_Fertility, "Above or Equal EU Avg", "Below EU Avg"))

regions_to_plot <- unique(simple_bar_chart_data$Region)
message(paste("\nAttempting to plot bar charts for regions:", paste(regions_to_plot, collapse = ", ")))

for (current_region in regions_to_plot) {
  plot_data <- simple_bar_chart_data %>%
    filter(Region == current_region)
  
  if (nrow(plot_data) == 0) {
    message(paste("WARNING: No data found for region:", current_region, ". Skipping bar chart generation for this region."))
    next
  }
  
  plot_title <- paste0("Percentage of Countries ", current_region, "\nAbove/Below EU Average Total Fertility Rate (All Years)")
  file_name <- paste0("bar_chart_fertility_", tolower(gsub("[/\\s]", "_", current_region)), ".png")
  
  p <- ggplot(plot_data, aes(x = Above_Avg_Fertility_Label, y = Percentage, fill = Above_Avg_Fertility_Label)) +
    geom_bar(stat = "identity", width = 0.5) +
    geom_text(aes(label = paste0(round(Percentage, 1), "%")), vjust = -0.5, size = 4) +
    scale_fill_manual(values = c("Above or Equal EU Avg" = "darkgreen", "Below EU Avg" = "firebrick")) +
    labs(
      title = plot_title,
      x = "",
      y = "Percentage of Country-Year Observations (%)",
      fill = "Fertility vs. EU Average"
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(size = 10, face = "bold"),
      axis.title.y = element_text(size = 10),
      plot.title = element_text(hjust = 0.5, size = 12, face = "bold"),
      legend.position = "none"
    ) +
    ylim(0, 100)
  
  ggsave(file_name, plot = p, width = 7, height = 5, dpi = 300)
  message(paste0("Bar chart SAVED as '", file_name, "' for ", current_region))
}

# --- Step 5: Create Maps Showing Fertility Rate (2018 vs 2023) ---

# Apply map-specific modifications to data (re-coding only EL to GR)
euro_demographics_map_prep <- euro_demographics_final %>%
  mutate(
    Country = case_when(
      Country == "EL" ~ "GR", # Greece: Eurostat 'EL' to Map 'GR'
      TRUE ~ Country         # Keep other country codes as they are
    )
  )

# Get world map data
world_map <- ne_countries(scale = "medium", returnclass = "sf")

# Filter map data for Europe, EXCLUDING GB (UK)
europe_map <- world_map %>%
  filter((continent == "Europe" | sovereignt == "Russia") & iso_a2 != "GB") %>% # Exclude GB explicitly
  select(sovereignt, iso_a2, geometry)

# --- Diagnostic: Check map data for France (FR) ---
message("\n--- Map Data Check for France (FR) ---")
message(paste("Is 'FR' present in europe_map data (iso_a2 column)?", "FR" %in% unique(europe_map$iso_a2)))

# Filter fertility data for 2018 and 2023 (using the prepared data)
fertility_data_for_map <- euro_demographics_map_prep %>%
  filter(Year %in% c(2018, 2023)) %>%
  select(Country, Year, Total_Fertility_Rate)

# Merge fertility data with map data
map_data_final <- europe_map %>%
  left_join(fertility_data_for_map, by = c("iso_a2" = "Country"))

# --- Diagnostic: Check final merged map data for France (FR) ---
message("\n--- Final Merged Map Data Check for France (FR) ---")
message("Data for France (FR) in map_data_final for 2018 and 2023:")
print(map_data_final %>% filter(iso_a2 == "FR", Year %in% c(2018, 2023)))
message("NA values in Total_Fertility_Rate after merging for map (specifically for FR):")
print(map_data_final %>% filter(iso_a2 == "FR", Year %in% c(2018, 2023)) %>% summarise(na_count = sum(is.na(Total_Fertility_Rate))))


# Create the maps
ggplot(map_data_final %>% filter(Year %in% c(2018, 2023)),
       aes(fill = Total_Fertility_Rate)) +
  geom_sf(color = "white", size = 0.1) +
  coord_sf(xlim = c(-15, 35), ylim = c(35, 70), expand = FALSE) +
  scale_fill_viridis_c(option = "magma", direction = -1, na.value = "grey80",
                       name = "Total Fertility Rate") +
  labs(
    title = "Total Fertility Rate in European Countries (2018 vs 2023)",
    subtitle = "Highlighting potential changes between years (Excluding UK, Including FR, IS, CY if data available)"
  ) +
  facet_wrap(~ Year, ncol = 2) +
  theme_minimal() +
  theme(
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid.major = element_blank(),
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5),
    legend.position = "bottom"
  )

# Save the map plot
ggsave("europe_fertility_map_2018_2023_final.png", width = 12, height = 8, dpi = 300)

# --- Step 6: Line Charts ---

# Use all available years
line_data <- euro_demographics_final %>%
  group_by(Region, Year) %>%
  summarise(
    Avg_Fertility = mean(Total_Fertility_Rate, na.rm = TRUE),
    Avg_Age = mean(Age_First_Child, na.rm = TRUE),
    .groups = 'drop'
  )

# Fertility Rate Plot
p1 <- ggplot(line_data, aes(x = Year, y = Avg_Fertility, color = Region)) +
  geom_line(size = 1.2) +
  geom_point(size = 3) +
  geom_vline(xintercept = 2020, color = "red", linetype = "dashed", size = 1) +
  annotate("text", x = 2020.2, y = max(line_data$Avg_Fertility, na.rm = TRUE), 
           label = "COVID-19", color = "red", angle = 90, vjust = -0.5, size = 3) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
  scale_color_manual(values = c("Northern Europe" = "#2E8B57", 
                                "Southern Europe" = "#FF6347", 
                                "Central/Eastern Europe" = "#4682B4")) +
  labs(title = "Total Fertility Rate Over Time", x = "Year", y = "Total Fertility Rate") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

# Age of Fertility Plot
p2 <- ggplot(line_data, aes(x = Year, y = Avg_Age, color = Region)) +
  geom_line(size = 1.2) +
  geom_point(size = 3) +
  geom_vline(xintercept = 2020, color = "red", linetype = "dashed", size = 1) +
  annotate("text", x = 2020.2, y = max(line_data$Avg_Age, na.rm = TRUE), 
           label = "COVID-19", color = "red", angle = 90, vjust = -0.5, size = 3) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
  scale_color_manual(values = c("Northern Europe" = "#2E8B57", 
                                "Southern Europe" = "#FF6347", 
                                "Central/Eastern Europe" = "#4682B4")) +
  labs(title = "Age at First Child Over Time", x = "Year", y = "Age at First Child") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

print(p1)
print(p2)
ggsave("fertility_rate_trends.png", p1, width = 10, height = 6, dpi = 300)
ggsave("age_first_child_trends.png", p2, width = 10, height = 6, dpi = 300)
