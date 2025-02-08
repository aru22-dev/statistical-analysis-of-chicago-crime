# Load necessary libraries
library(dplyr)
library(tidyr)
library(ggplot2)

# Step 1: Load the dataset
crime_data <- read.csv("/Users/arundhatiraj/Downloads/Crime_Arrests_2019-24.csv")

# Step 2: Data Cleaning - Filter relevant data and handle missing values
crime_clean <- crime_data %>%
    filter(!is.na(location.description) & !is.na(primary.type)) %>%
    mutate(Location.Category = case_when(
        grepl("STREET|ROAD|AVENUE", location.description, ignore.case = TRUE) ~ "Street",
        grepl("RESIDENCE|HOME|APARTMENT", location.description, ignore.case = TRUE) ~ "Residential",
        grepl("PARK|SCHOOL|PUBLIC", location.description, ignore.case = TRUE) ~ "Public Space",
        TRUE ~ "Other"
    ))

# Step 3: Summarize data - Count crimes by Location Category and Crime Type
crime_summary <- crime_clean %>%
    group_by(Location.Category, primary.type) %>%
    summarise(Crime_Count = n(), .groups = "drop")

# Step 4: Prepare data for heatmap
crime_heatmap <- crime_summary %>%
    pivot_wider(names_from = primary.type, values_from = Crime_Count, values_fill = 0) %>%
    pivot_longer(cols = -Location.Category, names_to = "Crime.Type", values_to = "Crime_Count")

# Step 5: Generate Heatmap Visualization
ggplot(crime_heatmap, aes(x = Crime.Type, y = Location.Category, fill = Crime_Count)) +
    geom_tile(color = "white") +
    geom_text(aes(label = Crime_Count), color = "black", size = 3) +  # Annotate crime counts
    scale_fill_gradient(low = "lightblue", high = "darkred") +       # Adjust color scale
    labs(
        title = "Crime Distribution Across Location Categories",
        x = "Crime Type",
        y = "Location Category",
        fill = "Crime Count"
    ) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate X-axis labels for clarity