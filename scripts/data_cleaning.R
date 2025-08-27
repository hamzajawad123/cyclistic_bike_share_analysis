divvy_trips_2019_df <- read_csv("F:/Google_Data_Analysis/case_studies/case_study_1/case_study_1_datasets/Divvy_Trips_2019_Q1.csv")
divvy_trips_2020_df <- read_csv("F:/Google_Data_Analysis/case_studies/case_study_1/case_study_1_datasets/Divvy_Trips_2020_Q1.csv")

View(divvy_trips_2019_df)
View(divvy_trips_2020_df)

colnames(divvy_trips_2019_df)
colnames(divvy_trips_2020_df)

head(divvy_trips_2019_df)
head(divvy_trips_2020_df)

str(divvy_trips_2019_df)
str(divvy_trips_2020_df)

glimpse(divvy_trips_2019_df)
glimpse(divvy_trips_2020_df)

skim_without_charts(divvy_trips_2019_df)
skim_without_charts(divvy_trips_2020_df)

divvy_trips_2019_df <- clean_names(divvy_trips_2019_df)
colnames(divvy_trips_2019_df)

# Cleaning of 2019 dataset

# Converting date from character datatype to actual datetime datatype and dropping character datatype date columns
divvy_trips_2019_df <- divvy_trips_2019_df %>% 
  mutate(
    started_at = ymd_hms(start_time),
    ended_at = ymd_hms(end_time)
    ) %>% 
  select(-start_time, -end_time)

# Dropping rows having null values in columns(trip_id, started_at, ended_at)
divvy_trips_2019_df <- divvy_trips_2019_df %>% 
  filter(!is.na(trip_id), !is.na(started_at), !is.na(ended_at))

# Removing extra spaces from start, end, between of words and also removing the sign --> (*)
divvy_trips_2019_df <- divvy_trips_2019_df %>% 
  mutate(
    from_station_name = str_squish(str_remove_all(from_station_name,"\\(\\*\\)")),
    to_station_name = str_squish(str_remove_all(to_station_name,"\\(\\*\\)"))
    )

# Changing column from "usertype" to "member_casual" and its values(subscribers, customers) to new values(members, casual) to match them with data in 2020 dataset
divvy_trips_2019_df <- divvy_trips_2019_df %>% 
  mutate(
    member_casual = case_when(
      str_to_lower(usertype) == "subscriber" ~ "member",
      str_to_lower(usertype) == "customer" ~ "casual",
      TRUE ~ NA_character_
    )
  ) %>% 
  filter(!is.na(member_casual)) %>% 
  select(-usertype)
  
# Making four new columns("ride_length_min", "day_of_week", "month", "year")
divvy_trips_2019_df <- divvy_trips_2019_df %>% 
  mutate(
    ride_length_min = as.numeric(difftime(ended_at, started_at, units = "mins")),
    day_of_week = wday(started_at, label = TRUE, abbr = TRUE, week_start = 1),
    month = month(started_at, label = TRUE, abbr = TRUE),
    year = year(started_at)
  )

# Dropping rows having rides less than 1 minute and greater than 24 hours
divvy_trips_2019_df <- divvy_trips_2019_df %>% 
  filter(ride_length_min >= 1 & ride_length_min <= 24*60)

# Removing Duplicates
number_of_rows_before <- nrow(divvy_trips_2019_df)
divvy_trips_2019_df <- divvy_trips_2019_df %>% 
  distinct()
number_of_rows_after <- nrow(divvy_trips_2019_df)
rows_removed <- number_of_rows_before - number_of_rows_after
rows_removed

# Dropping unnecessary columns --> demographic columns("gender", "birthyear") and also one more column("bikeid")
divvy_trips_2019_df <- divvy_trips_2019_df %>% 
  select(-gender, -birthyear, -bikeid)

# Making Final Schema of 2019 dataset
divvy_trips_2019_df <- divvy_trips_2019_df %>%
  transmute(
    ride_id = as.character(trip_id),
    rideable_type = "docked_bike",
    started_at,
    ended_at,
    start_station_name = from_station_name,
    start_station_id = as.character(from_station_id),
    end_station_name = to_station_name,
    end_station_id = as.character(to_station_id),
    member_casual,
    ride_length_min,
    day_of_week,
    month,
    year
  )

# Cleaning of 2020 dataset

# Dropping unnecessary columns --> longitude and latitude columns
divvy_trips_2020_df <- divvy_trips_2020_df %>% 
  select(-start_lat, -start_lng, -end_lat, -end_lng)
View(divvy_trips_2020_df)

# Making four new columns("ride_length_min", "day_of_week", "month", "year")
divvy_trips_2020_df <- divvy_trips_2020_df %>% 
  mutate(
    ride_length_min = as.numeric(difftime(ended_at, started_at, units = "mins")),
    day_of_week = wday(started_at, label = TRUE, abbr = TRUE, week_start = 1),
    month = month(started_at, label = TRUE, abbr = TRUE),
    year = year(started_at)
  )

# Removing extra spaces from start, end, between of words and also removing the sign --> (*)
divvy_trips_2020_df <- divvy_trips_2020_df %>% 
  mutate(
    start_station_name = str_squish(str_remove_all(start_station_name,"\\(\\*\\)")),
    end_station_name = str_squish(str_remove_all(end_station_name, "\\(\\*\\)"))
  )

# Converting date from character datatype to actual datetime datatype and dropping character datatype date columns
divvy_trips_2020_df <- divvy_trips_2020_df %>% 
  mutate(
    started_at = ymd_hms(started_at),
    ended_at = ymd_hms(ended_at)
  )

# Dropping rows having null values in columns(ride_id, started_at, ended_at)
divvy_trips_2020_df <- divvy_trips_2020_df %>% 
  filter(!is.na(ride_id), !is.na(started_at), !is.na(ended_at))

# Dropping rows having rides less than 1 minute and greater than 24 hours
divvy_trips_2020_df <- divvy_trips_2020_df %>% 
  filter(ride_length_min >= 1 & ride_length_min <= 24*60)

# Removing Duplicates
number_of_rows_before <- nrow(divvy_trips_2020_df)
divvy_trips_2020_df <- divvy_trips_2020_df %>% 
  distinct()
number_of_rows_after <- nrow(divvy_trips_2020_df)
rows_removed <- number_of_rows_before - number_of_rows_after
rows_removed


# Making Final Schema of 2020 dataset
divvy_trips_2020_df <- divvy_trips_2020_df %>%
  transmute(
    ride_id = as.character(ride_id),
    rideable_type,
    started_at,
    ended_at,
    start_station_name,
    start_station_id = as.character(start_station_id),
    end_station_name,
    end_station_id = as.character(end_station_id),
    member_casual,
    ride_length_min,
    day_of_week,
    month,
    year
  )

# Merging both datasets
all_trips_df <- bind_rows(divvy_trips_2019_df, divvy_trips_2020_df)
View(all_trips_df)

# Saving cleaned datasets as CSV
write_csv(divvy_trips_2019_df, "divvy_trips_2019_cleaned_dataset.csv")
write_csv(divvy_trips_2020_df, "divvy_trips_2020_cleaned_dataset.csv")
write_csv(all_trips_df, "cleaned_merged_dataset_2019_2020.csv")

# Analyzing the cleaned dataset

all_trips_df <- read_csv("F:/Google_Data_Analysis/case_studies/case_study_1/case_study_1_datasets/cleaned_merged_dataset_2019_2020.csv")
glimpse(all_trips_df)
View(all_trips_df)

# Calculate the mean, max, min of ride_length
max_min_mean <- all_trips_df %>% 
  summarize(
    average_ride_length = mean(ride_length_min),
    max_ride_length = max(ride_length_min),
    min_ride_length = min(ride_length_min)
  )
max_min_mean

# Calculate the mode of day_of_week
day_mode <- all_trips_df %>%
  count(day_of_week) %>%    # same as group_by(day_of_week) + summarise(n = n())
  arrange(desc(n)) %>%
  slice(1)
day_mode

# Calculate the average ride_length for members and casual riders.
average_ride_length_for_each_group <- all_trips_df %>% 
  group_by(member_casual) %>% 
  summarize(average = mean(ride_length_min))
average_ride_length_for_each_group

# Calculate the average ride_length for users by day_of_week.
average_ride_length_for_each_group_by_day <- all_trips_df %>% 
  group_by(member_casual, day_of_week) %>% 
  summarize(average = mean(ride_length_min))
average_ride_length_for_each_group_by_day

# Calculate the number of rides for users by day_of_week
num_of_rides <- all_trips_df %>% 
  group_by(member_casual, day_of_week) %>% 
  summarize(ride_count = n())
num_of_rides

# Explore different seasons to make some observations
all_trips_df <- all_trips_df %>% 
  mutate(
    season = case_when(
      month %in% c("Dec", "Jan", "Feb") ~ "Winter",
      month %in% c("Mar", "Apr", "May") ~ "Spring",
      month %in% c("Jun", "Jul", "Aug") ~ "Summer",
      TRUE ~ "Autumn"
    )
  )

# Calculate the number of rides by Season for Members vs Casuals
number_of_rides_by_season_for_each_group <- all_trips_df %>% 
  group_by(season,member_casual) %>% 
  summarize(num_rides = n())
number_of_rides_by_season_for_each_group

# Calculate the average ride length by season for Members vs Casuals
average_ride_length_by_season_for_each_group <- all_trips_df %>% 
  group_by(season, member_casual) %>% 
  summarize(avg_ride_length = mean(ride_length_min))
average_ride_length_by_season_for_each_group

# Calculate the number of rides by month for Members vs Casuals
number_of_rides_by_month_for_each_group <- all_trips_df %>% 
  group_by(month,member_casual) %>% 
  summarize(num_rides = n())
number_of_rides_by_month_for_each_group

# Explore different times of day to make some observations
all_trips_df <- all_trips_df %>% 
  mutate(
    hour = hour(started_at),
    time_of_day = case_when(
      hour %in% 5:11 ~ "Morning",
      hour %in% 12:16 ~ "Afternoon",
      hour %in% 17:20 ~ "Evening",
      TRUE ~ "Night"
    )
  )

# Calculate the number of rides by time of day for Members vs Casuals
number_of_rides_by_time_of_day_for_each_group <- all_trips_df %>% 
  group_by(time_of_day,member_casual) %>% 
  summarize(num_rides = n())
number_of_rides_by_time_of_day_for_each_group

# Finding overall 10 most popular stations
ten_most_popular_stations <- all_trips_df %>% 
  count(start_station_name, member_casual) %>% 
  arrange(desc(n)) %>% 
  slice(1:10)
ten_most_popular_stations

# Finding 10 most popular stations for Members vs Casuals
ten_most_popular_stations_for_each_group <- all_trips_df %>%
  count(start_station_name, member_casual) %>%  
  arrange(member_casual, desc(n)) %>%                        
  group_by(member_casual) %>%                                        
  slice(1:10)                                                  
ten_most_popular_stations_for_each_group

# Explore different parts of week to make some observations
all_trips_df <- all_trips_df %>%
  mutate(
    week_part = case_when(
      day_of_week %in% c("Sat", "Sun") ~ "Weekend",
      TRUE ~ "Weekday"
    )
  )

# Calculate the number of rides by part of week for Members vs Casuals
number_of_rides_by_part_of_week_for_each_group <- all_trips_df %>% 
  group_by(week_part, member_casual) %>% 
  summarize(num_rides = n())
number_of_rides_by_part_of_week_for_each_group

# Visualizations for Share Phase

# ---------------------------------------------------
# 1. Total Number of Rides by User Type
# ---------------------------------------------------
num_of_rides <- all_trips_df %>%
  group_by(member_casual) %>%
  summarise(ride_count = n(), .groups = "drop")

p1 <- ggplot(num_of_rides, aes(x = member_casual, y = ride_count, fill = member_casual)) +
  geom_col(width = 0.6) +
  labs(
    title = "Total Number of Rides by User Type",
    subtitle = "Members consistently account for more rides than casual riders",
    x = "User Type", y = "Number of Rides", fill = "User Type"
  ) +
  scale_y_continuous(labels = comma) +
  scale_fill_manual(values = c("member" = "#2E86AB", "casual" = "#F18F01")) +
  geom_text(aes(label = comma(ride_count)), vjust = -0.08, fontface = "bold") +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, size = 13)
  )

# ---------------------------------------------------
# 2. Ride Patterns by Day of Week
# ---------------------------------------------------
rides_by_day <- all_trips_df %>%
  group_by(member_casual, day_of_week) %>%
  summarise(num_rides = n(), .groups = "drop") %>%
  mutate(day_of_week = factor(day_of_week, 
                              levels = c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")))

p2 <- ggplot(rides_by_day, aes(x = day_of_week, y = num_rides, fill = member_casual)) +
  geom_col(position = "dodge") +
  labs(
    title = "Ride Patterns by Day of Week",
    subtitle = "Casual riders peak on weekends, while members ride mostly on weekdays",
    x = "Day of Week", y = "Number of Rides", fill = "User Type"
  ) +
  scale_y_continuous(labels = comma) +
  scale_fill_manual(values = c("member" = "#2E86AB", "casual" = "#F18F01")) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 18, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, size = 13),
    axis.text.x = element_text(size = 12, face = "bold"),
    axis.text.y = element_text(size = 12, face = "bold")
  )

# ---------------------------------------------------
# 3. Average Ride Length by User Type
# ---------------------------------------------------
avg_ride_length <- all_trips_df %>%
  group_by(member_casual) %>%
  summarise(avg_length = mean(ride_length_min), .groups = "drop")

p3 <- ggplot(avg_ride_length, aes(x = member_casual, y = avg_length, fill = member_casual)) +
  geom_col(width = 0.6) +
  labs(
    title = "Average Ride Length by User Type",
    subtitle = "Casual riders take significantly longer trips than members",
    x = "User Type", y = "Average Ride Length (minutes)", fill = "User Type"
  ) +
  scale_y_continuous(labels = comma) +
  scale_fill_manual(values = c("member" = "#2E86AB", "casual" = "#F18F01")) +
  geom_text(aes(label = round(avg_length, 1)), vjust = -0.08, fontface = "bold") +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 18, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, size = 13)
  )

# ---------------------------------------------------
# 4. Monthly Trends by User Type
# ---------------------------------------------------
rides_by_month <- all_trips_df %>%
  group_by(member_casual, month) %>%
  summarise(num_rides = n(), .groups = "drop") %>%
  mutate(month = factor(month, levels = c("Jan", "Feb", "Mar")))

p4 <- ggplot(rides_by_month, aes(x = month, y = num_rides, color = member_casual, group = member_casual)) +
  geom_line(linewidth = 1.3) +
  geom_point(size = 3) +
  labs(
    title = "Monthly Ride Trends by User Type",
    subtitle = "Q1 Ride Patterns: Casual riders fluctuate, members stay steadier",
    x = "Month", y = "Number of Rides", color = "User Type"
  ) +
  scale_y_continuous(
    labels = comma,
    breaks = seq(0, 200000, 50000)   # <<< custom intervals
  ) +
  scale_color_manual(values = c("member" = "#2E86AB", "casual" = "#F18F01")) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 18, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, size = 13),
    axis.text.x = element_text(size = 12, face = "bold"),
    axis.text.y = element_text(size = 12, face = "bold")
  )

# ---------------------------------------------------
# 5. Top 5 Start Stations by User Type
# ---------------------------------------------------
top_stations <- all_trips_df %>%
  group_by(member_casual, start_station_name) %>%
  summarise(num_rides = n(), .groups = "drop") %>%
  group_by(member_casual) %>%
  slice_max(order_by = num_rides, n = 5)

p5 <- ggplot(top_stations, aes(x = reorder(start_station_name, num_rides), y = num_rides, fill = member_casual)) +
  geom_col(position = "stack") +
  coord_flip() +
  labs(
    title = "Top 5 Start Stations by User Type",
    subtitle = "Casual riders cluster near leisure areas, members near commuting hubs",
    x = "Start Station", y = "Number of Rides", fill = "User Type"
  ) +
  scale_y_continuous(labels = comma) +
  scale_fill_manual(values = c("member" = "#2E86AB", "casual" = "#F18F01")) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 18, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, size = 13)
  )

