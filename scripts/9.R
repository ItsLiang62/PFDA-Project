# Wang Liang Xuan, TP076334
#
#
#

# ----- Libraries -----
install.packages("tidyverse")

library(tidyverse)

# ----- Functions -----

mins_since_midnight = function(time) {
  mins = (time %/% 100) * 60 + (time %% 100)
  mins[time == 2400] = 0
  return (mins)
}

duration_in_mins = function(earlier_time, later_time, overnight=0) {
  earlier_time_mins = mins_since_midnight(earlier_time)
  later_time_mins   = mins_since_midnight(later_time)
  
  duration = (later_time_mins - earlier_time_mins) + 1440 * overnight
  return(duration)
}

mins_diff = function(baseline_time, actual_time, overnight=0) {
  baseline_time_mins = mins_since_midnight(baseline_time)
  actual_time_mins = mins_since_midnight(actual_time)
  mins_diff = actual_time_mins - baseline_time_mins
  
  diff = (1440 * overnight) + mins_diff
  return (diff)
}

nearest_mins_diff = function(more_mins, less_mins, clockwise=TRUE) {
  if (clockwise) {
    diff = less_mins + 1440 - more_mins
  } else {
    diff = more_mins - less_mins
  }
  return (diff)
}

num_overnight = function(baseline, actual, duration) {
  baseline_mins = mins_since_midnight(baseline)
  total_mins = baseline_mins + duration
  actual_mins = mins_since_midnight(actual)
  total_mins_since = total_mins %% 1440
  
  overnight = floor(total_mins / 1440)

  # case: actual time has lower mins, clockwise addition
  overnight = ifelse(
    actual_mins < baseline_mins & duration >= 0,
    ifelse(
      overnight == 0, 
      1, # no overnight after clockwise addition, illogical
      ifelse(
        (total_mins_since > actual_mins) &
          (nearest_mins_diff(total_mins_since, actual_mins, TRUE) <
             nearest_mins_diff(total_mins_since, actual_mins, FALSE)),
        overnight + 1,
        ifelse(
          (actual_mins > total_mins_since) &
            (nearest_mins_diff(actual_mins, total_mins_since, TRUE) <
               nearest_mins_diff(actual_mins, total_mins_since, FALSE)),
          ifelse(overnight - 1 == 0, 1, overnight - 1), # safeguard
          overnight
        )
      )
    ),
    overnight
  )
  
  # case: actual time has lower mins, anti-clockwise subtraction
  overnight = ifelse(
    actual_mins < baseline_mins & duration < 0 & overnight < 0,
    ifelse(
      (total_mins_since < actual_mins) &
        (nearest_mins_diff(actual_mins, total_mins_since, TRUE) <
           nearest_mins_diff(actual_mins, total_mins_since, FALSE)),
      overnight - 1,
      ifelse(
        (actual_mins < total_mins_since) &
          (nearest_mins_diff(total_mins_since, actual_mins, TRUE) <
             nearest_mins_diff(total_mins_since, actual_mins, FALSE)),
        overnight + 1,
        overnight
      )
    ),
    overnight
  )
  
  # case: actual time has higher mins, anti-clockwise subtraction
  overnight = ifelse(
    actual_mins >= baseline_mins & duration <= 0,
    ifelse(
      overnight == 0,
      -1, # no overnight after anti-clockwise subtraction, illogical
      ifelse(
        (total_mins_since > actual_mins) &
          (nearest_mins_diff(total_mins_since, actual_mins, TRUE) <
             nearest_mins_diff(total_mins_since, actual_mins, FALSE)),
        ifelse(overnight + 1 == 0, -1, overnight + 1), # safeguard
        ifelse(
          (actual_mins > total_mins_since) &
            (nearest_mins_diff(actual_mins, total_mins_since, TRUE) <
               nearest_mins_diff(actual_mins, total_mins_since, FALSE)),
          overnight - 1,
          overnight
        )
      )
    ),
    overnight
  )
  
  # case: actual time has higher mins, clockwise addition
  overnight = ifelse(
    actual_mins >= baseline_mins & duration > 0 & overnight > 0,
    ifelse(
      (total_mins_since < actual_mins) &
        (nearest_mins_diff(actual_mins, total_mins_since, TRUE) <
           nearest_mins_diff(actual_mins, total_mins_since, FALSE)),
      overnight - 1,
      ifelse(
        (actual_mins < total_mins_since) &
          (nearest_mins_diff(total_mins_since, actual_mins, TRUE) <
             nearest_mins_diff(total_mins_since, actual_mins, FALSE)),
        overnight + 1,
        overnight
      )
    ),
    overnight
  )
  
  overnight = ifelse(baseline_mins == actual_mins & duration == 0, 0, overnight)
  return(overnight)
}


# ----- Data Import -----
flights = read.csv("data/flights.csv")
airlines = read.csv("data/iata_airline_codes.csv")
airports = read.csv("data/iata_airport_codes.csv")

# ----- Pre-Validation Data Cleaning -----
str(flights)
head(flights)
colSums(is.na(flights))

# Drop the index column
flights = flights %>% select(-X)

# ----- Data validation -----

# To accurately recalculate ARRIVAL_DELAY and DEPARTURE_DELAY
# We record overnight count for arrival and departure delay
flights = flights %>% mutate(
  ARRIVAL_DELAY_OVERNIGHT_COUNT = num_overnight(SCHEDULED_ARRIVAL, ARRIVAL_TIME, ARRIVAL_DELAY),
  DEPARTURE_DELAY_OVERNIGHT_COUNT = num_overnight(SCHEDULED_DEPARTURE, DEPARTURE_TIME, DEPARTURE_DELAY)
)

# To accurately recalculate ELAPSED_TIME
# We record overnight count for elapsed time
flights = flights %>% mutate(
  ELAPSED_TIME_OVERNIGHT_COUNT = num_overnight(DEPARTURE_TIME, ARRIVAL_TIME, ELAPSED_TIME),
)

# To accurately recalculate AIR_TIME
# We record overnight count for air time
flights = flights %>% mutate(
  AIR_TIME_OVERNIGHT_COUNT = num_overnight(WHEELS_OFF, WHEELS_ON, AIR_TIME)
)

# To accurately recalculate TAXI_OUT
# We record overnight count for taxi out
flights = flights %>% mutate(
  TAXI_OUT_OVERNIGHT_COUNT = num_overnight(DEPARTURE_TIME, WHEELS_OFF, TAXI_OUT)
)

# To accurately recalculate TAXI_IN
# We record overnight count for taxi in
flights = flights %>% mutate(
  TAXI_IN_OVERNIGHT_COUNT = num_overnight(WHEELS_ON, ARRIVAL_TIME, TAXI_IN)
)

# To accurately recalculate SCHEDULED_TIME
# We record overnight count for scheduled time
flights = flights %>% mutate(
  SCHEDULED_TIME_OVERNIGHT_COUNT = num_overnight(SCHEDULED_DEPARTURE, SCHEDULED_ARRIVAL, SCHEDULED_TIME)
)

# Some ELAPSED_time don't match with DEPARTURE_TIME and ARRIVAL_TIME
flights %>% filter(
  duration_in_mins(DEPARTURE_TIME, ARRIVAL_TIME, ELAPSED_TIME_OVERNIGHT_COUNT) != ELAPSED_TIME
) %>% select(
  DEPARTURE_TIME, ELAPSED_TIME, ARRIVAL_TIME, ELAPSED_TIME_OVERNIGHT_COUNT
)

# DEPARTURE_TIME and ARRIVAL_TIME are assumed always valid
# Recalculate ELAPSED_TIME
flights = flights %>% mutate(
  ELAPSED_TIME = duration_in_mins(DEPARTURE_TIME, ARRIVAL_TIME, ELAPSED_TIME_OVERNIGHT_COUNT)
)

# Some AIR_TIME don't match with WHEELS_OFF and WHEELS_ON
flights %>% filter(
  duration_in_mins(WHEELS_OFF, WHEELS_ON, AIR_TIME_OVERNIGHT_COUNT) != AIR_TIME
) %>% select(
  WHEELS_OFF, AIR_TIME, WHEELS_ON, AIR_TIME_OVERNIGHT_COUNT
)

# WHEELS_OFF and WHEELS_ON are assumed always valid
# Recalculate AIR_TIME
flights = flights %>% mutate(
  AIR_TIME = duration_in_mins(WHEELS_OFF, WHEELS_ON, AIR_TIME_OVERNIGHT_COUNT)
)

# TAXI_OUT match with DEPARTURE_TIME and WHEELS_OFF
# No problem here
flights %>% filter(
  duration_in_mins(DEPARTURE_TIME, WHEELS_OFF, TAXI_OUT_OVERNIGHT_COUNT) != TAXI_OUT
) %>% select(
  DEPARTURE_TIME, TAXI_OUT, WHEELS_OFF, TAXI_OUT_OVERNIGHT_COUNT
)

# TAXI_IN match with WHEELS_ON and ARRIVAL_TIME
# No problem here
flights %>% filter(
  duration_in_mins(WHEELS_ON, ARRIVAL_TIME, TAXI_IN_OVERNIGHT_COUNT) != TAXI_IN
) %>% select(
  WHEELS_ON, TAXI_IN, ARRIVAL_TIME, TAXI_IN_OVERNIGHT_COUNT
)

# Some SCHEDULED_TIME don't match with SCHEDULED_DEPARTURE and SCHEDULED_ARRIVAL
flights %>% filter(
  duration_in_mins(SCHEDULED_DEPARTURE, SCHEDULED_ARRIVAL, SCHEDULED_TIME_OVERNIGHT_COUNT) != SCHEDULED_TIME
) %>% select(
  SCHEDULED_DEPARTURE, SCHEDULED_TIME, SCHEDULED_ARRIVAL, SCHEDULED_TIME_OVERNIGHT_COUNT
)

# SCHEDULED_DEPARTURE and SCHEDULED_ARRIVAL are assumed always valid
# Recalculate SCHEDULED_TIME
flights = flights %>% mutate(
  SCHEDULED_TIME = duration_in_mins(SCHEDULED_DEPARTURE, SCHEDULED_ARRIVAL, SCHEDULED_TIME_OVERNIGHT_COUNT)
)

# Some ELAPSED_TIME don't match the total of AIR_TIME, TAXI_IN, TAXI OUT
flights %>% filter(
  ELAPSED_TIME < AIR_TIME + TAXI_IN + TAXI_OUT
) %>% select(
  TAXI_OUT, AIR_TIME, TAXI_IN, ELAPSED_TIME
)

# Because AIR_TIME is higher than ELAPSED_TIME
# Due to AIR_TIME_OVERNIGHT_COUNT more than ELAPSED_TIME_OVERNIGHT_COUNT
# Due to poorly recorded WHEELS_OFF WHEELS_ON or DEPARTURE_TIME ARRIVAL_TIME, which we always assume valid
flights %>% filter(
  AIR_TIME_OVERNIGHT_COUNT > ELAPSED_TIME_OVERNIGHT_COUNT
) %>% select(
  TAXI_OUT, AIR_TIME, TAXI_IN, ELAPSED_TIME, DEPARTURE_TIME, ARRIVAL_TIME, ELAPSED_TIME_OVERNIGHT_COUNT, WHEELS_OFF, WHEELS_ON, AIR_TIME_OVERNIGHT_COUNT
)

# Delete these rows with data inconsistency
flights = flights %>% filter(
  !(AIR_TIME_OVERNIGHT_COUNT > ELAPSED_TIME_OVERNIGHT_COUNT)
)

# ARRIVAL_DELAY matches SCHEDULED_ARRIVAL and ARRIVAL_TIME
# No problem here
flights %>% filter(
  ARRIVAL_DELAY != mins_diff(SCHEDULED_ARRIVAL, ARRIVAL_TIME, ARRIVAL_DELAY_OVERNIGHT_COUNT),
) %>% select(
  SCHEDULED_ARRIVAL, ARRIVAL_DELAY, ARRIVAL_TIME, ARRIVAL_DELAY_OVERNIGHT_COUNT
)

# DEPARTRURE_DELAY matches SCHEDULED_DEPARTURE and DEPARTURE_TIME
# No problem here
flights %>% filter(
  DEPARTURE_DELAY != mins_diff(SCHEDULED_DEPARTURE, DEPARTURE_TIME, DEPARTURE_DELAY_OVERNIGHT_COUNT),
) %>% select(
  SCHEDULED_DEPARTURE, DEPARTURE_DELAY, DEPARTURE_TIME, DEPARTURE_DELAY_OVERNIGHT_COUNT
)


# Main data set for analysis
delayed_flights = flights %>% filter(
  ARRIVAL_DELAY > 0
)

# ARRIVAL_DELAY matches the sum of all delay types
# No problem here
delayed_flights %>% filter(
  ARRIVAL_DELAY != AIR_SYSTEM_DELAY + SECURITY_DELAY + AIRLINE_DELAY + LATE_AIRCRAFT_DELAY + WEATHER_DELAY
) %>% select(AIR_SYSTEM_DELAY, SECURITY_DELAY, AIRLINE_DELAY, LATE_AIRCRAFT_DELAY, WEATHER_DELAY, ARRIVAL_DELAY)

# --- Additional 

# ----- Post-Validation Data Cleaning -----
str(delayed_flights)
head(delayed_flights)
colSums(is.na(delayed_flights))

# Delete rows where all reasons for delay are NA
delayed_flights = delayed_flights %>% filter(!(if_all(
  c(AIR_SYSTEM_DELAY, 
    SECURITY_DELAY, 
    AIRLINE_DELAY, 
    LATE_AIRCRAFT_DELAY,
    WEATHER_DELAY), is.na)))

# Remove any duplicate rows
delayed_flights = delayed_flights %>% distinct()

# Inspecting airlines
str(airlines)
head(airlines)
colSums(is.na(airlines))

# Remove any duplicate rows based on IATA_CODE
airlines = airlines %>% distinct(IATA_CODE, .keep_all=TRUE)

# Saving the levels of airlines IATA_CODE
airlines_iata_levels = levels(as.factor(airlines$IATA_CODE)) 

# Making IATA_CODE categorical
airlines$IATA_CODE = factor(airlines$IATA_CODE, airlines_iata_levels)

# Inspecting airports
str(airports)
head(airports)
colSums(is.na(airports))

# Checking LATITUDE for outliers and replace NA with clean mean (ignoring outliers)
lat_outliers = boxplot.stats(airports$LATITUDE)$out
clean_mean_lat = mean(airports$LATITUDE[!(airports$LATITUDE %in% lat_outliers)], na.rm=TRUE)

airports$LATITUDE[is.na(airports$LATITUDE)] = clean_mean_lat

# Checking LONGITUDE for outliers and replace NA with clean mean (ignoring outliers)
long_outliers = boxplot.stats(airports$LONGITUDE)$out
clean_mean_long = mean(airports$LONGITUDE[!(airports$LONGITUDE %in% long_outliers)], na.rm=TRUE)

airports$LONGITUDE[is.na(airports$LONGITUDE)] = clean_mean_long

# Remove any duplicate rows based on IATA_CODE
airports = airports %>% distinct(IATA_CODE, .keep_all=TRUE)

# Saving the levels of airports IATA_CODE
airports_iata_levels = levels(as.factor(airports$IATA_CODE)) 

# Making IATA_CODE categorical
airports$IATA_CODE = factor(airports$IATA_CODE, airports_iata_levels)

# Back to flights, make AIRLINE, ORIGIN_AIRPORT, DESTINATION_AIRPORT categorical
delayed_flights$AIRLINE = factor(delayed_flights$AIRLINE, airlines_iata_levels)
delayed_flights$ORIGIN_AIRPORT = factor(delayed_flights$ORIGIN_AIRPORT, airports_iata_levels)
delayed_flights$DESTINATION_AIRPORT = factor(delayed_flights$DESTINATION_AIRPORT, airports_iata_levels)

# Make YEAR, MONTH, DAY and DAY_OF_WEEK categorical as well
delayed_flights$YEAR = as.factor(delayed_flights$YEAR)
delayed_flights$MONTH = as.factor(delayed_flights$MONTH)
delayed_flights$DAY = as.factor(delayed_flights$DAY)
delayed_flights$DAY_OF_WEEK = as.factor(delayed_flights$DAY_OF_WEEK)

# Remove rows where ORIGIN_AIRPORT or DESTINATION_AIRPORT are NA after categorization
delayed_flights = delayed_flights %>% filter(
  !is.na(ORIGIN_AIRPORT) & !is.na(DESTINATION_AIRPORT)
)

# Drop DIVERTED and CANCELLED column because they store only one unique value
unique(delayed_flights$DIVERTED)
unique(delayed_flights$CANCELLED)
delayed_flights = delayed_flights %>% select(-DIVERTED, -CANCELLED)

# ----- [Wang Liang Xuan, TP076334] -----
# Objective 1: To investigate the effect of airline carriers on airline-caused delays

# Analysis 1.1: How do different airlines compare in average airline-caused delays?

# Average airline-caused delay per airline
airline_delay_summary = delayed_flights %>%
  group_by(AIRLINE) %>%
  summarise(
    avg_airline_delay = mean(AIRLINE_DELAY, na.rm=TRUE),
    median_airline_delay = median(AIRLINE_DELAY, na.rm=TRUE),
    count = n()
  ) %>%
  arrange(desc(avg_airline_delay))

print(airline_delay_summary)

# Visualization
ggplot(delayed_flights, aes(x = AIRLINE, y = AIRLINE_DELAY)) +
  geom_boxplot(fill = "skyblue", outlier.color = "red", alpha=0.6) +
  labs(
    title = "Distribution of Airline-Caused Delays by Airline",
    x = "Airline",
    y = "Airline Delay (minutes)"
  ) +
  theme_minimal()

# Analysis 1.2: Are the differences in airline-caused delays across airlines statistically significant?

# One-way ANOVA
anova_model = aov(AIRLINE_DELAY ~ AIRLINE, data = delayed_flights)
summary(anova_model)

# Post-hoc test
TukeyHSD(anova_model)

# Analysis 1.3: What external factors moderate the relationship between airline and airline-caused delay?

# Average delay by airline and day of week
airline_day_summary = delayed_flights %>%
  group_by(AIRLINE, DAY_OF_WEEK) %>%
  summarise(avg_delay = mean(AIRLINE_DELAY, na.rm=TRUE)) %>%
  ungroup()

print(airline_day_summary)

# Visualization: Heat Map
ggplot(airline_day_summary, aes(x = factor(DAY_OF_WEEK), y = AIRLINE, fill = avg_delay)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "lightyellow", high = "red") +
  labs(
    title = "Average Airline Delay by Airline and Day of Week",
    x = "Day of Week",
    y = "Airline",
    fill = "Avg Delay (mins)"
  ) +
  theme_minimal()

# ----- [Name, ID] -----
# Objective 2:

# Analysis 2.1:


# Analysis 2.2:


# Analysis 2.3:


# ----- [Name, ID] -----
# Objective 3:

# Analysis 3.1:


# Analysis 3.2:


# Analysis 3.3:


# ----- [Name, ID] -----
# Objective 4:

# Analysis 4.1:


# Analysis 4.2:


# Analysis 4.3:


# ----- Additional features -----

# Extra Feature 1
# Route Congestion Score

# Count how many flights depart from each origin
origin_counts <- delayed_flights %>%
  group_by(ORIGIN_AIRPORT) %>%
  summarise(origin_flights = n())

# Count how many flights arrive at each destination
dest_counts <- delayed_flights %>%
  group_by(DESTINATION_AIRPORT) %>%
  summarise(dest_flights = n())

# Join these counts back to the data set
delayed_flights <- delayed_flights %>%
  left_join(origin_counts, by = "ORIGIN_AIRPORT") %>%
  left_join(dest_counts, by = "DESTINATION_AIRPORT")

# Create new feature: Route congestion score
delayed_flights <- delayed_flights %>%
  mutate(route_congestion = origin_flights + dest_flights)

# Check the new feature
head(delayed_flights %>% select(ORIGIN_AIRPORT, DESTINATION_AIRPORT, route_congestion))

# Extra Feature 2


# Extra Feature 3


# Extra Feature 4

