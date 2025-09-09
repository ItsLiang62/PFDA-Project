# Wang Liang Xuan, TP076334
#
#
#

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


# ----- Data import -----
flights = read.csv("data/flights.csv")
airlines = read.csv("data/iata_airline_codes.csv")
airports = read.csv("data/iata_airport_codes.csv")

# ----- Data cleaning -----
head(flights)
colSums(is.na(flights))

# Drop the index column
flights = flights %>% select(-X)

# Delete rows where all these columns are NA
# Now, no NA entry exist
flights = flights %>% filter(!(if_all(
  c(AIR_SYSTEM_DELAY, 
    SECURITY_DELAY, 
    AIRLINE_DELAY, 
    LATE_AIRCRAFT_DELAY), is.na)))

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


# ----- Student ID: TP076334 -----
# Objective 1:

# Analysis 1.1:


# Analysis 1.2:


# Analysis 1.3:


# ----- Student ID: -----
# Objective 2:

# Analysis 2.1:


# Analysis 2.2:


# Analysis 2.3:


# ----- Student ID: -----
# Objective 3:

# Analysis 3.1:


# Analysis 3.2:


# Analysis 3.3:


# ----- Student ID: -----
# Objective 4:

# Analysis 4.1:


# Analysis 4.2:


# Analysis 4.3:


# ----- Additional research -----

# Extra Research 1


# Extra Research 2


# Extra Research 3


# Extra Research 4

