library(dplyr)
# Create dataframe for sticking goals into
start_date <- as.Date('2017-03-13')
df <- data.frame(date = seq(start_date,
                            start_date + 60,
                            by = 1))
df$day <- weekdays(df$date)

# Starting values (ie, baseline fitness)
baseline <- data.frame(#extra = NA,
                       #weight = NA,
                       cardio_minutes = 10,
                       # sprints = 40,
                       pushups = 15,
                       pullups = 5,
                       dips = 5,
                       leg_lifts = 5,
                       crunches = 15,
                       back_crunches = 5,
                       planks = 30,
                       squats = 10,
                       jumps = 10,
                       calf_raises = 15)



# people <- c('megan', 'kevin')
people <- 'megan'

# Go through each date / exercise / person combination
# and populate a dataframe
result <- expand.grid(date = df$date,
                      activity = names(baseline),
                      person = people) %>%
  arrange(date, person) %>%
  mutate(day = weekdays(date)) %>%
  mutate(scheduled = NA,
         done = NA)
result$activity <- as.character(result$activity)
result$person <- as.character(result$person)

days_to_increase <- 7
dates <- sort(unique(result$date))
for (i in 1:length(dates)){
  date <- dates[i]
  # Get days since_start
  days_since_start <- as.numeric(date - min(result$date))
  # Get the multiplication value
  multiplier <- 
    ((days_since_start %/% days_to_increase) * 0.25) + 1 
  for(activity in sort(unique(result$activity))){
    print(activity)
    result$scheduled[result$date == date &
                   result$activity == activity] <-
      baseline[,activity] * multiplier
  }
}

# Set to 0 the non-workout days
result$scheduled[result$day %in%
                   c('Tuesday',
                     'Thursday',
                     'Saturday',
                     'Sunday') &
                   result$activity != 'cardio_minutes'] <- 
  NA

# Set all weekends to 0
result$scheduled[result$day %in%
                   c('Saturday',
                     'Sunday')] <- NA

# Write over df
df <- result; rm(result, baseline)

# Get rid of the extra rows
remove <-
  is.na(df$scheduled) &
          !df$activity %in% c('extra', 'weight')
df <- df[!remove,]

# Remove the person column
df$person <- NULL

# Get nothing in done column
df$done <- ''

# Round
df$scheduled <- round(df$scheduled)

readr::write_csv(df, '~/Desktop/df.csv')
