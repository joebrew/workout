library(dplyr)
# Create dataframe for sticking goals into
start_date <- as.Date('2016-09-05')
df <- data.frame(date = seq(start_date,
                            start_date + 100,
                            by = 1))
df$day <- weekdays(df$date)

# Starting values (ie, baseline fitness)
baseline <- data.frame(extra = NA,
                       weight = NA,
                       cardio_minutes = 20,
                       pushups = 200,
                       pullups = 40,
                       dips = 40,
                       leg_lifts = 200,
                       crunches = 200,
                       planks = 200,
                       stair_minutes = 20,
                       lunges = 40,
                       jumps = 40,
                       calf_raises = 40)



people <- c('joe', 'ben')

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

days_to_increase <- 14
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

# Manually make some person specific modifications
# ben starts at 2- pullups and 20 dips
# ben doesn't do lunges or jumps but he does do stair minutes
result$scheduled[result$person == 'ben' &
                  result$activity == 'pullups'] <-
  result$scheduled[result$person == 'ben' &
                    result$activity == 'pullups'] - 20
result$scheduled[result$person == 'ben' &
                  result$activity == 'dips'] <-
  result$scheduled[result$person == 'ben' &
                    result$activity == 'dips'] - 20
result$scheduled[result$person == 'ben' &
                  result$activity == 'lunges'] <- NA
result$scheduled[result$person == 'ben' &
                  result$activity == 'jumps'] <- NA
result$scheduled[result$person == 'joe' &
                  result$activity == 'stair_minutes'] <- NA

# Write over df
df <- result; rm(result, baseline)

# Get rid of the extra rows
remove <-
  is.na(df$scheduled) &
          !df$activity %in% c('extra', 'weight')
df <- df[!remove,]


readr::write_csv(df, '~/Desktop/df.csv')
