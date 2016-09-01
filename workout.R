# Create dataframe for sticking goals into
start_date <- as.Date('2016-09-05')
df <- data.frame(date = seq(start_date,
                            start_date + 100,
                            by = 1))
df$day <- weekdays(df$date)

# Starting values (ie, baseline fitness)
baseline <- data.frame(cardio_minutes = 20,
                       pushups = 200,
                       pullups_joe = 40,
                       pullups_ben = 20,
                       dips = 40,
                       leg_lifts = 200,
                       crunches = 200,
                       ab_squeezes = 20,
                       stair_minutes_ben = 20,
                       lunges_joe = 40,
                       squats_joe = 40,
                       jumps_joe = 40,
                       calf_raises = 40)

# Populate df with empty columns
for (j in names(baseline)){
  df[,j] <- NA
}

# Populate workout plan 
days_to_increase <- 14

for (i in 1:nrow(df)){
  multiplier <- ((i %/% days_to_increase) * 0.5) + 1
  for (j in names(baseline)){
    df[i,j] <- baseline[,j] * multiplier
  }
}

# Define which days are cardio_minutes, workout and rest days
df$cardio_minutes_day <- df$day %in% c('Monday',
                                'Tuesday',
                                'Wednesday',
                                'Thursday',
                                'Friday')
df$workout_day <- df$day %in% c('Monday',
                                'Wednesday',
                                'Friday')

df$cardio_minutes[!df$cardio_minutes_day] <- 0
not_workout <- which(!df$workout_day)
for (j in names(baseline)[names(baseline) != 'cardio_minutes']){
  for (i in 1:nrow(df)){
    if(i %in% not_workout){
      df[i,j] <- 0
    }
  }
}

readr::write_csv(df, '~/Desktop/df.csv')
