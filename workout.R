# Create dataframe for sticking goals into
start_date <- as.Date('2016-08-08')
df <- data.frame(date = seq(start_date,
                            start_date + 100,
                            by = 1))
df$day <- weekdays(df$date)

# Starting values (ie, baseline fitness)
baseline <- data.frame(cardio = 10,
                       pushups = 100,
                       pullups = 20,
                       dips = 20,
                       leg_lifts = 100,
                       crunches = 100,
                       ab_squeezes = 10,
                       lunges = 20,
                       squats = 10,
                       jumps = 10,
                       vertical_lifts = 10)

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

# Define which days are cardio, workout and rest days
df$cardio_day <- df$day %in% c('Monday',
                                'Tuesday',
                                'Wednesday',
                                'Thursday',
                                'Friday')
df$workout_day <- df$day %in% c('Monday',
                                'Wednesday',
                                'Friday')

df$cardio[!df$cardio_day] <- 0
not_workout <- which(!df$workout_day)
for (j in names(baseline)[names(baseline) != 'cardio']){
  for (i in 1:nrow(df)){
    if(i %in% not_workout){
      df[i,j] <- 0
    }
  }
}

readr::write_csv(df, '~/Desktop/df.csv')
