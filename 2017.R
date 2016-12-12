# Libraries
library(dplyr)

# Create some objects for exercises and dates, etc.
dates <- seq(as.Date('2016-12-12'),
             as.Date('2017-12-31'),
             by = 1)
dow <- weekdays(dates)
day_number <- 1:length(dates)
exercises <- c('pullups',
               'pushups',
               'hanging_knee_balls',
               'ab_roller',
               'hanging_leg_lifts',
               'dips',
               'crunches',
               'dynamic_plank',
               'plank',
               'calf_raises',
               'cardio_minutes',
               'sprints',
               'stairs')
people <- c('joe', 'ben', 'andrew', 'coloma')

# Define function for creating a day
create_day <- function(person = 'joe',
                       date = '2016-12-30'){
  date <- as.Date(date)
  # WORKOUT DAY
  if(weekdays(date) %in% c('Monday', 'Wednesday', 'Friday')){
    df <- data.frame(date = date,
                     dow = weekdays(date),
                     person = person,
                     exercise = exercises,
                     value = NA)
    
    if(person == 'joe'){
      df$value[df$exercise == 'pushups'] <- 100
      df$value[df$exercise == 'pullups'] <- 50
      df$value[df$exercise == 'hanging_knee_balls'] <- 0
      df$value[df$exercise == 'hanging_leg_lifts'] <- 50
      df$value[df$exercise == 'cardio_minutes'] <- 30
      df$value[df$exercise == 'dips'] <- 50
      df$value[df$exercise == 'crunches'] <- 0
      df$value[df$exercise == 'dynamic_plank'] <- 50
      df$value[df$exercise == 'sprints'] <- 5
      df$value[df$exercise == 'stairs'] <- 0
      df$value[df$exercise == 'calf_raises'] <- 50
      
      
    } else if(person == 'ben'){
      df$value[df$exercise == 'pushups'] <- 200
      df$value[df$exercise == 'pullups'] <- 50
      df$value[df$exercise == 'hanging_knee_balls'] <- 0
      df$value[df$exercise == 'ab_roller'] <- 50
      df$value[df$exercise == 'hanging_leg_lifts'] <- 0
      df$value[df$exercise == 'cardio_minutes'] <- 30
      df$value[df$exercise == 'dips'] <- 0
      df$value[df$exercise == 'crunches'] <- 200
      df$value[df$exercise == 'plank'] <- 5
      df$value[df$exercise == 'sprints'] <- 0
      df$value[df$exercise == 'stairs'] <- 20
      
    } else if(person == 'andrew'){
      df$value[df$exercise == 'pushups'] <- 100
      df$value[df$exercise == 'pullups'] <- 50
      df$value[df$exercise == 'hanging_knee_balls'] <- 50
      df$value[df$exercise == 'hanging_leg_lifts'] <- 50
      df$value[df$exercise == 'cardio_minutes'] <- 30
      df$value[df$exercise == 'dips'] <- 50
      df$value[df$exercise == 'crunches'] <- 0
      df$value[df$exercise == 'dynamic_plank'] <- 50
      df$value[df$exercise == 'sprints'] <- 5
      df$value[df$exercise == 'stairs'] <- 0
    } else if(person == 'coloma'){
      df$value[df$exercise == 'pushups'] <- 100
      df$value[df$exercise == 'pullups'] <- 10
      df$value[df$exercise == 'hanging_knee_balls'] <- 50
      df$value[df$exercise == 'hanging_leg_lifts'] <- 0
      df$value[df$exercise == 'cardio_minutes'] <- 30
      df$value[df$exercise == 'dips'] <- 50
      df$value[df$exercise == 'crunches'] <- 0
      df$value[df$exercise == 'dynamic_plank'] <- 50
      df$value[df$exercise == 'sprints'] <- 5
      df$value[df$exercise == 'stairs'] <- 0
    }
    
    # CARDIO ONLY DAY
  } else if (weekdays(date) %in% c('Tuesday', 'Thursday')){
    df <- data.frame(date = date,
                     dow = weekdays(date),
                     person = person,
                     exercise = c('cardio_minutes'),
                     value = 30) 
    # REST DAY
  } else {
    df <- data.frame(date = date,
                     dow = weekdays(date),
                     person = person,
                     exercise = c('extra'),
                     value = '') 
  }
  return(df)
}

i <- 0
results_list <- list()
for (the_date in 1:length(dates)){
  for(the_person in 1:length(people)){
    i <- i + 1
    x <- create_day(date = dates[the_date],
                    person = people[the_person])
    results_list[[i]] <- x
    message(the_person)
    message(the_date)
  }
}
final <- do.call('rbind', results_list)
final <-
  final %>%
  filter(value != 0,
         !is.na(value))

final$done <- ''
final$day_number <- as.numeric(final$date - min(final$date)) + 1
final <- 
  final %>%
  dplyr::select(date, 
                dow,
                day_number,
                person, 
                exercise,
                value, 
                done)
readr::write_csv(final, '~/Desktop/final.csv')

