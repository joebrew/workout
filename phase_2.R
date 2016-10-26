library(dplyr)

on_day <- data_frame(
  type = c(rep('pushups', 7),
           'dips',
           rep('pullups', 6),
           rep('core', 5),
           rep('squats', 3),
           'calf raises',
           'sprints'),
  activity = c('regular', 'diamond', 'wide', 'shoulder', 'plyo', 'elevated', 'forward',
               'dips',
               'palms forward', 'palms inward', 'palms backward', 'wide', 'high', '1 arm',
               'knee balls', 'horizontal legs out', 'dynamic plank', 'plank', 'back crunches',
               'lunges', 'pistol squats', 'jumps',
               'calf raises',
               'sprints'),
  scheduled = c(240, 10, 10, 10, 10, 10, 10,
                60,
                10, 10, 10, 10, 10, 10,
                60, 10, 60, 300, 60,
                60, 60, 60,
                60,
                60))

cardio_day <- data_frame(
  type = c('extra', 'weight', 'cardio_minutes'),
  activity = c('extra', 'weight', 'cardio_minutes'),
  scheduled = c(NA, NA, 30)
)

off_day <- data_frame(
  type = c('extra', 'weight'),
  activity = c('extra', 'weight'),
  scheduled = c(NA, NA)
)

make_day <- function(date = Sys.Date(),
                     ben = FALSE){
  date <- as.Date(date)
  dow <- weekdays(date)
  its_on <- dow %in% c('Monday', 'Wednesday', 'Friday')
  if(its_on){
    the_rows <- on_day
    if(ben){
      # Remove the leg stuff
      the_rows <-
        the_rows %>%
        filter(!type %in% c('squats', 'sprints'))
      # Add stairs
      the_rows <- 
        rbind(the_rows,
              data_frame(type = 'stairs',
                         activity = 'stairs',
                         scheduled = 30))
    }
  } else {
    if(dow %in% c('Tuesday', 'Thursday')){
      the_rows <- cardio_day  
    } else {
      the_rows <- off_day
    }
  } 
  # Add columns
  the_rows$date <- date
  the_rows$day <- dow
  the_rows$done <- NA
  the_rows$day_number <- as.numeric(date - as.Date('2016-09-05')) +1
  the_rows$person <- ifelse(ben, 'ben', 'joe')
  
  # Reorder
  the_rows <-
    the_rows %>%
    dplyr::select(date, day, type, activity, person, 
                  scheduled, done, day_number)
  
  # Define the increase parameters
  days_since_phase_2 <- as.numeric(date - as.Date('2016-10-24')) + 1
  
  # Get the 2-week chunk number of phase 2
  chunk <- ((days_since_phase_2 - 1) %/% 14) + 1
  
  # Make the increases
  if(its_on){
    the_rows <-
      the_rows %>%
      mutate(
        scheduled = ifelse(type == 'pushups' & activity == 'regular',
                           scheduled - ((chunk - 1) * 60),
                           ifelse(type == 'pushups' & activity != 'regular',
                                  scheduled + (10 * (chunk - 1)),
                                  ifelse(type == 'pullups',
                                         scheduled + (2 * (chunk - 1)),
                                         ifelse(activity == 'horizontal legs out',
                                                scheduled + (10 * (chunk - 1)),
                                                scheduled)))))
    
  }
  
  return(the_rows)
}

results_list <- list()
dates <- seq(as.Date('2016-10-24'),
             as.Date('2016-12-13'),
             by = 1)
counter <- 1
  for(j in 1:length(dates)){
    for (person in c(FALSE, TRUE)){
      
    print(person)
    print(j)
    these <- make_day(date = dates[j],
                      ben = person)
    results_list[[counter]] <- these
    counter <- counter + 1
  }}

final <- do.call('rbind', results_list)
readr::write_csv(final, '~/Desktop/100daysofpain.csv')
