# Libraries
library(shiny)
library(gsheet)
library(tidyr)
library(dplyr)
library(ggplot2)
library(RColorBrewer)
library(DT)

# Read in data
df <- gsheet2tbl(url = 'https://docs.google.com/spreadsheets/d/1zuCFYKM3LUlGKQ5rVBAE7upzqlvoxv0YN4ziHT2Y9y8/edit#gid=885247896')

# Make date column
df$date <- as.Date(df$date)

# Make numeric
df$done <- as.numeric(df$done)

# Function for getting days done and days to go
days <- function(){
  start_date <- min(df$date)
  end_date <- max(df$date)
  today <- Sys.Date()
  return(c(
    as.numeric(today - start_date),
    as.numeric(end_date - today)
  ))
}

# Define function for plotting
plot_activity <- function(exercise = 'pullups'){
 
   # Subset the data
  sub_data <- df %>%
    filter(type == exercise)
  
  # Helper to get cumsum with na
  cum_summer <- function(x){
    x[is.na(x)] <- 0
    cumsum(x)
  }
  
  # Make cumulative if applicable
  sub_title <- ''
  if(exercise != 'weight'){
    sub_data <- sub_data %>%
      arrange(date) %>%
      group_by(person, date) %>%
      summarise(done = sum(done, na.rm = TRUE),
             scheduled = sum(scheduled, na.rm = TRUE)) %>%
      group_by(person) %>%
      mutate(done = cum_summer(done),
             scheduled = cum_summer(scheduled)) %>%
        ungroup
    sub_title <- 'Cumulative'
  }
  
  # Gather to include the scheduled column (if applicable)
  sub_data <- sub_data %>%
    gather(key = scheduled,
           value = done,
           scheduled:done)
  
  # Group
  sub_data <-
    sub_data %>%
    group_by(date, person, scheduled) %>%
    summarise(done = sum(done, na.rm = TRUE))

  # Make colors
  cols <- colorRampPalette(brewer.pal(9, 'Spectral'))(length(unique(sub_data$person)))
  # Plot
  g <-
    ggplot(data = sub_data,
           aes(x = date,
               y = done,
               # group = person,
               color = person,
               lty = scheduled)) +
    geom_line(alpha = 0.6) +
    scale_color_manual(name = '',
                       values = cols) +
    xlab('Date') +
    ylab(exercise) +
    theme_bw() +
    ggtitle(exercise, sub_title) +
  xlim(min(df$date), max(df$date))
  return(g)
}
#

# Define function for table
activity_table <- function(exercise = 'pullups'){
  require(DT)
  # Subset the data
  sub_data <- df %>%
    filter(activity == exercise)
  
  # Make cumulative if applicable
  if(exercise != 'weight'){
    sub_data <- sub_data %>%
      group_by(person) %>%
      mutate(done_cumulative = cumsum(done),
             scheduled_cumulative = cumsum(scheduled)) %>%
      ungroup
  }
  DT::datatable(sub_data)
  }

# Define function for today's workout
today_table <- function(date = NULL,
                        person = NULL){
  if(is.null(date)){
    the_date <- Sys.Date()
  } else {
    the_date <- date
  }
  the_person <- person
  sub_data <- df %>%
    filter(date == the_date,
           person %in% c(the_person)) %>%
    dplyr::select(-date, -day)
  DT::datatable(sub_data)
}

# Define UI for application that draws a histogram
ui <- shinyUI(fluidPage(
   
   # Application title
   titlePanel("100 days of pain"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
         dateInput('date', 'Date'),
         checkboxGroupInput(inputId = 'person', label = 'Who',
                            choices = c('ben', 'joe'),
                            selected = c('ben', 'joe'))
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
        h3(paste0(days()[1], ' days done.\n',
                  days()[2], ' days to go.')),
        br(),
        h1("Today's workout"),
        helpText('Select another date to the left to explore workouts for a different date'),
        dataTableOutput('today_table'),
        h1("Progress over time"),
        helpText('Pick an activity below and visualize what has been done and what is left to do.'),
        selectInput(inputId = 'activity',
                    label = 'activity', 
                    choices = c('calf_raises',  
                                'cardio_minutes',  
                                'crunches',  
                                'dips',  
                                # 'extra',  
                                'jumps',  
                                'leg_lifts',  
                                'lunges',  
                                'planks',  
                                'pullups',  
                                'pushups',  
                                'sprints',
                                'stair_minutes',
                                'weight'),
                    selected = 'pullups'),
        h3("Visual"),
         plotOutput("the_plot"),
        h3("Table"),
         dataTableOutput('the_table')
      )
   )
))

# Define server logic required to draw a histogram
server <- shinyServer(function(input, output) {
   
  
   output$the_plot <- renderPlot({
    plot_activity(exercise = input$activity)
   })
  
   output$today_table <- 
     renderDataTable({
       today_table(date = input$date,
                   person = input$person)
     })
   output$the_table <-
     renderDataTable({
       activity_table(exercise = input$activity)
     })
})

# Run the application 
shinyApp(ui = ui, server = server)

