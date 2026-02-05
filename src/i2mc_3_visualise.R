library(tidyverse)
library(shiny)
library(arrow)

# if fixation file not in folder: 
# download.file('https://share.eva.mpg.de/public.php/dav/files/4bzN8QaCS4TBBtr/?accept=zip', 'data/temp/i2mc_fix.parquet')
df_test <- read_parquet('data/temp/i2mc_fix.parquet')

shinyApp(ui = fluidPage(
  sidebarLayout(
    sidebarPanel(
      fluidRow(
        column(12, selectInput('name', 'Name:',
                               unique(df_test$participant_name)))
      ),
      fluidRow(
        column(12, radioButtons('recname', 'Recording name:',
                                1:16))
      ),
      fluidRow(
        column(12, verbatimTextOutput('text_legend'))
      )
  ),
    mainPanel(
      fluidRow(
        column(12, textOutput('text'))
      ),
      fluidRow(
        column(12, plotOutput('plot'))
      ),
      fluidRow(
        column(12, plotOutput('plotLower'))
      )
    ))),
  server = function(input, output, session) {
    recchoices <- reactive({
      unique(filter(df_test, participant_name == input$name)$recording_name)
    })
    observeEvent(recchoices(), {
      choices <- recchoices()
      updateRadioButtons(inputId = 'recname', choices = choices)
    })
    
    recdata <- reactive({
      df_test %>% filter(participant_name == input$name & recording_name == input$recname)
    })
    
    fixdata <- reactive({
      recdata() %>% filter(eye_movement_type == 'Fixation') %>% 
        drop_na(recording_timestamp) %>% 
        group_by(eye_movement_type_index) %>% 
        summarise(xpos = first(fixation_point_x),
                  ypos = first(fixation_point_y),
                  dur = first(gaze_event_duration),
                  startT = first(media_timestamp),
                  endT = last(media_timestamp))
    })
    
    i2mcdata <- reactive({
      recdata() %>%
        drop_na(recording_timestamp) %>% 
        drop_na(i2mc_index) %>% 
        group_by(i2mc_index) %>% 
        summarise(xpos = first(xpos),
                  ypos = first(ypos),
                  startT = first(media_timestamp),
                  endT = last(media_timestamp))
    })
    
    gaze_plot_x <- reactive({
      ggplot() + 
        lims(y = c(-500, 2500)) +
        geom_linerange(aes(y = xpos, xmin = startT, xmax = endT), data = fixdata(), linewidth = 5, colour = 'deeppink') +
        guides(colour = guide_legend(title = 'tobii')) +
        geom_linerange(aes(y = xpos, xmin = startT, xmax = endT), data = i2mcdata(), linewidth = 1, colour = 'black') +
        geom_line(aes(media_timestamp, gaze_point_left_x), linewidth = .2, alpha = .3, colour = 'darkgreen', data = recdata())+
        geom_line(aes(media_timestamp, gaze_point_right_x), linewidth = .2, alpha = .3, colour = 'red', data = recdata()) 
    })
    
    gaze_plot_y <- reactive({
      ggplot() + 
        lims(y = c(-500, 1600)) +
        geom_linerange(aes(y = ypos, xmin = startT, xmax = endT), data = fixdata(), linewidth = 5, colour = 'deeppink') +
        geom_linerange(aes(y = ypos, xmin = startT, xmax = endT), data = i2mcdata(), linewidth = 1, colour = 'black') +
        geom_line(aes(media_timestamp, gaze_point_left_y), linewidth = .2, alpha = .3, colour = 'darkgreen', data = recdata())+
        geom_line(aes(media_timestamp, gaze_point_right_y), linewidth = .2, alpha = .3, colour = 'red', data = recdata())
    })
    
    output$plot <- renderPlot({
      gaze_plot_x() +
        theme_classic() +
        geom_rect(aes(xmin = recdata()$media_timestamp[match(T, recdata()$toi)],
                      xmax = max(recdata()$media_timestamp),
                      ymin = 0,
                      ymax = 1920),
                  alpha = .2) +
        labs(x = 'Media time',
             y = 'X gaze coordinates')
    })
    
    output$plotLower <- renderPlot({
      gaze_plot_y() +
        theme_classic() +
        geom_rect(aes(xmin = recdata()$media_timestamp[match(T, recdata()$toi)],
                      xmax = max(recdata()$media_timestamp),
                      ymin = 0,
                      ymax = 1080),
                  alpha = .2) +
        labs(x = 'Media time',
             y = 'Y gaze coordinates')
    })
    
    output$text <- renderText({
      vid <- unique(recdata()$presented_stimulus_name)
      date <- unique(recdata()$recording_date)
      sprintf('Date: %s, Condition: %s', date, vid)
    })
    
    output$text_legend <- renderText({
      "Dark green line: left eye gaze\nRed line: right eye gaze\nPink dash: tobii fixations\nBlack dash: i2mc fixations\nGrey rectangle:\nscreen size limits and TOI"
    })
  }
)

