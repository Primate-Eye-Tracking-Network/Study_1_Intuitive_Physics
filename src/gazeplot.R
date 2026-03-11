library(tidyverse)
library(shiny)
library(arrow)

# if data file not in folder:
# download.file('https://share.eva.mpg.de/public.php/dav/files/ZByZWD5HTiPpHWd/?accept=zip', 'data/temp/plots.parquet')
df_plots <- read_parquet('data/temp/plots.parquet')

colours <- c('Calibration'='red', 'check' = 'darkolivegreen', 'grid' = 'cyan4', 'test' = 'purple')
shinyApp(ui = fluidPage(
  sidebarLayout(
    sidebarPanel(width = 2,
                 fluidRow(
                   column(12, radioButtons('stim_type', 'Stimulus type:',
                                           unique(df_plots$stim_type)[2:5],
                                           selected = 'grid'))
                 ),
                 fluidRow(
                   column(12, radioButtons('fix_algorithm', 'Fix algorithm:',
                                           c('tobii', 'i2mc'))
                 )),
                 fluidRow(
                   column(12, numericInput("scalex", label = "X-scale", 
                                          value = 100, 
                                          min = 1))
                 ),
                 fluidRow(
                   column(12, numericInput("scaley", label = "Y-shift", 
                                          value = 0, 
                                          min = 1))
                 ),
                 fluidRow(
                   column(12, radioButtons('name', 'Name:',
                                           arrange(distinct(df_plots, participant_name), participant_name)$participant_name))
                 )),
    mainPanel(
      fluidRow(
        column(6, textOutput('text'))
      ),
      fluidRow(
        column(12, plotOutput('plot', height = 1000))
      )
    ))),
  server = function(input, output, session) {
    tableinfo <- reactive({
      df_plots %>% filter(participant_name == input$name) %>% 
        group_by(recname, recording_date, timeline_name) %>% summarise(n = n())
    })
    
    plotdata <- reactive({
      filtered <- df_plots %>% 
        filter(participant_name == input$name) 
    })
    
    observeEvent(plotdata(), {
      updateNumericInput(session, 'scalex', value = 100)
      updateNumericInput(session, 'scaley', value = 0)
    })
    
    rects <- reactive({
      if (input$stim_type == 'grid'){
        tibble(x = c(rep(c(300,950, 1650),each =3)),
               y=c(rep(c(200,500,900), 3)), 
               width = 100, height = 100)
      } 
      else if (input$stim_type == 'check'){
        tibble(x = c(150, 150, 850, 1600,1600),
               y=c(50, 900, 500, 50, 900), 
               width = 100, height = 100)
      } else{
        tibble(x = NaN, y = NaN, width = NaN, height = NaN)
      }
    })
    
    fixdata <- reactive({
      if (input$fix_algorithm == 'tobii') {
        plotdata() %>%
          filter(eye_movement_type == 'Fixation') %>%
          filter(stim_type == input$stim_type) %>% 
          group_by(recname, eye_movement_type_index, stim_type) %>%
          summarise(dur = mean(gaze_event_duration),
                    idx = mean(eye_movement_type_index),
                    size = n(),
                    xpos = mean(fixation_point_x),
                    ypos = mean(fixation_point_y),
                    startT = first(media_timestamp),
                    endT = last(media_timestamp)) %>%
          mutate(xpos_shift = 1920 - ((1920 - xpos) * input$scalex/100),
                 ypos_shift =  ypos - input$scaley)
      }else if (input$fix_algorithm == 'i2mc'){
        plotdata() %>%
          drop_na(recording_timestamp) %>%
          drop_na(i2mc_index) %>%
          group_by(recname, i2mc_index) %>%
          filter(stim_type == input$stim_type) %>% 
          summarise(xpos = first(xpos),
                    ypos = first(ypos),
                    startT = first(media_timestamp),
                    endT = last(media_timestamp)) %>%
          mutate(size = endT - startT,
                 xpos_shift = 1920 - ((1920 - xpos) * input$scalex/100),
                 ypos_shift = ypos - input$scaley)
      }
    })

    main_plot <- reactive({
      ggplot() +
        lims(x = c(-500, 2500), y = c(1580, -500)) +
        labs(x = 'X coordinates', y = 'Y coordinates') +
        coord_equal(ratio = 1) +
        theme_classic() +
        geom_rect(aes(xmin = 0,
                      xmax = 1920,
                      ymin = 0,
                      ymax = 1080),
                  fill = NA,
                  colour = 'black') +
        geom_point(aes(gaze_point_x, gaze_point_y, colour = stim_type), alpha = .01, data = plotdata()) +
        geom_point(aes(xpos_shift, ypos_shift,  size = size), colour = 'black', shape = 1, alpha = .5, data = fixdata()) +
        geom_point(aes(xpos, ypos, size = size), colour = colours[input$stim_type], alpha = 1, shape = 1, data = fixdata()) +
        guides(colour = guide_legend(override.aes = list(alpha = 1, size = 3)),
               size = F) +
        geom_rect(aes(xmin = x,
                      xmax = x + width,
                      ymin = y,
                      ymax = y + height),
                  fill = NA,
                  colour = colours[input$stim_type],
                  data = rects())
    })

    output$plot <- renderPlot({
      main_plot()
      
    })
  })

  