library(tidyverse)
library(shiny)
library(arrow)

# if fixation file not in folder: 
# download.file('https://share.eva.mpg.de/public.php/dav/files/EnGmKy2xdCjTRR3/?accept=zip', 'data/temp/i2mc_fix_human.parquet')
df_test <- read_parquet('data/temp/i2mc_fix_human.parquet') #TODO: only data for gravity_ver2_n_test
# if loc file not in folder: 
# download.file('https://share.eva.mpg.de/public.php/dav/files/jAo8pao3DCr3tDf/?accept=zip', 'data/temp/apple_loc.parquet')
locs <- read_parquet('data/temp/apple_loc.parquet') %>%  #TODO: for all videos
  mutate(frame = index + 1) %>% 
  left_join(select(df_test, c(video_frame_index, media_timestamp)),
            by = join_by(frame == video_frame_index), 
            multiple = 'first')


shinyApp(ui = fluidPage(
  sidebarLayout(
    sidebarPanel(
      fluidRow(
        column(12, radioButtons('recname', 'Recording session:',
                                unique(df_test$recording_session_label)))
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
    # recchoices <- reactive({
    #   unique(filter(df_test, participant_id == input$name)$recording_session_label)
    # })
    # observeEvent(recchoices(), {
    #   choices <- recchoices()
    #   updateRadioButtons(inputId = 'recname', choices = choices)
    # })
    
    recdata <- reactive({
      df_test %>% filter(recording_session_label == input$recname)
    })
    
    fixdata <- reactive({
      recdata() %>% filter(!is.na(right_fix_index)) %>% 
        group_by(right_fix_index) %>% 
        summarise(xpos = mean(average_gaze_x), #TODO: FIXATION POINT NOT EXPORTED FROM EYELINK?
                  ypos = mean(average_gaze_y),
                  startT = first(media_timestamp),
                  endT = last(media_timestamp))
    })
    
    i2mcdata <- reactive({
      recdata() %>%
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
        geom_line(aes(media_timestamp, apple_x), data= locs, linewidth = 5, colour = 'green', alpha = .5) + # Apple path. not to scale
        geom_linerange(aes(y = xpos, xmin = startT, xmax = endT), data = fixdata(), linewidth = 5, colour = 'deeppink') +
        guides(colour = guide_legend(title = 'eyelink')) +
        geom_linerange(aes(y = xpos, xmin = startT, xmax = endT), data = i2mcdata(), linewidth = 1, colour = 'black') +
        geom_line(aes(media_timestamp, left_gaze_x), linewidth = .2, alpha = .3, colour = 'darkgreen', data = recdata())+
        geom_line(aes(media_timestamp, right_gaze_x), linewidth = .2, alpha = .3, colour = 'red', data = recdata()) 
    })
    
    gaze_plot_y <- reactive({
      ggplot() + 
        lims(y = c(-500, 1600)) +
        geom_line(aes(media_timestamp, apple_y), data= locs, linewidth = 5, colour = 'green', alpha = .5) +
        geom_linerange(aes(y = ypos, xmin = startT, xmax = endT), data = fixdata(), linewidth = 5, colour = 'deeppink') +
        geom_linerange(aes(y = ypos, xmin = startT, xmax = endT), data = i2mcdata(), linewidth = 1, colour = 'black') +
        geom_line(aes(media_timestamp, left_gaze_y), linewidth = .2, alpha = .3, colour = 'darkgreen', data = recdata())+
        geom_line(aes(media_timestamp, right_gaze_y), linewidth = .2, alpha = .3, colour = 'red', data = recdata())
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
      vid <- unique(recdata()$video_name)
      sprintf('Condition: %s', vid)
    })
    
    output$text_legend <- renderText({
      "Dark green line: left eye gaze\nRed line: right eye gaze\nPink dash: eyelink fixations\nBlack dash: i2mc fixations\nGrey rectangle:\nscreen size limits and TOI"
    })
  }
)

