library(shiny)
library(shinythemes)
library('spotifyr')
library(tidyverse)
library(knitr)
library(dplyr)
library(DT)
source("R/functions.R")

# Set up Spotify credentials
Sys.setenv(SPOTIFY_CLIENT_ID = '92e1aa37b4a54a70933f8f8cffdf1dee')
Sys.setenv(SPOTIFY_CLIENT_SECRET = '1ffc32ce7dbf43a9884cf44ba6eec02f')
access_token <- get_spotify_access_token()

#Get data from api
ArcticMonkeysRaw <- get_artist_audio_features('Arctic Monkeys')
NirvanaRaw <- get_artist_audio_features('Nirvana')
AviciiRaw <- get_artist_audio_features('Avicii')

#data cleaning
ArcticMonkeys <- ArcticMonkeysRaw[!duplicated(ArcticMonkeysRaw$track_name), ]
Nirvana <- NirvanaRaw[!duplicated(NirvanaRaw$track_name), ]
Avicii <- AviciiRaw[!duplicated(AviciiRaw$track_name), ]

#Merge
merged_data1 <- bind_rows(ArcticMonkeys, Nirvana)
merged_data2 <- bind_rows(merged_data1, Avicii)


#Select columns
Plot <- merged_data2[, c('danceability','energy',
                                       'speechiness','acousticness',
                                       'liveness','valence')]
Data <- merged_data2[, c('track_name','artist_name', 'danceability','energy',
                                       'speechiness','acousticness',
                                       'liveness','valence')]

# Define UI for application that draws a scatter plot
ui <- navbarPage(
                 title = "My Favorite Band Music Features",
                 theme = shinytheme("flatly"),
                 tabPanel("Scatter Plot",
                          sidebarLayout(
                            sidebarPanel(
                              selectInput(inputId = "artist",
                                          label = "Select Artist:",
                                          choices = c("All", list("Arctic Monkeys", "Nirvana", "Avicii"))),
                              selectInput(inputId = "VarX",
                                          label = "Select X axis Variable:",
                                          choices = list("danceability","energy",
                                                         "speechiness","acousticness",
                                                         "liveness","valence")),
                              selectInput(inputId = "VarY",
                                          label = "Select Y axis Variable:",
                                          choices = list("energy",
                                                         "speechiness","acousticness",
                                                         "liveness","valence","danceability")),
                              submitButton("Proceed")
                            ),
                            
                            # Show a plot of the generated distribution
                            mainPanel(
                              plotOutput("scatter")
                            )
                          )
                 ),
                 tabPanel(title="Data",
                          titlePanel(title="Musical Features DataTable"),
                          mainPanel(
                            dataTableOutput("data")),
                          submitButton("Proceed")
                 ),
                 tabPanel(title = "About", includeMarkdown("about.Rmd"),),
                 )

# Define server logic required to draw a scatter plot
server <- function(input, output) {
  output$data <- renderDataTable({
    if (input$artist == "All") {
      datatable(Data, extensions = 'Responsive')
    } else {
      filtered_data <- Data %>% filter(artist_name == input$artist)
      datatable(filtered_data, extensions = 'Responsive')
    }
  })
  output$scatter <- renderPlot({
    if (input$artist == "All") {
      data_to_plot <- Plot
      color_data <- merged_data2$album_name
    } else {
      filtered_data <- merged_data2 %>% filter(artist_name == input$artist)
      data_to_plot <- filtered_data[, c('danceability', 'energy',
                                        'speechiness', 'acousticness',
                                        'liveness', 'valence')]
      color_data <- filtered_data$album_name
    }
    
    Features <- data_to_plot[, c(input$VarX, input$VarY)]
    
    ggplot(data = Features, aes(x = Features[, 1], y = Features[, 2]), color = "violet") +
      geom_smooth(method = "lm", se = F) +
      theme_set(theme_bw()) +
      geom_point(aes(col = color_data)) +
      labs(x = colnames(Features)[1], y = colnames(Features)[2],
           subtitle = paste("Scatter Plot of", input$VarX, "vs", input$VarY),
           title = "Scatterplot contrasting music features",
           caption = "Source: Spotify")
  })
}

# Run the application 
shinyApp(ui = ui, server = server)