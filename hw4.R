library(tidyverse)
library(stats)
library(ggplot2)
library(shiny)

#downloads and cleans up data
spotify_songs <- 
  read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-01-21/spotify_songs.csv') |> 
  select(-c(track_id, track_album_id, track_album_name, track_album_release_date, playlist_name, playlist_id))

#choices for ui picker
variables = colnames(spotify_songs |> select(-c(playlist_genre, playlist_subgenre, track_name, track_artist)))
genres = deframe(spotify_songs |> distinct(playlist_genre))

ui <- fluidPage(
  titlePanel("Using PCA for Song Recommendation"),
  sidebarLayout(
    sidebarPanel(
      selectInput(inputId = "features", label = "Select Variables:", choices = variables, selected = variables, multiple = TRUE),
      selectInput(inputId = "genre", label = "Select Genres:", choices = genres, selected = genres, multiple = TRUE),
      selectInput(inputId = "song", label = "Select Song to Display 10 Closest Songs To", choices = spotify_songs$track_name),
      div(tableOutput("closest_songs"), style = "font-size:75%")
    ),
    mainPanel(
      plotOutput("pca_projection"),
      fillRow(
        plotOutput("pc1_breakdown"),
        plotOutput("pc2_breakdown")
      )
    )
))

calc_pca <- function(data, feature_names){
  data.pca <- data |> 
    select(feature_names) |> 
    prcomp(center=TRUE, scale=TRUE, retx = TRUE)
  return(data.pca)
}

merge_pc1_pc2 <- function(data, pca_results){
  return(data |> mutate(pc1=pca_results$x[, 1], pc2=pca_results$x[, 2]))
}

gen_scatterplot <- function(data){
  plot <- ggplot(data)+
    geom_point(size=.1, aes(x=pc1, y=pc2, color=playlist_genre))+
    theme(axis.line=element_blank(),axis.text.x=element_blank(),
          axis.text.y = element_blank(),
          axis.ticks=element_blank(),
          panel.background=element_blank(),panel.border=element_blank(),panel.grid.major=element_blank(),
          panel.grid.minor=element_blank(),plot.background=element_blank())
  return(plot)    
}

gen_breakdown_plot <- function(pca, n){
  plot <- enframe(pca$rotation[,n]) |> #extract component from pca results
    mutate(value=abs(value)) |>
    ggplot(aes(x=reorder(name, value), y=value))+
    geom_col(aes(fill=name))+
    ggtitle(paste(sep="", "PC", n, " Breakdown"))+
    coord_flip()+
    theme(axis.line=element_blank(),axis.text.x=element_blank(),
          axis.ticks=element_blank(),
          axis.title.x=element_blank(),
          axis.title.y=element_blank(),legend.position="none",
          panel.background=element_blank(),panel.border=element_blank(),panel.grid.major=element_blank(),
          panel.grid.minor=element_blank(),plot.background=element_blank(),
          plot.title = element_text(hjust = 0.4))
  
  return(plot)
}

calc_dist <- function(data, x, y){
  data$distance <- sqrt(((data$pc1 - x)^2 + (data$pc2-y)^2))
  return(data)
}

select_closest_n <- function(data, x, y, n){
  data <- data |> 
    calc_dist(x=x, y=y) |> 
    arrange(distance) |> 
    head(n)
  
  return(data)
}

server <- function(input, output, session) {
  filtered_songs <- reactive({
    spotify_songs |> filter(playlist_genre %in% input$genre)
    })
  
  pca_results <- reactive({
    calc_pca(filtered_songs(), input$features)
  })
  
  data_with_pca <- reactive(merge_pc1_pc2(filtered_songs(), pca_results()))
  
  selected_song <- reactive({
    as.list(data_with_pca() |> filter(track_name==input$song) |> slice(1))
  })
  
  closest_songs_list <- reactive({
    select_closest_n(data_with_pca(), selected_song()$pc1, selected_song()$pc2, 10)
  })
  
  observe({
    updateSelectInput(inputId = "song", choices = filtered_songs()$track_name)
  })
  
  output$pca_projection <- renderPlot({
    gen_scatterplot(data_with_pca())
  })
  
  output$pc1_breakdown <- renderPlot({
    gen_breakdown_plot(pca_results(), 1)
  })
  
  output$pc2_breakdown <- renderPlot({
    gen_breakdown_plot(pca_results(), 2)
  })  

  output$closest_songs <- renderTable({
    closest_songs_list() |> select(c("track_name", "track_artist", "playlist_subgenre"))
  })
}

shinyApp(ui, server)