library(shiny)
library(ggvis)
library(dplyr)
library(ggplot2)
library(lubridate)

axis_vars <- c(
  "Danceability" = "danceability",
  "Energy" = "energy",
  "Speechiness" = "speechiness",
  "Acousticness" = "acousticness",
  "Liveness" = "liveness",
  "Instrumentalness"="instrumentalness",
  "Tempo" = "tempo",
  "Weeks on Chart" = "weeks_on_chart",
  "Peak Position" = "peak_pos",
  "Duration (minutes)" = "duration"
)

audio_features <- c(
  "Tempo" = "tempo",
  "Duration (min)" = "duration_min",
  "Liveness" = "liveness", 
  "Danceability" = "danceability",
  "Energy" = "energy",
  "Loudness" = "loudness",
  "Speechiness" = "speechiness",
  "Acousticness" = "acousticness",
  "Valence" = "valence"
)

groups <- c(
  "Nothing" = "nothing",
  "Genre" = "GenreFix",
  "Mode" = "mode",
  "Season" = "season"
)

# Define UI ----
ui <- navbarPage(
  theme = "style.css",
  "Billboard's Hot 100",
  tabPanel("Song Explorer",
        fluidRow(
          wellPanel(
            h2("Variables"),
            fluidRow(
              column(6,
                     selectInput("xvar", "X-axis variable", axis_vars, selected = "danceability")
              ),
              column(6,
                     selectInput("yvar", "Y-axis variable", axis_vars, selected = "peak_pos")
              )
            )
          )
        ),
        fluidRow(
          column(8,
            ggvisOutput("plot1"),
            helpText(span( textOutput("n_songs"), "songs selected."))
          ),
          column(4,
                 wellPanel(
                   h2("Filters"),
                   sliderInput("year", "Year", 2000, 2019, value = c(2000, 2019),
                               sep = ""),
                   sliderInput("weeks", "Minimum number of weeks on the charts",
                               1, 50, 10, step = 1),
                   
                   sliderInput("peak_pos", "Minimum peak position on the charts",
                               1, 100, 100, step = 1),
                   selectInput("genre", "Genre",
                               c("All", "pop", "hip hop/rap", "alternative/rock", "country", "soul/jazz",
                                 "r%b")),
                   textInput("artist", "Artist name contains (e.g., Taylor)"),
                   textInput("title", "Song name contains (e.g., Old Town)")
                 )    
          )
        ),
        fluidRow(
          column(1),
          column(10,
            DT::dataTableOutput("songTable")
          ),
          column(1)
        )
  ),
  tabPanel("Trend Explorer",
           fluidRow(
             wellPanel(
               fluidRow(
                 column(6,
                        selectInput("trend_y", "Y-axis variable", audio_features, selected = "energy")
                 ),
                 column(6,
                        selectInput("trend_group", "Group By", groups, selected = "nothing")
                 )
               )
             )
           ),
           fluidRow(
             plotOutput("plot2")
           )
           
  )
)



# Define server logic ----
server <- function(input, output) {
  
  UniqueSongs <- read.csv(file="shinyDataUnique.csv", header=TRUE, sep=",")
  AllSongs <- read.csv(file="songfinal2.csv", header=TRUE, sep=",")
  
  MODE.func = function(data){
    
    x = data$mode
    
    y = rep(NA,length(x))
    
    for (i in seq_along(x)){
      y[i] =  if(x[i]  %in% c(1)){
        "Major"
      }else{
        "Minor"
      }
    }
    return(factor(y))
  }
  
  trend_data <-reactive({
    
    if(input$trend_group != "nothing"){
      if(input$trend_group == "GenreFix"){
        AllSongs$date <- as.Date( paste(AllSongs$year, 1 ,1, sep = "." ), format="%Y.%m.%d")
      }
      else{
        AllSongs$date <- as.Date(paste( AllSongs$year, AllSongs$month ,1, sep = "." ), format="%Y.%m.%d")
      }
      group_bys <- c("date", input$trend_group)
    }
    else{
      AllSongs$date <- as.Date(paste( AllSongs$year, AllSongs$month ,1, sep = "." ), format="%Y.%m.%d")
      group_bys <- c("date")
    }
    
    data <- AllSongs %>%
      group_by_at(group_bys) %>%
      summarise(
        n = n(),
        duration_min = mean(duration_ms)/(60*1000),
        tempo = mean(tempo),
        liveness = mean(liveness),
        danceability = mean(danceability),
        energy = mean(energy),
        loudness = mean(loudness),
        speechiness = mean(speechiness),
        acousticness = mean(acousticness),
        valence = mean(valence)
      )
      print(data)
      if(input$trend_group == "GenreFix"){
        data <- data %>%
          rename(Genre=GenreFix)
      }
      else if(input$trend_group == "mode"){
        data$mode = MODE.func(data) 
      }
    
      data
  })
  
  song_data <- reactive({
    
    pos <- input$peak_pos
    weeks <- input$weeks
    minyear <- input$year[1]
    maxyear <- input$year[2]
    
    s <- UniqueSongs %>%
      filter(
        peak_pos <= pos,
        weeks_on_chart >= weeks,
        year >= minyear,
        year <= maxyear,
      )
    
    if (input$genre != "All") {
      genre <- input$genre
      s <- s %>% filter(GenreFix == genre)
    }
    if (!is.null(input$artist) && input$artist != "") {
      inputArtist <- input$artist
      s <- s %>% filter(grepl(inputArtist, artist, fixed=TRUE))
    }
    if (!is.null(input$title) && input$title != "") {
      title <- input$title
      s <- s %>% filter(grepl(title, song, fixed=TRUE))
    }
    
    s$was_number_one <- character(nrow(s))
    s$was_number_one[s$peak_pos > 1] <- "No"
    s$was_number_one[s$peak_pos == 1] <- "Yes"
    s
  })
  
  
  song_tooltip <- function(x) {
    if (is.null(x)) return(NULL)
    if (is.null(x$songID)) return(NULL)

    UniqueSongs <- isolate(song_data())
    song<- UniqueSongs[UniqueSongs$songID == x$songID, ]
    
    paste0("<b>", song$song, "</b><br>",
           song$artist, "<br>",
           song$year
    )
  }
  
  # A reactive expression with the ggvis plot
  vis <- reactive({
    xvar_name <- names(axis_vars)[axis_vars == input$xvar]
    yvar_name <- names(axis_vars)[axis_vars == input$yvar]
    
    xvar <- prop("x", as.symbol(input$xvar))
    yvar <- prop("y", as.symbol(input$yvar))
    
    song_data %>%
      ggvis(x = xvar, y = yvar) %>%
      layer_points(size := 50, size.hover := 120,
                   fillOpacity := 0.3, fillOpacity.hover := 0.8, stroke = ~was_number_one, key := ~songID) %>%
      add_tooltip(song_tooltip, "hover") %>%
      add_axis("x", title = xvar_name) %>%
      add_axis("y", title = yvar_name) %>%
      add_legend("stroke", title = "#1 Hit", values = c("Yes", "No")) %>%
      scale_nominal("stroke", domain = c("Yes", "No"),
                    range = c("#1DB954", "#aaa")) %>%
      set_options(width = "auto", height = 600)
  })
  
  table <- reactive({
    t <- song_data() %>%
      rename(Year = year, `Peak Position` = peak_pos, Genre = GenreFix, `Song Title` = song, `Artist Name` = artist,
             `Weeks on Chart` = weeks_on_chart, Danceability = danceability, Energy = energy, Loudness = loudness,
             Speechines = speechiness, Acousticness = acousticness, Instrumentalness = instrumentalness, Key = key,
             Mode = mode, Valence = valence, Tempo = tempo, `#1 Hit` = was_number_one, Duration = duration, Liveness = liveness) %>%
      select(Year, `Song Title`, `Artist Name`, `Genre`, `Peak Position`) %>%
      arrange(Year, `Song Title`)
    t
  })
  
  trend_graph <- reactive({

  })
  
  get_trend_graph <- function(){
    if(input$trend_group == "mode"){
      dates<- trend_data()$date
      brks <- dates[seq(1, length(trend_data()$date), 12)]
      lbls <- lubridate::year(brks)
      
      graph <- ggplot(trend_data(), aes(x=date)) + 
        geom_line(aes_string(y=input$trend_y, col=input$trend_group)) +  
        scale_x_date(labels = lbls, breaks = brks) +  
        scale_color_manual(labels = c("Major", "Minor"), 
                           values = c("Major"="#00ba38", "Minor"="#f8766d")) +  
        theme(axis.text.x = element_text(angle = 90, vjust=0.5, size = 8), 
              panel.grid.minor = element_blank())
    }
    else if(input$trend_group == "GenreFix"){
      
      genres = c( "country", "pop", "hip hop/rap")
      genreColors = c( "country"="#202cb3", "pop"="#84259c", "hip hop/rap"="#2ac9c9")
      data <- trend_data()[trend_data()$Genre %in% genres, ]
      dates<- unique(data$date)
      brks <-  dates
      lbls <- lubridate::year(brks)
      print(data)
      graph <- ggplot(data, aes(x=date)) + 
        geom_line(aes_string(y=input$trend_y, col="Genre")) +  
        scale_x_date(labels = lbls, breaks = brks) +  
        scale_color_manual(labels = genres, 
                           values = genreColors) +  # line color
        theme(axis.text.x = element_text(angle = 90, vjust=0.5, size = 8), 
              panel.grid.minor = element_blank())
    }
    else{
      graph <- ggplot(trend_data(), aes(x=date)) + 
        geom_line(aes_string(y=input$trend_y),color='#1DB954')
    }
    return(graph)
  }
  
  vis %>% bind_shiny("plot1")
  
  output$n_songs <- renderText({ nrow(song_data()) })
  
  output$songTable <- DT::renderDataTable(table(), 
                                          options = list(searching = FALSE,
                                                                  rowCallback = DT::JS(
                                                                    'function(row, data) {
        // Bold cells for those >= 5 in the first column
        if (parseFloat(data[5]) == 1)
          $("td", row).css("background-color", "#1DB954");
      }'))
  )
  
  
  output$plot2 <- renderPlot({
    get_trend_graph()
  })
}

# Run the app ----
shinyApp(ui = ui, server = server)
