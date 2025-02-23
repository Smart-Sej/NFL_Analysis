library(shiny)
library(ggplot2)
library(dplyr)
library(plotly)
library(tidyr)
library(lubridate)

# ------------------------------------------------------------------
# LOAD DATA for Position-Specific Insights (players.csv)
players <- read.csv("players.csv", stringsAsFactors = FALSE)
# Standardize column names to lowercase
colnames(players) <- tolower(colnames(players))
# Drop rows where nflid is missing
players <- players %>% drop_na(nflid)

# If 'age' is not present but 'birthdate' exists, compute age
if (!"age" %in% colnames(players) && "birthdate" %in% colnames(players)) {
  players$age <- as.numeric(format(Sys.Date(), "%Y")) - as.numeric(substr(players$birthdate, 1, 4))
}

# Clean the 'height' column:
players$height <- as.character(players$height)
valid_heights <- grepl("^\\d+$", players$height)
numeric_heights <- as.numeric(players$height[valid_heights])
mean_height <- mean(numeric_heights, na.rm = TRUE)
players$height[!valid_heights] <- mean_height
players$height <- as.numeric(players$height)

# ------------------------------------------------------------------
# LOAD DATA for NFL Sports Analytics (Team & Cumulative Metrics)
week_data <- read.csv("week_data.csv", stringsAsFactors = FALSE)
plays     <- read.csv("plays.csv", stringsAsFactors = FALSE)
games     <- read.csv("games.csv", stringsAsFactors = FALSE)

# Standardize column names to lowercase
colnames(week_data) <- tolower(colnames(week_data))
colnames(plays)     <- tolower(colnames(plays))
colnames(games)     <- tolower(colnames(games))
# players already standardized

# Drop rows with missing nflid from week_data
week_data <- week_data %>% drop_na(nflid)

# Prepare data for Team Metrics:
wd <- week_data %>%
  left_join(games, by = "gameid") %>%
  mutate(actualteam = ifelse(team == "home", hometeamabbr, visitorteamabbr)) %>%
  left_join(players, by = "nflid") %>%
  mutate(teamtype = ifelse(team == "home", "Home Team", "Away Team"))

# Compute player age in wd if birthdate exists
if ("birthdate" %in% colnames(wd)) {
  wd$age <- as.numeric(format(Sys.Date(), "%Y")) - as.numeric(substr(wd$birthdate, 1, 4))
}

# Function to compute team metrics (averages) for a given team type
compute_team_metrics <- function(team_type) {
  data <- wd
  if (team_type != "All Teams") {
    data <- data %>% filter(teamtype == team_type)
  }
  data %>%
    group_by(actualteam) %>%
    summarise(
      Avg_Age = mean(age, na.rm = TRUE),
      Avg_Weight = mean(weight, na.rm = TRUE),
      Avg_Height = mean(height, na.rm = TRUE)
    ) %>%
    rename(team = actualteam)
}

nfl_team_metrics <- list(
  "Home Team" = compute_team_metrics("Home Team"),
  "Away Team" = compute_team_metrics("Away Team"),
  "All Teams" = compute_team_metrics("All Teams")
)

# ------------------------------------------------------------------
# Prepare data for NFL Performance Benchmarks
performance_metrics <- week_data %>%
  group_by(nflid) %>%
  summarise(
    Peak_Speed = max(s, na.rm = TRUE),
    Peak_Acceleration = max(a, na.rm = TRUE)
  ) %>%
  arrange(desc(Peak_Speed))
performance_metrics <- performance_metrics %>%
  left_join(players %>% select(nflid, displayname), by = "nflid")

# ------------------------------------------------------------------
# Prepare data for NFL Weekly Trends
if (!"week" %in% colnames(week_data)) {
  weekly_trends <- week_data %>% 
    left_join(games %>% select(gameid, week), by = "gameid") %>%
    group_by(week) %>%
    summarise(across(c(s, a), mean, na.rm = TRUE)) %>%
    arrange(week)
} else {
  weekly_trends <- week_data %>% 
    group_by(week) %>%
    summarise(across(c(s, a), mean, na.rm = TRUE)) %>%
    arrange(week)
}
valid_metrics <- c()
if ("s" %in% colnames(week_data)) valid_metrics <- c(valid_metrics, "Avg_Speed" = "s")
if ("a" %in% colnames(week_data)) valid_metrics <- c(valid_metrics, "Avg_Acceleration" = "a")

# ------------------------------------------------------------------
# Prepare data for NFL 4-Hour Trends
# Convert the time column from ISO 8601 format to POSIXct using ymd_hms() with tz='UTC'
four_hour_trends <- week_data %>%
  mutate(time_clean = gsub("T", " ", time),
         time_clean = gsub("Z", "", time_clean),
         time_clean = ymd_hms(time_clean, tz = "UTC")) %>%
  filter(!is.na(time_clean)) %>%
  mutate(time_4hr = floor_date(time_clean, unit = "4 hours")) %>%
  group_by(time_4hr) %>%
  summarise(
    Total_Distance = sum(dis, na.rm = TRUE),
    Avg_Speed = mean(s, na.rm = TRUE)
  ) %>%
  arrange(time_4hr)

# ------------------------------------------------------------------
# For NFL Spatial Analysis:
if (!all(c("playid", "nflid", "x", "y") %in% colnames(week_data))) {
  stop("Required columns ('playid', 'nflid', 'x', 'y') are missing in week_data.csv")
}
unique_plays <- unique(week_data$playid)

# ------------------------------------------------------------------
# UI
ui <- fluidPage(
  titlePanel("NFL Analytics Dashboard"),
  tabsetPanel(
    # Tab 1: Position-Specific Insights
    tabPanel("Position-Specific Insights",
             sidebarLayout(
               sidebarPanel(
                 selectInput("position", "Select Position:", choices = unique(players$position))
               ),
               mainPanel(
                 tabsetPanel(
                   tabPanel("Height vs Weight", plotlyOutput("scatter_plot")),
                   tabPanel("Average Attributes", plotlyOutput("bar_chart"))
                 )
               )
             )
    ),
    # Tab 2: NFL Sports Analytics (Team & Cumulative Player Metrics)
    tabPanel("NFL Sports Analytics",
             sidebarLayout(
               sidebarPanel(
                 selectInput("metric", "Select Metric:", 
                             choices = c("Cumulative Player Metrics", "Team Metrics")),
                 conditionalPanel(
                   condition = "input.metric == 'Cumulative Player Metrics'",
                   selectInput("player", "Select Player:", choices = unique(week_data$displayname))
                 ),
                 conditionalPanel(
                   condition = "input.metric == 'Team Metrics'",
                   selectInput("team_type", "Select Team Type:", 
                               choices = c("Home Team", "Away Team", "All Teams")),
                   selectInput("analysis_type", "Select Analysis Type:", 
                               choices = c("Average Metrics", "Distribution by Age", "Distribution by Height", "Distribution by Weight")),
                   uiOutput("dynamic_dropdown_team")
                 )
               ),
               mainPanel(
                 tableOutput("metrics"),
                 plotOutput("plot")
               )
             )
    ),
    # Tab 3: NFL Performance Benchmarks
    tabPanel("NFL Performance Benchmarks",
             sidebarLayout(
               sidebarPanel(
                 selectInput("perf_metric", "Select Performance Metric:", 
                             choices = c("Peak Speed" = "Peak_Speed", "Peak Acceleration" = "Peak_Acceleration"))
               ),
               mainPanel(
                 h3("Top 10 Players"),
                 tableOutput("topPlayers"),
                 plotOutput("metricPlot")
               )
             )
    ),
    # Tab 4: NFL Spatial Analysis: Player Positions
    tabPanel("NFL Spatial Analysis",
             sidebarLayout(
               sidebarPanel(
                 selectInput("play", "Select a Play:", choices = unique_plays)
               ),
               mainPanel(
                 h3("Player Positions on the Field"),
                 plotlyOutput("spatialPlot")
               )
             )
    ),
    # Tab 5: NFL Weekly Trends
    tabPanel("NFL Weekly Trends",
             sidebarLayout(
               sidebarPanel(
                 selectInput("weekly_metric", "Select Performance Metric:", choices = names(valid_metrics))
               ),
               mainPanel(
                 h3("Weekly Performance Trends"),
                 plotlyOutput("weeklyPlot")
               )
             )
    ),
    # Tab 6: NFL 4-Hour Trends
    tabPanel("NFL 4-Hour Trends",
             sidebarLayout(
               sidebarPanel(
                 h4("Aggregated Every 4 Hours")
               ),
               mainPanel(
                 h3("4-Hour Performance Trends"),
                 plotlyOutput("fourHourPlot")
               )
             )
    )
  )
)

# ------------------------------------------------------------------
# Server Logic
server <- function(input, output, session) {
  # ---------------------------
  # For Position-Specific Insights
  filtered_data <- reactive({
    players %>% filter(position == input$position)
  })
  
  output$scatter_plot <- renderPlotly({
    p <- ggplot(filtered_data(), aes(x = weight, y = height, color = age)) +
      geom_point(size = 3) +
      labs(title = "Height vs Weight", x = "Weight (lbs)", y = "Height (inches)") +
      theme_minimal()
    ggplotly(p)
  })
  
  output$bar_chart <- renderPlotly({
    avg_data <- players %>%
      group_by(position) %>%
      summarise(
        Average_Height = mean(height, na.rm = TRUE),
        Average_Weight = mean(weight, na.rm = TRUE),
        Average_Age = mean(age, na.rm = TRUE)
      )
    avg_data_long <- avg_data %>%
      pivot_longer(cols = c(Average_Height, Average_Weight, Average_Age),
                   names_to = "Attribute", values_to = "Value")
    p <- ggplot(avg_data_long, aes(x = position, y = Value, fill = Attribute)) +
      geom_bar(stat = "identity", position = "dodge") +
      labs(title = "Average Attributes by Position", x = "Position", y = "Value") +
      theme_minimal()
    ggplotly(p)
  })
  
  # ---------------------------
  # For NFL Sports Analytics (Team & Cumulative Player Metrics)
  output$dynamic_dropdown_team <- renderUI({
    req(input$team_type)
    teams <- wd %>% 
      { if(input$team_type != "All Teams") filter(., teamtype == input$team_type) else . } %>%
      pull(actualteam) %>% unique() %>% sort()
    selectInput("team", "Select Team (or All):", choices = c("All", teams))
  })
  
  player_data <- reactive({
    req(input$player)
    week_data %>% filter(displayname == input$player)
  })
  
  team_physical_data <- reactive({
    req(input$team_type)
    data <- wd
    if(input$team_type != "All Teams"){
      data <- data %>% filter(teamtype == input$team_type)
    }
    if (!is.null(input$team) && input$team != "All") {
      data <- data %>% filter(actualteam == input$team)
    }
    data
  })
  
  output$plot <- renderPlot({
    if (input$metric == "Cumulative Player Metrics") {
      data <- player_data()
      ggplot(data, aes(x = frameid, y = s)) +
        geom_line(color = "blue") +
        ggtitle(paste("Speed Over Time for", input$player)) +
        xlab("Frame ID") +
        ylab("Speed (m/s)")
    } else if (input$metric == "Team Metrics") {
      if (input$analysis_type == "Average Metrics") {
        data <- nfl_team_metrics[[input$team_type]]
        if (!is.null(input$team) && input$team != "All") {
          data <- data %>% filter(team == input$team)
        }
        ggplot(data, aes(x = reorder(team, Avg_Weight), y = Avg_Weight)) +
          geom_bar(stat = "identity", fill = "steelblue") +
          theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
          labs(title = paste("Average Weight by", input$team_type),
               y = "Weight (lbs)", x = "Team")
      } else {
        data <- team_physical_data()
        variable <- switch(input$analysis_type,
                           "Distribution by Age" = "age",
                           "Distribution by Height" = "height",
                           "Distribution by Weight" = "weight")
        ggplot(data, aes_string(x = variable)) +
          geom_histogram(bins = 30, fill = "darkgreen", color = "white", alpha = 0.7) +
          labs(title = paste(input$analysis_type, "for", 
                             ifelse(input$team == "All", "all teams", input$team)),
               x = variable,
               y = "Count")
      }
    }
  })
  
  output$metrics <- renderTable({
    if (input$metric == "Cumulative Player Metrics") {
      data <- player_data()
      data.frame(
        Player = input$player,
        Total_Distance = sum(data$dis, na.rm = TRUE),
        Avg_Speed = mean(data$s, na.rm = TRUE),
        Num_Plays = length(unique(data$playid))
      )
    } else if (input$metric == "Team Metrics") {
      if (input$analysis_type == "Average Metrics") {
        data <- nfl_team_metrics[[input$team_type]]
        if (!is.null(input$team) && input$team != "All") {
          data <- data %>% filter(team == input$team)
        }
        data
      } else {
        data <- team_physical_data()
        variable <- switch(input$analysis_type,
                           "Distribution by Age" = "age",
                           "Distribution by Height" = "height",
                           "Distribution by Weight" = "weight")
        data %>% summarise(
          Min = min(get(variable), na.rm = TRUE),
          Q1 = quantile(get(variable), 0.25, na.rm = TRUE),
          Median = median(get(variable), na.rm = TRUE),
          Mean = mean(get(variable), na.rm = TRUE),
          Q3 = quantile(get(variable), 0.75, na.rm = TRUE),
          Max = max(get(variable), na.rm = TRUE)
        )
      }
    }
  })
  
  # ---------------------------
  # For NFL Performance Benchmarks
  top_players <- reactive({
    performance_metrics %>%
      arrange(desc(.data[[input$perf_metric]])) %>%
      head(10)
  })
  
  output$topPlayers <- renderTable({
    top_players()
  })
  
  output$metricPlot <- renderPlot({
    ggplot(top_players(), aes(x = reorder(displayname, .data[[input$perf_metric]]), y = .data[[input$perf_metric]])) +
      geom_bar(stat = "identity", fill = "darkorange") +
      coord_flip() +
      labs(title = paste("Top 10 Players by", input$perf_metric), x = "Player", y = input$perf_metric) +
      theme_minimal()
  })
  
  # ---------------------------
  # For NFL Spatial Analysis: Player Positions (Tab 4)
  spatial_data <- reactive({
    week_data %>% 
      left_join(players %>% select(nflid, displayname), by = "nflid") %>%
      filter(playid == input$play)
  })
  
  output$spatialPlot <- renderPlotly({
    p <- ggplot(spatial_data(), aes(x = x, y = y)) +
      geom_point(aes(color = displayname.x), size = 3) + 
      labs(title = paste("Player Positions for Play", input$play),
           x = "Field X Position",
           y = "Field Y Position") +
      theme_minimal() +
      coord_fixed() +
      guides(color = guide_legend(title = "Player Name"))
    ggplotly(p)
  })
  
  # ---------------------------
  # For NFL Weekly Trends (Tab 5)
  output$weeklyPlot <- renderPlotly({
    req(input$weekly_metric)
    p <- ggplot(weekly_trends, aes(x = week, y = .data[[valid_metrics[[input$weekly_metric]]]])) +
      geom_line(color = "blue", size = 1.2) +
      geom_point(size = 3, color = "red") +
      labs(title = paste("Trend of", input$weekly_metric, "Over Weeks"),
           x = "Week",
           y = input$weekly_metric) +
      theme_minimal()
    ggplotly(p)
  })
  
  # ---------------------------
  # For NFL 4-Hour Trends (Tab 6)
  output$fourHourPlot <- renderPlotly({
    # Convert ISO 8601 time strings to POSIXct using ymd_hms() with tz = "UTC"
    four_hour_trends <- week_data %>%
      mutate(time_clean = gsub("T", " ", time),
             time_clean = gsub("Z", "", time_clean),
             time_clean = ymd_hms(time_clean, tz = "UTC")) %>%
      filter(!is.na(time_clean)) %>%
      mutate(time_4hr = floor_date(time_clean, unit = "4 hours")) %>%
      group_by(time_4hr) %>%
      summarise(
        Total_Distance = sum(dis, na.rm = TRUE),
        Avg_Speed = mean(s, na.rm = TRUE)
      ) %>%
      arrange(time_4hr)
    
    p <- ggplot(four_hour_trends, aes(x = time_4hr, y = Avg_Speed)) +
      geom_line(color = "blue", size = 1.2) +
      geom_point(size = 3, color = "red") +
      labs(title = "Trend of Average Speed Over 4-Hour Intervals",
           x = "Time (4-Hour Intervals)",
           y = "Average Speed") +
      theme_minimal()
    ggplotly(p)
  })
}



# Run the application
shinyApp(ui = ui, server = server)
