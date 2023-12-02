library(shiny)
library(shinydashboard)
library(ggplot2)
library(DT)

# Define UI
ui <- dashboardPage(skin = "green",
  dashboardHeader(title = "Interactive PK/PD Dashboard"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("Data Analysis", tabName = "data_analysis", icon = icon("chart-bar")),
      menuItem("Settings", tabName = "settings", icon = icon("sliders-h"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "dashboard",
        h2("Welcome to the Interactive PK/PD Dashboard")
      ),
      tabItem(tabName = "data_analysis",
              h3("Data Analysis Section"),
              fileInput("fileUpload", "Upload Data File", accept = c(".csv")),
              selectInput("xAxis", "Select X Axis", choices = NULL),
              selectInput("yAxis", "Select Y Axis", choices = NULL),
              selectInput("groupBy", "Group By", choices = NULL),
              plotOutput("pkPlot"),
              DTOutput("dataHead")
      ),
      tabItem(tabName = "settings",
        h3("Settings")
      )
      )
    )
)

# Define server logic (placeholder)
server <- function(input, output,session) {
  # Path to the sample data file
  sampleDataPath <- "sample_ADAM_dataset.csv"  # Adjust the path if the file is in a subdirectory
  
  # Load the sample data
  sampleData <- read.csv(sampleDataPath)
  
  # Reactive expression for uploaded data
  uploadedData <- reactive({
    inFile <- input$fileUpload
    if (is.null(inFile)) {
      return(sampleData)  # Return the sample data if no file is uploaded
    }
    read.csv(inFile$datapath)
  })
  
  # Update the choices for the axis selectors
  observe({
    data <- uploadedData()
    if (is.null(data)) return()
    colnames <- names(data)
    updateSelectInput(session, "xAxis", choices = colnames, selected = colnames[1])
    updateSelectInput(session, "yAxis", choices = colnames, selected = colnames[2])
  })
  
  # Generate the plot
  output$pkPlot <- renderPlot({
    data <- uploadedData()
    if (is.null(data) || is.null(input$xAxis) || is.null(input$yAxis)) {
      return(NULL)
    }
    ggplot(data, aes(!!sym(input$xAxis), !!sym(input$yAxis))) + 
      geom_point() +
      theme_minimal() +
      labs(x = input$xAxis, y = input$yAxis, title = "PK Concentration Visualization")
  })
  
  # Add Color list
  NNcol <-  c(
    # Original colors
    "#FFFFFF", # white         (CMYK:   0, 0,  0,  0)
    "#001965", # dark_blue     (Pantone 280 C        CMYK: 100,75,  0, 35)
    "#009FDA", # light_blue    (Pantone Process Cyan CMYK: 100, 5,  5,  0)
    "#E64A0E", # lava_red      (Pantone 173 C        CMYK:   5,90,100,  5)
    "#82786F", # granite_grey  (Pantone Warm Grey 9C CMYK:   0,14, 19, 60)
    "#AEA79F", # concrete_grey (Pantone Warm Grey 5C CMYK:  11,13, 14, 32)
    "#C7C2BA", # marble_grey   (Pantone Warm Grey 3C CMYK:   9,12, 12, 20)
    "#E0DED8", # pearl_grey    (Pantone Warm Grey 1C CMYK:   3, 3,  6,  7)
    "#001423", # black         (CMYK:  60, 0,  0,100)
    "#3F9C35", # forest_green  (Pantone 362 C        CMYK:  80, 0,100,  2)
    "#739600", # grass_green   (Pantone 377 C        CMYK:  56, 1,100, 22)
    "#C9DD03", # lime_green    (Pantone 381 C        CMYK:  29, 0, 96,  0)
    "#007C92", # ocean_blue    (Pantone 3145 C       CMYK: 100,11, 28, 20)
    "#72B5CC", # sky_blue      (Pantone 7458 C       CMYK:  53, 3,  7,  9)
    "#C2DEEA", # misty_blue    (Pantone 290 C        CMYK:  23, 0,  1,  0)
    "#D47600", # sunset_orange (Pantone 1385 C       CMYK:   2,60,100,  3)
    "#EAAB00", # golden_yellow (Pantone 124 C        CMYK:   0,32,100,  0)
    "#82786F", # granite_gray  (Pantone Warm Grey 9C CMYK:   0,14, 19, 60)
    "#AEA79F", # concrete_gray (Pantone Warm Grey 5C CMYK:  11,13, 14, 32)
    "#C7C2BA", # marble_gray   (Pantone Warm Grey 3C CMYK:   9,12, 12, 20)
    "#E0DED8", # pearl_gray    (Pantone Warm Grey 1C CMYK:   3, 3,  6,  7)
    
    # Additional colors
    "#FF5733", # fiery_red     (CMYK: 0, 65, 80, 0)
    "#FFC300", # sunshine_yellow (CMYK: 0, 20, 100, 0)
    "#DAF7A6", # light_green   (CMYK: 15, 0, 30, 5)
    "#581845", # deep_purple   (CMYK: 60, 100, 0, 50)
    "#C70039", # crimson       (CMYK: 0, 100, 68, 18)
    "#900C3F", # wine_red      (CMYK: 0, 100, 50, 44)
    "#FFC0CB", # pink          (CMYK: 0, 25, 20, 0)
    "#50C878", # emerald       (CMYK: 75, 0, 60, 10)
    "#6A5ACD", # slate_blue    (CMYK: 70, 65, 0, 0)
    "#008080", # teal          (CMYK: 100, 0, 30, 25)
    "#0E2F44", # navy_blue     (CMYK: 100, 85, 30, 50)
    "#E6E6FA", # lavender      (CMYK: 20, 15, 0, 2)
    "#FFD700", # gold          (CMYK: 0, 16, 100, 0)
    "#B5651D", # bronze        (CMYK: 42, 60, 100, 25)
    "#800000", # maroon        (CMYK: 0, 100, 100, 50)
    "#4682B4", # steel_blue    (CMYK: 58, 25, 0, 30)
    "#9FE2BF", # sea_green     (CMYK: 38, 0, 25, 25)
    "#DFFF00", # chartreuse    (CMYK: 20, 0, 100, 0)
    "#DE3163", # cerise        (CMYK: 0, 90, 20, 20)
    "#CCCCFF", # periwinkle    (CMYK: 20, 20, 0, 0)
    "#663399", # rebecca_purple (CMYK: 75, 75, 0, 0)
    "#FF7F50"  # coral         (CMYK: 0, 50, 65, 0)
  )
  
  
  
  
  # Reactive expression for uploaded data
  uploadedData <- reactive({
    inFile <- input$fileUpload
    if (is.null(inFile)) {
      return(sampleData)
    }
    read.csv(inFile$datapath)
  })
  
  # Update the choices for the axis and group by selectors
  observe({
    data <- uploadedData()
    if (is.null(data)) return()
    colnames <- names(data)
    updateSelectInput(session, "xAxis", choices = colnames, selected = colnames[1])
    updateSelectInput(session, "yAxis", choices = colnames, selected = colnames[2])
    updateSelectInput(session, "groupBy", choices = colnames, selected = NULL)
  })
  
  # Generate the plot with grouping and lines
  output$pkPlot <- renderPlot({
    data <- uploadedData()
    if (is.null(data) || is.null(input$xAxis) || is.null(input$yAxis)) {
      return(NULL)
    }
    if (!is.null(input$groupBy) && input$groupBy != "") {
      # Convert to factor or character for grouping if necessary
      if (!is.factor(data[[input$groupBy]]) && !is.character(data[[input$groupBy]])) {
        data[[input$groupBy]] <- as.character(data[[input$groupBy]])
      }
      p <- ggplot(data, aes_string(x = input$xAxis, y = input$yAxis))
      p <- p + aes_string(color = input$groupBy, group = input$groupBy)
      p <- p + geom_line()
      p <- p + scale_color_manual(values = NNcol)
    }
    p + geom_point() + 
      theme_minimal() +
      theme(legend.position = "bottom") +
      labs(x = input$xAxis, y = input$yAxis, color = input$groupBy, title = "PK Concentration Visualization")
  })
  
  # Update for DT rendering
  output$dataHead <- renderDT({
    data <- uploadedData()
    if (is.null(data)) return(NULL)
    datatable(data, options = list(
      pageLength = 10,  # Default number of entries to show
      lengthMenu = c(10, 25, 50, 100),  # Dropdown for number of entries
      searchHighlight = TRUE  # Highlight search terms
    ))
  }, server = FALSE)  # server = FALSE for client-side processing (useful for search, sort, etc.)
  
}

# Run the application 
shinyApp(ui = ui, server = server)
