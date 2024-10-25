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
        h2("Welcome to the Interactive PK/PD Dashboard"),
        actionButton("togglePlot", "Show/Hide Plot"),
        verbatimTextOutput("dataSummary")
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
        h3("Settings"),
        selectInput("colorTheme", "Select Color Theme", choices = c("Default", "Dark", "Light")),
        actionButton("refreshData", "Refresh Data"),
        downloadButton("exportReport", "Export Report")
      )
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  # Path to the sample data file
  sampleDataPath <- "sample_ADAM_dataset.csv"
  sampleData <- read.csv(sampleDataPath)
  
  # Reactive expression for uploaded data
  uploadedData <- reactive({
    inFile <- input$fileUpload
    if (is.null(inFile)) {
      return(sampleData)
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
    updateSelectInput(session, "groupBy", choices = colnames, selected = NULL)
  })
  
  # Generate the plot
  output$pkPlot <- renderPlot({
    data <- uploadedData()
    if (is.null(data) || is.null(input$xAxis) || is.null(input$yAxis)) {
      return(NULL)
    }
    ggplot(data, aes_string(x = input$xAxis, y = input$yAxis, color = input$groupBy)) +
      geom_point() +
      geom_line() +
      theme_minimal() +
      labs(x = input$xAxis, y = input$yAxis, title = "PK Concentration Visualization")
  })
  
  # Show or hide plot based on button click
  observeEvent(input$togglePlot, {
    toggle("pkPlot")
  })
  
  # Display data summary
  output$dataSummary <- renderPrint({
    data <- uploadedData()
    if (is.null(data)) return("No data available.")
    summary(data)
  })
  
  # Update for DT rendering
  output$dataHead <- renderDT({
    data <- uploadedData()
    if (is.null(data)) return(NULL)
    datatable(data, options = list(
      pageLength = 10,
      lengthMenu = c(10, 25, 50, 100),
      searchHighlight = TRUE
    ))
  }, server = FALSE)
  
  # Refresh data action
  observeEvent(input$refreshData, {
    session$reload()
  })
  
  # Export report
  output$exportReport <- downloadHandler(
    filename = function() {
      paste("report-", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      data <- uploadedData()
      write.csv(data, file)
    }
  )
}

# Run the application 
shinyApp(ui = ui, server = server)
