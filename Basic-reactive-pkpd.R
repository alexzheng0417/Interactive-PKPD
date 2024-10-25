library(shiny)
library(shinydashboard)
library(ggplot2)
library(DT)
library(shinyjs)
library(shinyWidgets)
library(rmarkdown)

# Define UI
ui <- dashboardPage(skin = "green",
                    dashboardHeader(title = "Interactive PK/PD Dashboard"),
                    dashboardSidebar(
                      sidebarMenu(
                        menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
                        menuItem("Data Analysis", tabName = "data_analysis", icon = icon("chart-bar")),
                        menuItem("Settings", tabName = "settings", icon = icon("sliders-h")),
                        menuItem("Report Generator", tabName = "report", icon = icon("file-alt"))
                      )
                    ),
                    dashboardBody(
                      useShinyjs(),  # For JavaScript integration
                      tabItems(
                        tabItem(tabName = "dashboard",
                                h2("Welcome to the Interactive PK/PD Dashboard"),
                                actionButton("togglePlot", "Show/Hide Plot"),
                                verbatimTextOutput("dataSummary"),
                                plotOutput("mainPlot", height = "400px")
                        ),
                        tabItem(tabName = "data_analysis",
                                h3("Data Analysis Section"),
                                fileInput("fileUpload", "Upload Data File", accept = c(".csv")),
                                selectInput("xAxis", "Select X Axis", choices = NULL),
                                selectInput("yAxis", "Select Y Axis", choices = NULL),
                                selectInput("groupBy", "Group By", choices = NULL),
                                selectInput("plotType", "Select Plot Type", choices = c("Scatter", "Line", "Bar", "Heatmap")),
                                plotOutput("pkPlot"),
                                DTOutput("dataHead")
                        ),
                        tabItem(tabName = "settings",
                                h3("Settings"),
                                selectInput("colorTheme", "Select Color Theme", choices = c("Default", "Dark", "Light")),
                                actionButton("refreshData", "Refresh Data"),
                                pickerInput("dataFilter", "Filter Data", choices = NULL, multiple = TRUE, options = list(`actions-box` = TRUE)),
                                downloadButton("exportReport", "Export Report")
                        ),
                        tabItem(tabName = "report",
                                h3("Report Generator"),
                                dateRangeInput("dateRange", "Select Date Range"),
                                actionButton("generateReport", "Generate PDF Report"),
                                downloadButton("downloadPDF", "Download PDF")
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
    updatePickerInput(session, "dataFilter", choices = unique(data[[1]]), selected = NULL)
  })
  
  # Generate the plot based on selected plot type
  output$pkPlot <- renderPlot({
    data <- uploadedData()
    if (is.null(data) || is.null(input$xAxis) || is.null(input$yAxis)) {
      return(NULL)
    }
    plotType <- input$plotType
    p <- ggplot(data, aes_string(x = input$xAxis, y = input$yAxis, color = input$groupBy))
    if (plotType == "Scatter") {
      p <- p + geom_point()
    } else if (plotType == "Line") {
      p <- p + geom_line()
    } else if (plotType == "Bar") {
      p <- p + geom_bar(stat = "identity")
    } else if (plotType == "Heatmap") {
      p <- p + geom_tile(aes(fill = !!sym(input$yAxis)))
    }
    p + theme_minimal() + labs(title = "Interactive PK/PD Visualization")
  })
  
  # Display data summary
  output$dataSummary <- renderPrint({
    data <- uploadedData()
    if (is.null(data)) return("No data available.")
    summary(data)
  })
  
  # Render data table
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
  
  # Generate PDF report
  observeEvent(input$generateReport, {
    showNotification("Generating report... This may take a moment.")
    # Add report generation logic here (using R Markdown)
  })
  
  # Download report
  output$downloadPDF <- downloadHandler(
    filename = function() {
      paste("report-", Sys.Date(), ".pdf", sep = "")
    },
    content = function(file) {
      # Add logic to generate and save PDF
    }
  )
}

# Run the application 
shinyApp(ui = ui, server = server)
