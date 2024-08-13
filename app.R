library(shiny)
library(shinydashboard)
library(dplyr)
library(tidyr)
library(ggplot2)
library(rsconnect)

ui <- fluidPage(
  dashboardPage(
    dashboardHeader(title="Cumulative Paid Claims", 
                    titleWidth = 300,
                    tags$li(class="dropdown",
                            tags$a(href="https://shiny.posit.co/",
                                   "ShinyApp"
                            )
                    )
    ),
    
    #side bar menu ---------------------
    dashboardSidebar(
      sidebarMenu(
        id = "sidebar",
        menuItem("Dataset", tabName = "Data", icon=icon("database") ),
        menuItem(text = "Result", tabName= "Result", icon=icon("chart-line"))
      )
    ),
    
    #body of project --------------------
    dashboardBody(
      tabItems(
        tabItem(tabName = "Data",
                fileInput("data_file", "Upload your claims data here (Only .csv file is accepted.)", accept = ".csv"),
                textOutput("instructions"),
                imageOutput("image_instructions")
        ),
        tabItem(tabName = "Result",
                tabBox(id="t2", width = 12,
                       tabPanel("Cumulative Paid Claims",
                                numericInput("tail_factor", "Tail factor:", value = 1.1, min = 1.0),
                                tableOutput("table")
                       ),
                       tabPanel("Plot",
                                plotOutput("graph", width = "800px"),
                                uiOutput("download_ui")
                       )
                       
                )
        )
      )
    )
  )
)

server <- function(input, output, session) {
  
  validated_file <- reactive({
    req(input$data_file)
    ext <- tools::file_ext(input$data_file$datapath)
    validate(need(ext == "csv", "Please upload a csv file."))
    read.csv(input$data_file$datapath)
  })
  
  #Reactive expression to process the data ---------------------
  processed_data <- reactive({
    data <- validated_file()
    data$Amount.of.Claims.Paid <- as.numeric(gsub(",", "", data$Amount.of.Claims.Paid))
    
    data <- data %>%
      group_by(Loss.Year) %>%
      arrange(Loss.Year, Development.Year) %>%
      mutate(Cumulative.Amount = cumsum(Amount.of.Claims.Paid)) %>%
      ungroup()
    
    table <- data %>%
      select(Loss.Year, Development.Year, Cumulative.Amount) %>%
      pivot_wider(names_from = Development.Year, values_from = Cumulative.Amount) %>%
      arrange(Loss.Year)
    
    #Convert table to a data frame ----------------------
    table <- as.data.frame(table)
    
    #Fill NA values dynamically based on provided rules --------------------
    DY <- ncol(table)
    AY <- nrow(table)
    
    for (i in 1:AY) {
      for (j in 2:DY) {
        if (is.na(table[i, j])) {
          factor <- sum(table[1:i-1, j], na.rm = TRUE) / sum(table[1:i-1, j - 1], na.rm = TRUE)
          table[i, j] <- table[i, j-1] * factor
        }
      }
    }
    
    if (input$tail_factor != 1) {
      for (i in 1:AY) {
        table[i, DY+1] <- table[i, DY] * input$tail_factor
        colnames(table)[DY+1] <- DY
      }
    }
    
    table
  })
  
  output$instructions <- renderText({
    "Please provide a .csv file that have a format like this"
  })
  
  output$image_instructions <- renderImage({
    list(src = "sample_csv.png", 
         contentType = "image/png", 
         width = "400px",
         deleteFile = FALSE)}, deleteFile = FALSE
  )
  
  output$table <- renderTable({
    processed_data()
  })
  
  output$download_ui <- renderUI({
    req(input$data_file)
    downloadButton("download_button", "Download your results as Excel here", icon = shiny::icon("download"))
  })
  
  output$download_button <- downloadHandler(
    filename = function() {
      "Cumulative Paid Claims Result.csv"
    },
    content = function(file) {
      write.csv(processed_data(), file)
    }
  )
  
  output$graph <- renderPlot({
    processed_data() %>%
      pivot_longer(cols = -Loss.Year, names_to = "Development.Year", values_to = "Cumulative.Amount") %>%
      mutate(Development.Year = as.numeric(Development.Year)) %>%
      ggplot(aes(x = Development.Year, y = Cumulative.Amount, color = factor(Loss.Year))) +
      geom_point() +
      geom_text(aes(label = round(Cumulative.Amount, 0)), vjust = -1.0) +
      geom_smooth() +
      labs(x = "Development Year", y = "Cumulative Paid Claims", color = "Loss Year") +
      theme_bw()
  })
}

shinyApp(ui, server)