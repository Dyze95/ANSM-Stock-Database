library(shiny)
library(DT)
library(dplyr)

df_stock <- read.csv("../database-stock.csv", sep=";", stringsAsFactors = FALSE)
df_stock$Date <- as.Date(df_stock$Date)

# Define UI for application
ui <- fluidPage(
  titlePanel("Bilan des stocks"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("DCI", "DCI", unique(df_stock$DCI)),
      selectInput("forme", "Forme", NULL),
      selectInput("dosage", "Dosage", NULL)
    ),
    
    mainPanel(
      plotOutput("graph"),
      dataTableOutput("table", width = "50%")
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  
  graph_data <- reactive({
    if(input$dosage == "Tous") {
      graph_data <- df_stock[df_stock$DCI == input$DCI & df_stock$Forme == input$forme,]
      graph_data <- aggregate(graph_data["Stock_U_equiv"], by=list(Date=graph_data$Date), FUN=sum)
      graph_data$Stock <- graph_data$Stock_U_equiv
      graph_data$Stock_U_equiv <- NULL
      graph_data
    } else {
      graph_data <- df_stock[df_stock$DCI == input$DCI & df_stock$Forme == input$forme & df_stock$Dosage == input$dosage,]
      graph_data <- aggregate(graph_data["Stock_U"], by=list(Date=graph_data$Date), FUN=sum)
      graph_data$Stock <- graph_data$Stock_U
      graph_data$Stock_U <- NULL
      graph_data
    }
  })
  
  output$graph <- renderPlot({
    plot(x=graph_data()$Date, y=graph_data()$Stock, ylim=c(0, 1.2*max(graph_data()$Stock)), type="l")
  })
  
  output$table <- renderDataTable(graph_data() %>% mutate(Stock = prettyNum(Stock, big.mark = " ")),
                                  rownames = FALSE)
  
  observeEvent(input$DCI, {
    updateSelectInput(session, "forme", choices = unique(df_stock$Forme[df_stock$DCI == input$DCI]))
  })
  
  observeEvent(input$forme, {
    updateSelectInput(session, "dosage", choices = c("Tous",unique(df_stock$Dosage[df_stock$DCI == input$DCI & df_stock$Forme == input$forme])))
  })
}

shinyApp(ui, server)