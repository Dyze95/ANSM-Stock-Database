library(shiny)
library(DT)
library(dplyr)

options(scipen=999)

df_stock <- read.csv("../database-stock_hebdo.csv", sep=";", stringsAsFactors = FALSE)
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
      plotOutput("graph", height = "500px"),
      dataTableOutput("table", width = "50%")
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  
  graph_data <- eventReactive(c(input$DCI, input$forme, input$dosage),{
    if(input$dosage == "Tous") {
      graph_data <- df_stock[df_stock$DCI == input$DCI & df_stock$Forme == input$forme,]
      if(nrow(graph_data) > 0) {
        graph_data <- aggregate(graph_data["Stock_U_equiv"], by=list(Date=graph_data$Date), FUN=sum)
        graph_data$Stock <- graph_data$Stock_U_equiv
        graph_data$Stock_U_equiv <- NULL
        graph_data
      } else data.frame()
    } else {
      graph_data <- df_stock[df_stock$DCI == input$DCI & df_stock$Forme == input$forme & df_stock$Dosage == input$dosage,]
      if(nrow(graph_data) > 0) {
        graph_data <- aggregate(graph_data["Stock_U"], by=list(Date=graph_data$Date), FUN=sum)
        graph_data$Stock <- graph_data$Stock_U
        graph_data$Stock_U <- NULL
        graph_data
      } else data.frame()
    }
  })
  
  output$graph <- renderPlot({
    if(nrow(graph_data()) > 0) {
      par(mar = c(6.5, 6.5, 0.5, 0.5), mgp = c(5, 1, 0))
      plot(x=graph_data()$Date, y=graph_data()$Stock, ylim=c(0, 1.2*max(graph_data()$Stock)), type="l",
           xlab = "Date", ylab = "Stock en unites communes de dispensiation", las = 1)
    }
  })
  
  output$table <- renderDataTable({
    if(nrow(graph_data()) > 0) {
      graph_data() %>% mutate(Stock = prettyNum(Stock, big.mark = " "))
    }
  }, rownames = FALSE)
  
  observeEvent(input$DCI, {
    updateSelectInput(session, "forme", choices = unique(df_stock$Forme[df_stock$DCI == input$DCI]))
    updateSelectInput(session, "dosage", choices = c("Tous",unique(df_stock$Dosage[df_stock$DCI == input$DCI & df_stock$Forme == input$forme])))
  })
  
  observeEvent(input$forme, {
    updateSelectInput(session, "dosage", choices = c("Tous",unique(df_stock$Dosage[df_stock$DCI == input$DCI & df_stock$Forme == input$forme])))
  })
}

shinyApp(ui, server)