library(shiny)
library(shinyjs)
library(DT)
library(stringi)
library(plyr)

source("module_DCI.R")
source("module_dosage.R")
source("module_fichier.R")

CIP_data <- read.csv("CIP_Data.csv", sep=";", stringsAsFactors = FALSE)
CIP_dosage <- read.csv("CIP_Dosage.csv", sep=";", stringsAsFactors = FALSE)

# Define UI for application
ui <- fluidPage(
  useShinyjs(),
  tags$script("Shiny.addCustomMessageHandler('resetInputValue', function(variableName){
                Shiny.onInputChange(variableName, null);
                });
                "),
  
  titlePanel("DCI"),
  
  sidebarLayout(
    sidebarPanel(
      text_DCI_input("DCI"),
      add_DCI_button("DCI"),
      file_generate_button("fichier")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("DCI", DCI_output("DCI")),
        tabPanel("Dosages", new_dosage_output("dosage")),
        tabPanel("Fichier", file_output("fichier"))
      )
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  
  shinyInput <- function(FUN, len, start, id, ...) {
    #print(len)
    #print(start)
    inputs <- character(len)
    if(len > 0) {
      for (i in 1:len) {
        inputs[i] <- as.character(FUN(paste0(id, i-1+start), ...))
      }
    }
    inputs
  }
  
  react <- reactiveValues()
  react$df_DCI <- callModule(module_DCI, "DCI", CIP_data)
  
  tmp <- callModule(module_dosage, "dosage", react$df_DCI, CIP_data, CIP_dosage)
  react$df_Dosage <- tmp[[1]]
  react$df_new_dosage <- tmp[[2]]
  
  callModule(module_fichier, "fichier", react$df_Dosage, react$df_new_dosage, CIP_data, CIP_dosage)
}

shinyApp(ui, server)