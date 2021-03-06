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
  tags$style(HTML('#dosage-table_Dosage table.dataTable tr.selected td, table.dataTable td.selected {background-color: palegreen !important;}')), 
  
  titlePanel("Création des fichiers de suivi"),
  
  verticalLayout(
    sidebarPanel(width = 7, fluidRow(
      column(width = 7, text_DCI_input("DCI")),
      add_DCI_button("DCI"),
      file_generate_button("fichier")
    )),
    
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

  df_DCI <- callModule(module_DCI, "DCI", CIP_data)
  
  tmp <- callModule(module_dosage, "dosage", df_DCI, CIP_data, CIP_dosage)
  df_dosage <- tmp[[1]]
  selection_CIP7 <- tmp[[2]]
  
  callModule(module_fichier, "fichier", df_dosage, selection_CIP7, CIP_data, CIP_dosage)
  
}

shinyApp(ui, server)