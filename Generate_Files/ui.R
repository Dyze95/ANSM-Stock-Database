# Define UI for application
ui <- fluidPage(
  
  tags$script("Shiny.addCustomMessageHandler('resetInputValue', function(variableName){
                Shiny.onInputChange(variableName, null);
                });
                "),
  
  titlePanel("DCI"),
  
  sidebarLayout(
    sidebarPanel(
      text_DCI_input("DCI"),
      add_DCI_button("DCI"),
      actionButton("generate_button", "Générer le fichier Excel")
      #actionButton("show_modal", "Voir"),
      #checkboxGroupInput("forme_1", label=NULL, choices=c(1,2,3), selected=c(1,2)),
      #pickerInput("test_select", "Test", paste0("Forme injectable pour perfusion numero ",c(1:10)),
      #            selected=c("Choix A", "Choix B", "Choix D"), multiple=TRUE)
      #verbatimTextOutput("test")
      
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("DCI", DCI_output("DCI")),
        tabPanel("Dosages", new_dosage_output("dosage")),
        tabPanel("Fichier", DT::dataTableOutput("table_fichier"))
      )
    )
  )
)