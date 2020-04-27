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