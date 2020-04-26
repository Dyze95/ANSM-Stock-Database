library(shiny)
library(DT)
library(stringi)
library(plyr)

CIP_data <- read.csv("CIP_Data.csv", sep=";", stringsAsFactors = FALSE)
CIP_dosage <- read.csv("CIP_Dosage.csv", sep=";", stringsAsFactors = FALSE)

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
  
  react$df_fichier <- reactive({
    df_fichier <- merge(react$df_Dosage(), CIP_data, by="CIP7", all.x=TRUE) #CIP_data[CIP_data$CIP7 %in% react$df_Dosage()$CIP7,]
    df_fichier$Dosage <- df_fichier$Dosage.x
    df_fichier$Unites <- df_fichier$Unites.x
    column_order <- c("Laboratoire",
                      setdiff(colnames(df_fichier),
                              c("Laboratoire", "Dosage.x", "Dosage.y", "Unites.x", "Unites.y")))
    df_fichier <- df_fichier[,column_order]
  })
  
  output$table_fichier = DT::renderDataTable(react$df_fichier(), rownames = FALSE)
  
  observeEvent(input$generate_button, {
    textInputInline <- function(inputId, label, value = NULL) {
      tags$tr(width = "100%",
              tags$td(width = "40%", div(strong(label))),
              tags$td(width = "60%", textInput(inputId, label = NULL, value = value)))
    }
    
    
    output$text_generate <- renderText("Avez-vous bien vérifié que les
                                           informations de l'onglet Dosages sont correctes ?
                                           Elles contiennent des informations sur les CIP7 non
                                           rencontrés jusqu'à présent. Ces informations
                                           seront enregistrées dans le système et non modifiables.")
    output$text_column <- renderText("Veuillez choisir ci-dessous les noms de colonnes
                                         qui seront affichées dans le fichier.")
    showModal(modalDialog(
      title = "Generation du fichier Excel",
      textOutput("text_generate"),
      textOutput("text_columns"),
      h3(""),
      tags$table(width="80%",
                 textInputInline("column_1", "Colonne 1 : ", value = "Stock à date (boîtes)"),
                 textInputInline("column_2", "Colonne 2 : ", value = "Ventes J-1 (boites)"),
                 textInputInline("column_3", "Colonne 3 : ", value = "Conso Mensuelle Habituelle (boites)"),
                 textInputInline("column_4", "Colonne 4 : ", value = "Approvisionnement S16 (boites)"),
                 textInputInline("column_5", "Colonne 5 : ", value = "Approvisionnement S17 (boites)"),
                 textInputInline("column_6", "Colonne 6 : ", value = "Approvisionnement S18 (boites)"),
                 textInputInline("column_7", "Colonne 7 : ", value = "Approvisionnement S19 (boites)"),
                 textInputInline("column_8", "Colonne 8 : ", value = "Approvisionnement S20 (boites)"),
                 textInputInline("column_9", "Colonne 9 : ", value = "Approvisionnement S21 (boites)"),
                 textInputInline("column_10", "Colonne 10 : ", value = "Commentaires")
      ),
      footer = tagList(
        modalButton("Annuler"),
        downloadButton("download", "Telecharger")
      )
    ))
  })
  
  output$download <- downloadHandler(filename = "Point_Stock.csv", content = function(file) {
    removeModal()
    df_fichier <- react$df_fichier()
    if(nrow(df_fichier) > 0) {
      df_fichier[input$column_1] <- ""
      df_fichier[input$column_2] <- ""
      df_fichier[input$column_3] <- ""
      df_fichier[input$column_4] <- ""
      df_fichier[input$column_5] <- ""
      df_fichier[input$column_6] <- ""
      df_fichier[input$column_7] <- ""
      df_fichier[input$column_8] <- ""
      df_fichier[input$column_9] <- ""
      df_fichier[input$column_10] <- ""
      write.table(df_fichier, file, sep=";", row.names=FALSE)
    }
    
    if(nrow(react$df_new_dosage()) > 0) {
      df_dosage <- rbind(CIP_dosage, react$df_new_dosage()[c("CIP7", "Dosage", "Unites")])
    } else {
      df_dosage <- CIP_dosage
    }
    write.table(df_dosage, "CIP_Dosage.csv", sep=";", row.names=FALSE)
  }, contentType = "text/csv")
}