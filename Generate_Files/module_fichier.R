file_generate_button <- function(id) {
  ns <- NS(id)
  actionButton(ns("generate_button"), "Générer le fichier Excel", style = "margin-top: 25px")
}

file_output <- function(id) {
  ns <- NS(id)
  DT::dataTableOutput(ns("table_fichier"))
}

module_fichier <- function(input, output, session, df_dosage, df_new_dosage, CIP_data, CIP_dosage) {
  ns <- session$ns
  
  df_fichier <- reactive({
    df_fichier <- merge(df_dosage(), CIP_data, by="CIP7", all.x=TRUE) #CIP_data[CIP_data$CIP7 %in% react$df_Dosage()$CIP7,]
    df_fichier$Dosage <- df_fichier$Dosage.x
    df_fichier$Unites <- df_fichier$Unites.x
    column_order <- c("Laboratoire",
                      setdiff(colnames(df_fichier),
                              c("Laboratoire", "Dosage.x", "Dosage.y", "Unites.x", "Unites.y")))
    df_fichier <- df_fichier[,column_order]
  }) 
  
  output$table_fichier = DT::renderDataTable(df_fichier(), rownames = FALSE)
  
  observeEvent(input$generate_button, {
    textInputInline <- function(inputId, label, value = NULL) {
      tags$tr(width = "100%",
              tags$td(width = "40%", div(strong(label))),
              tags$td(width = "60%", textInput(ns(inputId), label = NULL, value = value)))
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
      textOutput(ns("text_generate")),
      textOutput(ns("text_column")),
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
        downloadButton(ns("download"), "Telecharger")
      )
    ))
  })
  
  output$download <- downloadHandler(filename = "Point_Stock.csv", content = function(file) {
    removeModal()
    df_fichier_final <- df_fichier()
    if(nrow(df_fichier_final) > 0) {
      df_fichier_final[input$column_1] <- ""
      df_fichier_final[input$column_2] <- ""
      df_fichier_final[input$column_3] <- ""
      df_fichier_final[input$column_4] <- ""
      df_fichier_final[input$column_5] <- ""
      df_fichier_final[input$column_6] <- ""
      df_fichier_final[input$column_7] <- ""
      df_fichier_final[input$column_8] <- ""
      df_fichier_final[input$column_9] <- ""
      df_fichier_final[input$column_10] <- ""
      write.table(df_fichier_final, file, sep=";", row.names=FALSE)
    }
    
    if(nrow(df_new_dosage()) > 0) {
      df_dosage_final <- rbind(CIP_dosage, df_new_dosage()[c("CIP7", "Dosage", "Unites")])
    } else {
      df_dosage_final <- CIP_dosage
    }
    write.table(df_dosage_final, "CIP_Dosage.csv", sep=";", row.names=FALSE)
  }, contentType = "text/csv")
}