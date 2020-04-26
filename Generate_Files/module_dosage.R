new_dosage_output <- function(id) {
  ns <- NS(id)
  DT::dataTableOutput(ns("table_Dosage"))
}

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

module_dosage <- function(input, output, session, df_DCI, CIP_data, CIP_dosage) {
  ns <- session$ns
  react <- reactiveValues(selectedRow = 0,
                          df_user_dosage = data.frame(CIP7 = character(), Dosage = character(), Unites = character(),
                                                      stringsAsFactors = F))
  
  df_dosage <- reactive({
    selected_CIP7 <- if(nrow(df_DCI()) > 0) {
      unlist(lapply(1:nrow(df_DCI()), function(i) {
        CIP_data$CIP7[CIP_data$DCI == df_DCI()$DCI[i] &
                        CIP_data$Forme %in% strsplit(df_DCI()$Formes[i],"<br>")[[1]]]
      }))
    } else character()
    
    data.frame(
      CIP7 = selected_CIP7,
      Dosage = sapply(selected_CIP7, function(i) {
        if(i %in% CIP_dosage$CIP7) {
          CIP_dosage$Dosage[CIP_dosage$CIP7 == i]
        } else if(i %in% react$df_user_dosage$CIP7) {
          react$df_user_dosage$Dosage[react$df_user_dosage$CIP7 == i]
        } else {
          CIP_data$Dosage[CIP_data$CIP7 == i]
        }
      }),
      Unites = sapply(selected_CIP7, function(i) {
        if(i %in% CIP_dosage$CIP7) {
          CIP_dosage$Unites[CIP_dosage$CIP7 == i]
        } else if(i %in% react$df_user_dosage$CIP7) {
          react$df_user_dosage$Unites[react$df_user_dosage$CIP7 == i]
        } else {
          CIP_data$Unites[CIP_data$CIP7 == i]
        }
      }),
      stringsAsFactors = F
    )
  })

  df_new_dosage <- reactive({
    selected_CIP7 <- setdiff(df_dosage()$CIP7, CIP_dosage$CIP7)
    data.frame(
      CIP7 = selected_CIP7,
      Specialite = CIP_data$Specialite[CIP_data$CIP7 %in% selected_CIP7],
      Presentation = CIP_data$Presentation[CIP_data$CIP7 %in% selected_CIP7],
      Dosage = df_dosage()$Dosage[df_dosage()$CIP7 %in% selected_CIP7],
      Unites = df_dosage()$Unites[df_dosage()$CIP7 %in% selected_CIP7],
      Actions = shinyInput(actionButton, length(selected_CIP7), 1, "button_", label = "Modifier",
                           onclick = paste0("Shiny.onInputChange(\"",ns("modify_dosage_button"),"\",  this.id)"))
    )
  })
  
  output$table_Dosage <- DT::renderDataTable(df_new_dosage(),
                                            selection="multiple",
                                            rownames = FALSE,
                                            escape = FALSE,
                                            options = list(
                                              dom = 'BRrltpi',
                                              autoWidth=TRUE,
                                              #lengthMenu = list(c(10, 50, -1), c('10', '50', 'All')),
                                              ColReorder = TRUE,
                                              preDrawCallback = JS('function() { Shiny.unbindAll(this.api().table().node()); }'),
                                              drawCallback = JS('function() { Shiny.bindAll(this.api().table().node()); } ')
                                            )
  )
  
  observeEvent(input$modify_dosage_button, {
    react$selectedRow <- as.numeric(strsplit(input$modify_dosage_button, "_")[[1]][2])
    showModal(modalDialog(
      title = "",
      textInput(ns("dosage"), label = "Veuillez entrer un nouveau dosage",
                value = df_new_dosage()$Dosage[react$selectedRow]),
      textInput(ns("unites"), label = "Veuillez entrer un nouveau nombre d'unites par boite",
                value = df_new_dosage()$Unites[react$selectedRow]),
      footer = tagList(
        modalButton("Annuler"),
        actionButton(ns("modal_dosage_ok"), "OK")
      )
    ))
    session$sendCustomMessage(type = 'resetInputValue', message = ns("modify_dosage_button"))
  })
  
  observeEvent(input$modal_dosage_ok, {
    removeModal()
    #print(df_DCI()[react$selectedRow, "Formes"])
    if(df_new_dosage()$CIP7[react$selectedRow] %in% react$df_user_dosage$CIP7) {
      react$df_user_dosage$Dosage[react$df_user_dosage$CIP7 == df_new_dosage()$CIP7[react$selectedRow]] <- input$dosage
      react$df_user_dosage$Unites[react$df_user_dosage$CIP7 == df_new_dosage()$CIP7[react$selectedRow]] <- input$unites
    } else {
      react$df_user_dosage <- rbind(react$df_user_dosage, data.frame(
        CIP7 = df_new_dosage()$CIP7[react$selectedRow],
        Dosage = input$dosage,
        Unites = input$unites,
        stringsAsFactors = FALSE
      ))
    }
  })
  
  return(list(df_dosage, df_new_dosage))
}