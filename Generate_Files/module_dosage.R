new_dosage_output <- function(id) {
  ns <- NS(id)
  DT::dataTableOutput(ns("table_Dosage"))
}

shinyInput <- function(FUN, len, start, id, ...) {
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
                          old_CIP7 = numeric(),
                          previous_page = NULL,
                          df_user_dosage = data.frame(CIP7 = character(), Dosage = character(), Unites = character(),
                                                      stringsAsFactors = F))
  
  df_dosage <- reactive({
    selected_CIP7 <- if(nrow(df_DCI()) > 0) {
      unlist(lapply(1:nrow(df_DCI()), function(i) {
        CIP_data$CIP7[CIP_data$DCI == df_DCI()$DCI[i] &
                        CIP_data$Forme %in% strsplit(df_DCI()$Formes[i],"<br>")[[1]]]
      }))
    } else character()
    
    react$new_CIP7 <- setdiff(selected_CIP7, isolate(react$old_CIP7))
    
    df_dosage <- CIP_data[CIP_data$CIP7 %in% selected_CIP7, c("CIP7", "DCI", "Specialite", "Presentation")]
    
    df_dosage$Dosage <- sapply(df_dosage$CIP7, function(i) {
      if(i %in% react$df_user_dosage$CIP7) {
        react$df_user_dosage$Dosage[react$df_user_dosage$CIP7 == i]
      } else if(i %in% CIP_dosage$CIP7) {
        CIP_dosage$Dosage[CIP_dosage$CIP7 == i]
      } else {
        CIP_data$Dosage[CIP_data$CIP7 == i]
      }
    })
    
    df_dosage$Unites <- sapply(df_dosage$CIP7, function(i) {
      if(i %in% react$df_user_dosage$CIP7) {
        react$df_user_dosage$Unites[react$df_user_dosage$CIP7 == i]
      } else if(i %in% CIP_dosage$CIP7) {
        CIP_dosage$Unites[CIP_dosage$CIP7 == i]
      } else {
        CIP_data$Unites[CIP_data$CIP7 == i]
      }
    })
    
    df_dosage$Actions <- shinyInput(actionButton, nrow(df_dosage), 1, "button_", label = "Modifier",
                                        onclick = paste0("Shiny.onInputChange(\"",ns("modify_dosage_button"),"\",  this.id);"),
                                        onmousedown = "event.stopPropagation();")
    
    df_dosage
  })

  
  output$table_Dosage <- DT::renderDataTable(df_dosage(), rownames = F, escape = F,
                                             selection = list(selected = which(df_dosage()$CIP7 %in% isolate(react$selection_CIP7))),
                                             options = list(
                                              dom = 'BRrltpi',
                                              autoWidth=TRUE,
                                              displayStart=react$previous_page,
                                              #lengthMenu = list(c(10, 50, -1), c('10', '50', 'All')),
                                              ColReorder = TRUE,
                                              preDrawCallback = JS('function() { Shiny.unbindAll(this.api().table().node()); }'),
                                              drawCallback = JS('function() { Shiny.bindAll(this.api().table().node()); } ')
                                             )
  )
  
  observe({
    react$selection_CIP7 <- isolate(df_dosage())$CIP7[input$table_Dosage_rows_selected]
  })
  
  observeEvent(react$new_CIP7, {
    react$selection_CIP7 <- c(react$selection_CIP7,
                              intersect(react$new_CIP7, CIP_dosage$CIP7))
  })
  
  observeEvent(df_dosage(), {
    react$old_CIP7 <- df_dosage()$CIP7
  })
  
  observeEvent(input$modify_dosage_button, {
    react$previous_page <- input$table_Dosage_rows_current[1] - 1
    react$selectedRow <- as.numeric(strsplit(input$modify_dosage_button, "_")[[1]][2])
    showModal(modalDialog(
      title = "",
      textInput(ns("dosage"), label = "Veuillez entrer un nouveau dosage",
                value = df_dosage()$Dosage[react$selectedRow]),
      uiOutput(ns("error_dosage")),
      numericInput(ns("unites"), label = "Veuillez entrer un nouveau nombre d'unites par boite",
                value = df_dosage()$Unites[react$selectedRow], min = 1),
      footer = tagList(
        modalButton("Annuler"),
        actionButton(ns("modal_dosage_ok"), "OK")
      )
    ))
    session$sendCustomMessage(type = 'resetInputValue', message = ns("modify_dosage_button"))
  })
  
  observeEvent(input$dosage, {
    if(grepl("^[0-9,]+mg$", input$dosage) || grepl("^[0-9,]+mg/mL;[0-9,]+mL$", input$dosage)) {
      runjs("document.getElementById('dosage-dosage').style.border = ''")
      output$error_dosage <- NULL
    } else {
      runjs("document.getElementById('dosage-dosage').style.border = 'solid red'")
      output$error_dosage <- renderUI(p("Attention, les dosages sont en general au format Xmg ou Xmg/mL;YmL", style = "color:red;font-size:10px"))
    }
  })
  
  observeEvent(input$modal_dosage_ok, {
    removeModal()
    if(!df_dosage()$CIP7[react$selectedRow] %in% react$selection_CIP7) {
      react$selection_CIP7 <- c(react$selection_CIP7, df_dosage()$CIP7[react$selectedRow])
    }
    if(df_dosage()$CIP7[react$selectedRow] %in% react$df_user_dosage$CIP7) {
      react$df_user_dosage$Dosage[react$df_user_dosage$CIP7 == df_dosage()$CIP7[react$selectedRow]] <- input$dosage
      react$df_user_dosage$Unites[react$df_user_dosage$CIP7 == df_dosage()$CIP7[react$selectedRow]] <- input$unites
    } else {
      react$df_user_dosage <- rbind(react$df_user_dosage, data.frame(
        CIP7 = df_dosage()$CIP7[react$selectedRow],
        Dosage = input$dosage,
        Unites = input$unites,
        stringsAsFactors = FALSE
      ))
    }
  })
  
  return(list(df_dosage, reactive(react$selection_CIP7)))
}