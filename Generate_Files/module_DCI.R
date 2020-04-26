DCI_output <- function(id) {
  ns <- NS(id)
  DT::dataTableOutput(ns("table_DCI"))
}

text_DCI_input <- function(id) {
  ns <- NS(id)
  textInput(ns("add_DCI"), "DCI à ajouter")
}

add_DCI_button<- function(id) {
  ns <- NS(id)
  actionButton(ns("add_button"), "Ajouter")
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

module_DCI <- function(input, output, session, CIP_data) {
  ns <- session$ns
  react <- reactiveValues(selectedRow = "",
                          previous_DCI_page = NULL,
                          df_DCI = data.frame(stringsAsFactors = FALSE))
  
  output$table_DCI = DT::renderDataTable({
    react$df_DCI
  }, rownames = FALSE, escape=FALSE, server=FALSE,
  options = list(
    dom = 'BRrltpi',
    autoWidth=TRUE,
    displayStart=react$previous_DCI_page,
    #lengthMenu = list(c(10, 50, -1), c('10', '50', 'All')),
    #ColReorder = TRUE,
    preDrawCallback = JS('function() { Shiny.unbindAll(this.api().table().node()); }'),
    drawCallback = JS('function() { Shiny.bindAll(this.api().table().node()); } ')
  ))
  
  observeEvent(input$modify_button, {
    react$previous_DCI_page <- input$table_DCI_rows_current[1] - 1
    react$selectedRow <- as.character(strsplit(input$modify_button, "_")[[1]][2])
    DCI <- react$df_DCI[react$selectedRow, "DCI"]
    showModal(modalDialog(
      title = DCI,
      checkboxGroupInput(ns("modal_forme"), label = NULL,
                         choices = unique(as.character(CIP_data$Forme[CIP_data$DCI == DCI])),
                         selected = strsplit(react$df_DCI[react$selectedRow,"Formes"],"<br>")[[1]]),
      footer = tagList(
        modalButton("Annuler"),
        actionButton(ns("modal_ok"), "OK")
      )
    ))
    session$sendCustomMessage(type = 'resetInputValue', message = ns("modify_button"))
  })
  
  observeEvent(input$modal_ok, {
    removeModal()
    #print(react$df_DCI[react$selectedRow, "Formes"])
    react$df_DCI[react$selectedRow, "Formes"] <- paste0(input$modal_forme,collapse="<br>")
  })
  
  observeEvent(input$delete_button, {
    react$previous_DCI_page <- input$table_DCI_rows_current[1] - 1
    selectedRow <- as.character(strsplit(input$delete_button, "_")[[1]][2])
    #print(selectedRow)
    #print(input$select_button)
    DCI <- react$df_DCI[selectedRow, "DCI"]
    #print(DCI)
    react$df_DCI <- react$df_DCI[row.names(react$df_DCI) != selectedRow,]
    #print(df$DCI)
    #df$CIP <- df$CIP[df$CIP$DCI != DCI,]
  })
  
  observeEvent(input$add_button, {
    nb_new_DCI <- sum(grepl(stri_trans_general(input$add_DCI, "Latin-ASCII"),setdiff(unique(CIP_data$DCI),react$df_DCI$DCI), ignore.case = TRUE))
    
    output$text_add_warning <- renderText({
      paste0("Vous êtes sûr le point d'ajouter ",
             nb_new_DCI, " nouveaux DCI. Confirmez-vous cette saisie ?")
    })
    
    if(nb_new_DCI > 19) {
      showModal(modalDialog(
        title ="Attention",
        textOutput(ns("text_add_warning")),
        footer = tagList(
          actionButton(ns("modal_add_no"), "Non"),
          actionButton(ns("modal_add_ok"), "Oui")
        )
      ))
    } else {
      add_DCI() 
    }
  })
  
  observeEvent(input$modal_add_ok, {removeModal(); add_DCI()})
  observeEvent(input$modal_add_no, {
    removeModal()
    updateTextInput(session, ns("add_DCI"), value = "")
  })
  
  add_DCI <- function() {  
    start <- if(nrow(react$df_DCI) == 0) 1 else max(as.numeric(row.names(react$df_DCI)))+1
    CIP_to_add <- CIP_data[grepl(stri_trans_general(input$add_DCI, "Latin-ASCII"),CIP_data$DCI, ignore.case = TRUE),]
    CIP_to_add <- CIP_to_add[!CIP_to_add$DCI %in% react$df_DCI$DCI,]
    
    if(nrow(CIP_to_add) > 0) {
      DCI_to_add <- data.frame(DCI=unique(CIP_to_add$DCI), stringsAsFactors = FALSE)
      
      DCI_to_add$Formes <- sapply(1:nrow(DCI_to_add), function(i) {
        paste0(unique(CIP_to_add$Forme[CIP_to_add$DCI == DCI_to_add$DCI[i]]),collapse="<br>")
      })
      DCI_to_add$Actions <- paste0(shinyInput(actionButton, nrow(DCI_to_add), start, "button_", label = "Modifier les formes",
                                              onclick = paste0("Shiny.onInputChange(\"",ns("modify_button"), "\",  this.id)")), "  ",
                                   shinyInput(actionButton, nrow(DCI_to_add), start, "button_", label = "Supprimer",
                                              onclick = paste0("Shiny.onInputChange(\"",ns("delete_button"), "\",  this.id)")))
      row.names(DCI_to_add) <- as.character(start:(start+nrow(DCI_to_add)-1))
      react$df_DCI <- rbind(react$df_DCI, DCI_to_add)
    }
    updateTextInput(session, ns("add_DCI"), value = "")
  }
  
  return(reactive(react$df_DCI))
}