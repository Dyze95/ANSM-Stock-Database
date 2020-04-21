library(shiny)
library(DT)
library(stringi)
library(shinyWidgets)

#CIS <- read.delim(file = "CIS_bdpm.txt", quote = "", fill = F, stringsAsFactors = F, header = F, fileEncoding="ISO-8859-1")
CIP_data <- read.csv("CIP_Data.csv", sep=";", stringsAsFactors = FALSE)

# Define UI for application
ui <- fluidPage(
    
    tags$script("Shiny.addCustomMessageHandler('resetInputValue', function(variableName){
                Shiny.onInputChange(variableName, null);
                });
                "),
    
    titlePanel("DCI"),

    sidebarLayout(
        sidebarPanel(
            textInput("add_DCI", "DCI à ajouter"),
            actionButton("add_button", "Ajouter"),
            #actionButton("show_modal", "Voir"),
            #checkboxGroupInput("forme_1", label=NULL, choices=c(1,2,3), selected=c(1,2)),
            #pickerInput("test_select", "Test", paste0("Forme injectable pour perfusion numero ",c(1:10)),
            #            selected=c("Choix A", "Choix B", "Choix D"), multiple=TRUE)
            #verbatimTextOutput("test")
            
        ),
        
        mainPanel(
            tabsetPanel(
                tabPanel("DCI", DT::dataTableOutput("table_DCI")),
                tabPanel("Dosages", DT::dataTableOutput("table_Dosage")),
                tabPanel("Fichier", DT::dataTableOutput("table_CIP"))
            ),
            actionButton("generate_button", "Générer le fichier Excel")
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
    
    #test <- reactiveValues(text = "Hello")
    selected <- reactiveValues()
    
    react <- reactiveValues()
    react$df_DCI <- data.frame(stringsAsFactors = FALSE)
    react$df_new_dosage = data.frame(CIP7 = character(), New_Dosage = character())
    #react$df_CIP <- data.frame(stringsAsFactors = FALSE)
    #react$df_Dosage <- data.frame(
    #                         CIP7 = c(1,2,3,4),
    #                         Dosage = c("50mg", "100mg", "200mg", "300mg"),
    #                         New_Dosage = c("50mg", "100mg", "200mg", "300mg"),
    #                         Actions = shinyInput(actionButton, 4, 1, "button_", label = "Modifier",
    #                                              onclick = 'Shiny.onInputChange(\"modify_dosage_button\",  this.id)'),
    #                         stringsAsFactors = FALSE)
    
    output$table_DCI = DT::renderDataTable({
        react$df_DCI
    }, rownames = FALSE, escape=FALSE,
                                           options = list(
                                               dom = 'BRrltpi',
                                               autoWidth=TRUE,
                                               #lengthMenu = list(c(10, 50, -1), c('10', '50', 'All')),
                                               ColReorder = TRUE,
                                               preDrawCallback = JS('function() { Shiny.unbindAll(this.api().table().node()); }'),
                                               drawCallback = JS('function() { Shiny.bindAll(this.api().table().node()); } ')
                                           ))
    
    output$table_CIP = DT::renderDataTable({
        CIP_data[sapply(1:nrow(CIP_data), function(i) {
            if(CIP_data$DCI[i] %in% react$df_DCI$DCI){
                row_name <- row.names(react$df_DCI)[which(CIP_data$DCI[i] == react$df_DCI$DCI)]
                return(CIP_data$Forme[i] %in% strsplit(react$df_DCI[row_name,"Formes"],"<br>")[[1]])
            } else {
                return(FALSE)
            }
        }),]
    }, rownames = FALSE)
    
    output$table_Dosage = DT::renderDataTable({
        selected_CIP7 <- CIP_data[sapply(1:nrow(CIP_data), function(i) {
            if(CIP_data$DCI[i] %in% react$df_DCI$DCI){
                row_name <- row.names(react$df_DCI)[which(CIP_data$DCI[i] == react$df_DCI$DCI)]
                return(CIP_data$Forme[i] %in% strsplit(react$df_DCI[row_name,"Formes"],"<br>")[[1]])
            } else {
                return(FALSE)
            }
        }), "CIP7"]
        react$df_Dosage <- data.frame(
            CIP7 = selected_CIP7,
            Specialite = as.character(CIP_data$Specialite[CIP_data$CIP7 %in% selected_CIP7]),
            Presentation = as.character(CIP_data$Presentation[CIP_data$CIP7 %in% selected_CIP7]),
            Dosage = as.character(CIP_data$Dosage[CIP_data$CIP7 %in% selected_CIP7]),
            New_Dosage = sapply(selected_CIP7, function(CIP) {
                if(CIP %in% react$df_new_dosage$CIP7) {
                    return(react$df_new_dosage$New_Dosage[react$df_new_dosage$CIP7 == CIP])
                } else {
                    return(CIP_data$Dosage[CIP_data$CIP7 == CIP])
                }
            }),
            Actions = shinyInput(actionButton, length(selected_CIP7), 1, "button_", label = "Modifier",
                                 onclick = 'Shiny.onInputChange(\"modify_dosage_button\",  this.id)')
        )
    },
        selection="multiple",
        rownames = TRUE,
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
    
    #output$test = renderText({
    #    input$dosage_2
    #})
    
    #observeEvent(input$forme_1, {
    #    test$text <- input$forme_1
    #    print("VU")
    #    print(input$forme_1)
    #})
    
    observeEvent(input$validate_button, {
        selectedRow <- as.character(strsplit(input$validate_button, "_")[[1]][2])
        print(selectedRow)
        print(input[[paste0("dosage_",selectedRow)]])
        react$df_Dosage[selectedRow,"New_Dosage"] <- as.character(input[[paste0("dosage_",selectedRow)]])
        #updateActionButton(session,paste0("button_",selectedRow), label = "Modifier")
        react$df_Dosage[selectedRow,"Actions"] <- as.character({
            actionButton(paste0("button_",selectedRow), label = "Modifier",
                         onclick = 'Shiny.onInputChange(\"modify_dosage_button\",  this.id)')
        })
        #print(input$dosage_1)
        #print(input$validate_button)
    })
    
    observeEvent(input$modify_dosage_button, {
        react$selectedRow <- as.character(strsplit(input$modify_dosage_button, "_")[[1]][2])
        #react$df_Dosage[selectedRow, "New_Dosage"] <- as.character({
        #    textInput(paste0("dosage_",selectedRow),
        #              value = input[[paste0("dosage_",selectedRow)]], label = NULL)
        #})
        #updateActionButton(session,paste0("button_",selectedRow), label = "Modifier")
        #react$df_Dosage[selectedRow,"Actions"] <- as.character({
        #    actionButton(paste0("button_",selectedRow), label = "Valider",
        #                 onclick = 'Shiny.onInputChange(\"validate_button\",  this.id)')
        #})
        #print(input$dosage_1)
        #print(input$validate_button)
        showModal(modalDialog(
            title = "",
            textInput("dosage", label = "Veuillez entrer un nouveau dosage",
                                value = react$df_Dosage[react$selectedRow, "New_Dosage"]),
            footer = tagList(
                modalButton("Annuler"),
                actionButton("modal_dosage_ok", "OK")
            )
        ))
        session$sendCustomMessage(type = 'resetInputValue', message = "modify_dosage_button")
    })
    
    observeEvent(input$modal_dosage_ok, {
        removeModal()
        #print(react$df_DCI[react$selectedRow, "Formes"])
        if(react$df_Dosage[react$selectedRow, "CIP7"] %in% react$df_new_dosage$CIP7) {
            react$df_new_dosage$New_Dosage[react$df_new_dosage$CIP7 == react$df_Dosage[react$selectedRow, "CIP7"]] <- input$dosage
        } else {
            react$df_new_dosage <- rbind(react$df_new_dosage, data.frame(
                CIP7 = react$df_Dosage[react$selectedRow, "CIP7"],
                New_Dosage = input$dosage,
                stringsAsFactors = FALSE
            ))
        }
        print(react$df_new_dosage)
    })
    
    observeEvent(input$modify_button, {
        react$selectedRow <- as.character(strsplit(input$modify_button, "_")[[1]][2])
        DCI <- react$df_DCI[react$selectedRow, "DCI"]
        showModal(modalDialog(
            title = DCI,
            checkboxGroupInput("modal_forme", label = NULL,
                        choices = unique(as.character(CIP_data$Forme[CIP_data$DCI == DCI])),
                        selected = strsplit(react$df_DCI[react$selectedRow,"Formes"],"<br>")[[1]]),
            footer = tagList(
                modalButton("Annuler"),
                actionButton("modal_ok", "OK")
            )
        ))
        session$sendCustomMessage(type = 'resetInputValue', message = "modify_button")
    })
    
    observeEvent(input$modal_ok, {
        removeModal()
        #print(react$df_DCI[react$selectedRow, "Formes"])
        react$df_DCI[react$selectedRow, "Formes"] <- paste0(input$modal_forme,collapse="<br>")
    })
    
    observeEvent(input$delete_button, {
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
        #print(input$add_DCI)
        start <- if(nrow(react$df_DCI) == 0) 1 else max(as.numeric(row.names(react$df_DCI)))+1
        CIP_to_add <- CIP_data[grepl(stri_trans_general(input$add_DCI, "Latin-ASCII"),CIP_data$DCI, ignore.case = TRUE),]
        CIP_to_add <- CIP_to_add[!CIP_to_add$DCI %in% react$df_DCI$DCI,]
        #View(CIP_to_add)
        if(nrow(CIP_to_add) > 0) {
            DCI_to_add <- data.frame(DCI=unique(CIP_to_add$DCI), stringsAsFactors = FALSE)
        
            DCI_to_add$Formes <- sapply(1:nrow(DCI_to_add), function(i) {
                paste0(unique(CIP_to_add$Forme[CIP_to_add$DCI == DCI_to_add$DCI[i]]),collapse="<br>")
            })
                #sapply(1:nrow(DCI_to_add), function(i){
                #as.character(checkboxGroupInput(paste0("forme_",start+i-1),label=NULL,
                #                                choices=unique(CIP_to_add$Forme[CIP_to_add$DCI == DCI_to_add$DCI[i]]),
                #                                selected=selected$x_1))#unique(CIP_to_add$Forme[CIP_to_add$DCI == DCI_to_add$DCI[i]])))
            #})
                
            #print("OK")
            DCI_to_add$Actions <- paste0(shinyInput(actionButton, nrow(DCI_to_add), start, "button_", label = "Modifier les formes",
                                             onclick = 'Shiny.onInputChange(\"modify_button\",  this.id)'), "  ",
                                         shinyInput(actionButton, nrow(DCI_to_add), start, "button_", label = "Supprimer",
                                                    onclick = 'Shiny.onInputChange(\"delete_button\",  this.id)'))
            #View(DCI_to_add)
            #sapply(CIP_from_DCI$CIP7, function(x) {
            #    return(as.character(actionButton(paste0("button_", x), label = "Supprimer",
            #                              mononclick = 'Shiny.onInputChange(\"select_button\",  this.id)')))
            #})
            #View(CIP_from_DCI)
            row.names(DCI_to_add) <- as.character(start:(start+nrow(DCI_to_add)-1))
            react$df_DCI <- rbind(react$df_DCI, DCI_to_add)
            #df$CIP <- rbind(df$CIP, CIP_to_add)
        }
        updateTextInput(session, "add_DCI", value = "")
    })
}

# Run the application 
shinyApp(ui = ui, server = server)

