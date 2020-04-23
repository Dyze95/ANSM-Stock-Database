library(shiny)
library(DT)
library(stringi)
library(shinyWidgets)
library(plyr)

#CIS <- read.delim(file = "CIS_bdpm.txt", quote = "", fill = F, stringsAsFactors = F, header = F, fileEncoding="ISO-8859-1")
CIP_data <- read.csv("CIP_Data.csv", sep=";", stringsAsFactors = FALSE)
CIP_dosage <- read.csv("CIP_Dosage.csv", sep=";", stringsAsFactors = FALSE)

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
            actionButton("generate_button", "Générer le fichier Excel")
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
                tabPanel("Fichier", DT::dataTableOutput("table_fichier"))
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
    
    #test <- reactiveValues(text = "Hello")
    selected <- reactiveValues()
    
    react <- reactiveValues()
    react$df_DCI <- data.frame(stringsAsFactors = FALSE)
    react$df_user_dosage = data.frame(CIP7 = character(), Dosage = character(), Unites = character(),
                                     stringsAsFactors = F)
    
    react$df_Dosage <- reactive({
        selected_CIP7 <- if(nrow(react$df_DCI) > 0) {
            unlist(lapply(1:nrow(react$df_DCI), function(i) {
                CIP_data$CIP7[CIP_data$DCI == react$df_DCI$DCI[i] &
                                  CIP_data$Forme %in% strsplit(react$df_DCI$Formes[i],"<br>")[[1]]]
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
    
    react$df_new_dosage <- reactive({
        selected_CIP7 <- setdiff(react$df_Dosage()$CIP7, CIP_dosage$CIP7)
        data.frame(
            CIP7 = selected_CIP7,
            Specialite = CIP_data$Specialite[CIP_data$CIP7 %in% selected_CIP7],
            Presentation = CIP_data$Presentation[CIP_data$CIP7 %in% selected_CIP7],
            Dosage = react$df_Dosage()$Dosage[react$df_Dosage()$CIP7 %in% selected_CIP7],
            Unites = react$df_Dosage()$Unites[react$df_Dosage()$CIP7 %in% selected_CIP7],
            Actions = shinyInput(actionButton, length(selected_CIP7), 1, "button_", label = "Modifier",
                                 onclick = 'Shiny.onInputChange(\"modify_dosage_button\",  this.id)')
        )
    })
    
    react$df_fichier <- reactive({
        df_fichier <- merge(react$df_Dosage(), CIP_data, by="CIP7", all.x=TRUE) #CIP_data[CIP_data$CIP7 %in% react$df_Dosage()$CIP7,]
        df_fichier$Dosage <- df_fichier$Dosage.x
        df_fichier$Unites <- df_fichier$Unites.x
        column_order <- c("Laboratoire",
                          setdiff(colnames(df_fichier),
                                  c("Laboratoire", "Dosage.x", "Dosage.y", "Unites.x", "Unites.y")))
        df_fichier <- df_fichier[,column_order]
    })
    
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
    
    output$table_fichier = DT::renderDataTable(react$df_fichier(), rownames = FALSE)
    
    output$table_Dosage = DT::renderDataTable(react$df_new_dosage(),
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
    
    observeEvent(input$modify_dosage_button, {
        react$selectedRow <- as.numeric(strsplit(input$modify_dosage_button, "_")[[1]][2])
        showModal(modalDialog(
            title = "",
            textInput("dosage", label = "Veuillez entrer un nouveau dosage",
                                value = react$df_new_dosage()$Dosage[react$selectedRow]),
            textInput("unites", label = "Veuillez entrer un nouveau nombre d'unites par boite",
                                value = react$df_new_dosage()$Unites[react$selectedRow]),
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
        if(react$df_new_dosage()$CIP7[react$selectedRow] %in% react$df_user_dosage$CIP7) {
            react$df_user_dosage$Dosage[react$df_user_dosage$CIP7 == react$df_new_dosage()$CIP7[react$selectedRow]] <- input$dosage
            react$df_user_dosage$Unites[react$df_user_dosage$CIP7 == react$df_new_dosage()$CIP7[react$selectedRow]] <- input$unites
        } else {
            react$df_user_dosage <- rbind(react$df_user_dosage, data.frame(
                CIP7 = react$df_new_dosage()$CIP7[react$selectedRow],
                Dosage = input$dosage,
                Unites = input$unites,
                stringsAsFactors = FALSE
            ))
        }
        print(react$df_user_dosage)
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
        nb_new_DCI <- sum(grepl(stri_trans_general(input$add_DCI, "Latin-ASCII"),setdiff(unique(CIP_data$DCI),react$df_DCI$DCI), ignore.case = TRUE))
        
        output$text_add_warning <- renderText({
        paste0("Vous êtes sûr le point d'ajouter ",
               nb_new_DCI, " nouveaux DCI. Confirmez-vous cette saisie ?")
        })
        
        if(nb_new_DCI > 19) {
            showModal(modalDialog(
                title ="Attention",
                textOutput("text_add_warning"),
                footer = tagList(
                    actionButton("modal_add_no", "Non"),
                    actionButton("modal_add_ok", "Oui")
                )
            ))
        } else {
           add_DCI() 
        }
    })
    
    observeEvent(input$modal_add_ok, {removeModal(); add_DCI()})
    observeEvent(input$modal_add_no, {
        removeModal()
        updateTextInput(session, "add_DCI", value = "")
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
                                             onclick = 'Shiny.onInputChange(\"modify_button\",  this.id)'), "  ",
                                         shinyInput(actionButton, nrow(DCI_to_add), start, "button_", label = "Supprimer",
                                                    onclick = 'Shiny.onInputChange(\"delete_button\",  this.id)'))
            row.names(DCI_to_add) <- as.character(start:(start+nrow(DCI_to_add)-1))
            react$df_DCI <- rbind(react$df_DCI, DCI_to_add)
        }
        updateTextInput(session, "add_DCI", value = "")
    }
    
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

# Run the application 
shinyApp(ui = ui, server = server)