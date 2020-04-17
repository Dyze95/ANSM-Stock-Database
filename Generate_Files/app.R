library(shiny)
library(DT)

#CIS <- read.delim(file = "CIS_bdpm.txt", quote = "", fill = F, stringsAsFactors = F, header = F, fileEncoding="ISO-8859-1")
CIP_data <- read.csv("CIP_Data.csv", sep=";")

# Define UI for application
ui <- fluidPage(
    
    titlePanel("DCI"),

    sidebarLayout(
        sidebarPanel(
            textInput("add_DCI", "DCI à ajouter"),
            actionButton("add_button", "Ajouter")
            #textOutput("text")
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
        for (i in 1:len) {
            inputs[i] <- as.character(FUN(paste0(id, i-1+start), ...))
        }
        inputs
    }
    
    test <- reactiveValues(text = "Hello")
    
    df <- reactiveValues(DCI = data.frame(stringsAsFactors = FALSE),
                         CIP = data.frame(stringsAsFactors = FALSE),
                         df_Dosage = data.frame(
                             CIP7 = c(1,2,3,4),
                             Dosage = c("50mg", "100mg", "200mg", "300mg"),
                             New_Dosage = shinyInput(textInput, 4, 1, "dosage_", label = NULL),
                             Actions = shinyInput(actionButton, 4, 1, "button_", label = "Valider",
                                                  onclick = 'Shiny.onInputChange(\"validate_button\",  this.id)'),
                             stringsAsFactors = FALSE))
    
    output$table_DCI = DT::renderDataTable(df$DCI, escape=FALSE)
    
    output$table_CIP = DT::renderDataTable(df$CIP, escape=FALSE)
    
    output$table_Dosage = DT::renderDataTable({
        datatable(
            df$df_Dosage,
            selection="multiple",
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
    })
    
    #output$text = renderText(test$text)
    
    observeEvent(input$validate_button, {
        selectedRow <- as.character(strsplit(input$validate_button, "_")[[1]][2])
        test$text <- input$dosage_1
        print(input$dosage_1)
        print(input$validate_button)
    })
    
    observeEvent(input$select_button, {
        selectedRow <- as.character(strsplit(input$select_button, "_")[[1]][2])
        #print(selectedRow)
        #print(input$select_button)
        DCI <- df$DCI[selectedRow, "DCI"]
        #print(DCI)
        df$DCI <- df$DCI[row.names(df$DCI) != selectedRow,]
        #print(df$DCI)
        df$CIP <- df$CIP[df$CIP$DCI != DCI,]
    })
    
    observeEvent(input$add_button, {
        #print(input$add_DCI)
        start <- if(nrow(df$DCI) == 0) 1 else max(as.numeric(row.names(df$DCI)))+1
        CIP_to_add <- CIP_data[grepl(stri_trans_general(input$add_DCI, "Latin-ASCII"),CIP_data$DCI, ignore.case = TRUE),]
        CIP_to_add <- CIP_to_add[!CIP_to_add$DCI %in% df$DCI$DCI,]
        #View(CIP_to_add)
        if(nrow(CIP_to_add) > 0) {
            DCI_to_add <- data.frame(DCI=unique(CIP_to_add$DCI), stringsAsFactors = FALSE)
        
            #print(length(DCI_to_add))
            DCI_to_add$Actions <- shinyInput(actionButton, nrow(DCI_to_add), start, "button_", label = "Supprimer",
                                             onclick = 'Shiny.onInputChange(\"select_button\",  this.id)')
            #View(DCI_to_add)
            #sapply(CIP_from_DCI$CIP7, function(x) {
            #    return(as.character(actionButton(paste0("button_", x), label = "Supprimer",
            #                              onclick = 'Shiny.onInputChange(\"select_button\",  this.id)')))
            #})
            #View(CIP_from_DCI)
            row.names(DCI_to_add) <- as.character(start:(start+nrow(DCI_to_add)-1))
            df$DCI <- rbind(df$DCI, DCI_to_add)
            df$CIP <- rbind(df$CIP, CIP_to_add)
        }
        updateTextInput(session, "add_DCI", value = "")
    })
}

# Run the application 
shinyApp(ui = ui, server = server)


