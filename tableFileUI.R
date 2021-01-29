
library(shinydashboardPlus)

tableFileUI <- function(id, inputBoxTitle = "Input", outputBoxTitle = "Output",
                        csvlabel = "CSV file", label_1 = "time", label_2 = "logN",
                        default_frame = data.frame(c(0, 10), c(70, 80))) {
    
    ns <- NS(id)
    
    names(default_frame) <- c(label_1, label_2)

    tagList(
        fluidRow(
            
            ## Input
            
            tabBox(title = inputBoxTitle, id = ns("my_tabBox"), side = "right",
                   selected = "Excel",
                   
                   # tabPanel("Old",
                   #          matrixInput(inputId = ns("manual_table"),
                   #                      label = paste(label_1, label_2, sep = " - "),
                   #                      data = default_frame)
                   #          ),
                   
                   tabPanel("Manual",
                            rHandsontableOutput(ns("hot"))
                   ),
                   tabPanel("Text",
                            fileInput(ns("file"), csvlabel),
                            radioButtons(ns("sep"), "Separator",
                                         c(Comma = ",", Semicolon = ";", Tab = "\t"), "\t"),
                            radioButtons(ns("dec"), "Decimal Point",
                                         c(Point = ".", Comma = ","), ".")
                            ),
                   tabPanel("Excel",
                            fileInput(ns("excel_file"), "Excel file"),
                            textInput(ns("excel_sheet"), "Sheet name", "Sheet1"),
                            numericInput(ns("excel_skip"), "Skip", 0)
                            )
                   
                   ),
            
            ## Output
            
            boxPlus(title = outputBoxTitle, collapsible = TRUE, status = "primary",
                    solidHeader = TRUE, closable = FALSE,
                    actionButton(ns("update_table"), "Refresh"),
                    # tableOutput(ns("my_table"))
                    plotlyOutput(ns("my_plot")),
                    # tags$hr(),
                    footer = downloadLink(ns("export_table"), "Export")
                    )
            
        )
        

    )

}






