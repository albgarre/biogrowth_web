


body <- dashboardBody(
    shinyDashboardThemes(
        theme = "poor_mans_flatly"
    ),
    tabItems(
        tabItem(tabName = "welcome",
                # h2("Welcome tab")
                boxPlus(width = 12,
                    withMathJax(includeMarkdown("welcome_page.md"))
                    )
                ),
        
        ## Utils ---------------------------------------------------------------
        
        tabItem(tabName = "utils",
                fluidRow(
                    boxPlus(title = "Q0 to lambda", status = "primary",
                            solidHeader = TRUE, collapsible = FALSE, closable = FALSE,
                            column(12,
                                   numericInput("utils_in_Q0", "Q0 (.)", 1e-3, min = 0),
                                   numericInput("utils_mu_1", "Growth rate (log CFU/h)",
                                                .8, min = 0)
                                   ),
                            valueBoxOutput("utils_out_lambda", width = 12)
                            ),
                    boxPlus(title = "lambda to Q0", status = "primary",
                            solidHeader = TRUE, collapsible = FALSE, closable = FALSE,
                            column(12,
                                   numericInput("utils_in_lambda", "Lag phase (h)", 10, min = 0),
                                   numericInput("utils_mu_2", "Growth rate (log CFU/h)",
                                                .6, min = 0)
                                   ),
                            valueBoxOutput("utils_out_Q0", width = 12)
                            
                            )
                )
        ),
        
        ## Static prediction -----------------------------------------------------------------
        
        tabItem(tabName = "st_prediction",
                fluidRow(
                    boxPlus(title = tagList("Model definition",
                                            actionBttn("help_static_prediction",
                                                       label = NULL,
                                                       style = "bordered",
                                                       icon = icon("info"),
                                                       size = "xs"
                                                       )
                                            ),
                            status = "primary",
                        solidHeader = TRUE, collapsible = FALSE, closable = FALSE,
                        pickerInput(
                            "modelStaticPrediction",
                            "Primary growth model",
                            primary_model_data() %>% set_names(., .) %>% as.list()
                        ),
                        wellPanel(
                            uiOutput("static_pars")
                        ),
                        numericInput("static_max_time", "Maximum time", 80, min = 0),
                        textInput("static_pred_simName", "Simulation name", "Growth condition I"),
                        bsTooltip("static_pred_simName", paste("The growth curves are coloured according to this name.",
                                                               "If it already exists, replaces the curve"),
                                  "right", options = list(container = "body")),
                        actionButton("static_pred_addSim", "Add/Edit Simulation"),
                        actionButton("static_pred_cleanUp", "Clear plot")
                    ),
                    boxPlus(title = "Model predictions",
                            status = "success",
                        solidHeader = TRUE, closable = FALSE,
                        plotlyOutput("plot_static_prediction"),
                        dropdownButton(circle = TRUE, status = "success", 
                                       icon = icon("gear"), width = "300px",
                                       textInput("static_xaxis", "Label of x-axis", "Storage time"),
                                       textInput("static_yaxis", "Label of y-axis", "Log microbial count")
                        ),
                        br(),
                        downloadButton("static_export", "Export simulations")

                    )
                ),
                fluidRow(
                    boxPlus(title = "Time to X log count", status = "success",
                        solidHeader = TRUE, closable = FALSE,
                        numericInput("static_tgt_count", "Target count", 2),
                        tableOutput("static_timeToTable")
                        ),
                    boxPlus(title = "Log count at time X", status = "success",
                             solidHeader = TRUE, closable = FALSE,
                             numericInput("static_tgt_time", "Time", 40),
                             tableOutput("static_countAtTimeTable")
                             )
                )
                ),
        
        ## Stochastic static prediction ----------------------------------------
        
        tabItem(
            tabName = "stoc_prediction",
            fluidRow(
                boxPlus(title = tagList("Model definition",
                                        actionBttn("help_stoc_prediction_definition",
                                                   label = NULL,
                                                   style = "bordered",
                                                   icon = icon("info"),
                                                   size = "xs"
                                        )
                                        ), 
                        status = "primary",
                    solidHeader = TRUE, closable = FALSE,
                    pickerInput(
                        "mosdelStaticPredictionStoc",
                        "Primary growth model",
                        primary_model_data() %>% set_names(., .) %>% as.list()
                    ),
                    wellPanel(
                        uiOutput("stoc_pars")
                    ),
                    numericInput("max_time_stoc_static", "Maximum time", value = 80, min = 0),
                    numericInput("n_sims_static", "Number of simulations", value = 1000, min = 0),
                    actionButton("stoc_calculate", "Calculate"),
                    actionButton("stoc_resetSeed", "Reset seed")
                ),
                boxPlus(title = tagList("Stochastic predictions",
                                        actionBttn("help_stoc_prediction_result",
                                                   label = NULL,
                                                   style = "bordered",
                                                   icon = icon("info"),
                                                   size = "xs"
                                        )
                        ),
                        status = "success",
                        closable = FALSE, solidHeader = TRUE,
                        # fluidRow(
                        #     column(8, tags$h3("Stochastic growth curve")),
                        #     column(4, actionBttn("help_stoc_plot", icon = icon("info")))
                        # ),
                    tags$h3("Stochastic growth curve"),
                    dropdownButton(circle = TRUE, status = "success", 
                                   icon = icon("gear"), width = "300px",
                                   textInput("static_xaxis_stoc", "Label of x-axis", "Storage time"),
                                   textInput("static_yaxis_stoc", "Label of y-axis", "Log microbial count"),
                                   colourInput("static_linecol_stoc", "Line colour", "black"),
                                   colourInput("static_ribbon80fill_stoc", "Colour of narrow ribbon", "grey"),
                                   colourInput("static_ribbon90fill_stoc", "Colour of wide ribbon", "grey"),
                                   numericInput("static_linesize_stoc", "Line size", 1, min = 0),
                                   selectInput("static_linetype_stoc", "Line type", 
                                               choices = list("solid", "dashed", "dotted", "dotdash",
                                                              "longdash", "twodash")),
                                   numericInput("static_alpha80_stoc", "Transparency narrow ribbon", .5, min = 0, max = 1, step = .1),
                                   numericInput("static_alpha90_stoc", "Transparency wide ribbon", .5, min = 0, max = 1, step = .1)
                    ),
                    plotlyOutput("plot_static_prediction_stoc") %>% withSpinner(color = "#2492A8"),
                    br(),
                    downloadBttn("static_stoc_down", label = "Export quantiles", 
                                 color = "success", style = "simple"),
                    tags$hr(),
                    tags$h3("Time to reach a microbial count"),
                    numericInput("tgt_cont_stoc_static", "Target log microbial count", value = 4,
                                 width = "50%"),
                    column(12, plotlyOutput("plot_static_timedistrib")),
                    column(12, tableOutput("table_static_timedistrib"))
                )
            )
        ),
        
        ## Dynamic prediction --------------------------------------------------------------
        
        tabItem(tabName = "dyna_prediction",
                fluidRow(
                    boxPlus(title = tagList("Data input", 
                                            actionBttn("help_dyna_prediction",
                                                       label = NULL,
                                                       style = "bordered",
                                                       icon = icon("info"),
                                                       size = "xs"
                                            )
                                            ),
                            solidHeader = TRUE,
                        status = "primary", closable = FALSE,
                        fileInput("dynPred_excel_file", "Excel file"),
                        textInput("dynPred_excel_sheet", "Sheet name", "Sheet1"),
                        numericInput("dynPred_excel_skip", "Skip", 0),
                        tags$hr(),
                        downloadLink("dynPred_download_example", "Download example")
                    ),
                    boxPlus(status = "primary", closable = FALSE,
                        plotOutput("dynPred_plot_input")
                    )
                ),
                fluidRow(
                    boxPlus(title = "Primary model", closable = FALSE,
                        solidHeader = TRUE, status = "primary",
                        numericInput("dynPred_muopt", "mu_opt", 0.5, min = 0),
                        bsTooltip("dynPred_muopt", "The maximum specific growth rate under optimal conditions",
                                  "right", options = list(container = "body")),
                        numericInput("dynPred_logNmax", "logNmax", 8),
                        bsTooltip("dynPred_logNmax", "Logarithm of the maximum count in the stationary phase",
                                  "right", options = list(container = "body")),
                        numericInput("dynPred_logN0", "logN0", 0),
                        bsTooltip("dynPred_logN0", "Logarithm of the initial count",
                                  "right", options = list(container = "body")),
                        numericInput("dynPred_Q0", "Q0", 1e-3, min = 0),
                        bsTooltip("dynPred_Q0", "Initial value of the variable describing the lag phase; large values = no-lag",
                                  "right", options = list(container = "body")),
                        numericInput("dynPred_maxtime", "Total time", 50, min = 0),
                        bsTooltip("dynPred_maxtime", "Duration of the simulation",
                                  "right", options = list(container = "body"))

                    ),
                    boxPlus(title = "Secondary model", closable = FALSE,
                        solidHeader = TRUE, status = "primary",
                        actionButton("dynPred_update", "Update"),
                        tags$div(id = 'dynPredPlaceholder')
                        )
                ),
                
                fluidRow(
                    boxPlus(title = tagList("Prediction", 
                                            actionBttn("help_dyna_prediction_res",
                                                       label = NULL,
                                                       style = "bordered",
                                                       icon = icon("info"),
                                                       size = "xs"
                                            )
                                            ),
                            closable = FALSE,
                        solidHeader = TRUE, status = "success",
                        actionButton("dynPred_calculate", "Calculate!"),
                        tags$hr(),
                        textInput("dynPred_xlabel", "Label of x-axis", "Time"),
                        textInput("dynPred_ylabel", "Label of y-axis", "logN"),
                        colourInput("dynPred_linecol", "Line colour", "black"),
                        numericInput("dynPred_linesize", "Line size", 2, min = 0),
                        pickerInput("dynPred_linetype", "Line type",
                                    list(solid = NA, dash = 'dash', 
                                         dot = 'dot', dashdot = 'dashdot')
                                    ),
                        prettySwitch("dynPred_addFactor", "Plot a factor?",
                                     status = "success",
                                     slim = TRUE),
                        conditionalPanel(
                            condition = "input.dynPred_addFactor",
                            wellPanel(
                                textInput("dynPred_added_factor", "What factor?", "temperature"),
                                textInput("dynPred_secylabel", "Label of secondary axis", "temperature"),
                                colourInput("dynPred_linecol2", "Line colour", "red"),
                                numericInput("dynPred_linesize2", "Line size", 2, min = 0),
                                pickerInput("dynPred_linetype2", "Line type",
                                            list(solid = NA, dash = 'dash', 
                                                 dot = 'dot', dashdot = 'dashdot'),
                                            selected = "dot"
                                            )
                            )
                        ),
                        
                        prettySwitch("dynPred_add_timeToX", "Add time to log count?",
                                     status = "success",
                                     slim = TRUE),
                        conditionalPanel(
                            condition = "input.dynPred_add_timeToX",
                            wellPanel(
                                numericInput("dynPred_tgt_count", "Target log count", 2),
                                colourInput("dynPred_linecol3", "Line colour", "blue"),
                                numericInput("dynPred_linesize3", "Line size", 2, min = 0),
                                pickerInput("dynPred_linetype3", "Line type",
                                            list(solid = NA, dash = 'dash', 
                                                 dot = 'dot', dashdot = 'dashdot'),
                                            selected = "dash"
                                )
                            )
                        )
                        
                        
                    ),
                    boxPlus(status = "success", closable = FALSE,
                        tags$h3("Predicted growth"),
                        plotlyOutput("dynPred_plot_growth"),
                        downloadButton("dynPred_down_growth", "Export growth curve"),
                        tags$hr(),
                        tags$h3("Variation of the gamma factors"),
                        plotlyOutput("dynPred_gammaPlot"),
                        downloadButton("dynPred_down_gamma", "Export gamma curve")
                        )
                )
                
                ),
        
        ## Static fit ----------------------------------------------------------
        
        tabItem(tabName = "st_fit",
                fluidRow(
                    tableFileUI("pred_micro_data",
                                inputBoxTitle = "Input Microbial counts",
                                outputBoxTitle = "Microbial data"
                    )
                ),
                fluidRow(
                    box(title = tagList("Model parameters", 
                                        actionBttn("help_stat_fit_pars",
                                                   label = NULL,
                                                   style = "bordered",
                                                   icon = icon("info"),
                                                   size = "xs"
                                        )
                                        ),
                        
                        solidHeader = TRUE, status = "primary",
                        selectInput(
                            "model_static_fit",
                            "Primary growth model",
                            primary_model_data() %>% set_names(., .) %>% as.list()
                        ),
                        wellPanel(
                            uiOutput("static_fit_pars")
                        ),
                        actionButton("button_static_fit", "Fit model")
                        
                        ),
                    box(title = "Model fit",
                        solidHeader = TRUE, status = "success",
                        dropdownButton(circle = TRUE, status = "success", 
                                       icon = icon("gear"), width = "300px",
                                       textInput("static_fit_xlab", "x-axis label", "Time"),
                                       textInput("static_fit_ylab", "y-axis label", "logN"),
                                       hr(),
                                       colourInput("static_fit_linecol", "Line colour", "black"),
                                       numericInput("static_fit_linesize", "Line size", 1, min = 0),
                                       selectInput("static_fit_linetype", "Line type",
                                                   choices = list("solid", "dashed", "dotted", "dotdash",
                                                                  "longdash", "twodash"),
                                                   selected = "solid"),
                                       hr(),
                                       colourInput("static_fit_pointcol", "Point colour", "black"),
                                       numericInput("static_fit_pointsize", "Point size", 3, min = 0),
                                       numericInput("static_fit_pointtype", "Point type", 16, 
                                                    min = 0, max = 25, step = 1)
                        ),
                        br(),
                        plotlyOutput("plot_static_fit") %>% withSpinner(color = "#2492A8"),
                        )
                ),
                fluidRow(
                    box(title = "Fitted parameters", status = "warning",
                        solidHeader = TRUE, collapsible = TRUE,
                        tableOutput("static_fit_par"),
                        tags$hr(),
                        tableOutput("static_fit_residual_table")
                    ),
                    box(title = tagList("Fit diagnostics", 
                                        actionBttn("help_stat_fit_diag",
                                                   label = NULL,
                                                   style = "bordered",
                                                   icon = icon("info"),
                                                   size = "xs"
                                        )
                                        ),
                        status = "warning",
                        solidHeader = TRUE, collapsible = TRUE,
                        tags$h3("Residuals plot"),
                        plotlyOutput("static_fit_residual"),
                        tags$hr(),
                        tags$h3("Histogram of the residuals"),
                        plotOutput("static_fit_resHist"),
                        tags$hr(),
                        tags$h3("Shapiro-Wilk test of the residuals"),
                        verbatimTextOutput("static_fit_shapiro")
                        )
                    )
                ),
        
        ## Dynamic fit ----------------------------------------------
        
        tabItem(tabName = "dyna_fit",
                fluidRow(
                    tableFileUI("dynFit_micro_data",
                                inputBoxTitle = "Input Microbial counts",
                                outputBoxTitle = "Microbial data"
                    )
                ),
                fluidRow(
                    box(title = tagList("Input environmental conditions",
                                        actionBttn("help_dyna_fit",
                                                   label = NULL,
                                                   style = "bordered",
                                                   icon = icon("info"),
                                                   size = "xs"
                                        )
                                        ), 
                        solidHeader = TRUE,
                        status = "primary",
                        fileInput("dynFit_excel_file", "Excel file"),
                        textInput("dynFit_excel_sheet", "Sheet name", "Sheet1"),
                        numericInput("dynFit_excel_skip", "Skip", 0),
                        downloadLink("dynFit_download_example", "Download example"),
                        bsTooltip("dynFit_excel_sheet", 
                                  "The Excel file must contain a column named time and as many additional columns as environmental factors",
                                  "right", options = list(container = "body"))
                    ),
                    box(status = "primary",
                        plotlyOutput("dynFit_plot_input")
                    )
                ),
                fluidRow(
                    box(title = "Primary model", solidHeader = TRUE,
                        status = "primary",
                        fluidRow(
                            column(6,
                                   numericInput("dynFit_N0", "N0 (CFU/g)", 10, min = 0)
                            ),
                            column(6,
                                   awesomeCheckbox("dynFit_N0_fix", "fixed?")
                            )
                        ),
                        
                        bsTooltip("dynFit_N0",
                                  "Initial microbial count",
                                  "right", options = list(container = "body")),
                        br(),
                        fluidRow(
                            column(6,
                                   numericInput("dynFit_Q0", "Q0 (·)", 1e-3, min = 0)
                            ),
                            column(6,
                                   awesomeCheckbox("dynFit_Q0_fix", "fixed?")
                            )
                        ),
                        bsTooltip("dynFit_Q0",
                                  "Initial value of the variable describing the lag phase",
                                  "right", options = list(container = "body")),
                        br(),
                        fluidRow(
                            column(6,
                                   numericInput("dynFit_muopt", "mu_opt (log10 CFU/h)", .5, min = 0)
                            ),
                            column(6,
                                   awesomeCheckbox("dynFit_muopt_fix", "fixed?")
                            )
                        ),
                        bsTooltip("dynFit_muopt", 
                                  "Maximum specific growth rate under optimal conditions",
                                  "right", options = list(container = "body")),
                        br(),
                        fluidRow(
                            column(6,
                                   numericInput("dynFit_Nmax", "Nmax (CFU/g)", 1e8, min = 0)
                            ),
                            column(6,
                                   awesomeCheckbox("dynFit_Nmax_fix", "fixed?")
                            )
                        ),
                        bsTooltip("dynFit_Nmax", 
                                  "Maximum microbial count in the stationary phase",
                                  "right", options = list(container = "body"))
                        ),
                    box(title = "Secondary models", solidHeader = TRUE,
                        status = "primary",
                        actionButton("dynFit_update", "Update"),
                        tags$div(id = 'dynFitPlaceholder')
                        )
                ),
                fluidRow(
                    box(title = "Fitting algorithm", solidHeader = TRUE,
                        status = "primary",
                        selectInput("dynFit_algorithm", "Algorithm",
                                    list(`Non-linear regression`="nlr",
                                         `MCMC` = "MCMC")),
                        conditionalPanel(
                            condition = "input.dynFit_algorithm == 'MCMC'",
                            numericInput("dynFit_niter", "Number of iterations", 1000, 
                                         min = 0, step = 1),
                            actionButton("dynFit_seed", "Reset seed")
                        ),
                        tags$hr(),
                        actionButton("dynFit_fitModel", "Fit model")
                        ),
                    box(title = "Fitted model", solidHeader = TRUE,
                        status = "success",
                        plotlyOutput("dynFit_modelPlot") %>% withSpinner(color = "#2492A8"),
                        tags$hr(),
                        
                        textInput("dynFit_xlabel", "Label of x-axis", "Time"),
                        textInput("dynFit_ylabel", "Label of y-axis", "logN"),
                        colourInput("dynFit_linecol", "Line colour", "black"),
                        numericInput("dynFit_linesize", "Line size", 2, min = 0),
                        pickerInput("dynFit_linetype", "Line type",
                                    list(solid = NA, dash = 'dash',
                                         dot = 'dot', dashdot = 'dashdot')
                        ),
                        colourInput("dynFit_pointcol", "Point colour", "maroon"),
                        numericInput("dynFit_pointsize", "Point size", 10, min = 0),
                        hr(),
                        prettySwitch("dynFit_addFactor", "Plot a factor?",
                                     status = "success",
                                     slim = TRUE),
                        conditionalPanel(
                            condition = "input.dynFit_addFactor",
                            wellPanel(
                                textInput("dynFit_added_factor", "What factor?", "temperature"),
                                textInput("dynFit_secylabel", "Label of secondary axis", "temperature"),
                                colourInput("dynFit_linecol2", "Line colour", "red"),
                                numericInput("dynFit_linesize2", "Line size", 2, min = 0),
                                pickerInput("dynFit_linetype2", "Line type",
                                            list(solid = NA, dash = 'dash',
                                                 dot = 'dot', dashdot = 'dashdot'),
                                            selected = "dot"
                                )
                            )
                        )
                        )
                ),
                fluidRow(
                    box(title = "Parameter estimates", solidHeader = TRUE,
                        status = "warning",
                        reactableOutput("dynFit_par_summary"),
                        tags$hr(),
                        tableOutput("dynFit_residualTable")
                        ),
                    box(title = tagList("Fitting diagnostics", 
                                        actionBttn("help_dyna_fit_diag",
                                                   label = NULL,
                                                   style = "bordered",
                                                   icon = icon("info"),
                                                   size = "xs"
                                        )
                                        ),
                        status = "warning",
                        solidHeader = TRUE,
                        tags$h3("Residual plot"),
                        plotOutput("dynFit_resPlot"),
                        tags$hr(),
                        tags$h3("Histogram of the residuals"),
                        plotOutput("dynFit_resHist"),
                        tags$h3("Shapiro-Wilk test of the residuals"),
                        verbatimTextOutput("dynFit_shapiro"),
                        conditionalPanel(
                            condition = "input.dynFit_algorithm == 'MCMC'",
                            tags$h3("Convergence of the Markov chain"),
                            plotOutput("dynFit_MCMC_chain"),
                            tags$h3("Pairs plot"),
                            plotOutput("dynFit_MCMC_pairs")
                        )
                        )
                )
                ),
        
        ## Cardinal fit ------------------------------------------------
        
        tabItem(tabName = "cardinal",
                fluidRow(
                    box(title = "Data input", solidHeader = TRUE,
                        status = "primary",
                        fileInput("card_excel_file", "Excel file"),
                        textInput("card_excel_sheet", "Sheet name", "Sheet1"),
                        numericInput("card_excel_skip", "Skip", 0),
                        tags$hr(),
                        downloadLink("card_download_example", "Download example"),
                        bsTooltip("card_excel_sheet", 
                                  "The excel file must have one column named mu and as many additional columns as environmental factors",
                                  "right", options = list(container = "body"))
                    ),
                    box(
                        plotOutput("card_plot_input")
                    )
                ),
                fluidRow(
                    box(width = 12, title = "Model definition", 
                        solidHeader = TRUE, status = "primary",
                        actionButton("card_update", "Update"),
                        tags$hr(),
                        fluidRow(
                            column(6,
                                   numericInput("card_muopt", "mu_opt", 0.5)
                                   ),
                            column(6,
                                   checkboxInput("card_muopt_fix", "fixed?")
                                   )
                            
                        ),
                        tags$div(id = 'cardPlaceholder') ,
                        selectInput("card_transformation", "Transformation",
                                    list(`Square root` = "sq", 
                                         `Log transform` = "log", 
                                         `No tranformation` = "none"),
                                    width = "50%"
                        ),
                        bsTooltip("card_transformation", 
                                  paste("By default, the SSE of the square root of the growth rate is minimized. That can be changed here.",
                                        "Be mindful of the distribution of the residuals"),
                                  "right", options = list(container = "body")),
                        tags$hr(),
                        actionButton("card_fitModel", "Fit model")
                    )
                    
                ),
                fluidRow(
                    box(title = "Parameter estimates", status = "warning",
                        solidHeader = TRUE,
                        tableOutput("card_fit_results") %>% withSpinner(color = "#2492A8"),
                        tags$hr(),
                        tableOutput("card_residual_table")
                    ),
                    box(title = "Residuals diagnostics", solidHeader = TRUE,
                        status = "warning",
                        tags$h3("Residuals plot"),
                        plotOutput("card_res_plot"),
                        tags$hr(),
                        tags$h3("Histogram of the residuals"),
                        plotOutput("card_res_hist"),
                        tags$hr(),
                        tags$h3("Shapiro-Wilk test of residuals"),
                        verbatimTextOutput("card_shapiro")
                        )
                )
                ),
        
        ## Global fitting ------------------------------------------------------
        
        tabItem(tabName = "global_fit",
                fluidRow(
                    box(title = "Input microbial counts", solidHeader = TRUE,
                        status = "primary",
                        fileInput("globalFit_excel_file_count", "Excel file"),
                        downloadLink("globalFit_download_example_count", "Download example")
                    ),
                    box(status = "primary",
                        plotlyOutput("globalFit_plot_input_count")
                    )
                ),
                fluidRow(
                    box(title = "Input environmental conditions", solidHeader = TRUE,
                        status = "primary",
                        fileInput("globalFit_excel_file_env", "Excel file"),
                        downloadLink("globalFit_download_example_env", "Download example")
                    ),
                    box(status = "primary",
                        plotlyOutput("globalFit_plot_input_env")
                    )
                ),
                fluidRow(
                    box(title = "Primary model", solidHeader = TRUE,
                        status = "primary",
                        numericInput("globalFit_N0", "N0 (CFU/g)", 10, min = 0, width = "30%"),
                        awesomeCheckbox("globalFit_N0_fix", "fixed?", width = "30%"),
                        bsTooltip("globalFit_N0", 
                                  "Initial microbial count",
                                  "right", options = list(container = "body")),
                        tags$hr(),
                        numericInput("globalFit_Q0", "Q0 (·)", 1e-3, min = 0, width = "30%"),
                        awesomeCheckbox("globalFit_Q0_fix", "fixed?", width = "30%"),
                        bsTooltip("globalFit_Q0", 
                                  "Initial value of the variable describing the lag phase",
                                  "right", options = list(container = "body")),
                        tags$hr(),
                        numericInput("globalFit_muopt", "mu_opt (log CFU/h)", .5, min = 0, width = "30%"),
                        awesomeCheckbox("globalFit_muopt_fix", "fixed?", width = "30%"),
                        bsTooltip("globalFit_muopt", 
                                  "Maximum specific growth rate under optimal conditions",
                                  "right", options = list(container = "body")),
                        tags$hr(),
                        numericInput("globalFit_Nmax", "Nmax (CFU/g)", 1e8, min = 0, width = "30%"),
                        awesomeCheckbox("globalFit_Nmax_fix", "fixed?", width = "30%"),
                        bsTooltip("globalFit_Nmax", 
                                  "Maximum microbial count in the stationary phase",
                                  "right", options = list(container = "body"))
                    ),
                    box(title = "Secondary models", solidHeader = TRUE,
                        status = "primary",
                        actionButton("globalFit_update", "Update"),
                        tags$div(id = 'globalFitPlaceholder')
                    )
                ),
                fluidRow(
                    box(title = "Fitting algorithm", solidHeader = TRUE,
                        status = "primary",
                        selectInput("globalFit_algorithm", "Algorithm",
                                    list(`Non-linear regression`="nlr",
                                         `MCMC` = "MCMC")),
                        conditionalPanel(
                            condition = "input.globalFit_algorithm == 'MCMC'",
                            numericInput("globalFit_niter", "Number of iterations", 1000, 
                                         min = 0, step = 1),
                            actionButton("globalFit_seed", "Reset seed")
                        ),
                        tags$hr(),
                        actionButton("globalFit_fitModel", "Fit model")
                    ),
                    box(title = "Fitted model", solidHeader = TRUE,
                        status = "success",
                        plotOutput("globalFit_modelPlot") %>% withSpinner(color = "#2492A8"),
                        tags$hr(),
                        checkboxInput("globalFit_addFactor", "Plot a factor?"),
                        textInput("globalFit_added_factor", "What factor?", "temperature"),
                        textInput("globalFit_xlabel", "Label of x-axis", "Time"),
                        textInput("globalFit_ylabel", "Label of y-axis", "logN"),
                        textInput("globalFit_secylabel", "Label of secondary axis", "temperature")
                    )
                ),
                fluidRow(
                    box(title = "Parameter estimates", solidHeader = TRUE,
                        status = "warning",
                        reactableOutput("globalFit_par_summary"),
                        tags$hr(),
                        tableOutput("globalFit_residualTable")
                    ),
                    box(title = "Fitting diagnostics", status = "warning",
                        solidHeader = TRUE,
                        tags$h3("Residual plot"),
                        plotOutput("globalFit_resPlot"),
                        tags$hr(),
                        tags$h3("Histogram of the residuals"),
                        plotOutput("globalFit_resHist"),
                        checkboxInput("globalFit_separate_hist", "Separate by experiment?"),
                        tags$h3("Shapiro-Wilk test of the residuals"),
                        verbatimTextOutput("globalFit_shapiro"),
                        conditionalPanel(
                            condition = "input.globalFit_algorithm == 'MCMC'",
                            tags$h3("Convergence of the Markov chain"),
                            plotOutput("globalFit_MCMC_chain"),
                            tags$h3("Pairs plot"),
                            plotOutput("globalFit_MCMC_pairs")
                        )
                    )
                )
                )
    )
    
)










