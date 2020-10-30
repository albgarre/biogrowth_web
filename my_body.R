


body <- dashboardBody(
    tabItems(
        tabItem(tabName = "welcome",
                # h2("Welcome tab")
                box(width = 12,
                    withMathJax(includeMarkdown("welcome_page.md"))
                    )
                ),
        
        ## Static prediction -----------------------------------------------------------------
        
        tabItem(tabName = "st_prediction",
                fluidRow(
                    box(title = "Model definition", status = "primary",
                        solidHeader = TRUE, collapsible = FALSE,
                        selectInput(
                            "modelStaticPrediction",
                            "Primary growth model",
                            list(Baranyi = "Baranyi", 
                                 `Modified Gompertz` = "modGompertz",
                                 `Tri-linear` = "Trilinear")
                        ),
                        wellPanel(
                            numericInput("static_pred_logN0", "logN0", 0),
                            bsTooltip("static_pred_logN0", "The logarithm of the initial microbial count",
                                      "right", options = list(container = "body")),
                            numericInput("static_pred_mu", "mu", 0.2, min = 0),
                            bsTooltip("static_pred_mu", "The maximum specific growth rate",
                                      "right", options = list(container = "body")),
                            numericInput("static_pred_lambda", "lambda", 20, min = 0),
                            bsTooltip("static_pred_lambda", "The duration of the lag phase",
                                      "right", options = list(container = "body")),
                            conditionalPanel(
                                condition = "input.modelStaticPrediction != 'modGompertz'",
                                numericInput("static_pred_logNmax", "logNmax", 8),
                                bsTooltip("static_pred_logNmax", "The logarithm of the maximum count in the stationary phase",
                                          "right", options = list(container = "body"))
                            ),
                            conditionalPanel(
                                condition = "input.modelStaticPrediction == 'modGompertz'",
                                numericInput("static_pred_C", "C", 6),
                                bsTooltip("static_pred_C", "The difference between the log initial concentration and the log maximum concentration",
                                          "right", options = list(container = "body"))
                            ),
                            numericInput("static_max_time", "Maximum time", 80, min = 0),
                            bsTooltip("static_max_time", "Duration of the simulation",
                                      "right", options = list(container = "body"))
                        ),
                        textInput("static_pred_simName", "Simulation name", "Growth condition I"),
                        bsTooltip("static_pred_simName", paste("The growth curves are coloured according to this name.",
                                                               "If it already exists, replaces the curve"),
                                  "right", options = list(container = "body")),
                        actionButton("static_pred_addSim", "Add/Edit Simulation"),
                        actionButton("static_pred_cleanUp", "Clean plot")
                        
                        
                    ),
                    box(title = "Model predictions", status = "success",
                        solidHeader = TRUE,
                        plotOutput("plot_static_prediction"),
                        # bsTooltip("plot_static_prediction", "Growth curves under static conditions",
                        #           paste("This plot shows the predicted growth curves for the models defined.",
                        #                 "The lines are coloured according to the names you have defined.",
                        #                 "To edit any line, you just have to redefine model parameters and use the same name.",
                        #                 "Note that the validity of the predictions depends on the validity of the model parameters.",
                        #                 "Also, these predictions (in principle) are only valid under static environmental conditions.",
                        #                 sep = "\n"),
                        #           trigger = "click"
                        #           ),
                        tags$hr(),
                        column(6,
                               textInput("static_xaxis", "Label of x-axis", "Storage time")
                               # checkboxInput("static_time_to_count", "Include time to log count")
                               ),
                        column(6,
                               textInput("static_yaxis", "Label of y-axis", "Log microbial count")
                               
                               ),
                        tags$hr(),
                        downloadButton("static_export", "Export simulations")

                    )
                ),
                fluidRow(
                    box(title = "Time to X log count", status = "success",
                        solidHeader = TRUE,
                        numericInput("static_tgt_count", "Target count", 2),
                        tableOutput("static_timeToTable")
                        )
                )
                ),
        
        ## Stochastic static prediction
        
        tabItem(
            tabName = "stoc_prediction",
            fluidRow(
                box(title = "Model definition", status = "primary",
                    solidHeader = TRUE,
                    selectInput(
                        "modelStaticPredictionStoc",
                        "Primary growth model",
                        list(Baranyi = "Baranyi", 
                             `Modified Gompertz` = "modGompertz",
                             `Tri-linear` = "Trilinear")
                    ),
                    wellPanel(
                        fluidRow(
                            h4("log N0"),
                            column(6,
                                   numericInput("static_pred_mlogN0", "Mean", 0)
                            ),
                            column(6,
                                   numericInput("static_pred_sdlogN0", "SD", .5, min = 0)
                            ),
                            bsTooltip("static_pred_mlogN0", "Mean and standard deviation of a log-normally distributed initial count")
                        ),
                        fluidRow(
                            h4("sqrt(mu)"),
                            column(6,
                                   numericInput("static_pred_mmu", "Mean", 1)
                            ),
                            column(6,
                                   numericInput("static_pred_sdmu", "SD", .1, min = 0)
                            ),
                            bsTooltip("static_pred_mmu", 
                                      "Mean and standard deviation of a maximum growth rate whose square root follows a normal distribution")
                        ),
                        fluidRow(
                            h4("sqrt(lambda)"),
                            column(6,
                                   numericInput("static_pred_mlambda", "Mean", 5)
                            ),
                            column(6,
                                   numericInput("static_pred_sdlambda", "SD", 1, min = 0)
                            ),
                            bsTooltip("static_pred_mlambda", 
                                      "Mean and standard deviation of a lag phase duration whose square root follows a normal distribution")
                        ),
                        fluidRow(
                            h4("log Nmax"),
                            column(6,
                                   numericInput("static_pred_mlogNmax", "Mean", 6)
                            ),
                            column(6,
                                   numericInput("static_pred_sdlogNmax", "SD", .5, min = 0)
                            ),
                            bsTooltip("static_pred_mlogNmax", "Mean and standard deviation of a log-normally distributed maximum count")
                        ),
                        numericInput("max_time_stoc_static", "Maximum time", value = 80, min = 0),
                        bsTooltip("max_time_stoc_static", "Maximum time for the simulations"),
                        numericInput("n_sims_static", "Number of simulations", value = 1000, min = 0),
                        bsTooltip("n_sims_static", "Number of Monte Carlo simulations to estimate the distribution of the microbial count.")
                    ),
                    actionButton("stoc_calculate", "Calculate")
                ),
                box(title = "Stochastic predictions", status = "success",
                    solidHeader = TRUE,
                    tags$h3("Stochastic growth curve"),
                    plotOutput("plot_static_prediction_stoc"),
                    column(6,
                           textInput("static_xaxis_stoc", "Label of x-axis", "Storage time")
                    ),
                    column(6,
                           textInput("static_yaxis_stoc", "Label of y-axis", "Log microbial count")
                    ),
                    downloadButton("static_stoc_down", label = "Export quantiles"),
                    tags$hr(),
                    tags$h3("Time to a microbial count"),
                    numericInput("tgt_cont_stoc_static", "Target log microbial count", value = 4,
                                 width = "50%"),
                    column(12, plotOutput("plot_static_timedistrib")),
                    column(12, tableOutput("table_static_timedistrib"))
                )
            )
        ),
        
        ## Dynamic prediction --------------------------------------------------------------
        
        tabItem(tabName = "dyna_prediction",
                fluidRow(
                    box(title = "Data input", solidHeader = TRUE,
                        status = "primary",
                        fileInput("dynPred_excel_file", "Excel file"),
                        textInput("dynPred_excel_sheet", "Sheet name", "Sheet1"),
                        numericInput("dynPred_excel_skip", "Skip", 0),
                        tags$hr(),
                        downloadLink("dynPred_download_example", "Download example")
                    ),
                    box(status = "primary",
                        plotOutput("dynPred_plot_input")
                    )
                ),
                fluidRow(
                    box(title = "Primary model",
                        solidHeader = TRUE, status = "primary",
                        numericInput("dynPred_muopt", "mu_opt", 0.5, min = 0),
                        numericInput("dynPred_logNmax", "logNmax", 8),
                        numericInput("dynPred_logN0", "logN0", 0),
                        numericInput("dynPred_Q0", "Q0", 1e-3, min = 0),
                        numericInput("dynPred_maxtime", "Total time", 50, min = 0)

                    ),
                    box(title = "Secondary model",
                        solidHeader = TRUE, status = "primary",
                        actionButton("dynPred_update", "Update"),
                        tags$div(id = 'dynPredPlaceholder')
                        )
                ),
                
                fluidRow(
                    box(title = "Prediction",
                        solidHeader = TRUE, status = "success",
                        actionButton("dynPred_calculate", "Calculate!"),
                        tags$hr(),
                        checkboxInput("dynPred_addFactor", "Plot a factor?"),
                        textInput("dynPred_added_factor", "What factor?", "temperature"),
                        textInput("dynPred_xlabel", "Label of x-axis", "Time"),
                        textInput("dynPred_ylabel", "Label of y-axis", "logN"),
                        textInput("dynPred_secylabel", "Label of secondary axis", "temperature"),
                        checkboxInput("dynPred_add_timeToX", "Add time to log count?"),
                        numericInput("dynPred_tgt_count", "Target log count", 2)
                        
                    ),
                    box(status = "success",
                        tags$h3("Predicted growth"),
                        plotOutput("dynPred_plot_growth"),
                        downloadButton("dynPred_down_growth", "Export growth curve"),
                        tags$hr(),
                        tags$h3("Variation of the gamma factors"),
                        plotOutput("dynPred_gammaPlot"),
                        downloadButton("dynPred_down_gamma", "Export gamma curve")
                        )
                )
                
                ),
        
        ## Static fit
        
        tabItem(tabName = "st_fit",
                fluidRow(
                    tableFileUI("pred_micro_data",
                                inputBoxTitle = "Input Microbial counts",
                                outputBoxTitle = "Microbial data"
                    )
                ),
                fluidRow(
                    box(title = "Model parameters",
                        solidHeader = TRUE, status = "primary",
                        selectInput(
                            "model_static_fit",
                            "Primary growth model",
                            list(Baranyi = "Baranyi", 
                                 `Modified Gompertz` = "modGompertz",
                                 `Tri-linear` = "Trilinear")
                        ),
                        wellPanel(
                            fluidRow(
                                column(6, numericInput("static_fit_logN0", "logN0", 2)),                            
                                column(2, checkboxInput("static_fit_logN0_fix", "fixed?", value = FALSE))
                            ),
                            fluidRow(
                                column(6, numericInput("static_fit_mu", "mu", .2)),                            
                                column(2, checkboxInput("static_fit_mu_fix", "fixed?", value = FALSE))
                            ),
                            fluidRow(
                                column(6, numericInput("static_fit_lambda", "lambda", 25)),                            
                                column(2, checkboxInput("static_fit_lambda_fix", "fixed?", value = FALSE))
                            ),
                            fluidRow(
                                conditionalPanel(
                                    condition = "input.model_static_fit != 'modGompertz'",
                                    column(6, numericInput("static_fit_logNmax", "logNmax", 8)),                            
                                    column(2, checkboxInput("static_fit_logNmax_fix", "fixed?", value = FALSE))
                                )
                            ),
                            fluidRow(
                                conditionalPanel(
                                    condition = "input.model_static_fit == 'modGompertz'",
                                    column(6, numericInput("static_fit_C", "C", 6)),                            
                                    column(2, checkboxInput("static_fit_C_fix", "fixed?", value = FALSE))
                                )
                            ),
                            fluidRow(
                                actionButton("button_static_fit", "Fit model")
                            )
                        )
                        
                        ),
                    box(title = "Model fit",
                        solidHeader = TRUE, status = "success",
                        plotOutput("plot_static_fit"),
                        tags$hr(),
                        textInput("static_fit_xlab", "x-axis label", "Time"),
                        textInput("static_fit_ylab", "y-axis label", "logN")
                        )
                ),
                fluidRow(
                    box(title = "Fitted parameters", status = "warning",
                        solidHeader = TRUE, collapsible = TRUE,
                        tableOutput("static_fit_par"),
                        tags$hr(),
                        tableOutput("static_fit_residual_table")
                    ),
                    box(title = "Fit diagnostics", status = "warning",
                        solidHeader = TRUE, collapsible = TRUE,
                        tags$h3("Residual plot"),
                        plotOutput("static_fit_residual"),
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
                    box(title = "Input environmental conditions", solidHeader = TRUE,
                        status = "primary",
                        fileInput("dynFit_excel_file", "Excel file"),
                        textInput("dynFit_excel_sheet", "Sheet name", "Sheet1"),
                        numericInput("dynFit_excel_skip", "Skip", 0),
                        downloadLink("dynFit_download_example", "Download example")
                    ),
                    box(status = "primary",
                        plotOutput("dynFit_plot_input")
                    )
                ),
                fluidRow(
                    box(title = "Primary model", solidHeader = TRUE,
                        status = "primary",
                        numericInput("dynFit_N0", "N0", 10, min = 0, width = "30%"),
                        checkboxInput("dynFit_N0_fix", "known?", width = "30%"),
                        tags$hr(),
                        numericInput("dynFit_Q0", "Q0", 1e-3, min = 0, width = "30%"),
                        checkboxInput("dynFit_Q0_fix", "known?", width = "30%"),
                        tags$hr(),
                        numericInput("dynFit_muopt", "mu_opt", .5, min = 0, width = "30%"),
                        checkboxInput("dynFit_muopt_fix", "known?", width = "30%"),
                        tags$hr(),
                        numericInput("dynFit_Nmax", "Nmax", 1e8, min = 0, width = "30%"),
                        checkboxInput("dynFit_Nmax_fix", "known?", width = "30%")
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
                        plotOutput("dynFit_modelPlot"),
                        tags$hr(),
                        checkboxInput("dynFit_addFactor", "Plot a factor?"),
                        textInput("dynFit_added_factor", "What factor?", "temperature"),
                        textInput("dynFit_xlabel", "Label of x-axis", "Time"),
                        textInput("dynFit_ylabel", "Label of y-axis", "logN"),
                        textInput("dynFit_secylabel", "Label of secondary axis", "temperature")
                        )
                ),
                fluidRow(
                    box(title = "Parameter estimates", solidHeader = TRUE,
                        status = "warning",
                        tableOutput("dynFit_par_summary"),
                        tags$hr(),
                        tableOutput("dynFit_residualTable")
                        ),
                    box(title = "Fitting diagnostics", status = "warning",
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
                        downloadLink("card_download_example", "Download example")
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
                        tags$hr(),
                        actionButton("card_fitModel", "Fit model")
                    )
                    
                ),
                fluidRow(
                    box(title = "Parameter estimates", status = "warning",
                        solidHeader = TRUE,
                        tableOutput("card_fit_results"),
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
                )
    )
    
)










