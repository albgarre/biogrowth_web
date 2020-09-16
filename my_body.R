


body <- dashboardBody(
    tabItems(
        tabItem(tabName = "welcome",
                h2("Welcome tab")
                ),
        
        ## Static prediction
        
        tabItem(tabName = "st_prediction",
                h2("Deterministic prediction"),
                fluidRow(
                    box(
                        selectInput(
                            "modelStaticPrediction",
                            "Primary growth model",
                            list(Baranyi = "Baranyi", 
                                 `Modified Gompertz` = "modGompertz",
                                 `Tri-linear` = "Trilinear")
                        ),
                        wellPanel(
                            numericInput("static_pred_logN0", "logN0", 0),
                            numericInput("static_pred_mu", "mu", 0.2, min = 0),
                            numericInput("static_pred_lambda", "lambda", 20, min = 0),
                            conditionalPanel(
                                condition = "input.modelStaticPrediction != 'modGompertz'",
                                numericInput("static_pred_logNmax", "logNmax", 8)
                            ),
                            conditionalPanel(
                                condition = "input.modelStaticPrediction == 'modGompertz'",
                                numericInput("static_pred_C", "C", 6)
                            ),
                            numericInput("static_max_time", "Maximum time", 80, min = 0)
                        )
                        
                        
                    ),
                    box(
                        plotOutput("plot_static_prediction"),
                        tags$hr(),
                        column(6,
                               textInput("static_xaxis", "Label of x-axis", "Storage time"),
                               checkboxInput("static_time_to_count", "Include time to log count")
                               ),
                        column(6,
                               textInput("static_yaxis", "Label of y-axis", "Log microbial count"),
                               numericInput("static_tgt_count", "Target count", 2)
                               )

                    )
                ),
                h2("Stochastic prediction"),
                fluidRow(
                    box(
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
                                )
                            ),
                            fluidRow(
                                h4("sqrt(mu)"),
                                column(6,
                                       numericInput("static_pred_mmu", "Mean", 1)
                                ),
                                column(6,
                                       numericInput("static_pred_sdmu", "SD", .1, min = 0)
                                )
                            ),
                            fluidRow(
                                h4("sqrt(lambda)"),
                                column(6,
                                       numericInput("static_pred_mlambda", "Mean", 5)
                                ),
                                column(6,
                                       numericInput("static_pred_sdlambda", "SD", 1, min = 0)
                                )
                            ),
                            fluidRow(
                                h4("log Nmax"),
                                column(6,
                                       numericInput("static_pred_mlogNmax", "Mean", 6)
                                ),
                                column(6,
                                       numericInput("static_pred_sdlogNmax", "SD", .5, min = 0)
                                )
                            ),
                            numericInput("max_time_stoc_static", "Maximum time", value = 80, min = 0),
                            numericInput("tgt_cont_stoc_static", "Target log microbial count", value = 4),
                            numericInput("n_sims_static", "Number of simulations", value = 1000, min = 0)
                        )
                    ),
                    box(
                        plotOutput("plot_static_prediction_stoc"),
                        column(6,
                               textInput("static_xaxis_stoc", "Label of x-axis", "Storage time")
                        ),
                        column(6,
                               textInput("static_yaxis_stoc", "Label of y-axis", "Log microbial count")
                        ),
                        tags$hr(),
                        column(12, plotOutput("plot_static_timedistrib")),
                        column(12, tableOutput("table_static_timedistrib"))
                    )
                )
                ),
        
        ## Dynamic prediction
        
        tabItem(tabName = "dyna_prediction",
                h2("Predictions under dynamic conditions")
                ),
        
        ## Static fit
        
        tabItem(tabName = "st_fit",
                fluidRow(
                    tableFileUI("pred_micro_data",
                                inputBoxTitle = "Input Microbial counts",
                                outputBoxTitle = "Microbial data",
                                default_frame = data.frame(c(0, 5, 7.5, 2.5, 6, 8),
                                                           c(1e6, 1e5, 15000, 800000, 30000, 1e3)
                                )
                    )
                ),
                fluidRow(
                    box(title = "Model parameters",
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
                        plotOutput("plot_static_fit"))
                ),
                fluidRow(
                    box(title = "Fitted parameters", status = "warning",
                        solidHeader = TRUE, collapsible = TRUE,
                        tableOutput("static_fit_par")
                    ),
                    box(title = "Fit diagnostics", status = "warning",
                        solidHeader = TRUE, collapsible = TRUE,
                        plotOutput("static_fit_residual")
                        )
                    )
                ),
        
        ## Dynamic fit
        
        tabItem(tabName = "dyna_fit",
                h2("Model fitting under dynamic conditions")
                ),
        
        ## Cardinal fit
        
        tabItem(tabName = "cardinal",
                fluidRow(
                    box(
                        fileInput("card_excel_file", "Excel file"),
                        textInput("card_excel_sheet", "Sheet name", "Sheet1"),
                        numericInput("card_excel_skip", "Skip", 0)
                    ),
                    box(
                        plotOutput("card_plot_input")
                    )
                ),
                fluidRow(
                    box(width = 12,
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
                        actionButton("card_fitModel", "Fit model")
                    )
                    
                ),
                fluidRow(
                    box(
                        tableOutput("card_fit_results")
                    )
                )
                )
    )
    
)










