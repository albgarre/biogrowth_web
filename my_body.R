


body <- dashboardBody(
    tabItems(
        tabItem(tabName = "welcome",
                h2("Welcome tab")
                ),
        ###########################################
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
        
        ################################################
        
        tabItem(tabName = "dyna_prediction",
                h2("Predictions under dynamic conditions")
                ),
        
        ################################################
        
        tabItem(tabName = "st_fit",
                h2("Model fitting under static conditions")
                ),
        
        ################################################
        
        tabItem(tabName = "dyna_fit",
                h2("Model fitting under dynamic conditions")
                ),
        
        ###############################################
        
        tabItem(tabName = "cardinal",
                h2("Model fitting of cardinal parameters")
                )
    )
    
)
