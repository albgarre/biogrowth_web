
library(tidyverse)
library(biogrowth)
library(readxl)

source("tableFileUI.R")
source("tableFile.R")

server <- function(input, output) {
    
    ## Static predictions -----------------------------------------------------
    
    static_prediction <- reactive({
        
        predict_isothermal_growth(input$modelStaticPrediction, 
                                  seq(0, input$static_max_time, length = 1000), 
                                  list(logN0 = input$static_pred_logN0,
                                       mu = input$static_pred_mu,
                                       lambda = input$static_pred_lambda,
                                       logNmax = input$static_pred_logNmax,
                                       C = input$static_pred_C)
                                  )
        
    })
    
    output$plot_static_prediction <- renderPlot({
        
        p <- plot(static_prediction()) +
            xlab(input$static_xaxis) +
            ylab(input$static_yaxis)
        
        
        if (input$static_time_to_count) {
            
            my_time <- time_to_logcount(static_prediction(), input$static_tgt_count)
            
            p <- p + geom_vline(xintercept = my_time, linetype = 2) +
                geom_label(x = my_time, y = input$static_pred_logN0,
                           label = round(my_time, 2))
        }
        
        p
        
    })
    
    ## Stochastic static predictions -------------------------------------------------
    
    static_prediction_stoc <- reactive({
        
        predict_stochastic_growth(input$modelStaticPredictionStoc, 
                                  seq(0, input$max_time_stoc_static, length = 100), 
                                  input$n_sims_static,
                                  mean_logN0 = input$static_pred_mlogN0, 
                                  sd_logN0 = input$static_pred_sdlogN0,
                                  mean_sqmu = input$static_pred_mmu,
                                  sd_sqmu = input$static_pred_sdmu,
                                  mean_sqlambda = input$static_pred_mlambda, 
                                  sd_sqlambda = input$static_pred_sdlambda,
                                  mean_logNmax = input$static_pred_mlogNmax,
                                  sd_logNmax = input$static_pred_sdlogNmax)
        
    })
    
    
    static_time_distrib <- reactive({
        
        my_dist <- distribution_to_logcount(static_prediction_stoc(), input$tgt_cont_stoc_static)
        
    })
    
    
    output$plot_static_prediction_stoc <- renderPlot(
        
        plot(static_prediction_stoc()) + xlab(input$static_xaxis_stoc) + ylab(input$static_yaxis_stoc)
        
    )
    
    output$plot_static_timedistrib <- renderPlot({
        
        plot(static_time_distrib())
        
    })
    
    output$table_static_timedistrib <- renderTable({
        
        static_time_distrib()$summary
    })
    
    ## Static fitting -----------------------------------------------------
    
    pred_micro_data <- callModule(tableFile, "pred_micro_data",
                                  default_data = tibble(time = c(0, 25, 50, 75, 100),
                                                        logN = c(2, 2.5, 7, 8, 8))
                                  )
    
    static_fit_results <- eventReactive(input$button_static_fit, {
        
        my_model <- input$model_static_fit
        my_data <- as_tibble(pred_micro_data())
        
        start <- list()
        known <- list()
        
        if (isTRUE(input$static_fit_logN0_fix)) {
            known$logN0 <- input$static_fit_logN0
        } else {
            start$logN0 <- input$static_fit_logN0
        }
        
        if (isTRUE(input$static_fit_mu_fix)) {
            known$mu <- input$static_fit_mu
        } else {
            start$mu <- input$static_fit_mu
        }
        
        if (isTRUE(input$static_fit_lambda_fix)) {
            known$lambda <- input$static_fit_lambda
        } else {
            start$lambda <- input$static_fit_lambda
        }
        
        if (isTRUE(input$static_fit_logNmax_fix)) {
            known$logNmax <- input$static_fit_logNmax
        } else {
            start$logNmax <- input$static_fit_logNmax
        }
        
        if (isTRUE(input$static_fit_C_fix)) {
            known$C <- input$static_fit_C
        } else {
            start$C <- input$static_fit_C
        }
        
        if (my_model == "modGompertz") {  # To avoid singularities
            start$logNmax <- NULL
            known$logNmax <- NULL
        } else {
            start$C <- NULL
            known$C <- NULL
        }

        static_fit <- fit_isothermal_growth(my_data, my_model,
                                            unlist(start),
                                            unlist(known)
                                            )
        
        
    })
    
    output$plot_static_fit <- renderPlot({
        static_fit_results() %>% plot()
    })
    
    output$static_fit_par <- renderTable({
        
        summary(static_fit_results())$par %>%
            as_tibble(rownames = "Parameter") %>%
            select(Parameter, Estimate, `Std. Error`)
        
    })
    
    output$static_fit_residual <- renderPlot({
        
        tibble(res = static_fit_results()$fit$residuals,
               time = static_fit_results()$data$time) %>%
            ggplot(aes(x = time, y = res)) +
            geom_point() +
            geom_hline(yintercept = 0, linetype = 2) +
            geom_smooth(se=FALSE) +
            xlab("Time") + ylab("Residual")
        
    })
    
    ## Cardinal fitting ------------------------------------------------
    
    ## Data input
    
    card_excelFile <- reactive({
        validate(need(input$card_excel_file, label = "Excel"))
        input$card_excel_file
    })
    
    card_excel_frame <- reactive({
        read_excel(card_excelFile()$datapath,
                   sheet = input$card_excel_sheet,
                   skip = input$card_excel_skip,
                   col_types = "numeric")
    })
    
    output$card_plot_input <- renderPlot({
        
        card_excel_frame() %>%
            gather(var, value, -mu) %>%
            ggplot() +
                geom_point(aes(x = value, y = mu)) +
                facet_wrap("var", scales = "free_x") +
                xlab("")
        
    })
    
    ## Dynamic secondary model selector
    
    card_id_dynamic <- c() # so I can remove them later
    
    observeEvent(input$card_update, {
        
        my_names <- card_excel_frame() %>%
            select(-mu) %>%
            names()
        
        # Remove old ones
        
        if (length(card_id_dynamic) > 0) {
            
            for (each_id in card_id_dynamic) {
                removeUI(
                    ## pass in appropriate div id
                    selector = paste0('#', each_id)
                )
            }
            
            card_id_dynamic <<- c()
            
        }
        
        # Insert new ones
        
        for (each_name in my_names) {
            
            id <- paste0('card_', each_name)
            
            insertUI(
                selector = '#cardPlaceholder',
                ui = tags$div(
                    tags$h3(paste("Condition:", each_name)), 
                    selectInput(paste0(id, "_model"),
                                "Model type",
                                list(`Cardinal` = "CPM", Zwietering = "Zwietering")),
                    fluidRow(
                        column(6,
                               numericInput(paste0(id, "_xmin"), "Xmin", 0)
                               ),
                        column(6,
                               checkboxInput(paste0(id, "_xmin_fix"), "fixed?")
                               )
                    ),
                    fluidRow(
                        column(6,
                               numericInput(paste0(id, "_xopt"), "Xopt", 37)
                        ),
                        column(6,
                               checkboxInput(paste0(id, "_xopt_fix"), "fixed?")
                        )
                    ),
                    fluidRow(
                        column(6,
                               numericInput(paste0(id, "_xmax"), "Xmax (only in CMP model)", 45)
                        ),
                        column(6,
                               checkboxInput(paste0(id, "_xmax_fix"), "fixed?")
                        )
                    ),
                    # conditionalPanel(
                    #     condition = sprintf("input.%s == 'CMP'", paste0(id, "_model")),
                    #     numericInput(paste0(id, "_xmax"), "Xmax", 45),
                    # ),
                    fluidRow(
                        column(6,
                               numericInput(paste0(id, "_n"), "n", 1)
                        ),
                        column(6,
                               checkboxInput(paste0(id, "_n_fix"), "fixed?")
                        )
                    ),
                    tags$hr(),
                    id = id
                )
            )
            
            card_id_dynamic <<- c(card_id_dynamic, id)
        }

    })
    
    ## Model fitting
    
    card_gamma_fit <- eventReactive(input$card_fitModel, {
        
        ## Extract model parameters
        
        my_factors <- card_excel_frame() %>%
            select(-mu) %>%
            names()
        
        sec_model_names <- c()
        this_p <- list()
        known_pars <- list()
        
        if (isTRUE(input$card_muopt_fix)) {
            known_pars$mu_opt <- input$card_muopt
        } else {
            this_p$mu_opt <- input$card_muopt
        }
        
        for (i in 1:length(my_factors)) {
            
            factor_name <- my_factors[[i]]
            factor_id <- card_id_dynamic[[i]]
            
            
            model_id <- paste0(factor_id, "_model")
            sec_model_names[factor_name] <- input[[model_id]]
            
            xmin_id <- paste0(factor_id, "_xmin")
            
            if (isTRUE(input[[paste0(xmin_id, "_fix")]])) {
                known_pars[[paste0(factor_name, "_xmin")]] <- input[[xmin_id]]
            } else {
                this_p[[paste0(factor_name, "_xmin")]] <- input[[xmin_id]]
            }
            
            xopt_id <- paste0(factor_id, "_xopt")
            
            if (isTRUE(input[[paste0(xopt_id, "_fix")]])) {
                known_pars[[paste0(factor_name, "_xopt")]] <- input[[xopt_id]]
            } else {
                this_p[[paste0(factor_name, "_xopt")]] <- input[[xopt_id]]
            }
            
            # this_p[[paste0(factor_name, "_xopt")]] <- input[[xopt_id]]
            
            xmax_id <- paste0(factor_id, "_xmax")
            
            if (isTRUE(input[[paste0(xmax_id, "_fix")]])) {
                known_pars[[paste0(factor_name, "_xmax")]] <- input[[xmax_id]]
            } else {
                this_p[[paste0(factor_name, "_xmax")]] <- input[[xmax_id]]
            }
            
            # this_p[[paste0(factor_name, "_xmax")]] <- input[[xmax_id]]
            
            n_id <- paste0(factor_id, "_n")
            
            if (isTRUE(input[[paste0(n_id, "_fix")]])) {
                known_pars[[paste0(factor_name, "_n")]] <- input[[n_id]]
            } else {
                this_p[[paste0(factor_name, "_n")]] <- input[[n_id]]
            }
            
            # this_p[[paste0(factor_name, "_n")]] <- input[[n_id]]
            
        }
        
        print("To fit:")
        print(this_p)
        print("")
        print("Fixed")
        print(known_pars)
        
        ## Fit the model
        
        fit_secondary_growth(card_excel_frame(),
                             this_p, known_pars, sec_model_names)
    })
    
    output$card_fit_results <- renderTable({
        aa <- card_gamma_fit() %>% summary()
        
        as_tibble(aa$par, rownames = "Parameter")
        
    })

}





























