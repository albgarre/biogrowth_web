library(tidyverse)
library(biogrowth)
library(readxl)
library(cowplot)
library(FME)

source("tableFileUI.R")
source("tableFile.R")

data("example_dynamic_growth")

server <- function(input, output) {
    
    ## Static predictions -----------------------------------------------------
    
    static_prediction_list <- reactiveVal(list())
    
    observeEvent(input$static_pred_addSim, {
        
        new_pred <- predict_isothermal_growth(input$modelStaticPrediction, 
                                  seq(0, input$static_max_time, length = 100), 
                                  list(logN0 = input$static_pred_logN0,
                                       mu = input$static_pred_mu,
                                       lambda = input$static_pred_lambda,
                                       logNmax = input$static_pred_logNmax,
                                       C = input$static_pred_C)
        )
        
        new_list <- static_prediction_list()
        new_list[[input$static_pred_simName]] <- new_pred
        
        static_prediction_list(new_list)

    })
    
    observeEvent(input$static_pred_cleanUp, {
        # static_prediction_frame(NULL)
        static_prediction_list(list())
    })
    
    # static_prediction <- reactive({
    #     
        # predict_isothermal_growth(input$modelStaticPrediction, 
        #                           seq(0, input$static_max_time, length = 1000), 
        #                           list(logN0 = input$static_pred_logN0,
        #                                mu = input$static_pred_mu,
        #                                lambda = input$static_pred_lambda,
        #                                logNmax = input$static_pred_logNmax,
        #                                C = input$static_pred_C)
        # )
    #     
    # })
    
    output$plot_static_prediction <- renderPlot({
        
        if (length(static_prediction_list()) == 0) {
            
            return(ggplot() + geom_blank())
            
        }
        
        static_prediction_list() %>%
            map(.,
                ~.$simulation
                ) %>%
            imap_dfr(., 
                     ~ mutate(.x, sim = .y)
                     ) %>%
            ggplot() + 
                geom_line(aes(x = time, y = logN, colour = sim), size = 1) +
                xlab(input$static_xaxis) +
                ylab(input$static_yaxis) +
                theme_cowplot() +
                theme(legend.title = element_blank())
        
    })
    
    output$static_timeToTable <- renderTable({
        
        if (length(static_prediction_list()) == 0) {
            
            return(tibble())
            
        }
        
        static_prediction_list() %>%
            map(., ~ time_to_logcount(., input$static_tgt_count)) %>%
            imap_dfr(.,
                     ~ tibble(Model = .y, 
                              `Target count` = input$static_tgt_count,
                              Time = .x)
                     )
        
        
    })
    
    output$static_export <- downloadHandler(
        filename = "growth-curve.csv",
        content = function(file) {
            
            static_prediction_list() %>%
                map(., ~ .$simulation) %>%
                imap_dfr(., ~ mutate(.x, simulation = .y)) %>%
                write_excel_csv(., path = file)
            # write_excel_csv(static_prediction_list(), path = file)
            
        }
    )
    
    ## Stochastic static predictions -------------------------------------------------
    
    static_prediction_stoc <- eventReactive(input$stoc_calculate, withProgress(
        message = "Calculation in progress",
        {
            
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
        )
    
    
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
        
        static_time_distrib()$summary %>%
            set_names("Mean", "Standard error", "Median", "Q10", "Q90")
    })
    
    output$static_stoc_down <- downloadHandler(
        filename = "stochastic-growth.csv",
        content = function(file) {
            
            write_excel_csv(static_prediction_stoc()$quantiles, path = file)
            
        }
    )
    
    ## Static fitting -----------------------------------------------------
    
    pred_micro_data <- callModule(tableFile, "pred_micro_data",
                                  default_data = tibble(time = c(0, 25, 50, 75, 100),
                                                        logN = c(2, 2.5, 7, 8, 8)
                                                        )
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
        static_fit_results() %>%
            plot() + xlab(input$static_fit_xlab) + ylab(input$static_fit_ylab)
    })
    
    output$static_fit_par <- renderTable({
        
        summary(static_fit_results())$par %>%
            as_tibble(rownames = "Parameter") %>%
            select(Parameter, Estimate, `Std. Error`) %>%
            mutate(`CI 95% left` = Estimate - 1.96*`Std. Error`,
                   `CI 95% right` = Estimate + 1.96*`Std. Error`)
        
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
    
    output$static_fit_resHist <- renderPlot({
        
        # tibble(Residual = static_fit_results()$fit$residuals) %>%
            # ggplot() + geom_histogram(aes(Residual))
        
        aa <- tibble(res = static_fit_results()$fit$residuals)
        
        ggplot(aa, aes(x = res)) + 
            geom_histogram(aes(y = ..density..)) + 
            geom_function(fun = dnorm, args = list(mean = mean(aa$res), sd = sd(aa$res)),
                          linetype = 2, colour = "blue", size = 1) +
            xlab("Residual") + 
            ylab("Frequency")
        
    })
    
    output$static_fit_residual_table <- renderTable(digits = 3, {
        
        n_par <- length(static_fit_results()$starting_point)
        n_dat <- nrow(static_fit_results()$data)
        
        tibble(res = static_fit_results()$fit$residuals,
               res2 = res^2) %>%
            summarize(ME = mean(res, na.rm = TRUE),
                      MSE = mean(res2, na.rm = TRUE)
            ) %>%
            mutate(RMSE = sqrt(MSE),
                   SER = sqrt(MSE*n_dat/(n_dat - n_par)),
                   Bf = 10^ME,
                   Af = 10^RMSE,
                   df = n_dat - n_par
            )
        
    })
    
    output$static_fit_shapiro <- renderText({
        
        test_res <- shapiro.test(static_fit_results()$fit$residuals)
        
        p_val <- test_res$p.value
        
        if (p_val < 0.05) {
            paste("There is enough evidence to state that residuals are not normally distributed; p-value =",
                  round(p_val, 3))
        } else {
            paste("There is NOT enough evidence to state that residuals are not normally distributed; p-value =",
                  round(p_val, 3))
        }
        
    })
    
    # output$static_fit_download_example <- downloadHandler(
    #     filename = "example_dyna.xlsx",
    #     content = function(file) {
    #         file.copy("example_dyna.xlsx", file)
    #     }
    # )
    
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
    
    output$card_download_example <- downloadHandler(
        filename = "example_cardinal.xlsx",
        content = function(file) {
            file.copy("example_cardinal.xlsx", file)
        }
    )
    
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
                               numericInput(paste0(id, "_xmax"), "Xmax (only in cardinal model)", 45)
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
            
            if (input[[model_id]] == "Zwietering") {
                this_p[[paste0(factor_name, "_xmax")]] <- NULL
                known_pars[[paste0(factor_name, "_xmax")]] <- NULL
            }
            
        }
        
        # print("To fit:")
        # print(this_p)
        # print("")
        # print("Fixed")
        # print(known_pars)
        
        ## Fit the model
        
        fit_secondary_growth(card_excel_frame(),
                             this_p, known_pars, sec_model_names,
                             transformation = input$card_transformation)
    })
    
    output$card_fit_results <- renderTable({
        aa <- card_gamma_fit() %>% summary()
        
        aa$par %>%
            as_tibble(rownames = "Parameter") %>%
            select(Parameter, Estimate, `Std. Error`) %>%
            mutate(`CI 95% left` = Estimate - 1.96*`Std. Error`,
                   `CI 95% right` = Estimate + 1.96*`Std. Error`)
        
    })
    
    output$card_res_hist <- renderPlot({
        
        # tibble(Residuals = card_gamma_fit()$fit_results$residuals) %>%
        #     ggplot() + geom_histogram(aes(Residuals))
        
        aa <- tibble(res = card_gamma_fit()$fit_results$residuals)
        
        ggplot(aa, aes(x = res)) + 
            geom_histogram(aes(y = ..density..)) + 
            geom_function(fun = dnorm, args = list(mean = mean(aa$res), sd = sd(aa$res)),
                          linetype = 2, colour = "blue", size = 1) +
            xlab("Residual") + 
            ylab("Frequency")
        
    })
    
    output$card_res_plot <- renderPlot({
        
        # Residual vs observation
        
        p1 <- card_gamma_fit()$data %>%
            mutate(res = card_gamma_fit()$fit_results$residuals
                   ) %>%
            ggplot(aes(x = mu, y = res)) + 
            geom_point() +
            geom_hline(yintercept = 0, linetype = 2, colour = "grey") +
            geom_smooth(se = FALSE) +
            xlab("Observed growth rate") + 
            ylab("Residual")   
        
        # Prediction vs observation
        
        aa <- card_gamma_fit()$data %>%
            mutate(res = card_gamma_fit()$fit_results$residuals)
        
        out <- if (card_gamma_fit()$transformation == "sq") {
            
            aa %>% 
                select(observed = sq_mu, res)
                # mutate(mu_pred = (sq_mu + res)^2)
            
        } else if (card_gamma_fit()$transformation == "none") {
            
            aa %>%
                select(observed = mu, res) 
                # mutate(mu_pred = mu + res)
            
        } else if (card_gamma_fit()$transformation == "log") {
            
            aa %>%
                select(observed = log_mu, res)
                # mutate(mu_pred = 10^(log_mu + res))
        }
        
        p2 <- out %>%
            mutate(predicted = observed + res) %>%
            ggplot(aes(x = observed, y = predicted)) +
            geom_point() +
            geom_abline(slope = 1, intercept = 0, linetype = 2) +
            geom_smooth(method = "lm", se = FALSE, colour = "grey") +
            xlab("Observed growth rate (same scale as fitting)") +
            ylab("Predicted growth rate (same scale as fitting)")
            
        # p2 <- ggplot(out) + 
        #     geom_point(aes(x = mu_pred, y = mu)) + 
        #     xlab("Predicted growth rate") + ylab("Observed growth rate")
        
        plot_grid(p1, p2, nrow = 1)
        
    })
    
    output$card_shapiro <- renderText({
        
        test_res <- shapiro.test(card_gamma_fit()$fit_results$residuals)
        
        p_val <- test_res$p.value
        
        if (p_val < 0.05) {
            paste("There is enough evidence to state that residuals are not normally distributed; p-value =",
                  round(p_val, 3))
        } else {
            paste("There is NOT enough evidence to state that residuals are not normally distributed; p-value =",
                  round(p_val, 3))
        }
        
    })
    
    output$card_residual_table <- renderTable(digits = 3, {
        
        n_par <- length(card_gamma_fit()$fit_results$par)
        n_dat <- nrow(card_gamma_fit()$data)
        
        tibble(res = card_gamma_fit()$fit_results$residuals,
               res2 = res^2) %>%
            summarize(ME = mean(res, na.rm = TRUE),
                      MSE = mean(res2, na.rm = TRUE)
            ) %>%
            mutate(RMSE = sqrt(MSE),
                   SER = sqrt(MSE*n_dat/(n_dat - n_par)),
                   Bf = 10^ME,
                   Af = 10^RMSE,
                   df = n_dat - n_par
            )
        
    })
    
    
    ## Dynamic prediction -----------------------------------------
    
    ## Data input
    
    dynPred_excelFile <- reactive({
        validate(need(input$dynPred_excel_file, label = "Excel"))
        input$dynPred_excel_file
    })
    
    dynPred_excel_frame <- reactive({
        read_excel(dynPred_excelFile()$datapath,
                   sheet = input$dynPred_excel_sheet,
                   skip = input$dynPred_excel_skip,
                   col_types = "numeric")
    })
    
    output$dynPred_plot_input <- renderPlot({
        
        dynPred_excel_frame() %>%
            gather(var, value, -time) %>%
            ggplot(aes(x = time, y = value)) +
            geom_line() +
            geom_point() +
            facet_wrap("var", scales = "free_y") +
            ylab("")
        
    })
    
    output$dynPred_download_example <- downloadHandler(
        filename = "example_dyna.xlsx",
        content = function(file) {
            file.copy("example_dyna.xlsx", file)
        }
    )
    
    ## Dynamic secondary model selector
    
    dynPred_id_dynamic <- c() # so I can remove them later

    observeEvent(input$dynPred_update, {

        my_names <- dynPred_excel_frame() %>%
            select(-time) %>%
            names()

        # Remove old ones

        if (length(dynPred_id_dynamic) > 0) {

            for (each_id in dynPred_id_dynamic) {
                removeUI(
                    ## pass in appropriate div id
                    selector = paste0('#', each_id)
                )
            }

            dynPred_id_dynamic <<- c()

        }

        # Insert new ones

        for (each_name in my_names) {

            id <- paste0('dynPred_', each_name)

            insertUI(
                selector = '#dynPredPlaceholder',
                ui = tags$div(
                    tags$h3(paste("Condition:", each_name)),
                    selectInput(paste0(id, "_model"),
                                "Model type",
                                list(`Cardinal` = "CPM", Zwietering = "Zwietering")),
                    
                    numericInput(paste0(id, "_xmin"), "Xmin", 0),
                    numericInput(paste0(id, "_xopt"), "Xopt", 37),
                    numericInput(paste0(id, "_xmax"), "Xmax (only in cardinal model)", 45),
                    numericInput(paste0(id, "_n"), "n", 1),
                    
                    tags$hr(),
                    id = id
                )
            )

            dynPred_id_dynamic <<- c(dynPred_id_dynamic, id)
        }

    })
    
    ## Model prediction
    
    dynPred_prediction <- eventReactive(input$dynPred_calculate, {
        
        ## Extract primary parameters
        
        primary_pars <- list(mu_opt = input$dynPred_muopt,
                     Nmax = 10^input$dynPred_logNmax,
                     N0 = 10^input$dynPred_logN0,
                     Q0 = input$dynPred_Q0)
        
        
        ## Extract secondary models
        
        my_factors <- dynPred_excel_frame() %>%
            select(-time) %>%
            names()
        
        sec_models <- list()

        for (i in 1:length(my_factors)) {
            
            factor_name <- my_factors[[i]]
            factor_id <- dynPred_id_dynamic[[i]]
            
            new_model <- list()
            
            model_id <- paste0(factor_id, "_model")
            new_model$model <- input[[model_id]]

            xmin_id <- paste0(factor_id, "_xmin")
            new_model$xmin <- input[[xmin_id]]

            xopt_id <- paste0(factor_id, "_xopt")
            new_model$xopt <- input[[xopt_id]]

            xmax_id <- paste0(factor_id, "_xmax")
            new_model$xmax <- input[[xmax_id]]

            n_id <- paste0(factor_id, "_n")
            new_model$n <- input[[n_id]]
            
            sec_models[[i]] <- new_model

        }

        names(sec_models) <- my_factors
        
        
        # print("+++++")
        # print(sec_models)
        # print(primary_pars)
        # print(my_factors)

        
        predict_dynamic_growth(seq(0, input$dynPred_maxtime, length = 1000),
                              dynPred_excel_frame(), primary_pars,
                              sec_models)
    })
    
    output$dynPred_plot_growth <- renderPlot({
        
        if (input$dynPred_addFactor) {
            p <- plot(dynPred_prediction(),
                 add_factor = input$dynPred_added_factor,
                 label_y1 = input$dynPred_ylabel,
                 label_y2 = input$dynPred_secylabel) + 
                xlab(input$dynPred_xlabel)
        } else {
            p <- plot(dynPred_prediction()) + 
                xlab(input$dynPred_xlabel) +
                ylab(input$dynPred_ylabel)
        }
        
        if (input$dynPred_add_timeToX) {
            
            my_t <- time_to_logcount(dynPred_prediction(),
                                     input$dynPred_tgt_count)
            
            p <- p + geom_vline(xintercept = my_t, linetype = 2) +
                geom_label(x = my_t, y = input$dynPred_tgt_count,
                           label = paste("t =", round(my_t, 1)))
            
        }
        p
        
    })
    
    output$dynPred_gammaPlot <- renderPlot({
        
        dynPred_prediction()$gammas %>%
            gather(var, gamma, -time) %>%
            ggplot() +
                geom_line(aes(x = time, y = gamma, colour = var)) +
                theme_cowplot() +
                theme(legend.title = element_blank()) +
                ylab("Value of gamma") + xlab("Time") 
            
    })
    
    output$dynPred_down_growth <- downloadHandler(
        filename = "dynamic-growth-curve.csv",
        content = function(file) {
            
            write_excel_csv(dynPred_prediction()$simulation, path = file)
            
        }
    )
    
    output$dynPred_down_gamma <- downloadHandler(
        filename = "dynamic-gamma-curve.csv",
        content = function(file) {
        
            write_excel_csv(dynPred_prediction()$gammas, path = file)
            
        }
    )
    
    ## Dynamic fitting -------------------------------------------
    
    ## Data input
    
    dynFit_micro_data <- callModule(tableFile, "dynFit_micro_data",
                                    # default_data = tibble(time = example_dynamic_growth$time,
                                    #                       logN = example_dynamic_growth$logN)
                                    default_data = tibble(time = c(0, 25, 50, 75, 100),
                                                          logN = c(2, 2.5, 7, 8, 8)
                                                          )
                                    )
    
    dynFit_excelFile <- reactive({
        validate(need(input$dynFit_excel_file, label = "Excel"))
        input$dynFit_excel_file
    })
    
    dynFit_excel_frame <- reactive({
        read_excel(dynFit_excelFile()$datapath,
                   sheet = input$dynFit_excel_sheet,
                   skip = input$dynFit_excel_skip,
                   col_types = "numeric")
    })
    
    output$dynFit_plot_input <- renderPlot({
        
        dynFit_excel_frame() %>%
            gather(var, value, -time) %>%
            ggplot(aes(x = time, y = value)) +
            geom_line() +
            geom_point() +
            facet_wrap("var", scales = "free_y") +
            ylab("")
        
    })
    
    output$dynFit_download_example <- downloadHandler(
        filename = "example_dyna_env.xlsx",
        content = function(file) {
            file.copy("example_dyna_env.xlsx", file)
        }
    )
    
    ## Dynamic secondary model selector
    
    dynFit_id_dynamic <- c() # so I can remove them later
    
    observeEvent(input$dynFit_update, {
        
        my_names <- dynFit_excel_frame() %>%
            select(-time) %>%
            names()
        
        # Remove old ones
        
        if (length(dynFit_id_dynamic) > 0) {
            
            for (each_id in dynFit_id_dynamic) {
                removeUI(
                    ## pass in appropriate div id
                    selector = paste0('#', each_id)
                )
            }
            
            dynFit_id_dynamic <<- c()
            
        }
        
        # Insert new ones
        
        for (each_name in my_names) {
            
            id <- paste0('dynFit_', each_name)
            
            insertUI(
                selector = '#dynFitPlaceholder',
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
                               numericInput(paste0(id, "_xmax"), "Xmax (only in cardinal model)", 45)
                        ),
                        column(6,
                               checkboxInput(paste0(id, "_xmax_fix"), "fixed?")
                        )
                    ),
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
            
            dynFit_id_dynamic <<- c(dynFit_id_dynamic, id)
        }
        
    })
    
    ## Model fitting
    
    dynFit_model <- eventReactive(input$dynFit_fitModel, {
        
        ## Extract model parameters
        
        my_factors <- dynFit_excel_frame() %>%
            select(-time) %>%
            names()
        
        sec_model_names <- c()
        start <- list()
        known_pars <- list()
        
        #- Primary model
        
        if (isTRUE(input$dynFit_muopt_fix)) {
            known_pars$mu_opt <- input$dynFit_muopt
        } else {
            start$mu_opt <- input$dynFit_muopt
        }
        
        if (isTRUE(input$dynFit_N0_fix)) {
            known_pars$N0 <- input$dynFit_N0
        } else {
            start$N0 <- input$dynFit_N0
        }
        
        if (isTRUE(input$dynFit_Nmax_fix)) {
            known_pars$Nmax <- input$dynFit_Nmax
        } else {
            start$Nmax <- input$dynFit_Nmax
        }
        
        if (isTRUE(input$dynFit_Q0_fix)) {
            known_pars$Q0 <- input$dynFit_Q0
        } else {
            start$Q0 <- input$dynFit_Q0
        }
        
        #- Secondary model
        
        for (i in 1:length(my_factors)) {
            
            factor_name <- my_factors[[i]]
            factor_id <- dynFit_id_dynamic[[i]]
            
            model_id <- paste0(factor_id, "_model")
            sec_model_names[factor_name] <- input[[model_id]]
            
            xmin_id <- paste0(factor_id, "_xmin")
            
            if (isTRUE(input[[paste0(xmin_id, "_fix")]])) {
                known_pars[[paste0(factor_name, "_xmin")]] <- input[[xmin_id]]
            } else {
                start[[paste0(factor_name, "_xmin")]] <- input[[xmin_id]]
            }
            
            xopt_id <- paste0(factor_id, "_xopt")
            
            if (isTRUE(input[[paste0(xopt_id, "_fix")]])) {
                known_pars[[paste0(factor_name, "_xopt")]] <- input[[xopt_id]]
            } else {
                start[[paste0(factor_name, "_xopt")]] <- input[[xopt_id]]
            }
            
            # start[[paste0(factor_name, "_xopt")]] <- input[[xopt_id]]
            
            xmax_id <- paste0(factor_id, "_xmax")
            
            if (isTRUE(input[[paste0(xmax_id, "_fix")]])) {
                known_pars[[paste0(factor_name, "_xmax")]] <- input[[xmax_id]]
            } else {
                start[[paste0(factor_name, "_xmax")]] <- input[[xmax_id]]
            }
            
            # start[[paste0(factor_name, "_xmax")]] <- input[[xmax_id]]
            
            n_id <- paste0(factor_id, "_n")
            
            if (isTRUE(input[[paste0(n_id, "_fix")]])) {
                known_pars[[paste0(factor_name, "_n")]] <- input[[n_id]]
            } else {
                start[[paste0(factor_name, "_n")]] <- input[[n_id]]
            }
            
            # start[[paste0(factor_name, "_n")]] <- input[[n_id]]
            
            if (input[[model_id]] == "Zwietering") {
                start[[paste0(factor_name, "_xmax")]] <- NULL
                known_pars[[paste0(factor_name, "_xmax")]] <- NULL
            }
            
        }
        
        print("To fit:")
        print(start)
        print("Fixed")
        print(known_pars)
        print("Model names:")
        print(sec_model_names)
        
        ## Fit the model
        
        if (input$dynFit_algorithm == "nlr") {
            
            out <- fit_dynamic_growth(dynFit_micro_data(),
                                      dynFit_excel_frame(), 
                                      starting_point = start,
                                      known_pars = known_pars, 
                                      sec_model_names)
            
            print(out)
            
        } else {
            
            out <- fit_MCMC_growth(dynFit_micro_data(),
                                   dynFit_excel_frame(), 
                                   start,
                                   known_pars, sec_model_names,
                                   niter = input$dynFit_niter)
            
        }
        
        out

        
    })
    
    ## Output of the results
    
    output$dynFit_modelPlot <- renderPlot({
        withProgress({
            
            if (input$dynFit_addFactor) {
                plot(dynFit_model(),
                     add_factor = input$dynFit_added_factor,
                     label_y1 = input$dynFit_ylabel,
                     label_y2 = input$dynFit_secylabel) + 
                    xlab(input$dynFit_xlabel)
            } else {
                plot(dynFit_model()) + 
                    xlab(input$dynFit_xlabel) +
                    ylab(input$dynFit_ylabel)
            }
            
        }, message = "Fitting the model")
        
    })
    
    output$dynFit_par_summary <- renderTable({
        
        my_model <- dynFit_model()
        
        if (is.FitDynamicGrowth(my_model)) {  # nlr fit
            
            summary(my_model)$par %>%
                as_tibble(rownames = "Parameter") %>%
                select(Parameter, Estimate, `Std. Error`) %>%
                mutate(`CI 95% left` = Estimate - 1.96*`Std. Error`,
                       `CI 95% right` = Estimate + 1.96*`Std. Error`)
            
        } else {  # MCMC fit
            summary(my_model) %>%
                as_tibble(rownames = "Index")
        }
        
        
    })
    
    output$dynFit_residualTable <- renderTable(digits = 3, {
        
        my_model <- dynFit_model()
        
        if (is.FitDynamicGrowth(my_model)) {  # nlr fit
            
            n_par <- length(my_model$fit_results$par)
            n_dat <- nrow(my_model$data)
            
            res <- my_model$data %>%
                mutate(res = my_model$fit_results$residuals) %>%
                pull(res) 
            

        } else {  # MCMC fit
            
            n_par <- ncol(my_model$fit_results$par)
            n_dat <- nrow(my_model$data)
            
            res <- my_model$best_prediction$simulation %>%
                select(time, logN) %>%
                as.data.frame() %>%
                modCost(model = ., 
                        obs = as.data.frame(my_model$data)) %>%
                .$residuals %>%
                .$res

        }
        
        tibble(res = res,
               res2 = res^2) %>%
            summarize(ME = mean(res, na.rm = TRUE),
                      MSE = mean(res2, na.rm = TRUE)
            ) %>%
            mutate(RMSE = sqrt(MSE),
                   SER = sqrt(MSE*n_dat/(n_dat - n_par)),
                   Bf = 10^ME,
                   Af = 10^RMSE,
                   df = n_dat - n_par
            )
        
    })
    
    output$dynFit_resPlot <- renderPlot({
        
        my_model <- dynFit_model()
        
        if (is.FitDynamicGrowth(my_model)) {  # nlr fit
            
            my_model$data %>%
                mutate(res = my_model$fit_results$residuals) %>%
                ggplot(aes(x = time, y = res)) +
                geom_point() +
                geom_smooth() +
                geom_hline(yintercept = 0, linetype = 2) +
                xlab("Time") + ylab("Residual")
            
        } else {  # MCMC fit
            
            my_cost <- my_model$best_prediction$simulation %>%
                select(time, logN) %>%
                as.data.frame() %>%
                modCost(model = ., 
                        obs = as.data.frame(my_model$data))
            
            ggplot(my_cost$residuals, aes(x = x, y = res)) +
                geom_point() +
                geom_smooth() +
                geom_hline(yintercept = 0, linetype = 2) +
                xlab("Time") + ylab("Residual")
            
        }

    })
    
    output$dynFit_resHist <- renderPlot({
        
        
        my_model <- dynFit_model()
        
        res <- if (is.FitDynamicGrowth(my_model)) {  # nlr fit
            
            my_model$data %>%
                mutate(res = my_model$fit_results$residuals) %>%
                pull(res) 
            
        } else {  # MCMC fit
            
            my_model$best_prediction$simulation %>%
                select(time, logN) %>%
                as.data.frame() %>%
                modCost(model = ., 
                        obs = as.data.frame(my_model$data)) %>%
                .$residuals %>%
                .$res

        }
        
        aa <- tibble(res = res)
        
        ggplot(aa, aes(x = res)) + 
            geom_histogram(aes(y = ..density..)) + 
            geom_function(fun = dnorm, args = list(mean = mean(aa$res), sd = sd(aa$res)),
                          linetype = 2, colour = "blue", size = 1) +
            xlab("Residual") + 
            ylab("Frequency")
        
    }) 
    
    output$dynFit_shapiro <- renderText({
        
        my_model <- dynFit_model()
        
        res <- if (is.FitDynamicGrowth(my_model)) {  # nlr fit
            
            my_model$data %>%
                mutate(res = my_model$fit_results$residuals) %>%
                pull(res) 
            
        } else {  # MCMC fit
            
            my_cost <- my_model$best_prediction$simulation %>%
                select(time, logN) %>%
                as.data.frame() %>%
                modCost(model = ., 
                        obs = as.data.frame(my_model$data)) %>%
                .$residuals %>%
                .$res
            
        }
        
        test_res <- shapiro.test(res)
        
        p_val <- test_res$p.value
        
        if (p_val < 0.05) {
            paste("There is enough evidence to state that residuals are not normally distributed; p-value =",
                  round(p_val, 3))
        } else {
            paste("There is NOT enough evidence to state that residuals are not normally distributed; p-value =",
                  round(p_val, 3))
        }
        
    })
    
    output$dynFit_MCMC_chain <- renderPlot({
        
        plot(dynFit_model()$fit_results)
        
    })
    
    output$dynFit_MCMC_pairs <- renderPlot({
        pairs(dynFit_model()$fit_results)
    })
    
    observeEvent(input$dynFit_seed, {
        print("Seed back to normal")
        set.seed(12412)
    })
    
    
    
}















