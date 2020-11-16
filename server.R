library(tidyverse)
library(biogrowth)
library(readxl)
library(cowplot)
library(FME)

source("tableFileUI.R")
source("tableFile.R")

data("example_dynamic_growth")

server <- function(input, output, session) {
    
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
    
    addPopover(session, "plot_static_prediction",
               "Growth curves under static conditions",
               paste("This plot shows the predicted growth curves for the models defined.",
                     "The lines are coloured according to the names you have defined.",
                     "To edit any line, you just have to redefine model parameters and use the same name.",
                     "Note that the validity of the predictions depends on the validity of the model parameters.",
                     "Also, these predictions (in principle) are only valid under static environmental conditions.",
                     sep = " "),
               trigger = "click"
               )
    
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
    
    addPopover(session, "plot_static_prediction_stoc",
               "Stochastic growth curves under static conditions",
               paste("This plot shows the prediction intervals according to the variation in the model parameters.",
                     "The solid line represents the median of the simulations.",
                     "The two dashed areas represent the area between the 10th and 90th, and 5th and 95th intervals.",
                     "The intervals are estimated by forward propagation using Monte Carlo simulations.",
                     "It is advised to repeat the calculations for with the same number of simulations. If the results vary, the number of simulations should be increased.",
                     "Note that the simulations are valid only for static conditions.",
                     "Also be mindful about what the variation of the parameters represent.",
                     sep = " "),
               trigger = "click", placement = "left"
    )
    
    output$plot_static_timedistrib <- renderPlot({
        
        plot(static_time_distrib())
        
    })
    
    addPopover(session, "plot_static_timedistrib",
               "Distributions of time to reach the selected microbial count",
               paste("This plot shows an estimation of the treatment time needed to reach some microbial count",
                     "Because simulations are stochastic, the result is a probability distribution according to the variation in the model parameters",
                     "The distribution is estimated using forward propagation with Monte Carlo simulations.",
                     "You should increase the number of iterations until the histogram is 'reasonably' smooth",
                     "The read line shows the median of the histogram, and the grey lines the 90th and and 10th quantiles.",
                     "Note that the simulations are valid only for static conditions.",
                     "Also be mindful about what the variation of the parameters represent.",
                     sep = " "),
               trigger = "click", placement = "left"
    )
    
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
    
    addPopover(session, "plot_static_fit",
               "Fitted growth curve under static conditions",
               paste("This plot compares the fitted model against the observations.",
                     "The data was used to fit the model, so the points should be 'reasonably' close",
                     "If they are not, it is quite likely that the starting values of the parameters were not appropiate",
                     "Alternatively, it is possible some parameter(s) was fixed to unrealistic values.",
                     "Note that this modelling approach is designed for data gathered under isothermal conditions.",
                     "In case of dynamic fits, please use the appropiate module.",
                     sep = " "),
               trigger = "click", placement = "left"
    )
    
    output$static_fit_par <- renderTable({
        
        summary(static_fit_results())$par %>%
            as_tibble(rownames = "Parameter") %>%
            select(Parameter, Estimate, `Std. Error`) %>%
            mutate(`CI 95% left` = Estimate - 1.96*`Std. Error`,
                   `CI 95% right` = Estimate + 1.96*`Std. Error`)
        
    })
    
    addPopover(session, "static_fit_par",
               "Table of parameter estimates",
               paste("This table gives the estimated parameter values and standard errors",
                     "Only the fitted parameters are included here (i.e. not the fixed parameters).",
                     "The confidence intervals are calculated as E(X) +/- SEM(X)*1.96.",
                     "If the model is not identifiable, the cells for the SEM and the CI will show 'NA'.",
                     "If this is the case, please consider repeating the fit with different starting values, or fixing some parameters.",
                     "Also, please check that there is no mistake in the data used for the fit.",
                     "Moreover, before blindly accepting the parameter estimates, it is advisable to check the fitting diagnostics.",
                     sep = " "),
               trigger = "click", placement = "right", options = list(container = "body")
    )
    
    output$static_fit_residual <- renderPlot({
        
        tibble(res = static_fit_results()$fit$residuals,
               time = static_fit_results()$data$time) %>%
            ggplot(aes(x = time, y = res)) +
            geom_point() +
            geom_hline(yintercept = 0, linetype = 2) +
            geom_smooth(se=FALSE) +
            xlab("Time") + ylab("Residual")
        
    })
    
    addPopover(session, "static_fit_residual",
               "Residuals plot",
               paste("This plot illustrates the residuals.",
                     "They should be centered around the origin and with constant variance.",
                     "The blue line is a trend line that helps identifying deviations from the theoretical result.",
                     sep = " "),
               trigger = "click", placement = "left"
    )
    
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
    
    addPopover(session, "static_fit_resHist",
               "Histogram of the residuals",
               paste("This plot shows a histogram of the residuals.",
                     "They should be normally distributed",
                     "The blue line shows the pdf of a normal distribution with the same mean and variance as the residuals.",
                     "The histogram should fit 'reasonably well' this line.",
                     sep = " "),
               trigger = "click", placement = "left"
    )
    
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
                                list(`Cardinal` = "CPM", 
                                     `Full Ratkowsky` = "fullRatkowsky",
                                     Zwietering = "Zwietering")),
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
                               numericInput(paste0(id, "_xopt"), "Xopt (not in Ratkowsky)", 37)
                        ),
                        column(6,
                               checkboxInput(paste0(id, "_xopt_fix"), "fixed?")
                        )
                    ),
                    fluidRow(
                        column(6,
                               numericInput(paste0(id, "_xmax"), "Xmax (not in Zwietering)", 45)
                        ),
                        column(6,
                               checkboxInput(paste0(id, "_xmax_fix"), "fixed?")
                        )
                    ),
                    fluidRow(
                        column(6,
                               numericInput(paste0(id, "_n"), "n (not in Cardinal)", 1)
                        ),
                        column(6,
                               checkboxInput(paste0(id, "_n_fix"), "fixed?")
                        )
                    ),
                    fluidRow(
                        column(6,
                               numericInput(paste0(id, "_c"), "c (only Ratkowsky)", .1)
                        ),
                        column(6,
                               checkboxInput(paste0(id, "_c_fix"), "fixed?")
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
            
            xmax_id <- paste0(factor_id, "_xmax")
            
            if (isTRUE(input[[paste0(xmax_id, "_fix")]])) {
                known_pars[[paste0(factor_name, "_xmax")]] <- input[[xmax_id]]
            } else {
                this_p[[paste0(factor_name, "_xmax")]] <- input[[xmax_id]]
            }
            
            n_id <- paste0(factor_id, "_n")
            
            if (isTRUE(input[[paste0(n_id, "_fix")]])) {
                known_pars[[paste0(factor_name, "_n")]] <- input[[n_id]]
            } else {
                this_p[[paste0(factor_name, "_n")]] <- input[[n_id]]
            }
            
            c_id <- paste0(factor_id, "_c")
            
            if (isTRUE(input[[paste0(c_id, "_fix")]])) {
                known_pars[[paste0(factor_name, "_c")]] <- input[[c_id]]
            } else {
                this_p[[paste0(factor_name, "_c")]] <- input[[c_id]]
            }
            
            ## Fixing the parameters that should not be there
            
            if (input[[model_id]] == "Zwietering") {
                this_p[[paste0(factor_name, "_xmax")]] <- NULL
                known_pars[[paste0(factor_name, "_xmax")]] <- NULL
                this_p[[paste0(factor_name, "_c")]] <- NULL
                known_pars[[paste0(factor_name, "_c")]] <- NULL
            }
            
            if (input[[model_id]] == "CPM") {
                this_p[[paste0(factor_name, "_c")]] <- NULL
                known_pars[[paste0(factor_name, "_c")]] <- NULL
            }
            
            if (input[[model_id]] == "fullRatkowsky") {
                this_p[[paste0(factor_name, "_xopt")]] <- NULL
                known_pars[[paste0(factor_name, "_xopt")]] <- NULL
                this_p[[paste0(factor_name, "_n")]] <- NULL
                known_pars[[paste0(factor_name, "_n")]] <- NULL
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
    
    ## Output
    
    output$card_fit_results <- renderTable({
        aa <- card_gamma_fit() %>% summary()
        
        aa$par %>%
            as_tibble(rownames = "Parameter") %>%
            select(Parameter, Estimate, `Std. Error`) %>%
            mutate(`CI 95% left` = Estimate - 1.96*`Std. Error`,
                   `CI 95% right` = Estimate + 1.96*`Std. Error`)
        
    })
    
    addPopover(session, "card_fit_results",
               "Table of parameter estimates",
               paste("This table contains the estimated parameter values and standard errors.",
                     "The confidence intervals are calculated as E(X) +/- 1.96*SE(X).",
                     "Cells with 'NA' values are an indicator of poor parameter identifiability.",
                     "If this is the case, try changing starting values of fixed model parameters.",
                     "Before blindly accepting these values, it is advisable to check the model diagnostics.",
                     sep = " "),
               trigger = "click", placement = "right", options = list(container = "body")
    )
    
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
    
    addPopover(session, "card_res_hist",
               "Histogram of the residuals",
               paste("The residuals should be normally distributed with mean zero.",
                     "The blue line shows the pdf of a normal distribution with the same mean and variance as the residuals.",
                     "The histogram should fit the line 'reasonably well'.",
                     sep = " "),
               trigger = "click", placement = "left"
    )
    
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
    
    addPopover(session, "card_res_plot",
               "Residuals plots",
               paste("The left plot illustrate the residuals of the model",
                     "They should be spread around the origin with constant variance.",
                     "The blue line is a trend line that may aid identifying deviations with respect to the ideal value.",
                     "The right plot compares the observed versus predicted observations.",
                     "In a perfect model, all the points would sit on the dashed line.",
                     "The grey line shows the regression line of predictions vs observations.",
                     sep = " "),
               trigger = "click", placement = "left"
    )
    
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
                                list(`Cardinal` = "CPM", 
                                     `Full Ratkowsky` = "fullRatkowsky",
                                     Zwietering = "Zwietering")),
                    numericInput(paste0(id, "_xmin"), "Xmin", 0),
                    numericInput(paste0(id, "_xopt"), "Xopt (not for Ratkowsky)", 37),
                    numericInput(paste0(id, "_xmax"), "Xmax (not for Zwietering)", 45),
                    numericInput(paste0(id, "_n"), "n (not for Ratkowsky)", 1),
                    numericInput(paste0(id, "_c"), "c (only for Ratkowsky)", 1),
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
            
            c_id <- paste0(factor_id, "_c")
            new_model$c <- input[[c_id]]
            
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
    
    addPopover(session, "dynPred_plot_growth",
               "Growth curve under dynamic conditions",
               paste("This plot shows the predicted microbial count under dynamic conditions.",
                     "The curve may deviate from the sigmoidal curve due to the variation of the environmental factors",
                     "Also, the curve may have a lag or stationary phase due to the inhibition by the environmental fators, not because of the usual interpretation under static conditions.",
                     sep = " "),
               trigger = "click", placement = "left"
    )
    
    
    output$dynPred_gammaPlot <- renderPlot({
        
        dynPred_prediction()$gammas %>%
            gather(var, gamma, -time) %>%
            ggplot() +
                geom_line(aes(x = time, y = gamma, colour = var)) +
                theme_cowplot() +
                theme(legend.title = element_blank()) +
                ylab("Value of gamma") + xlab("Time")  +
                theme(legend.position = "top")
            
    })
    
    addPopover(session, "dynPred_gammaPlot",
               "Variation of the gamma factors",
               paste("This plot shows the predicted variation of the gamma factors throughout storage.",
                     "In the gamma approach, each environmental factor acts as a correction factor between 0 and 1 that reduces the growth rate.",
                     "Hence, this plot illustrates which is the most limiting factor for each time point, and how large is its impact.",
                     "Note that this plot only shows the effect of the gamma factors. The lag phase can also be a very relevant limiting factor.",
                     sep = " "),
               trigger = "click", placement = "left"
    )
    
    
    
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
                                list(`Cardinal` = "CPM", 
                                     `Full Ratkowsky` = "fullRatkowsky",
                                     Zwietering = "Zwietering")),
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
                               numericInput(paste0(id, "_xopt"), "Xopt (not for Ratkowsky model)", 37)
                        ),
                        column(6,
                               checkboxInput(paste0(id, "_xopt_fix"), "fixed?")
                        )
                    ),
                    fluidRow(
                        column(6,
                               numericInput(paste0(id, "_xmax"), "Xmax (not for Zwietering model)", 45)
                        ),
                        column(6,
                               checkboxInput(paste0(id, "_xmax_fix"), "fixed?")
                        )
                    ),
                    fluidRow(
                        column(6,
                               numericInput(paste0(id, "_n"), "n (not for Ratkowsky)", 1)
                        ),
                        column(6,
                               checkboxInput(paste0(id, "_n_fix"), "fixed?")
                        )
                    ),
                    fluidRow(
                        column(6,
                               numericInput(paste0(id, "_c"), "c (only for Ratkowsky)", 1)
                        ),
                        column(6,
                               checkboxInput(paste0(id, "_c_fix"), "fixed?")
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
            
            c_id <- paste0(factor_id, "_c")
            
            if (isTRUE(input[[paste0(c_id, "_fix")]])) {
                known_pars[[paste0(factor_name, "_c")]] <- input[[c_id]]
            } else {
                start[[paste0(factor_name, "_c")]] <- input[[c_id]]
            }
            
            ## Remove parameters that should not be there
            
            if (input[[model_id]] == "Zwietering") {
                start[[paste0(factor_name, "_xmax")]] <- NULL
                known_pars[[paste0(factor_name, "_xmax")]] <- NULL
                start[[paste0(factor_name, "_c")]] <- NULL
                known_pars[[paste0(factor_name, "_c")]] <- NULL
            }
            
            if (input[[model_id]] == "CPM") {
                start[[paste0(factor_name, "_c")]] <- NULL
                known_pars[[paste0(factor_name, "_c")]] <- NULL
            }
            
            if (input[[model_id]] == "fullRatkosky") {
                start[[paste0(factor_name, "_n")]] <- NULL
                known_pars[[paste0(factor_name, "_n")]] <- NULL
                start[[paste0(factor_name, "_xopt")]] <- NULL
                known_pars[[paste0(factor_name, "_xopt")]] <- NULL
            }
            
        }
        
        # print("To fit:")
        # print(start)
        # print("Fixed")
        # print(known_pars)
        # print("Model names:")
        # print(sec_model_names)
        
        ## Fit the model
        
        if (input$dynFit_algorithm == "nlr") {
            
            out <- fit_dynamic_growth(dynFit_micro_data(),
                                      dynFit_excel_frame(), 
                                      starting_point = start,
                                      known_pars = known_pars, 
                                      sec_model_names)
            
            # print(out)
            
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
    
    addPopover(session, "dynFit_modelPlot",
               "Fitted vs observed counts under dynamic conditions",
               paste("This plot compares the fitted model against the observed counts under dynamic conditions",
                     "These points were used for parameter estimation, so the model should fit the data reasonably well.",
                     "If it doesn't, please try different starting values for the model parameters.",
                     "Alternativelly, please change the values of the fixed model parameters.",
                     sep = " "),
               trigger = "click", placement = "left", options = list(container = "body")
    )
    
    
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
    
    addPopover(session, "dynFit_par_summary",
               "Parameter estimates under dynamic conditions",
               paste("This table reports the estimated parameter values and standard errors",
                     "For non-linear regression, CI are calculated as E(X) +/- 1.96*SE(X)",
                     "For MCMC models, they are calculated as the quantiles of the posterior distribution.",
                     "In case some cells have 'NA' values, consider using more realistic starting values, or fixing some parameters.",
                     "Moreover, before blindly using these parameters, it is advisable to check the model diagnostics.",
                     sep = " "),
               trigger = "click", placement = "right", options = list(container = "body")
    )
    
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
    
    addPopover(session, "dynFit_resPlot",
               "Residuals plot",
               paste("This plot illustrates the residuals of the model",
                     "They should be distributed around the origin with constant variance.",
                     "The blue line shows a trend line, which may help identifying variations with respect to the ideal trend.",
                     sep = " "),
               trigger = "click", placement = "left", options = list(container = "body")
    )
    
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
    
    addPopover(session, "dynFit_resHist",
               "Histogram of the residuals",
               paste("This plot shows a histogram of the residuals.",
                     "They should be normally distributed with mean zero",
                     "The blue line shows the pdf of a normal distribution with the same mean and variance as the residuals.",
                     "The histogram should adjust 'reasonably well' to the pdf.",
                     sep = " "),
               trigger = "click", placement = "left", options = list(container = "body")
    )
    
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
    
    addPopover(session, "dynFit_MCMC_chain",
               "Convergence of the Markov chain",
               paste("This plot shows the evolution of the Markov chain.",
                     "The plot should look like 'noise', without any obvious trend.",
                     "If it does not, it is recommended to increase the number of MC samples.",
                     "Alternatively, one could change the starting values or the fixed parameters.",
                     sep = " "),
               trigger = "click", placement = "left", options = list(container = "body")
    )
    
    output$dynFit_MCMC_pairs <- renderPlot({
        pairs(dynFit_model()$fit_results)
    })
    
    addPopover(session, "dynFit_MCMC_pairs",
               "Posterior distribution of the parameters",
               paste("This plot illustrates the posterior distribution of the model parameters.",
                     "The histograms and pairs plot should be 'smooth'. Otherwise, parameter estimates may be unreliable.",
                     sep = " "),
               trigger = "click", placement = "left", options = list(container = "body")
    )
    
    observeEvent(input$dynFit_seed, {
        print("Seed back to normal")
        set.seed(12412)
    })
    
    
    ## Global fitting ---------------------------------------------------
    
    ## Input microbial counts
    
    globalFit_excelFile_count <- reactive({
        validate(need(input$globalFit_excel_file_count, label = "Excel"))
        input$globalFit_excel_file_count
    })
    
    globalFit_excel_frame_count <- reactive({
        
        my_path <- globalFit_excelFile_count()$datapath
        
        excel_sheets(my_path) %>%
            set_names(., .) %>%
            map(., ~ read_excel(my_path, sheet = ., col_types = "numeric"))
    })
    
    output$globalFit_plot_input_count <- renderPlot({
        
        globalFit_excel_frame_count() %>%
            imap_dfr(., ~ mutate(.x, exp = .y)) %>%
            ggplot(aes(x = time, y = logN, colour = exp)) +
            geom_point() +
            ylab("") +
            theme(legend.title = element_blank())
        
    })
    
    output$globalFit_download_example_count <- downloadHandler(
        filename = "example_global_count.xlsx",
        content = function(file) {
            file.copy("example_global_count.xlsx", file)
        }
    )
    
    ## Input environmental conditions
    
    globalFit_excelFile_env <- reactive({
        validate(need(input$globalFit_excel_file_env, label = "Excel"))
        input$globalFit_excel_file_env
    })
    
    globalFit_excel_frame_env <- reactive({
        
        my_path <- globalFit_excelFile_env()$datapath
        
        excel_sheets(my_path) %>%
            set_names(., .) %>%
            map(., ~ read_excel(my_path, sheet = ., col_types = "numeric"))
    })
    
    output$globalFit_plot_input_env <- renderPlot({
        
        globalFit_excel_frame_env() %>%
            map(., ~ gather(., var, value, -time)) %>%
            imap_dfr(., ~ mutate(.x, exp = .y)) %>%
            ggplot(aes(x = time, y = value, colour = exp)) +
            geom_line() +
            geom_point() +
            facet_wrap("var", scales = "free_y") +
            ylab("") +
            theme(legend.title = element_blank())
        
    })
    
    output$globalFit_download_example_env <- downloadHandler(
        filename = "example_global_env.xlsx",
        content = function(file) {
            file.copy("example_global_env.xlsx", file)
        }
    )
    
    ## Dynamic secondary model selector
    
    globalFit_id_dynamic <- c() # so I can remove them later
    
    observeEvent(input$globalFit_update, {
        
        my_names <- globalFit_excel_frame_env()[[1]] %>%
            select(-time) %>%
            names()
        
        # Remove old ones
        
        if (length(globalFit_id_dynamic) > 0) {
            
            for (each_id in globalFit_id_dynamic) {
                removeUI(
                    ## pass in appropriate div id
                    selector = paste0('#', each_id)
                )
            }
            
            globalFit_id_dynamic <<- c()
            
        }
        
        # Insert new ones
        
        for (each_name in my_names) {
            
            id <- paste0('globalFit_', each_name)
            
            insertUI(
                selector = '#globalFitPlaceholder',
                ui = tags$div(
                    tags$h3(paste("Condition:", each_name)), 
                    selectInput(paste0(id, "_model"),
                                "Model type",
                                list(`Cardinal` = "CPM", 
                                     `Full Ratkowsky` = "fullRatkowsky",
                                     Zwietering = "Zwietering")),
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
                               numericInput(paste0(id, "_xopt"), "Xopt (not for Ratkowsky model)", 37)
                        ),
                        column(6,
                               checkboxInput(paste0(id, "_xopt_fix"), "fixed?")
                        )
                    ),
                    fluidRow(
                        column(6,
                               numericInput(paste0(id, "_xmax"), "Xmax (not for Zwietering model)", 45)
                        ),
                        column(6,
                               checkboxInput(paste0(id, "_xmax_fix"), "fixed?")
                        )
                    ),
                    fluidRow(
                        column(6,
                               numericInput(paste0(id, "_n"), "n (not for Ratkowsky)", 1)
                        ),
                        column(6,
                               checkboxInput(paste0(id, "_n_fix"), "fixed?")
                        )
                    ),
                    fluidRow(
                        column(6,
                               numericInput(paste0(id, "_c"), "c (only for Ratkowsky)", 1)
                        ),
                        column(6,
                               checkboxInput(paste0(id, "_c_fix"), "fixed?")
                        )  
                    ),
                    tags$hr(),
                    id = id
                )
            )
            
            globalFit_id_dynamic <<- c(globalFit_id_dynamic, id)
        }
        
    })
    
    ## Model fitting
    
    globalFit_model <- eventReactive(input$globalFit_fitModel, {
        
        ## Extract model parameters
        
        my_factors <- globalFit_excel_frame_env()[[1]] %>%
            select(-time) %>%
            names()
        
        sec_model_names <- c()
        start <- list()
        known_pars <- list()
        
        #- Primary model
        
        if (isTRUE(input$globalFit_muopt_fix)) {
            known_pars$mu_opt <- input$globalFit_muopt
        } else {
            start$mu_opt <- input$globalFit_muopt
        }
        
        if (isTRUE(input$globalFit_N0_fix)) {
            known_pars$N0 <- input$globalFit_N0
        } else {
            start$N0 <- input$globalFit_N0
        }
        
        if (isTRUE(input$globalFit_Nmax_fix)) {
            known_pars$Nmax <- input$globalFit_Nmax
        } else {
            start$Nmax <- input$globalFit_Nmax
        }
        
        if (isTRUE(input$globalFit_Q0_fix)) {
            known_pars$Q0 <- input$globalFit_Q0
        } else {
            start$Q0 <- input$globalFit_Q0
        }
        
        #- Secondary model
        
        for (i in 1:length(my_factors)) {
            
            factor_name <- my_factors[[i]]
            factor_id <- globalFit_id_dynamic[[i]]
            
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
            
            c_id <- paste0(factor_id, "_c")
            
            if (isTRUE(input[[paste0(c_id, "_fix")]])) {
                known_pars[[paste0(factor_name, "_c")]] <- input[[c_id]]
            } else {
                start[[paste0(factor_name, "_c")]] <- input[[c_id]]
            }
            
            ## Remove parameters that should not be there
            
            if (input[[model_id]] == "Zwietering") {
                start[[paste0(factor_name, "_xmax")]] <- NULL
                known_pars[[paste0(factor_name, "_xmax")]] <- NULL
                start[[paste0(factor_name, "_c")]] <- NULL
                known_pars[[paste0(factor_name, "_c")]] <- NULL
            }
            
            if (input[[model_id]] == "CPM") {
                start[[paste0(factor_name, "_c")]] <- NULL
                known_pars[[paste0(factor_name, "_c")]] <- NULL
            }
            
            if (input[[model_id]] == "fullRatkosky") {
                start[[paste0(factor_name, "_n")]] <- NULL
                known_pars[[paste0(factor_name, "_n")]] <- NULL
                start[[paste0(factor_name, "_xopt")]] <- NULL
                known_pars[[paste0(factor_name, "_xopt")]] <- NULL
            }
            
        }

        # print("To fit:")
        # print(start)
        # print("Fixed")
        # print(known_pars)
        # print("Model names:")
        # print(sec_model_names)
        
        ## Prepare the data
        
        exp_data <- lapply(names(globalFit_excel_frame_env()), function(each_name) {
            
            list(
                data = globalFit_excel_frame_count()[[each_name]],
                conditions = globalFit_excel_frame_env()[[each_name]]
            )
            
        })
        
        names(exp_data) <- names(globalFit_excel_frame_env())
        
        ## Fit the model
        
        if (input$globalFit_algorithm == "nlr") {
            
            out <- fit_multiple_growth(starting_point = start,
                                       experiment_data = exp_data,
                                       known_pars = known_pars, 
                                       sec_model_names = sec_model_names)
            
            # print(out)
            
        } else {
            
            out <- fit_multiple_growth_MCMC(starting_point = start,
                                            experiment_data = exp_data,
                                            known_pars = known_pars, 
                                            sec_model_names = sec_model_names,
                                            niter = input$globalFit_niter)
            
        }
        
        out
        
        
    })
    
    ## Output
    
    output$globalFit_modelPlot <- renderPlot({

        withProgress({
            
            if (input$globalFit_addFactor) {
                plot(globalFit_model(),
                     add_factor = input$globalFit_added_factor,
                     label_y1 = input$globalFit_ylabel,
                     label_y2 = input$globalFit_secylabel,
                     label_x = input$globalFit_xlabel)
            } else {
                plot(globalFit_model(),
                     label_x = input$globalFit_xlabel,
                     label_y1 = input$globalFit_ylabel)
            }
            
        }, message = "Fitting the model")
        
    })
    
    output$globalFit_par_summary <- renderTable({
        
        my_model <- globalFit_model()
        
        if ("FitMultipleDynamicGrowth" %in% class(my_model)) {  # nlr fit
            
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
    
    addPopover(session, "globalFit_par_summary",
               "Parameter estimates under dynamic conditions",
               paste("This table reports the estimated parameter values and standard errors",
                     "For non-linear regression, CI are calculated as E(X) +/- 1.96*SE(X)",
                     "For MCMC models, they are calculated as the quantiles of the posterior distribution.",
                     "In case some cells have 'NA' values, consider using more realistic starting values, or fixing some parameters.",
                     "Moreover, before blindly using these parameters, it is advisable to check the model diagnostics.",
                     sep = " "),
               trigger = "click", placement = "right", options = list(container = "body")
    )
    
    output$globalFit_residualTable <- renderTable(digits = 3, {
        
        my_model <- globalFit_model()
        
        if (is.FitMultipleDynamicGrowth(my_model)) {  # nlr fit
            
            n_par <- length(my_model$fit_results$par)
            res <- residuals(my_model)$res
            n_dat <- my_model$data %>%
                map_dbl(., ~ nrow(.$data)) %>%
                sum()

            
        } else {  # MCMC fit
            
            n_par <- ncol(my_model$fit_results$par)
            res <- residuals(my_model)$res
            
            n_dat <- my_model$data %>%
                map_dbl(., ~ nrow(.$data)) %>%
                sum()

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
    
    output$globalFit_resPlot <- renderPlot({
        
        p <- residuals(globalFit_model()) %>%
            ggplot(aes(x = time, y = res, colour = exp)) + 
            geom_point() + 
            geom_smooth() +
            geom_hline(yintercept = 0, linetype = 2) + 
            xlab("Time") + ylab("Residual") +
            theme(legend.title = element_blank())
        
        p

    })
    
    addPopover(session, "globalFit_resPlot",
               "Residuals plot",
               paste("This plot illustrates the residuals of the model",
                     "They should be distributed around the origin with constant variance.",
                     "The blue line shows a trend line, which may help identifying variations with respect to the ideal trend.",
                     sep = " "),
               trigger = "click", placement = "left", options = list(container = "body")
    )
     
    output$globalFit_resHist <- renderPlot({
        
        my_res <- residuals(globalFit_model()) 
        
        p <- my_res %>%
            ggplot(aes(x = res)) +
            geom_histogram(aes(y = ..density..)) +
            geom_function(fun = dnorm, args = list(mean = mean(my_res$res), sd = sd(my_res$res)),
                          linetype = 2, colour = "blue", size = 1) +
            xlab("Residual") +
            ylab("Frequency")
        
        if (input$globalFit_separate_hist) {
            p <- p + facet_wrap("exp", scales = "free_y")
        }
        
        p

    })

    addPopover(session, "globalFit_resHist",
               "Histogram of the residuals",
               paste("This plot shows a histogram of the residuals.",
                     "They should be normally distributed with mean zero",
                     "The blue line shows the pdf of a normal distribution with the same mean and variance as the residuals.",
                     "The histogram should adjust 'reasonably well' to the pdf.",
                     sep = " "),
               trigger = "click", placement = "left", options = list(container = "body")
    )
    
    output$globalFit_shapiro <- renderText({

        res <- residuals(globalFit_model())$res

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
    
    output$globalFit_MCMC_chain <- renderPlot({

        plot(globalFit_model()$fit_results)

    })

    addPopover(session, "globalFit_MCMC_chain",
               "Convergence of the Markov chain",
               paste("This plot shows the evolution of the Markov chain.",
                     "The plot should look like 'noise', without any obvious trend.",
                     "If it does not, it is recommended to increase the number of MC samples.",
                     "Alternatively, one could change the starting values or the fixed parameters.",
                     sep = " "),
               trigger = "click", placement = "left", options = list(container = "body")
    )

    output$globalFit_MCMC_pairs <- renderPlot({
        pairs(globalFit_model()$fit_results)
    })

    addPopover(session, "globalFit_MCMC_pairs",
               "Posterior distribution of the parameters",
               paste("This plot illustrates the posterior distribution of the model parameters.",
                     "The histograms and pairs plot should be 'smooth'. Otherwise, parameter estimates may be unreliable.",
                     sep = " "),
               trigger = "click", placement = "left", options = list(container = "body")
    )
    
    observeEvent(input$globalFit_seed, {
        print("Seed back to normal")
        set.seed(12412)
    })
    
    
}















