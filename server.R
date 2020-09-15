
library(tidyverse)
library(biogrowth)

server <- function(input, output) {
    
    ## Static predictions
    
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
    
    ## Stochastic static predictions
    
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
}





























