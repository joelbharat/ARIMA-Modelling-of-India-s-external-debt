
server <- function(input, output, session) {
  
  dataset <- read.csv("C:/Users/joelb/R/R-India-External-Debt/Abridged_RBI_dataset.csv")

  #Dataframe to ts
  dataset_ts <- ts(dataset, start = 1991, end = 2020, frequency = 1)
  
  #Datatable
  output$tableData <- renderDataTable({
    head(dataset, 30) },
    options = list(scrollX=TRUE)
  )
  
  
  #Visualization Graphs
  output$p1 <- renderPlotly({ 
    timetk::plot_time_series(.data = dataset, .date_var = Year, .value = I_Total_Multilateral,
                       .interactive = FALSE, .y_lab = "Rupees in Crores", .x_lab = "Year",
                       .title = "TOTAL MULTILATERAL DEBT", .smooth_size = 0.2
    )
  })
  
  output$p2 <- renderPlotly({ 
    timetk::plot_time_series(.data = dataset, .date_var = Year, .value = II_Total_Bilateral,
                             .interactive = FALSE, .y_lab = "Rupees in Crores", .x_lab = "Year",
                             .title = "TOTAL BILATERAL DEBT", .smooth_size = 0.2)
  })
  
  output$p3 <- renderPlotly({ 
    timetk::plot_time_series(.data = dataset, .date_var = Year, .value = III_International_Monetary_Fund,
                             .interactive = FALSE, .y_lab = "Rupees in Crores", .x_lab = "Year",
                             .title = "INTERNATIONAL MONETARY FUND DEBT", .smooth_size = 0.2)
  
  })
  
  output$p4 <- renderPlotly({ 
    timetk::plot_time_series(.data = dataset, .date_var = Year, .value = IV_Total_Trade_Credit,
                             .interactive = FALSE, .y_lab = "Rupees in Crores", .x_lab = "Year",
                             .title = "TOTAL TRADE CREDIT", .smooth_size = 0.2)
    
  })
  
  output$p5 <- renderPlotly({ 
    timetk::plot_time_series(.data = dataset, .date_var = Year, .value = V_Total_Commercial_Borrowing,
                             .interactive = FALSE, .y_lab = "Rupees in Crores", .x_lab = "Year",
                             .title = "TOTAL COMMERCIAL BORROWINGS", .smooth_size = 0.2)
  })
  
  output$p6 <- renderPlotly({ 
    timetk::plot_time_series(.data = dataset, .date_var = Year, .value = VI_NRI_FC_Deposits,
                             .interactive = FALSE, .y_lab = "Rupees in Crores", .x_lab = "Year",
                             .title = "NRI AND FOREIGN CURRENCY DEPOSITS", .smooth_size = 0.2)
  })
  
  output$p7 <- renderPlotly({ 
    timetk::plot_time_series(.data = dataset, .date_var = Year, .value = VII_Total_Rupee_Debt,
                             .interactive = FALSE, .y_lab = "Rupees in Crores", .x_lab = "Year",
                             .title = "TOTAL RUPEE DEBT", .smooth_size = 0.2)
  })
  
  output$p8 <- renderPlotly({ 
    timetk::plot_time_series(.data = dataset, .date_var = Year, .value = VIII_Total_Long_Term_Debt,
                             .interactive = FALSE, .y_lab = "Rupees in Crores", .x_lab = "Year",
                             .title = "TOTAL LONG TERM DEBT", .smooth_size = 0.2)
  })
  
  output$p9 <- renderPlotly({
    timetk::plot_time_series(.data = dataset, .date_var = Year, .value = IX_Total_Short_term_Debt,
                             .interactive = FALSE, .y_lab = "Rupees in Crores", .x_lab = "Year",
                             .title = "TOTAL SHORT TERM DEBT", .smooth_size = 0.2)
  })
  
  output$p10 <- renderPlotly({ 
    timetk::plot_time_series(.data = dataset, .date_var = Year, .value = Gross_Total_Debt,
                             .interactive = FALSE, .y_lab = "Rupees in Crores", .x_lab = "Year",
                             .title = "GROSS TOTAL DEBT", .smooth_size = 0.2)
  })
  
  output$p11 <- renderPlotly({
    timetk::plot_time_series(.data = dataset, .date_var = Year, .value = Debt_Stock_GDP_Ratio,
                             .interactive = FALSE, .y_lab = "Ratio", .x_lab = "Year",
                             .title = "DEBT STOCK GDP RATIO", .smooth_size = 0.2)
  })
  
  output$p12 <- renderPlotly({ 
    timetk::plot_time_series(.data = dataset, .date_var = Year, .value = Debt_Service_Ratio,
                             .interactive = FALSE, .y_lab = "Ratio", .x_lab = "Year",
                             .title = "DEBT SERVICE RATIO", .smooth_size = 0.2)
  })

  observe({
    shinyjs::toggle("p1", condition = isTRUE(1 %in% input$select))
    shinyjs::toggle("p2", condition = isTRUE(2 %in% input$select))
    shinyjs::toggle("p3", condition = isTRUE(3 %in% input$select))
    shinyjs::toggle("p4", condition = isTRUE(4 %in% input$select))
    shinyjs::toggle("p5", condition = isTRUE(5 %in% input$select))
    shinyjs::toggle("p6", condition = isTRUE(6 %in% input$select))
    shinyjs::toggle("p7", condition = isTRUE(7 %in% input$select))
    shinyjs::toggle("p8", condition = isTRUE(8 %in% input$select))
    shinyjs::toggle("p9", condition = isTRUE(9 %in% input$select))
    shinyjs::toggle("p10", condition = isTRUE(10 %in% input$select))
    shinyjs::toggle("p11", condition = isTRUE(11 %in% input$select))
    shinyjs::toggle("p12", condition = isTRUE(12 %in% input$select))
  })
  #end of visualization plots
  
  #Forecast value
  output$forecast_value <-renderValueBox({
    if(input$selectForecastVar_id == 1 ){
      a=input$year_input
      if(a == 10){
        valueBox(700165.9,"Rupees in Crores")
      } else if(a == 9){
        valueBox(674598.2,"Rupees in Crores")
      } else if(a == 8){
        valueBox(649030.6,"Rupees in Crores")
      } else if(a == 7){
        valueBox(623462.9,"Rupees in Crores")
      } else if(a == 6){
        valueBox(597895.3,"Rupees in Crores")
      } else if(a == 5){
        valueBox(572327.6,"Rupees in Crores")
      } else if(a == 4){
        valueBox(546759.9,"Rupees in Crores")
      } else if(a == 3){
        valueBox(521192.3,"Rupees in Crores")
      } else if(a == 2){
        valueBox(495624.6,"Rupees in Crores")
      } else if(a == 1){
        valueBox(470057.0,"Rupees in Crores")
      }
      
    } else if(input$selectForecastVar_id == 2 ){
      a=input$year_input
      if(a == 10){
        valueBox(247927.2 ,"Rupees in Crores")
      } else if(a == 9){
        valueBox(243287.9,"Rupees in Crores")
      } else if(a == 8){
        valueBox(238648.6,"Rupees in Crores")
      } else if(a == 7){
        valueBox(234009.3,"Rupees in Crores")
      } else if(a == 6){
        valueBox(229370.0,"Rupees in Crores")
      } else if(a == 5){
        valueBox(224730.6,"Rupees in Crores")
      } else if(a == 4){
        valueBox(220091.3,"Rupees in Crores")
      } else if(a == 3){
        valueBox(215452.0,"Rupees in Crores")
      } else if(a == 2){
        valueBox(210812.7,"Rupees in Crores")
      } else if(a == 1){
        valueBox(206173.4,"Rupees in Crores")
      }
      
    } else if(input$selectForecastVar_id == 3 ){
      a=input$year_input
      if(a == 10){
        valueBox(40930.73 ,"Rupees in Crores")
      } else if(a == 9){
        valueBox(40930.73,"Rupees in Crores")
      } else if(a == 8){
        valueBox(40930.73,"Rupees in Crores")
      } else if(a == 7){
        valueBox(40930.73 ,"Rupees in Crores")
      } else if(a == 6){
        valueBox(40930.73 ,"Rupees in Crores")
      } else if(a == 5){
        valueBox(40930.73 ,"Rupees in Crores")
      } else if(a == 4){
        valueBox(40930.73,"Rupees in Crores")
      } else if(a == 3){
        valueBox(40930.73,"Rupees in Crores")
      } else if(a == 2){
        valueBox(40930.73,"Rupees in Crores")
      } else if(a == 1){
        valueBox(40930.73 ,"Rupees in Crores")
      }
      
    } else if(input$selectForecastVar_id == 4 ){
      a=input$year_input
      if(a == 10){
        valueBox(33844.350,"Rupees in Crores")
      } else if(a == 9){
        valueBox(35818.239,"Rupees in Crores")
      } else if(a == 8){
        valueBox( 37792.127,"Rupees in Crores")
      } else if(a == 7){
        valueBox(39766.016,"Rupees in Crores")
      } else if(a == 6){
        valueBox(41739.905,"Rupees in Crores")
      } else if(a == 5){
        valueBox(43713.794,"Rupees in Crores")
      } else if(a == 4){
        valueBox(45687.682,"Rupees in Crores")
      } else if(a == 3){
        valueBox(47661.571,"Rupees in Crores")
      } else if(a == 2){
        valueBox(49635.460,"Rupees in Crores")
      } else if(a == 1){
        valueBox(51609.348,"Rupees in Crores")
      }
      
    } else if(input$selectForecastVar_id == 5 ){
      a=input$year_input
      if(a == 10){
        valueBox(3173068,"Rupees in Crores")
      } else if(a == 9){
        valueBox(3021375,"Rupees in Crores")
      } else if(a == 8){
        valueBox(2869682,"Rupees in Crores")
      } else if(a == 7){
        valueBox(2717989,"Rupees in Crores")
      } else if(a == 6){
        valueBox(2566296,"Rupees in Crores")
      } else if(a == 5){
        valueBox(2414603,"Rupees in Crores")
      } else if(a == 4){
        valueBox(2262910,"Rupees in Crores")
      } else if(a == 3){
        valueBox(2111217,"Rupees in Crores")
      } else if(a == 2){
        valueBox(1959524,"Rupees in Crores")
      } else if(a == 1){
        valueBox(1807831,"Rupees in Crores")
      }
      
    } else if(input$selectForecastVar_id == 6 ){
      a=input$year_input
      if(a == 10){
        valueBox(1809674,"Rupees in Crores")
      } else if(a == 9){
        valueBox(1726245,"Rupees in Crores")
      } else if(a == 8){
        valueBox(1642816,"Rupees in Crores")
      } else if(a == 7){
        valueBox(1559387,"Rupees in Crores")
      } else if(a == 6){
        valueBox(1475958,"Rupees in Crores")
      } else if(a == 5){
        valueBox(1392529,"Rupees in Crores")
      } else if(a == 4){
        valueBox(1309099,"Rupees in Crores")
      } else if(a == 3){
        valueBox(1225670,"Rupees in Crores")
      } else if(a == 2){
        valueBox(1142241,"Rupees in Crores")
      } else if(a == 1){
        valueBox(1058812,"Rupees in Crores")
      }
      
    } else if(input$selectForecastVar_id == 7 ){
      a=input$year_input
      if(a == 10){
        valueBox(7704.03,"Rupees in Crores")
      } else if(a == 9){
        valueBox(7704.03,"Rupees in Crores")
      } else if(a == 8){
        valueBox(7704.03,"Rupees in Crores")
      } else if(a == 7){
        valueBox(7704.03,"Rupees in Crores")
      } else if(a == 6){
        valueBox(7704.03,"Rupees in Crores")
      } else if(a == 5){
        valueBox(7704.03,"Rupees in Crores")
      } else if(a == 4){
        valueBox(7704.03,"Rupees in Crores")
      } else if(a == 3){
        valueBox(7704.03,"Rupees in Crores")
      } else if(a == 2){
        valueBox(7704.03,"Rupees in Crores")
      } else if(a == 1){
        valueBox(7704.03,"Rupees in Crores")
      }
      
    }else if(input$selectForecastVar_id == 8 ){
      a=input$year_input
      if(a == 10){
        valueBox( 5702051,"Rupees in Crores")
      } else if(a == 9){
        valueBox(5463700 ,"Rupees in Crores")
      } else if(a == 8){
        valueBox(5225350 ,"Rupees in Crores")
      } else if(a == 7){
        valueBox(4987000 ,"Rupees in Crores")
      } else if(a == 6){
        valueBox(4748649,"Rupees in Crores")
      } else if(a == 5){
        valueBox(4510299,"Rupees in Crores")
      } else if(a == 4){
        valueBox(4271949 ,"Rupees in Crores")
      } else if(a == 3){
        valueBox(4033598,"Rupees in Crores")
      } else if(a == 2){
        valueBox(3795248,"Rupees in Crores")
      } else if(a == 1){
        valueBox(3556898,"Rupees in Crores")
      }
      
    } else if(input$selectForecastVar_id == 9 ){
      a=input$year_input
      if(a == 10){
        valueBox(1381115.4,"Rupees in Crores")
      } else if(a == 9){
        valueBox(1323574.7,"Rupees in Crores")
      } else if(a == 8){
        valueBox(1266034.0,"Rupees in Crores")
      } else if(a == 7){
        valueBox(1208493.2,"Rupees in Crores")
      } else if(a == 6){
        valueBox(1150952.5,"Rupees in Crores")
      } else if(a == 5){
        valueBox(1093411.8,"Rupees in Crores")
      } else if(a == 4){
        valueBox(1035871.1,"Rupees in Crores")
      } else if(a == 3){
        valueBox(978330.4,"Rupees in Crores")
      } else if(a == 2){
        valueBox(920789.7,"Rupees in Crores")
      } else if(a == 1){
        valueBox(863249.0,"Rupees in Crores")
      }
      
    } else if(input$selectForecastVar_id == 10 ){
      a=input$year_input
      if(a == 10){
        valueBox(7174041," Rupees in Crores")
      } else if(a == 9){
        valueBox(6866565,"Rupees in Crores")
      } else if(a == 8){
        valueBox(6559090,"Rupees in Crores")
      } else if(a == 7){
        valueBox(6251614,"Rupees in Crores")
      } else if(a == 6){
        valueBox(5944138,"Rupees in Crores")
      } else if(a == 5){
        valueBox(5636663,"Rupees in Crores")
      } else if(a == 4){
        valueBox(5329187,"Rupees in Crores")
      } else if(a == 3){
        valueBox(5021711,"Rupees in Crores")
      } else if(a == 2){
        valueBox(4714236,"Rupees in Crores")
      } else if(a == 1){
        valueBox(4406760,"Rupees in Crores")
      }
    }else if(input$selectForecastVar_id == 11 ){
      a=input$year_input
      if(a == 10){
        valueBox(20.64191,"")
      } else if(a == 9){
        valueBox(20.64191,"")
      } else if(a == 8){
        valueBox(20.64191,"")
      } else if(a == 7){
        valueBox(20.64191,"")
      } else if(a == 6){
        valueBox(20.64191,"")
      } else if(a == 5){
        valueBox(20.64191,"")
      } else if(a == 4){
        valueBox(20.64191,"")
      } else if(a == 3){
        valueBox(20.64191,"")
      } else if(a == 2){
        valueBox(20.64191,"")
      } else if(a == 1){
        valueBox(20.64191,"")
      }
    } else if(input$selectForecastVar_id == 12 ){
      a=input$year_input
      if(a == 10){
        valueBox(6.549599,"")
      } else if(a == 9){
        valueBox(6.549599,"")
      } else if(a == 8){
        valueBox(6.549599,"")
      } else if(a == 7){
        valueBox(6.549599,"")
      } else if(a == 6){
        valueBox(6.549599,"")
      } else if(a == 5){
        valueBox(6.549599,"")
      } else if(a == 4){
        valueBox(6.549599,"")
      } else if(a == 3){
        valueBox(6.549599,"")
      } else if(a == 2){
        valueBox(6.549599,"")
      } else if(a == 1){
        valueBox(6.549599,"")
      }
    }
  })
  
  
  #Forecast plots
  output$f1 <- renderPlot({ 
    multilat <- pull(dataset, I_Total_Multilateral)
    multilat_ts <- ts(multilat, start=1991)
    fcast <- forecast(multilat_ts, h=input$year_input)
    #plot(fcast)
    plot(fcast, type="b", lwd=5, col='blue', main="FORECAST OF TOTAL MULTILATERAL DEBT",
         ylab="Rupees in Crores", xlab="Years")
    legend("topleft",lty=1,bty = "n",col=c("blue"),c("Forecast"))
  })
  
  output$f2 <- renderPlot({ 
    bilat <- pull(dataset, II_Total_Bilateral)
    bilat_ts <- ts(bilat, start=1991)
    fcast_bilat <- forecast(bilat_ts, h=input$year_input)
    #plot(fcast_bilat)
    plot(fcast_bilat, type="b", lwd=5, col='blue', main="FORECAST OF TOTAL BILATERAL DEBT", ylab="Rupees in Crores", xlab="Years")
    legend("topleft",lty=1,bty = "n",col=c("blue"),c("Forecast"))
  })
  
  output$f3 <- renderPlot({ 
    imf <- pull(dataset, III_International_Monetary_Fund)
    imf_ts <- ts(imf, start=1991)
    fcast_imf <- forecast(imf_ts, h=input$year_input)
    #plot(fcast_imf)
    plot(fcast_imf, type="b", lwd=5, col='blue', main="FORECAST OF IMF DEBT", ylab="Rupees in Crores", xlab="Years")
    legend("topleft",lty=1,bty = "n",col=c("blue"),c("Forecast"))
  })
  
  output$f4 <- renderPlot({ 
    ttc <- pull(dataset, IV_Total_Trade_Credit)
    ttc_ts <- ts(ttc, start=1991)
    fcast_ttc <- forecast(ttc_ts, h=input$year_input)
    #plot(fcast_ttc)
    plot(fcast_ttc, type="b", lwd=5, col='blue', main="FORECAST OF TOTAL TRADE CREDIT", ylab="Rupees in Crores", xlab="Years")
    legend("topleft",lty=1,bty = "n",col=c("blue"),c("Forecast"))
  })
  
  output$f5 <- renderPlot({ 
    tcb <- pull(dataset, V_Total_Commercial_Borrowing)
    tcb_ts <- ts(tcb, start=1991)
    fcast_tcb <- forecast(tcb_ts, h=input$year_input)
    #plot(fcast_tcb)
    plot(fcast_tcb, type="b", lwd=5, col='blue', main="FORECAST OF TOTAL COMMERCIAL BORROWINGS", ylab="Rupees in Crores", xlab="Years")
    legend("topleft",lty=1,bty = "n",col=c("blue"),c("Forecast"))
  })
  
  output$f6 <- renderPlot({
    nri <- pull(dataset, VI_NRI_FC_Deposits)
    nri_ts <- ts(nri, start=1991)
    fcast_nri <- forecast(nri_ts, h=input$year_input)
    #plot(fcast_nri)
    plot(fcast_nri, type="b", lwd=5, col='blue', main="FORECAST OF NRI AND FOREIGN CURRENCY DEPOSITS", ylab="Rupees in Crores", xlab="Years")
    legend("topleft",lty=1,bty = "n",col=c("blue"),c("Forecast"))
  })
  
  output$f7 <- renderPlot({
    trd <- pull(dataset, VII_Total_Rupee_Debt)
    trd_ts <- ts(trd, start=1991)
    fcast_trd <- forecast(trd_ts, h=input$year_input)
    #plot(fcast_trd)
    plot(fcast_trd, type="b", lwd=5, col='blue', main="FORECAST OF TOTAL RUPEE DEBT", ylab="Rupees in Crores", xlab="Years")
    legend("bottomleft",lty=1,bty = "n",col=c("blue"),c("Forecast"))
  })
  
  output$f8 <- renderPlot({
    tltd <- pull(dataset, VIII_Total_Long_Term_Debt)
    tltd_ts <- ts(tltd, start=1991)
    fcast_tltd <-forecast(tltd_ts, h=input$year_input)
    #plot(fcast_tltd)
    plot(fcast_tltd, type="b", lwd=5, col='blue', main="FORECAST OF TOTAL LONG TERM DEBT", ylab="Rupees in Crores", xlab="Years")
    legend("topleft",lty=1,bty = "n",col=c("blue"),c("Forecast"))
  })
  
  output$f9 <- renderPlot({
    tstd <- pull(dataset, IX_Total_Short_term_Debt)
    tstd_ts <- ts(tstd, start=1991)
    fcast_tstd <- forecast(tstd_ts, h=input$year_input)
    #plot(fcast_tstd)
    plot(fcast_tstd, type="b", lwd=5, col='blue', main="FORECAST OF TOTAL SHORT TERM DEBT", ylab="Rupees in Crores", xlab="Years")
    legend("topleft",lty=1,bty = "n",col=c("blue"),c("Forecast"))
  })
  
  output$f10 <- renderPlot({
    gtd <- pull(dataset, Gross_Total_Debt)
    gtd_ts <- ts(gtd, start=1991)
    fcast_gtd <- forecast(gtd_ts, h=input$year_input)
    #plot(fcast_gts)
    plot(fcast_gtd, type="b", lwd=5, col='blue', main="FORECAST OF GROSS TOTAL DEBT", ylab="Rupees in Crores", xlab="Years")
    legend("topleft",lty=1,bty = "n",col=c("blue"),c("Forecast"))
  })
  
  output$f11 <- renderPlot({
    dsgratio <- pull(dataset, Debt_Stock_GDP_Ratio)
    dsgratio_ts <- ts(dsgratio, start = 1991)
    fcast_dsgr <- forecast(dsgratio_ts, h = input$year_input)
    #plot(fcast_gts)
    plot(fcast_dsgr, type="b", lwd=5, col='blue', main="FORECAST OF DEBT STOCK GDP RATIO", ylab="Ratio", xlab="Years")
    legend("topleft",lty=1,bty = "n",col=c("blue"),c("Forecast"))
  })
  
  output$f12 <- renderPlot({
    dsratio <- pull(dataset, Debt_Service_Ratio)
    dsratio_ts <- ts(dsratio, start = 1991)
    fcast_dsr <- forecast(dsratio_ts, h = input$year_input)
    #plot(fcast_dsr)
    plot(fcast_dsr, type="b", lwd=5, col='blue', main="FORECAST OF DEBT SERVICE RATIO", ylab="Ratio", xlab="Years")
    legend("topleft",lty=1,bty = "n",col=c("blue"),c("Forecast"))
  })
  
  observe({
    shinyjs::toggle("f1", condition = isTRUE(1 %in% input$selectForecastVar_id))
    shinyjs::toggle("f2", condition = isTRUE(2 %in% input$selectForecastVar_id))
    shinyjs::toggle("f3", condition = isTRUE(3 %in% input$selectForecastVar_id))
    shinyjs::toggle("f4", condition = isTRUE(4 %in% input$selectForecastVar_id))
    shinyjs::toggle("f5", condition = isTRUE(5 %in% input$selectForecastVar_id))
    shinyjs::toggle("f6", condition = isTRUE(6 %in% input$selectForecastVar_id))
    shinyjs::toggle("f7", condition = isTRUE(7 %in% input$selectForecastVar_id))
    shinyjs::toggle("f8", condition = isTRUE(8 %in% input$selectForecastVar_id))
    shinyjs::toggle("f9", condition = isTRUE(9 %in% input$selectForecastVar_id))
    shinyjs::toggle("f10", condition = isTRUE(10 %in% input$selectForecastVar_id))
    shinyjs::toggle("f11", condition = isTRUE(11 %in% input$selectForecastVar_id))
    shinyjs::toggle("f12", condition = isTRUE(12 %in% input$selectForecastVar_id))
    
  })
  

}#end-server