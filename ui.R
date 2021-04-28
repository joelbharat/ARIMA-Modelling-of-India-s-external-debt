
header <- dashboardHeader(
  title = "ARIMA MODELING OF INDIA'S EXTERNAL DEBT", titleWidth = 550
)

sidebar <- dashboardSidebar(
  width = 220,
  sidebarMenu(
    menuItem("Introduction", tabName = "intro_tab", icon = icon("dashboard"), selected = TRUE),
    menuItem("Description", icon = icon("th"), tabName = "description_tab"),
    menuItem("Data Table", tabName = "datatable_tab", icon = icon("table")),
    menuItem('Visualization', tabName = "visualization_tab", icon = icon('line-chart')),
    menuItem('ARIMA Modeling', tabName = "modeling_tab", icon=icon("dashboard")),
    menuItem("Forecast", tabName = "forecast_tab", icon = icon("line-chart"))
  )
)

body <- dashboardBody(
  shinyDashboardThemes(
    theme = "blue_gradient"
  ),

  tabItems(
    tabItem(tabName = "intro_tab",
            fluidPage(
              h1(span("A Short Introduction to ARIMA", style = "color:#008fb3")),
              fluidRow(
                column(width=8,
                       #tags$h3("A Short Introduction to ARIMA"),
                       tags$p(width=5,h4("ARIMA models are a popular and flexible class of forecasting model that utilize historical information to make predictions. 
                   This type of model is a basic forecasting technique that can be used as a foundation for more complex models.", align ="justify", style = "line-height:22px"),
                              h4("ARIMA stands for auto-regressive integrated moving average and is specified by these three order parameters: (p, d, q). 
                 ARIMA forecasting algorithm is based on the idea that the information in the past values of the time series, 
                 i.e  its own lags and the lagged forecast errors alone can be used to forecast future values. The process of fitting an 
                 ARIMA model is sometimes referred to as the Box-Jenkins method. ", align ="justify", style = "line-height:22px"), )),
                #column(width = 1),
                column(width=4, tags$img(src="1.png", height=230, width="85%" ))
                
              ),
              br(),
              br(),
              fluidRow(
                #column(width=7,tags$h2(width=5,"About Us"))
              ),
              fluidRow(
                column(width=5,tags$img(src="2.jpeg", height=270, width="100%")),
                br(), br(),
                column(width=7,tags$p(width=5,h4("An auto regressive (AR(p)) component is referring to the use of past values in the regression equation for the series Y. The 
               auto-regressive parameter p specifies the number of lags used in the model. ", align ="justify", style = "line-height:22px"), 
                                      h4("The d represents the degree of differencing in the integrated (I(d)) component. Differencing a series involves simply subtracting 
                 its current and previous values d times. Often, differencing is used to stabilize the series when the stationarity assumption is not 
                 met.", align ="justify", style = "line-height:22px"), 
                ))
              ),
              fluidRow(
                br(), br(),
                column(width=7,tags$p(style="text : 70;",width=5,
                                      h4("A moving average (MA(q)) component represents the error of the model as a combination of previous error terms. The order q determines 
                 the number of terms to include in the model. ", align ="justify", style = "line-height:22px"), 
                                      h4("ARIMA methodology does have its limitations. These models directly rely on past values, and therefore work best on long and stable 
                 series. Also note that ARIMA simply approximates historical patterns and therefore does not aim to explain the structure of the underlying 
                 data mechanism. ", align ="justify", style = "line-height:22px")
                )),
                column(width=5,tags$img(src="3.png",
                                        height=270,width="100%"))
              )
              
            )
    ),
    tabItem(tabName = "description_tab",
            fluidPage(
              h1(span("India's External Debt", style = "color:#008fb3")),
              h4("The external debt of India is the total debt the country owes to foreign creditors. The debtors can be the Union government, state governments, 
              corporations or citizens of India. The debt includes money owed to private commercial banks, foreign governments, or international financial institutions 
              such as the International Monetary Fund (IMF) and World Bank.",align ="justify", style = "line-height:22px"),
              h4("The Reserve Bank of India (RBI) has rich traditions of publishing data on various aspects of the Indian Economy through several of its publications. 
             Through this website (DBIE), data are mainly presented through time-series formatted reports. These reports have been organized under sectors and 
             sub-sectors according to their periodicities. The External Debt Dataset used in this project is a Time Series Data on India's external debt from 1991 
             to 2020. The dataset contains 64 features which have been categorised into 12 broad categories which are as given below along with a brief 
             description of each:- ", align ="justify", style = "line-height:22px"), 
              br(),
              h4("    (a)", span(strong (" Total Multilateral Debt."), style = "text-decoration: underline")," Multilateral debt is the money India owes to international 
              financial institutions such as the Asian Development Bank (ADB), the International Development Association (IDA), the International Bank for Reconstruction 
              and Development (IBRD), 
             the International Fund for Agricultural Development (IFAD) and others. Borrowing from the International Monetary Fund (IMF) are not included under
             multilateral debt, and are instead classified separately under the IMF head. The country's major creditors are the IDA, ADB, and IBRD. The IFAD 
             and a few other multilateral creditors hold the remaining portion of the multilateral debt.", align ="justify", style = "line-height:22px"),
              h4("     (b)", span(strong(" Total Bilateral Debt."), style = "text-decoration: underline"),"	Bilateral debt is the money India owes to foreign governments.", 
                 align ="justify", style = "line-height:22px"),
              h4("     (c)", span(strong(" IMF Loans."), style = "text-decoration: underline"),"		The International Monetary Fund, or IMF, 
              promotes international financial stability and monetary cooperation. It also facilitates international trade, promotes employment and sustainable economic 
              growth, and helps to reduce global poverty. The IMF is governed by and accountable to its 190 member countries",align ="justify", style = "line-height:22px"),
              h4("     (d)", span(strong(" Total Trade Credit."), style = "text-decoration: underline"),"	Trade credit is the amount of short-term loans provided by 
              suppliers to their customers upon purchase of their products. It is automatically created when the customers delay payment of bills to their suppliers.", 
                 align ="justify", style = "line-height:22px"),
              h4("     (e)", span(strong("	Total Commercial Borrowings."), style = "text-decoration: underline"),	"	Commercial Borrowing is the loan/ debt/ borrowings 
              taken by an eligible entity in India for commercial purpose, externally i.e. from any recognized entity outside India. However, these borrowings taken 
              must confirm with norms of the Reserve Bank of India (RBI).", align ="justify", style = "line-height:22px"),
              h4("     (f)", span(strong("	NRI and FC Deposit."), style = "text-decoration: underline"), "	These deposits are Foreign Currency Non-Resident Fixed Deposits 
              offered to NRIs as a significant way of attracting remittances from NRIs.  ", align ="justify", style = "line-height:22px"),
              h4("     (g)", span(strong(" Total Rupee Debt."), style = "text-decoration: underline"), "	Rupee debts are debts to be paid in rupees to foreigners by 
              Indian residents. Some debts can be paid in rupees.", align ="justify", style = "line-height:22px"),
              h4("     (h)", span(strong("	Total Long Term Debt."), style = "text-decoration: underline"),"	Long-term debt is debt that matures in more than one year. ", 
                 align ="justify", style = "line-height:22px"),
              h4("     (i)", span(strong("	Total Short Term Debt."), style = "text-decoration: underline"),"		Short-term debt includes all debt having an original maturity 
              of one year or less and interest in arrears on long-term debt.", align ="justify", style = "line-height:22px"),
              h4("     (j)", span(strong("  Gross Total Debt."), style = "text-decoration: underline"),"		Gross debt is the sum of Total Long Term Debt and Total 
              Short Term Debt.", align ="justify", style = "line-height:22px"),
              h4("     (k)", span(strong("  Debt Stock GDP Ratio."), style = "text-decoration: underline"),"	The debt-to-GDP ratio is the metric comparing a country's 
             public debt to its gross domestic product (GDP). By comparing what a country owes with what it produces, the debt-to-GDP ratio reliably indicates that 
             particular country's ability to pay back its debts. Often expressed as a percentage, this ratio can also be interpreted as the number of years needed to 
             pay back debt, if GDP is dedicated entirely to debt repayment. Because debt is a stock rather than a flow, it is measured as of a given date, usually the 
             last day of the fiscal year.", align ="justify", style = "line-height:22px"),
              h4("     (l)", span(strong("  Debt Service Ratio."), style = "text-decoration: underline"),"	A country's debt service ratio is the ratio of its debt 
              service payments (principal + interest) to its export earnings. A country's international finances are healthier when this ratio is low. For most countries 
              the ratio is between 0 and 20%.", align ="justify", style = "line-height:22px")
              #mainPanel
            ) #fluidPage        
    ),
    tabItem(tabName = "datatable_tab", dataTableOutput("tableData")),
    
    #Visualization tab
    tabItem(tabName = "visualization_tab", h1(span("Graphical Representation of Time Series Variables", style = "color:#008fb3")),
            br(),
            fluidRow(
              box(width = 8,
                  title = "Variable Selection", solidHeader = TRUE, status = "primary",
                  #background = "aqua",
                  
                  useShinyjs(), #Necessary to activate shinyjs
                  selectizeInput("select", "Variable/s:",
                              choices = c("Total Multilateral Debt" = '1', "Total Bilateral Debt" = '2', 
                                          "International Monetary Fund" = '3', "Total Trade Credit" = '4',
                                          "Total Commercial Borrowings" = '5', "NRI FC Deposits" = '6', 
                                          "Total Rupee Debt" = '7', "Total Long Term Debt" = '8', 
                                          "Total Short Term Debt" = '9', "Gross Total Debt" = '10',
                                          "Debt Stock GDP Ratio" = '11', "Debt Service Ratio" = '12'), 
                              multiple = TRUE)
                  
                  
              ),
            ),
            plotlyOutput("p1", width = "100%", height = "400px"),
            plotlyOutput("p2", width = "100%", height = "400px"),
            plotlyOutput("p3", width = "100%", height = "400px"),
            plotlyOutput("p4", width = "100%", height = "400px"),
            plotlyOutput("p5", width = "100%", height = "400px"),
            plotlyOutput("p6", width = "100%", height = "400px"),
            plotlyOutput("p7", width = "100%", height = "400px"),
            plotlyOutput("p8", width = "100%", height = "400px"),
            plotlyOutput("p9", width = "100%", height = "400px"),
            plotlyOutput("p10", width = "100%", height = "400px"),
            plotlyOutput("p11", width = "100%", height = "400px"),
            plotlyOutput("p12", width = "100%", height = "400px")
    ),
    
    #ARIMA modeling Tab
    tabItem(tabName = "modeling_tab", 
            fluidPage(
              h1(span("ARIMA Modeling", style = "color:#008fb3")),
              
              h3(span(strong("The process of modeling involves the following steps :-"), style = "text-decoration: underline"), align ="justify"),
              fluidRow(
                column(width=1),
                column(width=11,tags$h4("(a)	Plot, examine, and prepare series for modeling", align ="justify", style = "line-height:22px"),
                       h4("     (b)	Extract the seasonality component from the time series", align ="justify", style = "line-height:22px"),
                       h4("     (c)	Test for stationarity and apply appropriate transformations",align ="justify", style = "line-height:22px"),
                       h4("     (d)	Choose the order of an ARIMA model", align ="justify", style = "line-height:22px"),
                       h4("     (e)	Forecast the series", align ="justify", style = "line-height:22px"))
              ),
              br(),
              h3("     The activities at each step of the process, specific to the activities carried out with the RBI dataset, are as given below :-", align ="justify"),
              br(),
              h3(span(strong("Plot, Examine, And Prepare Series For Modeling."), style = "text-decoration: underline"), align ="justify"),
              fluidRow(
                column(width=1),
                column(width=11,tags$h4("(a)	The data was plotted and checked for its patterns and irregularities. No irregularities were detected.", align ="justify", style = "line-height:22px"),
                       h4("(b)	There were certain missing values which were imputed based on the values above and below them.", align ="justify", style = "line-height:22px"),
                )
              ),
              br(),
              h3(span(strong("Decompose the Data."), style = "text-decoration: underline"), align ="justify"),
              h4("     The data was checked for seasonality and trend. There was no seasonality or trend in the data.", align ="justify", style = "line-height:22px"),
              br(),
              h3(span(strong("Stationarity."), style = "text-decoration: underline"), align ="justify"),
              
              fluidRow(
                column(width=1),
                column(width=11,tags$h4("(a) The data was divided into train and test data in the 80:20 ratio.	", align ="justify", style = "line-height:22px"),
                       h4("(b) Stationarity of the data was checked using Augmented Dickey Fuller Test (ADF Test). All the variables of were non-stationary.", align ="justify", style = "line-height:22px"),
                       h4("(c) Differencing was carried out to make the data stationary. ADF Test was again carried out to ascertain the stationarity.", align ="justify", style = "line-height:22px"),
                       h4("(d) Their ACF and PACF plots were plotted to determine any residual autocorrelation. ", align ="justify", style = "line-height:22px"),
                )
              ),
              
              br(),
              
              h3(span(strong("Fitting a Model."), style = "text-decoration: underline"), align ="justify"),
              
              fluidRow(
                column(width=1),
                column(width=11,tags$h4("(a)	The inbuilt modeling function of R, auto.arima() was used to model each data series. Using this model, the train data was 
                 forecast into the test data period and the output compared with the test data.", align ="justify", style = "line-height:22px"),
                       h4("(b) Wherever fit was not satisfactory, several other models were tried out and the best fitting model was utilised. The model with 
                 the least Akaike Information Criteria (AIC) and Bayesian Information Criteria (BIC) was utilised.", align ="justify", style = "line-height:22px"),
                )
              ),
              
              br(),
              h3(span(strong("Forecasting."), style = "text-decoration: underline"), align ="justify"),
              h4("     Each data series was finally forecast into the future depending on the user input of the number of years for which the forecast 
                 value was sought.",  align ="justify", style = "line-height:22px"),
              br(),
              br()
              
            ) #fluidPage
    ), #tabItem
    
    #Forecast tab
    tabItem(tabName = "forecast_tab", h1("Forecasting Variables", style = "color:#008fb3"),
            fluidRow(
                box(width = 4, height = "120px",
                    title = "Select a Variable", solidHeader = TRUE, status = "primary",
                    useShinyjs(),
                    selectizeInput(
                      inputId = "selectForecastVar_id", label = "Variables:",
                      choices = c("Total Multilateral Debt" = '1', "Total Bilateral Debt" = '2', 
                                  "International Monetary Fund" = '3', "Total Trade Credit" = '4',
                                  "Total Commercial Borrowings" = '5', "NRI FC Deposits" = '6', 
                                  "Total Rupee Debt" = '7', "Total Long Term Debt" = '8', 
                                  "Total Short Term Debt" = '9', "Gross Total Debt" = '10',
                                  "Debt Stock GDP Ratio" = '11', "Debt Service Ratio" = '12'), 
                      multiple = FALSE,
                      selected = "Total Multilateral Debt"
                    )
                ),
                tags$div(box(width = 3, solidHeader = TRUE, status = "primary", height = "120px",
                             sliderInput(inputId = "year_input", "Select the number of years for forecast", min = 1, max = 10, value = 5), 
                             style="display:inline-block")),
                tags$div(box(width = 3, background = "aqua", height = "120px",
                             valueBoxOutput("forecast_value", width = "300px"), style="display:inline-block"))
            ),
            #plotOutput("forecast_plot1", width = "100%")
            plotOutput("f1", width = "100%", height = "400px"),
            plotOutput("f2", width = "100%", height = "400px"),
            plotOutput("f3", width = "100%", height = "400px"),
            plotOutput("f4", width = "100%", height = "400px"),
            plotOutput("f5", width = "100%", height = "400px"),
            plotOutput("f6", width = "100%", height = "400px"),
            plotOutput("f7", width = "100%", height = "400px"),
            plotOutput("f8", width = "100%", height = "400px"),
            plotOutput("f9", width = "100%", height = "400px"),
            plotOutput("f10", width = "100%", height = "400px"),
            plotOutput("f11", width = "100%", height = "400px"),
            plotOutput("f12", width = "100%", height = "400px")
    )
  )
)


# Put them together into a dashboardPage
dashboardPage(header,sidebar, body)
