
#invoke shiny libraries
library(shiny)
library(shinydashboard)


ui <- dashboardPage(skin = "green", title = "Synthetic data generator",
                    dashboardHeader(title = h4("Synthetic data generator"), 
                                    
                                    tags$li(a(href = 'https://github.com/SidharthMacherla/conjurer',
                                              icon("github"), title = "Go to source code"),
                                              class = "dropdown"),
                                    tags$li(a(href = 'https://www.foyi.co.nz/posts/documentation/documentationconjurer/',
                                              icon("book"), title = "Go to documentation"),
                                            class = "dropdown")
                                    ),#ends dashboard header
                    
                    dashboardSidebar(
                      sidebarMenu(
                        menuItem("Generate data", tabName = "tab1", icon = icon("cogs"))
                        
                        
                      )#ends sidebarMenu
                      
                    ),#ends dashboardSidebar
                    dashboardBody(
                      tabItems(
                        #&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
                        #UI Side: first tab
                        #&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
                        tabItem(tabName = "tab1",
                                fluidRow(
                                  box(title = NULL, status = "primary", solidHeader = TRUE, width = 5,
                                      h5(strong("Question: How many customers do you need?")),
                                      tags$i("Please select atleast 10 customers."),
                                      numericInput(inputId = "custCnt", label = NULL, 
                                                   value = 1000, step = 1, width = '25%'),
                                      hr(),
                                      
                                      h5(strong("Question: How many products do you need and what 
                                         should the price range be?")),
                                      tags$i("Please select atleast 10 products"),
                                      numericInput(inputId = "prodCnt", label = "# Of products", 
                                                   value = 30, step = 1, width = '25%'),
                                      
                                      numericInput(inputId = "maxPrice", label = "Max price of product", 
                                                   value = 100, step = 1, width = '25%'),
                                      tags$i("The Max price above will set the maximum value 
                                             in the slider below"),
                                      
                                      sliderInput(inputId = "prodPriceRange", label = "$Price range", 
                                                  min = 1, max = 100, value = c(5,35),step = 1, 
                                                  width = '50%', sep = ","),
                                      hr(),
                                      
                                      h5(strong("Question: Roughly, how many transactions should 
                                                be there for the year?")),
                                      numericInput(inputId = "txnCnt", label = NULL, 
                                                   value = 20000, step = 1, width = '25%')
                                      ),#ends 1st box
                                  
                                  box(title = NULL, status = "primary", solidHeader = TRUE, width = 7,
                                      
                                      h5(strong("Question: Is the seasonality monthly, quarterly or yearly?
                                      For example, if seasonality is monthly, then the number of 
                                      transactions will be high at the start of the month and 
                                      gradually decrease towards the end.")),
                                      radioButtons(inputId = "season", label = NULL, 
                                                   choices = c("Monthly", "Quarterly", "Yearly"),
                                                   selected = c("Yearly"), inline = TRUE,
                                                   width = '50%' 
                                                         ),#ends seasonality button
                                      hr(),
                                      
                                      h5(strong("Question: Should the number of transaction increase over 
                                         the year or decrease?")),
                                      radioButtons(inputId = "trendValue", label = NULL, 
                                                   choices = c("Increase", "Decrease"),
                                                   selected = c("Increase"), inline = TRUE,
                                                   width = '50%' 
                                      ),#ends trend button
                                      hr(),
                                      
                                      h5(strong("Question: Shoud there be a few outliers to make the 
                                         data seem more real' ?")),
                                      radioButtons(inputId = "outlierPresent", label = NULL, 
                                                   choices = c("Yes", "No"),
                                                   selected = c("Yes"), inline = TRUE,
                                                   width = '50%' 
                                      ),#ends trend button
                                      hr(),
                                      
                                      h5(strong("Question: Select what % of products must contribute
                                         to what % of transactions? For example, if you choose
                                         10% below, then 10% products will contribute to 90%
                                         transactions.")),
                                        numericInput(inputId = "paretoProd", label = "%Products", 
                                                     value = 10,min = 1,max = 99,step = 1, width = '25%'),
                                      hr(),
                                      
                                      h5(strong("Question: Select what % of customers must contribute
                                         to what % of transactions? For example, if you choose
                                         20% below, then 20% customers will contribute to 80%
                                         transactions.")),
                                      numericInput(inputId = "paretoCust", label = "%Customers", 
                                                   value = 10,min = 1,max = 99,step = 1, width = '25%')
                                      )#ends second box
                                  
                                ),#ends fluid row
                                fluidRow(
                                  box(title = NULL, status = "primary", width = 12, solidHeader = FALSE,
                                      actionButton(inputId = "generateDataButton", label = "Generate data", 
                                                   icon("paper-plane"), 
                                                   style="color: white; background-color: steelblue;  
                                                   border-color: steelblue"), br(),
                                      uiOutput("successDataGenerated"),
                                      downloadLink('downloadData', 'Download data in csv format')
                                      )#ends step2 box
                                )#ends fluidRow
                        )#ends firsttab
                    
                    )))


server <- function(input, output) {
  #&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
  #Server Side: Global variables
  #&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&  
  #set file size limit
  options(shiny.maxRequestSize=100*1024^2)
  
  #instantite global variables as NULL. These will be updated as the user sets their values
  globals <- reactiveValues(numOfCust = NULL, prodCnt = NULL, maxPriceLimit = NULL, 
                            minPrice = NULL, maxPrice = NULL, numOfTxn = NULL, season = NULL,
                            trendValue = NULL, outlierPresent = NULL, paretoProd = NULL,
                            dfFinal = NULL)
  
  
  #Set global variables
  observeEvent(input$custCnt, {globals$custCnt <- input$custCnt})
  observeEvent(input$prodCnt, {globals$prodCnt <- input$prodCnt})
  observeEvent(input$maxPrice, {globals$maxPriceLimit <- input$maxPrice})
  observeEvent(input$prodPriceRange, 
               {globals$minPrice <- input$prodPriceRange[1]
                globals$maxPrice <- input$prodPriceRange[2]
               })
  observeEvent(input$numOfTxn, {globals$numOfTxn <- input$numOfTxn})
  observeEvent(input$season, 
               {globals$season <- ifelse(globals$season == "Monthly", "m", ifelse(globals$season == "Quarterly", "q", "y"))       })
  observeEvent(input$trendValue, 
               {globals$trendValue <- ifelse(input$trendValue == "Increase", 1, -1)
               })
  observeEvent(input$outlierPresent,
               {
                 globals$outlierPresent <- ifelse(input$outlierPresent == "Yes", 1, 0)
               })
  observeEvent(input$paretoProd,{globals$paretoProd <- input$paretoProd})
  
  
  observeEvent(input$generateDataButton,
               {
                 globals$paretoProd <- input$paretoProd
               })
  
  observeEvent(input$generateDataButton,
               {
                 #remove any existing data and generate data
                 globals$dfFinal <- NULL
                 
                 #generate customers
                 customers <- conjurer::buildCust(numOfCust =  globals$custCnt)
                 
                 #generate customer names
                 custNames <- as.data.frame(conjurer::buildNames(numOfNames = globals$custCnt, 
                                                       minLength = 5, maxLength = 7))
                 
                 #set column heading
                 colnames(custNames) <- c("customerName")
                 
                 #assign customer names to customers
                 customer2name <- cbind(customers, custNames)
                 
                 #generate products
                 products <- conjurer::buildProd(numOfProd = globals$prodCnt, minPrice = globals$minPrice, 
                                       maxPrice = globals$maxPrice)
                 
                 #generate transactions
                 transactions <- conjurer::genTrans(cycles = "y", spike = 12, outliers = 1, transactions = 20000)
                 
                 #map customers to transactions
                 customer2transaction <- conjurer::buildPareto(customers, transactions$transactionID, 
                                                     pareto = c(globals$paretoProd,
                                                                (100-globals$paretoProd)))
                 
                 #set names
                 names(customer2transaction) <- c('transactionID', 'customer')
                 
                 #map products to transactions
                 product2transaction <- conjurer::buildPareto(products$SKU,transactions$transactionID,
                                                    pareto =  c(globals$paretoProd,
                                                                (100-globals$paretoProd)))
                 #set names
                 names(product2transaction) <- c('transactionID', 'SKU')
                 
                 #combine customer and transaction data
                 df1 <- merge(x = customer2transaction, y = product2transaction, by = "transactionID")
                 
                 df2 <- merge(x = df1, y = transactions, by = "transactionID", all.x = TRUE)
                 
                 #add customer names data
                 dfFinal <- merge(x = df2, y = customer2name, by.x = "customer", 
                                  by.y = "customers", all.x = TRUE)
                 
                 #rename columns for readability reasons
                 names(dfFinal) <- c("Customer ID", "Transaction ID", "SKU", "Day(1to365)", 
                                     "Month(1to12)", "Customer name")
                 
                 #update global variable
                 globals$dfFinal <- dfFinal
                 
                 output$successDataGenerated <- renderText(paste(as.character(icon("check")), 
                                                                    "Done. Click on the link below to download csv", sep = " "))
                 
                 
               })#ends generateDataButton
  
  #Download forecasted data that is reshaped
  output$downloadData <- downloadHandler(
    filename = function() {
      paste('syntheticData', '.csv', sep='')
    },
    content = function(con) {
      write.csv(globals$dfFinal, con, row.names = FALSE)
    }
  )
  
  #print(customers)
   
   
}

# Run the application 
shinyApp(ui = ui, server = server)

