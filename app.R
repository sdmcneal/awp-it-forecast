#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(DT)
library(xlsx)
library(timeDate)
library(reshape)
library(ggplot2)
library(tidyr)
library(dplyr)
library(ggpubr)
library(RColorBrewer)
library(scales)
library(data.table)

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("AWP US IT Financial Forecast"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
        sliderInput("benefitRateValue",
                    "Benefit Multiplier",
                    min = 1.0,
                    max = 2.0,
                    value = 1.55),
         sliderInput("supportSowMonthlyValue",
                     "Production Support SOW Monthly Value: ($k)",
                     min = 0,
                     max = 200,
                     value = 75),
         
         sliderInput("supportStartDate", 
                     "Start of Support SOW", 
                     min = as.Date("2019-01-01"),
                     max =as.Date("2019-12-01"),
                     value=as.Date("2019-09-01"),
                     timeFormat="%b"),
         
        #IOT
        sliderInput("iotSowMonthlyValue",
                    "IoT SOW Monthly: ($k)",
                    min = 0,
                    max = 500,
                    value = 100),
        
        sliderInput("iotDateRange", "IoT SOW Scope Duration", 
                    min = as.Date("2019-01-01"),
                    max =as.Date("2019-12-01"),
                    value=c(as.Date("2019-06-01"),as.Date("2019-12-01")),
                    dragRange=TRUE,timeFormat="%b"),
        #CPQ
        sliderInput("cpqSowMonthlyValue",
                    "CPQ & Customer Journey SOW Monthly Value: ($k)",
                    min = 0,
                    max = 500,
                    value = 100),
        
        sliderInput("cpqDateRange", "CPQ & Customer Journey SOW Duration", 
                    min = as.Date("2019-01-01"),
                    max =as.Date("2019-12-01"),
                    value=c(as.Date("2019-07-01"),as.Date("2019-12-01")),
                    dragRange=TRUE,timeFormat="%b"),
        
        # Start dates
        sliderInput("customerDirectorStartDate", 
                    "Start of Customer Experience Director", 
                    min = as.Date("2019-01-01"),
                    max =as.Date("2019-12-01"),
                    value=as.Date("2019-09-01"),
                    timeFormat="%b"),
        sliderInput("manufacturingDirectorStartDate", 
                    "Start of Manufacturing Director", 
                    min = as.Date("2019-01-01"),
                    max =as.Date("2019-12-01"),
                    value=as.Date("2019-09-01"),
                    timeFormat="%b"),
        sliderInput("dataDirectorStartDate", 
                    "Start of Digital Products Director", 
                    min = as.Date("2019-01-01"),
                    max =as.Date("2019-12-01"),
                    value=as.Date("2019-09-01"),
                    timeFormat="%b"),
        textInput("filename","Filename","tableau_source.csv"),
        actionButton("export","Export Tableau Source")
        
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
         #
        plotOutput("plot"),
        fluidRow(
          column(6,
                 dataTableOutput("summaryTable")),
          column(6,
                 dataTableOutput('sowTotals'))),
        textOutput("totalText"),
        dataTableOutput("scopeByMonth")
        
      )
   )
)

# Load csv
importFile <- function(filename, tabname) {
  read.xlsx( filename, tabname)
}

importResourceMaster <- function(sources, periods, salary.multiplier) {
  
  i.df <- copy(sources$rates.source.df)
  
  max.length <- 1000
  cost.rates.df <- data.frame(resource_id=integer(max.length),
                              name=character(max.length),
                              month=as.Date(rep(NA,max.length), origin = "1970-01-01"),
                              monthly_cost=double(max.length),
                              account=character(max.length), 
                              stringsAsFactors=FALSE)
  # cri = cost.rate.index
  cri <- 1
  
  for (row in 1:nrow(i.df)) {
    id <- i.df[row,"resource_id"]
    name <- as.character(i.df[row,"name"])
    startdate <- i.df[row,"start_date"]
    enddate <- i.df[row,"end_date"]
    rate <- i.df[row,"monthly_rate"]
    account <- as.character(i.df[row,"account"])
    
    
    applicabledates <- periods[ (periods >= startdate) &
                                  (periods <= enddate)]
    
    for (date in applicabledates) {
      cost.rates.df$resource_id[cri] <- id
      cost.rates.df$name[cri] <- name
      cost.rates.df$month[cri] <- as.Date(date, origin = "1970-01-01")
      
      cost.rates.df$account[cri] <- account
      if (account == "salary") {
        cost.rates.df$monthly_cost[cri] <- rate * salary.multiplier
      } else {
        cost.rates.df$monthly_cost[cri] <- rate
      }
      
      cri <- cri + 1
    } #end applicable dates
    
  } # end cost rates source
  return (cost.rates.df[!is.na(cost.rates.df$month),])
}


processResourceScope <- function(resource.scope.source.df, periods) {
  
  max.length <- 1000
  resource.scope.df <- data.frame(model=character(max.length),
                                  resource_id=integer(max.length),
                                  name=character(max.length),
                                  month=as.Date(rep(NA,max.length), origin = "1970-01-01"),
                                  scope=character(max.length), 
                                  percent=double(max.length),
                                  stringsAsFactors=FALSE)
  # cri = cost.rate.index
  cri <- 1
  
  for (row in 1:nrow(resource.scope.source.df)) {
    model <- as.character(resource.scope.source.df[row,"model"])
    id <- resource.scope.source.df[row,"resource_id"]
    name <- as.character(resource.scope.source.df[row,"name"])
    startdate <- resource.scope.source.df[row,"start_date"]
    enddate <- resource.scope.source.df[row,"end_date"]
    scope <- as.character(resource.scope.source.df[row,"scope"])
    percent <- resource.scope.source.df[row,"percent"]
    
    applicabledates <- periods[ (periods >= startdate) &
                                  (periods <= enddate)]
    
    for (date in applicabledates) {
      resource.scope.df$model[cri] <- model
      resource.scope.df$resource_id[cri] <- id
      resource.scope.df$name[cri] <- name
      resource.scope.df$month[cri] <- as.Date(date, origin = "1970-01-01")
      resource.scope.df$scope[cri] <- scope
      resource.scope.df$percent[cri] <- percent
      
      cri <- cri + 1
    } #end applicable dates
    
  } # end cost rates source
  
  scope.levels <- c("Customer Journey","Industry 4.0 and Telematics Analytics", "Telematics", "Commercial Excellence: Price Performance",
                    "Application Development", "Process Improvement","EDI", "RFID","Production Support","Production Support (Outsourced)")
  
  resource.scope.df$scope <- factor(resource.scope.df$scope, levels = scope.levels )
  
  return(resource.scope.df[ !is.na(resource.scope.df$month), ])
}
# Load raw data
loadRawData <- function(sources, benefit.multiplier) {
  # - generate month list
  year.dates <- seq(as.Date("2019/1/1"), 
                    by = "month", 
                    length.out = 12)
  #print(benefit.multiplier)
  
  rawData <- list()
  
  rawData$resource.cost.rates.df <- importResourceMaster(sources,
                                                           year.dates,
                                                           benefit.multiplier)
  
  rawData$resource.scope.df <- copy(sources$scope.source.df)
  
  return(rawData)
}

mergeRatesAndScope <- function(rates.df, scope.df, model) {
  # drop redundant name

  rates.df <- rates.df[,-which(names(rates.df) =="name")]
  
  cost.df <- merge(scope.df[scope.df$model == model,], 
                   rates.df, 
                   by=c("resource_id","month"))
  cost.df$scope_account <- paste(cost.df$scope, cost.df$account, sep = ' - ')
  cost.df$net_cost <- cost.df$monthly_cost * cost.df$percent
  
  return(cost.df)
}

# Define server logic required to draw a histogram
server <- function(input, output) {
   
  originalData <- list()
  originalData$rates.source.df <- read.xlsx('model3.xlsx',"resource_master")
  originalData$scope.source.df <- read.xlsx('model3.xlsx', "resource_scope")
  
  cost.details <- reactive({
    #load data
    benefit.multiplier <- as.double(input$benefitRateValue)
    
    rawData <- loadRawData(originalData, benefit.multiplier)
    
    scope<-rawData$resource.scope.df
    scope[scope$name== "Support SOW","start_date"] <- as.character(timeFirstDayInMonth(input$supportStartDate))
    
    iotStart <- timeFirstDayInMonth(input$iotDateRange[1])
    
    scope[scope$name== "IoT SOW","start_date"] <- as.character(iotStart)
    iotEnd <- timeFirstDayInMonth(input$iotDateRange[2])
    scope[scope$name== "IoT SOW","end_date"] <- as.character(iotEnd)
    
    cpqStart <- timeFirstDayInMonth(input$cpqDateRange[1])
    scope[scope$name== "CPQ SOW","start_date"] <- as.character(cpqStart)
    cpqEnd <- timeFirstDayInMonth(input$cpqDateRange[2])
    scope[scope$name== "CPQ SOW","end_date"] <- as.character(cpqEnd)
    
    
    scope[scope$name== "Director Customer","start_date"] <- as.character(timeFirstDayInMonth(input$customerDirectorStartDate))
    scope[scope$name== "Director Manufacturing","start_date"] <- as.character(timeFirstDayInMonth(input$manufacturingDirectorStartDate))
    scope[scope$name== "Director Data","start_date"] <- as.character(timeFirstDayInMonth(input$dataDirectorStartDate))
    
    resources <- rawData$resource.cost.rates.df
    resources[resources$name == "IoT SOW","monthly_cost"] <- as.integer(input$iotSowMonthlyValue) * 1000
    resources[resources$name == "CPQ SOW","monthly_cost"] <- as.integer(input$cpqSowMonthlyValue) * 1000
    resources[resources$name == "Support SOW","monthly_cost"] <- as.integer(input$supportSowMonthlyValue) * 1000
    
    year.dates <- seq(as.Date("2019/1/1"), 
                      by = "month", 
                      length.out = 12)
    
    resource.scope <- processResourceScope(scope, year.dates)
    #browser()
    cost <- mergeRatesAndScope(resources, resource.scope, "plan")
    
  })
  
  output$plot <- renderPlot( {
    
    cost.details() %>%
      group_by(month, scope) %>%
      summarize(cost=sum(net_cost)) %>%
      ggplot(aes(month)) +
      geom_col(aes(y=cost, fill=scope)) +
      scale_fill_manual(values = c(rev(brewer.pal(n = 8, name = "Blues")),
                                   rev(brewer.pal(n = 4, name = "Greens")[c(2,4)]))) +
      scale_y_continuous(labels = scales::comma) +
      labs(y = "Monthly Cost ($)")
   # 
    })
  output$summaryTable <- renderDataTable( {
    cost.summary <- cost.details() %>%
      group_by(scope) %>%
      summarize(cost = sum(net_cost)) %>%
      datatable() %>%
      formatCurrency("cost")
  })
  
  output$scopeByMonth <- renderDataTable( {
    cost.details() %>%
      cast(scope ~ month, sum, value = "net_cost") %>%
      datatable() %>%
      formatCurrency(2:13, digits = 0)
  })
  output$sowTotals <- renderDataTable( {
    cost.details() %>%
      filter(grepl("SOW",name)) %>%
      group_by(name) %>%
      summarize(cost = sum(net_cost)) %>%
      datatable() %>%
      formatCurrency("cost", digits = 0)
  })
  
  output$totalText <- renderText( {
    df <- cost.details()
    total <- sum(df[,"net_cost"])
    #browser()
    paste("Total Forecast: ",sprintf("$%.2fM",total/1000000.0))
  })
  
  observeEvent(input$export, {
    cost.details() %>%
      write.csv(input$filename)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

