#install.packages("shiny")
#import libraries
library(ggplot2)
library(shiny)
library(sqldf)
library(dplyr)

#import file
claim<-read.csv(file="D:/NEU Courses/Communication and Visualization/Week4/ABC Healthcare Company Claims Dataset.csv", header=TRUE, sep = ',')

ui<-shinyUI(fluidPage(
  
  #fluid page for dynamically adapting to screens of different resolutions.
  titlePanel("Dynamic Scatter Plot of Total Paid by Insurance and Patient Copayment"),
  sidebarLayout(
    sidebarPanel(
      
      radioButtons("x", "Select a State:",
                   list("Massachusetts"='a', "Connecticut"='b',
                        "New Hampshire"='c', "Rhode Island"='d')),
      
      selectInput("selected_bar", label = "Select Product Line:",
                  choices = c("Medicare" = 1,
                              "Commercial" = 2), # Choices
                  selected = 1)# Default Selection
    ),
    
    mainPanel(
      plotOutput("plot")
    )
  )
))

#writing server function
server<-shinyServer(function(input, output) {
  
  dataframe <- reactive({ 
    # Fetching Selected Plot
    plotNumber <- as.character(input$x)
    productNumber<-as.numeric(input$selected_bar)
    
    if (plotNumber=='a' & productNumber==1) {
      sql="SELECT TOTAL_PAID_BY_INSURANCE as Insurance, 
      PATIENT_COPAYMENT as Copayment  FROM claim WHERE PATIENT_STATE='MA' AND PRODUCT_LINE='Medicare'"
    }
    
    if (plotNumber=='b' & productNumber==1) {
      sql="SELECT TOTAL_PAID_BY_INSURANCE as Insurance, 
      PATIENT_COPAYMENT as Copayment FROM claim WHERE PATIENT_STATE='CT' AND PRODUCT_LINE='Medicare'"
    }
    
    if (plotNumber=='c' & productNumber==1) {
      sql="SELECT TOTAL_PAID_BY_INSURANCE as Insurance, 
      PATIENT_COPAYMENT as Copayment FROM claim WHERE PATIENT_STATE='NH' AND PRODUCT_LINE='Medicare'"
    }  
    
    if (plotNumber=='d' & productNumber==1) {
      sql="SELECT TOTAL_PAID_BY_INSURANCE as Insurance, 
      PATIENT_COPAYMENT as Copayment FROM claim WHERE PATIENT_STATE='RI' AND PRODUCT_LINE='Medicare'"
    }
    
    if (plotNumber=='a' & productNumber==2) {
      sql="SELECT TOTAL_PAID_BY_INSURANCE as Insurance, 
      PATIENT_COPAYMENT as Copayment  FROM claim WHERE PATIENT_STATE='MA' AND PRODUCT_LINE='Commercial'"
    }
    
    if (plotNumber=='b' & productNumber==2) {
      sql="SELECT TOTAL_PAID_BY_INSURANCE as Insurance, 
      PATIENT_COPAYMENT as Copayment FROM claim WHERE PATIENT_STATE='CT' AND PRODUCT_LINE='Commercial'"
    }
    
    if (plotNumber=='c' & productNumber==2) {
      sql="SELECT TOTAL_PAID_BY_INSURANCE as Insurance, 
      PATIENT_COPAYMENT as Copayment FROM claim WHERE PATIENT_STATE='NH' AND PRODUCT_LINE='Commercial'"
    }  
    if (plotNumber=='d' & productNumber==2) {
      sql="SELECT TOTAL_PAID_BY_INSURANCE as Insurance, 
      PATIENT_COPAYMENT as Copayment FROM claim WHERE PATIENT_STATE='RI' AND PRODUCT_LINE='Commercial'"
    }
    
    sqldf(sql)  
  })
  
  output$plot<-renderPlot({
    
    ggplot(dataframe(),mapping = aes(x=Copayment,y= Insurance)) +
      xlab("Patient Copayment")+
      ylab("Amount paid by insurance")+
      geom_point(size=3) +
      geom_smooth(method=lm, se=F, fullrange=T, level=0.95)+
      theme(
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.grid = element_blank(),
            panel.background = element_blank(),
            axis.line = element_line(color = "Black"))
  
  })

})
shinyApp(ui, server)