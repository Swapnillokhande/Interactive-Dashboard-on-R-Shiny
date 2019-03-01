#import libraries
#plot bar graph with drop down menu and a slider

library(ggplot2)
library(shiny)
library(sqldf)


# Read CSV into R
globalorder <- read.csv(file="D:/NEU Courses/Communication and Visualization/Week2/Week2_assignment/Global Superstore 2016 Orders.csv", header=TRUE, sep = ',')
#convert year as numeric
globalorder$Order.Year<-as.numeric(as.character(globalorder$Order.Year))

#change the column name
colnames(globalorder)[which(names(globalorder) == "Order.Year")] <- "Year_of_order"



ui<-shinyUI(fluidPage(    
  # Give the page a title
  titlePanel("Sales in different Product Categories by Market"),
  
  sidebarLayout(
    sidebarPanel(
      
      selectInput("selected_bar", label = "Select Product category:",
                  choices = c("Technology" = 1,
                              "Furniture" = 2, # Choices
                              "Office Supplies" = 3),
                  selected = 1),# Default Selection
      
      
      sliderInput(inputId = "year",
                  label = "Select a Year",
                  min = 2011,
                  max = 2014,
                  sep = "",
                  value = 2011)
    ),   
    mainPanel(
      h3(textOutput("caption")),
      plotOutput("plot")
      
      
    )
  )
)
)

server<-shinyServer(function(input, output) {
  
  dataframe <- reactive({ 
    # Fetching Selected Plot
    plotNumber <- as.numeric(input$selected_bar)
    yearSale <- as.numeric(input$year)
    
    if (plotNumber==1 & yearSale==2011) {
      sql="SELECT Market,SUM(Sales) as metric FROM globalorder WHERE Category='Technology' AND Year_of_order=2011 GROUP BY Market"
    }
    
    if (plotNumber==2 & yearSale==2011) {
      sql="SELECT Market,SUM(Sales) as metric FROM globalorder WHERE Category='Furniture' AND Year_of_order=2011 GROUP BY Market"
    }
    
    if (plotNumber==3 & yearSale==2011) {
      sql="SELECT Market,SUM(Sales) as metric FROM globalorder WHERE Category='Office Supplies' AND Year_of_order=2011 GROUP BY Market"
    }  
    
    if (plotNumber==1 & yearSale==2012) {
      sql="SELECT Market,SUM(Sales) as metric FROM globalorder WHERE Category='Technology' AND Year_of_order=2012 GROUP BY Market"
    }
    
    if (plotNumber==2 & yearSale==2012) {
      sql="SELECT Market,SUM(Sales) as metric FROM globalorder WHERE Category='Furniture' AND Year_of_order=2012 GROUP BY Market"
    }
    
    if (plotNumber==3 & yearSale==2012) {
      sql="SELECT Market,SUM(Sales) as metric FROM globalorder WHERE Category='Office Supplies' AND Year_of_order=2012 GROUP BY Market"
    }
    
    if (plotNumber==3 & yearSale==2013) {
      sql="SELECT Market,SUM(Sales) as metric FROM globalorder WHERE Category='Office Supplies' AND Year_of_order=2013 GROUP BY Market"
    } 
    
    if (plotNumber==1 & yearSale==2013) {
      sql="SELECT Market,SUM(Sales) as metric FROM globalorder WHERE Category='Technology' AND Year_of_order=2013 GROUP BY Market"
    }
    
    if (plotNumber==2 & yearSale==2013) {
      sql="SELECT Market,SUM(Sales) as metric FROM globalorder WHERE Category='Furniture' AND Year_of_order=2013 GROUP BY Market"
    }
    
    if (plotNumber==3 & yearSale==2014) {
      sql="SELECT Market,SUM(Sales) as metric FROM globalorder WHERE Category='Office Supplies' AND Year_of_order=2014 GROUP BY Market"
    } 
    
    if (plotNumber==1 & yearSale==2014) {
      sql="SELECT Market,SUM(Sales) as metric FROM globalorder WHERE Category='Technology' AND Year_of_order=2014 GROUP BY Market"
    }
    
    if (plotNumber==2 & yearSale==2014) {
      sql="SELECT Market,SUM(Sales) as metric FROM globalorder WHERE Category='Furniture' AND Year_of_order=2014 GROUP BY Market"
    }
    
     
    sqldf(sql)  
  }
  )
  
  
  output$plot<-renderPlot({
    
    ggplot(dataframe(),mapping = aes(x=reorder(Market, -metric),y=metric))+
      geom_bar(stat = 'identity', show.legend = FALSE, aes(fill="Red"))+
      theme(axis.title.x = element_blank(),
            axis.title.y = element_blank(),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.grid = element_blank(),
            panel.background = element_blank(),
            axis.line = element_line(color = "Black"))
    
  })
}
)


shinyApp(ui, server)

