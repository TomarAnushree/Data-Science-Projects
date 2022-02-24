library(shiny)
library(shinydashboard)
library(dashboardthemes)
library(data.table)
library(ggplot2)
library(plotly)
library(readxl)
library(dygraphs)
library(leaflet)
library(leaflet.minicharts)
#library(tigris) 
#library(tidyverse)
library(xts)


uscities<-read.csv('uscities.csv')
uscities<-uscities[,c("city","lat","lng")]
colnames(uscities)[1]<-"City"
uscities<-uscities[!duplicated(uscities$City),]

#vars <- c("avgsales" ,   "t_order" ,    "t_quantity",  "avg_Profit") 

header <- dashboardHeader(
  title="Superstore Data Dashboard",titleWidth=300)

sidebar <- dashboardSidebar(
  sidebarUserPanel(h4("by:","Anushree Tomar",align='center'),
    subtitle = a(href = "https://github.com/TomarAnushree", icon("link", class = "text-success"), "Github"),
    ),
  br(),
  br(),
  fileInput("file", label = h3("Upload Data",style = "color:white")),
  br(),
  br(),
  sidebarMenu(
    # Setting id makes input$tabs give the tabName of currently-selected tab
    id = "tabs",
    menuItem(h6("Superstore Data",style = "color:white"), tabName = "data",icon = icon("table")),
    menuItem(h6("Review Data",style = "color:white"), tabName = "reviewdata", icon = icon("chart-line")),
    menuItem(h6("K- means Clustering analysis",style = "color:white"), tabName = "clusteranalysis", icon = icon("chart-scatter"))
    
    
  )
)
body <- dashboardBody(
  shinyDashboardThemes(
    theme = "poor_mans_flatly"
  ),
  tabItems(
    tabItem("data",h3("Superstore Data",align = "center",style = "color:blue"),
            fluidRow(box(width = 12, status = "info", solidHeader = TRUE,
                         title = "Data Table",
                         collapsible = T,
                         dataTableOutput("table")))
              ),
    
    tabItem("reviewdata",
            fluidRow(
              box(downloadLink("downloadData1", "Download"),
                width = 12, status = "info", solidHeader = TRUE,
                title = "Average sales by City and Customer ID",
                collapsible = T,
                leafletOutput("plot2")),
             
               box(
                width = 12, status = "info", solidHeader = TRUE,
                title = "Sales Trend by Order Date",
                collapsible = T,
                dygraphOutput("plot3")),
              
               box(downloadLink("downloadData2", "Download"),
                 width = 12, status = "info", solidHeader = TRUE,
                         title = "Sales by State and categorry",
                         collapsible = T,
                         plotlyOutput("plot4")))
    ),
    tabItem("clusteranalysis",
            fluidRow(
              box(sliderInput("year","Year",min = 2017,max=2020,value=c(2017,2020)),
                  #selectInput('xcol', 'X Variable', vars,selected = vars[[1]]),
                  #selectInput('ycol', 'Y Variable', vars, selected = vars[[4]]),
                  numericInput('clusters', 'Cluster count', 3, min = 2, max = 9),
                  width = 12, status = "primary", solidHeader = TRUE,
                  collapsible = T,
                  title = "K-Means Clustering"
                ),
              box(
                width =12, status = "info",solidHeader = TRUE,
                title = "K-Means Clustering",
                collapsible = T,
                plotOutput("plot5")
              ),
               box(downloadLink("downloadData3", "Download"),
                width = 12, status = "info",solidHeader = TRUE,
                title = "K-Means Clustering by Customers",
                collapsible = T,
                dataTableOutput("plot6")
                
              )
              
            )
            
    )
  ))


shinyApp(
  ui = dashboardPage(header, sidebar, body),
  server = function(input, output,session) { 
    
    #table view
    
    
    output$table<-renderDataTable({infile<-input$file
    
    if(is.null(infile))
      return(NULL)
    data<-read_excel(infile$datapath)
    head(data,100)
    })
    
    # Average Sales by city and customer id
    tilesURL <- "http://server.arcgisonline.com/ArcGIS/rest/services/Canvas/World_Light_Gray_Base/MapServer/tile/{z}/{y}/{x}"
    basemap <- leaflet(width = "100%", height = "400px") %>%
      addTiles(tilesURL)
    
      output$plot2<-renderLeaflet({
      infile<-input$file
      
      if(is.null(infile))
        return(NULL)
      data<-read_excel(infile$datapath)
      new<-aggregate(Sales~`Customer ID`+City,data,mean)
      #add lat long
      avgsales<-merge(new,uscities,by="City")
      avgsales<-subset(avgsales,Sales>500)
      
      basemap %>% addPopups(avgsales$lng, avgsales$lat, avgsales$City,
                            options = popupOptions(closeButton = TRUE))%>%
         addMinicharts(
           avgsales$lng, avgsales$lat,
          chartdata = avgsales$Sales,
          showLabels = TRUE,
          width = 45
        )
    })  
    map<-reactive({infile<-input$file
    
    if(is.null(infile))
      return(NULL)
    data<-read_excel(infile$datapath)
    new<-aggregate(Sales~`Customer ID`+City,data,mean)
    #add lat long
    avgsales<-merge(new,uscities,by="City")
    avgsales<-subset(avgsales,Sales>500)
    })
    
    
    #sales trend
    output$plot3<-renderDygraph({infile<-input$file
    
    if(is.null(infile))
      return(NULL)
    data<-read_excel(infile$datapath)
    datan<-data[,c('Order Date','Sales','Profit')]
    don <-xts(x = datan$Sales, order.by = datan$`Order Date`)
    dygraph(don)%>% dyRangeSelector()
    })
    
    sales<-reactive({infile<-input$file
    
    if(is.null(infile))
      return(NULL)
    data<-read_excel(infile$datapath)
    df<- aggregate(Sales~State+Category,data,sum) })
    
    # sales by state and category
    output$plot4 <- renderPlotly({
      
      ggplotly(ggplot(data = sales(), 
                      aes(x=State, y=Sales, fill=Category)) + 
                 geom_bar(position = "dodge", stat = "identity") + ylab("sales") + 
                 xlab("State") + theme(legend.position="top",axis.text.x = element_text(angle = -90) 
                                       ,plot.title = element_text(size=15, face="bold"))+
                 ggtitle(" ") +labs(fill = "Category"))
      
    })
    
   
    cluster<-reactive({infile<-input$file
    if(is.null(infile))
      return(NULL)
    data<-read_excel(infile$datapath)
    data$year<-year(data$`Order Date`)
    
    data<-data.table(data)
    
    datay<-data[year>input$year[1]&year<input$year[2]]
    dataclust<-datay[,list("avgsales"=mean(Sales),"t_order"=length(`Order Date`),"t_quantity"=sum(Quantity),"avg_Profit"=mean(Profit)),by=list(`Customer ID`)]
    #dataclust<-dataclust[, c(input$xcol, input$ycol)]
    km<-kmeans(dataclust[,-1],input$clusters,nstart = 25, iter.max = 10) 
    #custom mode function
    mode<-function(x){uniqv<-unique(x)
    uniqv[which.max(tabulate(match(x,uniqv)))]}
    final2_membership<- data.frame(km$cluster,dataclust) # append cluster membership
    })
    
    
    # download cluster data
    output$downloadData3 <- downloadHandler(
      filename = function() {
        paste("clusterdata", ".csv", sep = "")
      },
      content = function(file) {
        write.csv(cluster(), file, row.names = FALSE)
      }
    )
    
    # download cluster data
    output$downloadData1 <- downloadHandler(
      filename = function() {
        paste("mapdata", ".csv", sep = "")
      },
      content = function(file) {
        write.csv(map(), file, row.names = FALSE)
      }
    )
    
    # download cluster data
    output$downloadData2 <- downloadHandler(
      filename = function() {
        paste("salescategdata", ".csv", sep = "")
      },
      content = function(file) {
        write.csv(sales(), file, row.names = FALSE)
      }
    )
    
    # kmeans clustering
    output$plot6<-renderDataTable({head(cluster())
    })
    
    
    output$plot5<-renderPlot({infile<-input$file
    if(is.null(infile))
      return(NULL)
    data<-read_excel(infile$datapath)
    data$year<-year(data$`Order Date`)
    data<-data.table(data)
    
    datay<-data[year>input$year[1]&year<input$year[2]]
    dataclust<-datay[,list("avgsales"=mean(Sales),"t_order"=length(`Order Date`),"t_quantity"=sum(Quantity),"avg_Profit"=mean(Profit)),by=list(`Customer ID`)]
    #dataclust<-dataclust[, list(input$xcol,input$ycol)]
    dataclust<-dataclust[, list(avgsales,avg_Profit)]
      km<-kmeans(dataclust, input$clusters)
      par(mar = c(6, 7, 0, 1))
      plot(dataclust,
           col = km$cluster,
           pch = 20, cex = 3)
      points(km$centers, pch = 4, cex = 4, lwd = 4)
    })
    
    
    
  }
  
)





