# There are in total three pages in this app
#in the "summary"page, you can find summary data 
#of parks within the selected area (a slider input)

#in the "Plot" page, you can find a scatter plot 
#of all parks within the selected area and 
#you can select y-axis and x-axis to be plotted, 
#you can find another 2 plots below the scatterplot, 
#these 2 plots are based on the division name you have selected.

#in the "Table" page, you can find a data table based on 
#the division name you have selected and the number of entries is based on 
#the number of samples you have inputted.
#You can always get a new set of samples by clikcing on the "get new samples"button.


library(shiny)
library(shinydashboard)
library(reshape2)
library(dplyr)
library(plotly)
library(shinythemes)
library(DT)
library(ggplot2)


#load csv data
park <- read.csv("park.csv")

#avoid plotly issues
pdf(NULL)

#application header and title
header<-dashboardHeader(title="Pittsburgh parks dashboard")

#dashboard sidebar
sidebar<-dashboardSidebar(
    sidebarMenu(
    id="tabs",
    menuItem("summary",icon=icon("list-alt"),tabName = "summary"),
    menuItem("Plot",icon=icon("bar-chart"),tabName="plot"),
    menuItem("Table",icon=icon("table"),tabName = "table",badgeLabel = "new",badgeColor="red"),
    
    #inputs
    #select a range of area, only parks within this area will be summarized and scatterplotted
    sliderInput(inputId="area_select",
                label="Select area within:",
                min=min(park$Shape__Area,na.rm=T),
                max=max(park$Shape__Area,na.rm=T),
                value=c(min(park$Shape__Area,na.rm=T),max(park$Shape__Area,na.rm=T)),
    ),
    #select y-axis and x-axis of the scatterplot
    selectInput(inputId="y",
                label="Y-aixs of scatterplot",
                choices=c("Acreage"="acreage",
                          "Square foot"="sqft",
                          "Shape area"="Shape__Area",
                          "Shape length"="Shape__Length"),
                selected="Shape__Area"),
    
    selectInput(inputId="x",
                label="X-aixs of scatterplot",
                choices=c("Acreage"="acreage",
                          "Square foot"="sqft",
                          "Shape area"="Shape__Area",
                          "Shape length"="Shape__Length"),
                selected="Shape__Length"),
   #select the column to be colored by
    selectInput(inputId="z",
                label="color by",
                choices=c("Type"="type_",
                          "Sector"="sector",
                          "Divname"="divname"),
                selected="sector"),
   
    numericInput(inputId="size",
                 label="point size in scatterplot",
                 value=1.5,min=1,max=3,step=0.1),
  #ask user to give a title to scatterplot
    textInput(inputId="plot_title",label="Plot title of scatterplot",placeholder="Enter text to be used as plot title"),
    #select the division for boxplot and barplot
      selectInput(inputId="selected_division",
                label="select divname for chart and table",
                choices=sort(unique(park$divname)),
                selected="Northern"),
    
  
    #number of samples to be shown in table
    numericInput(inputId="n_samp",
                 label="a random selection of n parks of this division shown in table",
                 min=1,max=nrow(park),value=15),
    
    actionButton(inputId = "write_csv", 
                 label = "Write CSV"),
    actionButton(inputId="get_new_sample",
                 label="get new samples")
    )
)
#dashboard body
body<-dashboardBody(tabItems(
    #summary data page
   tabItem("summary",
           fluidRow(
             infoBoxOutput("percent")
             
           ),
           fluidRow(
            valueBoxOutput("length"),
             valueBoxOutput("area")
          )),
    #plot page
    tabItem("plot",
           
            fluidRow(
                tabBox(title="scatterplot of all parks in the selected area",
                        width=12,
                        tabPanel("scatter",plotlyOutput("scatter_plot"))
                        )
                
            ),
            fluidRow(
                tabBox(title="barplot and boxplot of parks in selected division",
                       width=12,
                       tabPanel("barplot",plotlyOutput("plot_bar")),
                       tabPanel("boxplot",plotlyOutput("plot_box")))
            )
        
    ),
    
    #data table page
    tabItem("table",
            #show user how many seconds have been spent on this page
            textOutput(outputId="time_elapse"),
            fluidPage(
                box(title="key information of sampled parks in selected divname",
                    DT::dataTableOutput("table"),width=12)
            ))
)
    
)


ui<-dashboardPage(header,sidebar,body,skin="red")

#define server function
server<-function(input,output,session){
  #data subset to plot barplot and boxplot,and for table output
    park_subset<-reactive({
         req(input$selected_division)
        filter(park,divname %in% input$selected_division)
    })

    #update sample size for table output for different sessions
    observe({
    updateNumericInput(session,
                       inputId="n_samp",
                       value=min(15,nrow(park_subset())),
                       max=nrow(park_subset()))
    })
    #get new samples when user hits the button
    park_subset_sample<-eventReactive(
        eventExpr=input$get_new_sample,
        valueExpr={
        req(input$n_samp)
        sample_n(park_subset(), input$n_samp)},
        ignoreNULL=FALSE
    )
    
    
    #a subset of parks in selected shape area, dataset for scatterplot
    park_within<-reactive({
       filter(park,Shape__Area>=input$area_select[1]&Shape__Area<=input$area_select[2]) 
    })
    pretty_plot_title<-reactive({(input$plot_title)})
    #percentage info box
    output$percent<-renderInfoBox({
    
    pw<-park_within()
    num<-round((nrow(pw)/nrow(park))*100,2)
    infoBox("percent",value=paste(num,"%"),subtitle=paste(nrow(pw),"parks in selected area,",num,"% of total"),icon=icon("hand-point-right"),color="aqua",fill=FALSE)
})
    #shape length mean value box
    output$length<-renderValueBox({
        pw<-park_within()
        num<-round(mean(pw$Shape__Length,na.rm=T),5)*1000
        valueBox("Avg length",value=num,subtitle=paste("average Shape length in miles of",nrow(pw), "parks within selected Shape area"),icon=icon("sort-numeric-asc"),color="purple")
    })
    #shape area mean value box
    output$area<-renderValueBox({
      pw<-park_within()
      num<-round((mean(pw$Shape__Area,na.rm=T))*1000000,2)
      valueBox("Avg area",value=num,subtitle=paste("average Shape area in km2 of",nrow(pw), "parks within selected Shape area"),icon=icon("sort-numeric-asc"),color="olive")
    })
    
    
        #create scatterplot
    output$scatter_plot<-renderPlotly({
        ggplotly(ggplot(data=park_within(),aes_string(x=input$x,y=input$y,color=input$z))+
            geom_point(size=input$size)+
            labs(title=pretty_plot_title())+geom_smooth()+theme_bw()+scale_shape(),
            tooltip=c('x','y'))
        
        })
    #create barplot
    output$plot_bar<-renderPlotly({
          ggplotly(ggplot(park_subset(),aes(type_))+
          geom_bar(fill="green")+theme_minimal()+
        labs(x="type",title=paste("park distribution by type in:",input$selected_division)),tooltip="all")
            })
        
    
    #create boxplot
    output$plot_box<-renderPlotly({
      ggplotly(
        ggplot(park_subset(), aes(x = type_, y = Shape__Area)) + 
        geom_boxplot(color="purple")+scale_y_log10()+labs(x="type",y="log(shape area)")
      )
    })
    #record time viewing the table
    beg <- reactive({ Sys.time() }) 
    now <- reactive({ invalidateLater(millis = 1000);Sys.time() }) 
    diff <- reactive({ round(difftime(now(), beg(), units = "secs")) })
    output$time_elapse <- renderText({   
      paste("You have been viewing this table for", diff(), "seconds.") })
    #create data table
    output$table<-DT::renderDataTable( 
     DT::datatable(data=park_subset_sample()[,c(2,6,8,11,19)], 
                   
                  extensions='FixedHeader',
                  options=list(
                               autoWidth=TRUE,
                               fixedHeader=TRUE,
                               columnDefs = list(list(width = '100px', targets = c(1,2))),
                               pageLength=20
                               
                              ), 
                 
                  rownames=FALSE)
    
     %>%  formatStyle( 
       columns = 2, valueColumns = 2, 
       color = styleEqual(c("NP","CP", "RVR", "BTF","SU","RP","SCH","OTR"), c("red",
    "green", "blue", "grey","orange","purple","navy","olive"))  ) %>% 
    
       formatStyle(  columns = 5, 
                     background = styleColorBar(range(park_subset()[,19]), '#cab2d6'), 
                     backgroundSize =  '98% 88%', 
                     backgroundRepeat = 'no-repeat', 
                     backgroundPosition = 'center' ) %>%
      formatRound('Shape__Area',10) )
    
   
    
    # Write sampled data as csv ---------------------------------------
    observeEvent(eventExpr = input$write_csv, 
                 handlerExpr = {
                   filename <- paste0("sample parks in ",input$selected_division,str_replace_all(Sys.time(), ":|\ ", "_"), ".csv")
                   write.csv(park_subset_sample(), file = filename, row.names = FALSE) 
                 }
    )
    
    
}


#run the app
shinyApp(ui=ui,server=server)