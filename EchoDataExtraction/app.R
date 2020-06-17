#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)
library(tidyverse)
library(DT)

sidebar <- dashboardSidebar(
    sidebarMenu(
        menuItem(strong("About"), tabName = "home", icon = icon("home")),
        menuItem(strong("MI data"), icon = icon("th"), tabName = "MI",
                 fileInput("file1","Choose csv file to upload:", multiple=TRUE, accept =c("text/csv",
                                                                                                    "text/comma-separated-values,text/plain",
                                                                                                    ".csv")),
                 menuSubItem("display", tabName = "display1",icon = icon('table')),
                 textInput("dataset1","Please specify the file name to save:"),
                 downloadButton("downloadData1", "Download",style="display: block; margin: 0 auto; width: 200px;color: black;")
                 ),
        menuItem(strong("TAC data"), icon = icon("th"), tabName = "TAC",
                 fileInput("file2","Choose csv file to upload:", multiple=TRUE, accept =c("text/csv",
                                                                                          "text/comma-separated-values,text/plain",
                                                                                          ".csv")),
                 menuSubItem("display", tabName = "display2",icon = icon('able')),
                 textInput("dataset2","Please specify the file name to save:"),
                 downloadButton("downloadData2", "Download",style="display: block; margin: 0 auto; width: 200px;color: black;")
                 )
        )
)

body <- dashboardBody(
    tabItems(
        tabItem(tabName = "home",
                fluidPage(
                h1("About the app"),
                p("The app is to automate the process of ETL(extraction, transforming and loading) on raw echo data.", style = "font-size:20px"),
                h1("Example")
                )
        ),
        
        tabItem(tabName = "display1",
                h2("MI echo data"),
                fluidPage(
                    DT::dataTableOutput("content1")
                )
        ),
        
        tabItem(tabName = "display2",
                h2("TAC echo data"),
                fluidPage(
                    DT::dataTableOutput("content2")
                )
        )
    )
)

ui <- dashboardPage(
    dashboardHeader(title = "Echo Data Extraction"),
    sidebar,
    body
)

server <- function(input, output) { 
    finaldata1 <- reactive({
        # input$file1 will be NULL initially. After the user selects
        # and uploads a file, head of that data file by default,
        # or all rows if selected, will be shown.
        req(input$file1)
        # read the data from uploaded file
        data <- read.csv(input$file1$datapath,col.names = 1:15,header=FALSE,stringsAsFactors = FALSE)
        # capture the analysis by LV trace, mainly the Long axis and short axis systolic function
        index1<-grep("^LV Trace$",data[,1])
        # caputure the analysis by volume, mainly 3D result
        index2<-grep("3D-Mode",data[,2])
        
        # subset data
        data1<-data[index1,c(2,3,5)]
        data2<-data[index2,c(2,3,5)]
        
        # create the label by series
        label_index<-grep("Series Name",data[,1])
        label<-as.character(data[label_index,2])
        # create the ID by animal ID
        ID_index<-grep("Animal ID",data[,1])
        ID<-as.character(data[ID_index,2])
        # count the number of parameter for each analysis
        grouping1<-cut(index1,breaks = c(label_index,nrow(data)))
        reptimes1<-table(grouping1)
        # same as above
        grouping2<-cut(index2,breaks = c(label_index,nrow(data)))
        reptimes2<-table(grouping2)
        
        # tally up the labels and ID for each subset
        data1$labels<-rep(label,reptimes1)
        data1$ID<-rep(ID,reptimes1)
        
        data2$labels<-rep(label,reptimes2)
        data2$ID<-rep(ID,reptimes2)
        
        # tally up the parameter name
        data1$measurement<-with(data1,paste(sapply(X2,function(x) strsplit(x,"-")[[1]][1]),X3,sep="-"))
        num<-1:10
        data2$measurement<-paste(data2$X3,unlist(lapply(reptimes2, function(x) num[0:x])),sep = "") 
        
        # combine subsets
        colnames(data2)<-colnames(data1)
        data1<-rbind(data1,data2)
        # define row names
        names(data1)<-c("label","parameter","value","labels","ID","measurement")
        # keep 3 decimal places
        data1$value<-round(as.numeric(data1$value),3)
        # change from long format to wide format
        data3<-data1[3:6] %>% spread(measurement,value)
        names(data3)<-str_remove_all(names(data3),"[ ;-]")
        
        index<-which(data3$Volume1>data3$Volume2)
        if(!is_empty(index))
        {temp<-data3[index,"Volume1"]
        data3[index,"Volume1"]<-data3[index,"Volume2"]
        data3[index,"Volume2"]<-temp}
        if (!is_empty(data3))
        {s=c("ID","labels","BHeartRate","BVolumes","BVolumed","BStrokeVolume","BEjectionFraction",
             "BFractionalShortening","BCardiacOutput","MDiameters","MDiameterd","MVolumes",
             "MVolumed","MStrokeVolume","MEjectionFraction","MFractionalShortening",
             "MLVMassCor","MCardiacOutput","Volume1","Volume2")
        data3<-data3[,s[s%in%colnames(data3)]]
        tidynames=c("ID","labels","heart rate","LVsPSLAX","LVdPSLAX","PSLAXSV","LVEFPSLAX (%)","LVFSPSLAX (%)",
                    "PSLAXCO","dsSAX","ddSAX","LVsSAX","LVdSAX","SAXSV","LVEFSAX (%)","LVFSSAX (%)","LV Mass Corr",
                    "SAXCO","3D ESV","3D EDV")
        names(tidynames)=s
        colnames(data3)=tidynames[colnames(data3)]
        data3
        }
        
        else data3
        
    })
    
    finaldata2 <- reactive({
        # input$file1 will be NULL initially. After the user selects
        # and uploads a file, head of that data file by default,
        # or all rows if selected, will be shown.
        req(input$file2)
        # read the data from uploaded file
        data <- read.csv(input$file2$datapath,col.names = 1:15,header=FALSE,stringsAsFactors = FALSE)
        # capture the analysis by LV trace, mainly the Long axis and short axis systolic function
        index1<-grep("^LV Trace$",data[,1])
        # caputure the analysis by velocity and time, mainly the diastolic function
        index2<-grep("Velocity|Time",data[,3])
        # same as above
        index3<-grep("none",data[,3])
        # subset data
        data1<-data[index1,c(2,3,5)]
        data2<-data[index2,c(1,3,5)]
        data3<-data[index3,c(1,3,4)]
        
        # create the label by series
        label_index<-grep("Series Name",data[,1])
        label<-as.character(data[label_index,2])
        # create the ID by animal ID
        ID_index<-grep("Animal ID",data[,1])
        ID<-as.character(data[ID_index,2])
        # count the number of parameter for each analysis
        grouping1<-cut(index1,breaks = c(label_index,nrow(data)))
        reptimes1<-table(grouping1)
        # same as above
        grouping2<-cut(index2,breaks = c(label_index,nrow(data)))
        reptimes2<-table(grouping2)
        # same as above
        grouping3<-cut(index3,breaks = c(label_index,nrow(data)))
        reptimes3<-table(grouping3)
        # tally up the labels and ID for each subset
        data1$labels<-rep(label,reptimes1)
        data1$ID<-rep(ID,reptimes1)
        
        data2$labels<-rep(label,reptimes2)
        data2$ID<-rep(ID,reptimes2)
        
        data3$labels<-rep(label,reptimes3)
        data3$ID<-rep(ID,reptimes3)
        
        # tally up the parameter name
        data1$measurement<-with(data1,paste(sapply(X2,function(x) strsplit(x,"-")[[1]][1]),X3,sep="-"))
        data2$measurement<-data2$X1
        data3$measurement<-data3$X1
        
        # combine subsets
        colnames(data3)<-colnames(data2)
        data2<-rbind(data2,data3)
        
        colnames(data2)<-colnames(data1)
        data1<-rbind(data1,data2)
        # define row names
        names(data1)<-c("label","parameter","value","labels","ID","measurement")
        # keep 3 decimal places
        data1$value<-round(as.numeric(data1$value),3)
        # change from long format to wide format
        data4<-data1[3:6] %>% spread(measurement,value)
        names(data4)<-str_remove_all(names(data4),"[ ;-]")
        
        if (!is_empty(data4))
        {s=c("ID","labels","BHeartRate","BVolumes","BVolumed","BStrokeVolume","BEjectionFraction",
             "BFractionalShortening","BCardiacOutput","MDiameters","MDiameterd","MVolumes",
             "MVolumed","MStrokeVolume","MEjectionFraction","MFractionalShortening",
             "MLVMassCor","MCardiacOutput","MVE","MVA","E'","A'","MVE/A","MVE/E'","LCCAPSV","RCCAPSV")
        data4<-data4[,s[s%in%colnames(data4)]]
        tidynames=c("ID","labels","heart rate","LVsPSLAX","LVdPSLAX","PSLAXSV","LVEFPSLAX (%)","LVFSPSLAX (%)",
                    "PSLAXCO","dsSAX","ddSAX","LVsSAX","LVdSAX","SAXSV","LVEFSAX (%)","LVFSSAX (%)","LV Mass Corr",
                    "SAXCO","E","A","E'","A'","E/A","E/E'","LCA","RCA")
        names(tidynames)=s
        colnames(data4)=tidynames[colnames(data4)]
        data4}
        else data4
        
    })
    
    output$content1 <- DT::renderDataTable({
        DT::datatable(finaldata1(),options = list(scrollX = TRUE))
    })
    
    output$content2 <- DT::renderDataTable({
        DT::datatable(finaldata2(),options = list(scrollX = TRUE))
    })
    
    output$downloadData1 <- downloadHandler(
        filename = function() {
            if (input$dataset1 == "")
                paste("Untitled", "csv", sep=".")
            else 
                paste(input$dataset1, ".csv", sep = "")
        },
        
        content = function(file) {
            write.csv(finaldata1(), file, row.names = FALSE)
        })
    
    output$downloadData2 <- downloadHandler(
        filename = function() {
            if (input$dataset2 == "")
                paste("Untitled", "csv", sep=".")
            else 
                paste(input$dataset1, ".csv", sep = "")
        },
        
        content = function(file) {
            write.csv(finaldata2(), file, row.names = FALSE)
        })
    
    }


# Run the application 
shinyApp(ui = ui, server = server)
