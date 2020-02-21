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

# Define UI for data upload app
ui <- dashboardPage(
    # App title
    dashboardHeader(title = "MI/IR Echo Extraction"),
    # Sidebar with input and output definitions
    dashboardSidebar(
        fileInput("file1","Choose csv file to upload:", multiple=TRUE, accept =c("text/csv",
                                                                                 "text/comma-separated-values,text/plain",
                                                                                 ".csv")),
        textInput("dataset","Please specify the file name to save:"),
        downloadButton("downloadData", "Download")
        
    ),
    
    dashboardBody(
        fluidPage(
            DT::dataTableOutput("content")
        )
    )
)

server <- function(input, output) {
    
    finaldata <- reactive({
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
        
        data3
        
    })
    
    output$content <- DT::renderDataTable({
        DT::datatable(finaldata(),options = list(scrollX = TRUE))
    })
    
    output$downloadData <- downloadHandler(
        filename = function() {
            if (input$dataset == "")
                paste("Untitled", "csv", sep=".")
            else 
                paste(input$dataset, ".csv", sep = "")
        },
        
        content = function(file) {
            write.csv(finaldata(), file, row.names = FALSE)
        })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
