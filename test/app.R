library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
ui <- dashboardPagePlus(
    dashboardHeaderPlus(title = "AA Tester"),
    dashboardSidebar(
        dashboardSidebar(
            sidebarMenu(id = 'sidebarmenu',
                        menuItem('Introduction', tabName = 'intro', icon = icon('dashboard')),
                        menuItem('Explore Funds', tabName = 'expf',
                                 icon = icon('th'),
                                 menuSubItem('Choose Strategy',
                                             tabName = 'retAA',
                                             icon = icon('line-chart')), # point 1
                                 selectInput("str", "Strategies:", choices=c("Strategy 1",
                                                                             "Strategy 2",
                                                                             "Strategy 3",
                                                                             "Strategy 4",
                                                                             "Strategy 5",
                                                                             "Strategy 6",
                                                                             "Strategy 7",
                                                                             "Strategy 8"),multiple = T,selected = "Strategy 1"), # point 2
                                 fileInput("file1","Choose csv file to upload:", multiple=TRUE, accept =c("text/csv",
                                                                                                          "text/comma-separated-values,text/plain",
                                                                                                          ".csv")),
                                 textInput("dataset1","Please specify the file name to save:"),
                                 downloadButton("downloadData1", "Download",style="display: block; margin: 0 auto; width: 200px;color: black;")
                        )
                        ))
        
        
    ),
    dashboardBody(
        tabItems(
            # First tab content
            tabItem(tabName = "intro",
                    fluidRow(
                        h2("Intro tab content")
                    )
            ),
            
            # Second tab content
            tabItem(tabName = "retAA",
                    h2("Exp tab content"),
                    textOutput("userStr") # point 3
            )
        )
    )
)
#server.r
server <- function(input, output) {
    output$userStr <- renderText(input$str) # point 4
}

shiny::shinyApp(ui,server)



