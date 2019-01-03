shinyUI(fluidPage(
  titlePanel("Travel Visa Inequality"),
  
    tabsetPanel(
      tabPanel("Map", 
               sidebarLayout(
                 sidebarPanel(
                   radioButtons("SelectMap", "Select a Map",
                              c("Explore Visa Inequality", "Where can Germans Travel to?", "Who is allowed to travel to Germany?", 
                                "Differences between Traveling to and from Germany"))
                   
                   #uiOutput("ReverseOrder")
                   #verbatimTextOutput("hover2")
                  # verbatimTextOutput("click")
                 ),
                 mainPanel(
                   plotlyOutput("map"),
                   verbatimTextOutput("text")
                 ))),  
     tabPanel("Barplot", 
       sidebarLayout(
        sidebarPanel(
          radioButtons("BarType", "Select a graph type",
                     c("Visa Inequality", "Visa From", "Visa To")),
          uiOutput("SortBy"),
          uiOutput("ReverseOrder")
          #verbatimTextOutput("hover")
          #verbatimTextOutput("click")
      ),
      mainPanel(plotlyOutput("barplot")
      )))
  ) #close tabset panel
)#close shiny
) #close shiny