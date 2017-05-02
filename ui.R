library(shiny)
library(ggplot2)  # for the diamonds dataset

fluidPage(
  title = 'Examples of DataTables',
  
  conditionalPanel(
    'input.dataset === "Flight Table"',
    selectInput('show_vars', 'Columns in Flights to show:', multiple = TRUE,
                choices = names(origin_dest_agg),
                selected = c('CA', 'TX', 'NY'))
  ),

  sidebarLayout(
    sidebarPanel(
      checkboxGroupInput('show_vars2', 'Flight Origin by State:',
                         sort(unique(origin_dest_agg$dest_state)),
                         selected = c('CA', 'TX', 'NY'))
    ),
    mainPanel(
      tabsetPanel(
        id = 'dataset',
        tabPanel('Flight Table', DT::dataTableOutput('mytable1')),
        tabPanel('NetworkPlot', forceNetworkOutput("NetworkPlot",height='680px')),
        tabPanel('WordMap', plotOutput('WordMap', height = '680px'))
      )
      )     
    )
  )
 
