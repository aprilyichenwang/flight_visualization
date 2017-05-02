library(shiny)
library(ggplot2)  # for the diamonds dataset

fluidPage(
  title = 'Examples of DataTables',
  
  conditionalPanel(
    'input.dataset === "Flight Table"',
    checkboxGroupInput('show_vars', 'Columns in Flights to show:',
                       names(origin_dest_agg), 
                       selected = c("NY", "CA", "TX"))
  ),
      conditionalPanel(
        'input.dataset === "iris"',
         helpText('Display 5 records by default.')
        ),
  
  sidebarLayout(
    sidebarPanel(
      checkboxGroupInput('show_vars2', 'Flight Origin by State:',
                         sort(unique(origin_dest_agg$dest_state)),
                         selected = c('NY','CA', 'TX'))
    ),
    mainPanel(
      tabsetPanel(
        id = 'dataset',
        tabPanel('Flight Table', DT::dataTableOutput('mytable1')),
        tabPanel('NetworkPlot', forceNetworkOutput("NetworkPlot", height='680px')),
        tabPanel('WordMap', plotOutput('WordMap', height = '680px'))
        ))
    )
  )
 
