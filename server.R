library(shiny)
library(ggplot2)

function(input, output) {
  
  # choose columns to display
  output$mytable1 <- DT::renderDataTable({
    origin_dest_agg<-  origin_dest_agg[origin_dest_agg$dest_state %in% c(input$show_vars2) &
                                         origin_dest_agg$origin_state %in% c(input$show_vars2),]
    
    DT::datatable(origin_dest_agg[, input$show_vars, drop = FALSE])
  })
  
  # sorted columns are colored now because CSS are attached to them
  output$NetworkPlot <- renderForceNetwork({
    
    origin_dest_agg<-  origin_dest_agg[origin_dest_agg$dest_state %in% c(input$show_vars2) & origin_dest_agg$origin_state %in% c(input$show_vars2),]
    
    
    # Prepare NODES dataframe
    cities <- c(origin_dest_agg$origin_city, origin_dest_agg$dest_city)
    nodes <- data.frame(cities)
    nodes$states <- as.factor(c(origin_dest_agg$origin_state, origin_dest_agg$dest_state))
    nodes <- unique(nodes)
    
    # Prepare LINKS dataframe
    citie_levels <- levels(nodes$cities)
    links <- data.frame(origin_dest_agg[, c(1, 3, 5)])
    links$origin_city <- factor(links$origin_city, levels=citie_levels)
    links$origin_city <- as.integer(links$origin_city) - 1
    links$dest_city <- factor(links$dest_city, levels=citie_levels)
    links$dest_city <- as.integer(links$dest_city) - 1
    links$count <- links$count/max(links$count) *1
    
  
    
    forceNetwork(Links = links, Nodes = nodes,
                 Source = "origin_city", Target = "dest_city",
                 Value = "count", NodeID = "cities",
                 Group = "states", opacity = 0.9, legend = TRUE, zoom = FALSE,bounded=TRUE)
  })
  
  # customize the length drop-down menu; display 5 rows per page by default
  output$WordMap <- renderPlot({
    library(wordcloud)

    #Create a list of words
    
    x<-"Below shows the top flight destinations in the selected states"

    
    origin_dest_agg<-  origin_dest_agg[origin_dest_agg$dest_state %in% c(input$show_vars2) &
                                         origin_dest_agg$origin_state %in% c(input$show_vars2),]
    origin_agg <- data.frame(table(origin_dest_agg$origin_city))
    a = origin_agg$Var1

    #I give a frequency to each word of this list
    b = origin_agg$Freq

    #The package will automatically make the wordcloud ! (I add a white background)
    par(bg="white")
    pal2<-brewer.pal(8,'Dark2')
    wordcloud(a , b , rot.per=0.1,colors=pal2,alpha=0.8,
              main='Top Flight Destinations')
    
    
  
  })
  
}
