library(shiny)
library(ggplot2)
library(wordcloud)

function(input, output) {
  
  ################
  # Flight Table #
  ################
  output$mytable1 <- DT::renderDataTable({
    origin_dest_agg <- origin_dest_agg[origin_dest_agg$origin_state %in% c(input$show_vars2) &
                                       origin_dest_agg$dest_state %in% c(input$show_vars2), ]
    DT::datatable(origin_dest_agg[, input$show_vars, drop = FALSE])
  })
  
  #################
  # Force Network #
  #################
  output$NetworkPlot <- renderForceNetwork({
    origin_dest_agg <- origin_dest_agg[origin_dest_agg$origin_state %in% c(input$show_vars2) &
                                       origin_dest_agg$dest_state %in% c(input$show_vars2), ]
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
    links$count <- links$count/max(links$count) * 20.0
    
    forceNetwork(Links = links, Nodes = nodes,
                 Source = "origin_city", Target = "dest_city",
                 Value = "count", NodeID = "cities",
                 Group = "states", opacity = 0.9, legend = TRUE, zoom = TRUE, bounded=TRUE)
  })

  ############
  # Word Map #
  ############
  output$WordMap <- renderPlot({
    raw_origin_dest <- subset(raw_data, ,select=c("ORIGIN_CITY_NAME", "DEST_CITY_NAME"))
    raw_origin_dest$ORIGIN_CITY_NAME <- as.character(raw_origin_dest$ORIGIN_CITY_NAME)
    origin_separate <- separate(raw_origin_dest, ORIGIN_CITY_NAME, into = c("origin_city", "origin_state"), sep = ", ")
    origin_separate <- origin_separate[origin_separate$origin_state %in% c(input$show_vars2), ]
    origin_agg <- data.frame(table(origin_separate$origin_city))
    a = origin_agg$Var1
    b = origin_agg$Freq

    par(bg="white")
    pal2 <- brewer.pal(8,'Dark2')
    wordcloud(a , b , rot.per=0.1, colors=pal2,
              alpha=0.8, use.r.layout = FALSE)
  })
  
}
