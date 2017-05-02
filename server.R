library(shiny)
library(ggplot2)
library(wordcloud)

function(input, output) {
  
  ################
  # Flight Table #
  ################
  output$mytable1 <- DT::renderDataTable({
    temp <- raw_data[, c('ORIGIN_CITY_NAME', 'DEST_CITY_NAME')]
    temp$ORIGIN_CITY_NAME <- as.character(temp$ORIGIN_CITY_NAME)
    temp$DEST_CITY_NAME <- as.character(temp$DEST_CITY_NAME)
    temp <- temp %>% group_by(ORIGIN_CITY_NAME, DEST_CITY_NAME) %>% mutate(count = n())
    temp <- unique(temp[, 1:3])
    temp <- separate(temp, ORIGIN_CITY_NAME, into = c("origin_city", "origin_state"), sep = ", ")
    temp <- separate(temp, DEST_CITY_NAME, into = c("dest_city", "dest_state"), sep = ", ")
    temp <- temp[temp$origin_state %in% c(input$show_vars2) &
                 temp$dest_state %in% c(input$show_vars2), ]
    DT::datatable(temp[, input$show_vars, drop = FALSE])
  })
  
  #################
  # Force Network #
  #################
  output$NetworkPlot <- renderForceNetwork({
    temp <- origin_dest_agg[origin_dest_agg$origin_state %in% c(input$show_vars2) &
                            origin_dest_agg$dest_state %in% c(input$show_vars2), ]
    # Prepare NODES dataframe
    cities <- c(temp$origin_city, temp$dest_city)
    nodes <- data.frame(cities)
    nodes$states <- as.factor(c(temp$origin_state, temp$dest_state))
    nodes <- unique(nodes)
    
    # Prepare LINKS dataframe
    citie_levels <- levels(nodes$cities)
    links <- data.frame(temp[, c(1, 3, 5)])
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
  
  ##################
  # Dendro Network #
  ##################
  
  output$dendro <- renderDendroNetwork({
    temp <- origin_dest_agg[origin_dest_agg$origin_state %in% c(input$show_vars2) &
                              origin_dest_agg$dest_state %in% c(input$show_vars2), ]
    cities <- c(temp$origin_city, temp$dest_city)
    cities <- sort(unique(cities))
    m <- matrix(0, ncol = length(cities), nrow = length(cities))
    m <- data.frame(m)
    rownames(m) <- cities
    colnames(m) <- cities
    for (i in seq(1, nrow(temp))){
      row <- temp[i, ]
      m[row$origin_city, row$dest_city] = row$count
      m[row$dest_city, row$origin_city] = row$count
    }
    
    hc <- hclust(dist(m), "ave")
    dendroNetwork(hc, height = 600, opacity = input$opacity)
  })
  
}
