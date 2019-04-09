
server <- function(input, output, session) {
  ## FIRST PANEL
  output$hood <- renderUI({
    selectInput(inputId = "nhood", label = "Neighborhood")
  })
  output$results <- renderPrint({
    input$nhood
  })
  
  plt1 <- reactive({
    lst1 <- as.character(as.list(input$nhood))
    txt <- paste(paste("Neighborhood==\"", paste(lst1, sep = "", collapse = "\"| Neighborhood==\""), sep = ""), "\"", sep = "")
    df2 <- filter(df1, eval(parse(text = txt))) %>% group_by(rpt_mnth, Neighborhood) %>% summarise(count = n())
    y <- ggplot(df2, aes(x = rpt_mnth, y = (count), group = Neighborhood)) + geom_line(aes(color = (Neighborhood)), size = 1.2) +
      labs(x = "Date", y = "Count") + theme(
        axis.ticks.y = element_blank(), axis.ticks.x = element_blank(), panel.grid = element_blank(), panel.background = element_rect(fill = "#393c42", linetype = "solid"), axis.text.x = element_text(color = "white"),
        axis.text.y = element_text(color = "white"), axis.title.x = element_text(color = "white"), axis.title.y = element_text(color = "white"), axis.line = element_blank(),
        plot.background = element_rect(fill = "#393c42")
      )
    if (isTRUE(is.null(input$nhood))) {
      df2 <- df1 %>% group_by(rpt_mnth) %>% summarise(count = n())
      y <- ggplot(df2, aes(x = rpt_mnth, y = count)) + geom_line(size = 1.2, color = "red") +
        labs(x = "Date", y = "Count") +
        theme(
          axis.ticks.y = element_blank(), axis.ticks.x = element_blank(), panel.grid = element_blank(), panel.background = element_rect(fill = "#393c42", linetype = "solid"), axis.text.x = element_text(color = "white"),
          axis.text.y = element_text(color = "white"), axis.title.x = element_text(color = "white"), axis.title.y = element_text(color = "white"), axis.line = element_blank(),
          plot.background = element_rect(fill = "#393c42")
        )
    }
    
    return(y)
  })
  
  output$plot1 <- renderCachedPlot({
    plt1()
  },cacheKeyExpr = {input$nhood } )
  ### SECOND PANEL
  output$year1 <- renderUI({
    selectInput(inputId = "year1", label = "Year")
  })
  output$type <- renderUI({
    selectInput(inputId = "type", label = "Crime Type")
  })
  
  plt2 <- reactive({
    lst1 <- as.character(as.list(input$year1))
    lst2 <- as.character(as.list(input$type))
    lst3 <- as.character(as.list(input$negh2))
    txt <- paste(paste("UCR.Literal==\"", paste(lst2, sep = "", collapse = "\"| UCR.Literal==\""), sep = ""), "\"", sep = "")
    df3 <- filter(df1, rpt_yr == lst1 & (eval(parse(text = txt))))
    print(txt)
    lhs<-paste("y")
    mapfxn<-paste("ggmap(sq_",lst3,")+
                  geom_point(data=df3,aes(x = Longitude, y = Latitude,colour=(UCR.Literal)),size=6,inherit.aes = FALSE) + 
                  labs(fill=\"UCR Crime Type\")+
                  theme(axis.title=element_blank(), 
                  panel.background = element_rect(fill = \"#3d3e3f\"), 
                  plot.background = element_rect(fill = \"#3d3e3f\"), 
                  axis.text.x=element_blank(), 
                  axis.text.y=element_blank(), 
                  legend.title=element_text(size=10,color=\"white\"), 
                  legend.text=element_text(size=10,color=\"white\"), 
                  legend.background = element_rect(fill=\"#3d3e3f\"), 
                  legend.key.size = unit(3,\"line\"))+guides(colour = guide_legend(override.aes = list(size=6)))",sep="")
    
    eq<-paste(lhs,mapfxn,sep="<-")
    print(eq)
    eval(parse(text=eq))
    
    
    return(y)
  })
  output$plot2 <- renderCachedPlot({
    plt2()
  }, cacheKeyExpr = {c(input$year1,input$type,input$negh2) })
  
  #### THIRD PANEL
  output$year3 <- renderUI({
    selectInput(inputId = "year3", label = "Year")
  })
  output$type3 <- renderUI({
    selectInput(inputId = "type3", label = "Crime Type")
  })
  
  plt3 <- reactive({
    lst1 <- as.character(as.list(input$year3))
    lst2 <- as.character(as.list(input$type3))
    lst3 <- as.character(as.list(input$negh))
    txt <- paste(paste("UCR.Literal==\"", paste(lst2, sep = "", collapse = "\"| UCR.Literal==\""), sep = ""), "\"", sep = "")
    df3 <- filter(df1, rpt_yr == lst1 & (eval(parse(text = txt)))) 
    
    lhs<-paste("y")
    mapfxn<-paste("ggmap(sq_",lst3,")+
                  geom_point(data = df3, aes(x = Longitude, y = Latitude,color=UCR.Literal), inherit.aes = FALSE, alpha = .5) + 
                  stat_density2d( data = df3, aes(x = Longitude, y = Latitude, fill = ..level.., alpha = ..level..), 
                  size = 0.01, bins = 16, geom = \"polygon\",inherit.aes = FALSE) +                 theme(axis.title=element_blank(), 
                  panel.background = element_rect(fill = \"#3d3e3f\"), 
                  plot.background = element_rect(fill = \"#3d3e3f\"), 
                  axis.text.x=element_blank(), 
                  axis.text.y=element_blank(), 
                  legend.title=element_blank(), 
                  legend.text=element_blank(), 
                  legend.background = element_blank(),
                  legend.position=\"none\")",sep="")
    
    eq<-paste(lhs,mapfxn,sep="<-")
    print(eq)
    eval(parse(text=eq))
    
    return(y)
  })
  output$plot4 <- renderPlot({
    plt3()
  }, height = 800, width = 600)
  
  ### FOURTH PANEL
  output$year4 <- renderUI({
    selectInput(inputId = "year4", label = "Year")
  })
  plt4 <- reactive({
    df2 <- df1 %>% group_by(rpt_yr, rpt_mnth, rpt_day, time) %>% summarise(count = n())
    x2<-max(df2$count)
    df2$time[is.na(df2$time) | df2$time == 0] <- 24
    df2$count[is.na(df2$count)] <- 0
    lst1 <- as.character((input$year4))
    txt <- paste("rpt_yr==", input$year4, sep = "")
    df3 <- filter(df2, eval(parse(text = txt)))
    df3 <- df3[!duplicated(df3), ]
    
    df4 <- dcast(df3, rpt_mnth + rpt_day + rpt_yr ~ time, fun.aggregate = mean)
    df4[is.na(df4)] <- 0
    df4 <- gather(df4, time, count, `1`:`24`, factor_key = T)
    df4$time <- as.integer(df4$time)
    p <- ggplot(df4, aes(rpt_day, time, fill = count)) + geom_tile(color = "white", size = 0.1)
    p <- p + facet_grid(rpt_yr ~ rpt_mnth)
    p <- p + scale_y_continuous(trans = "reverse", breaks = unique(df2$time))
    p <- p + scale_x_continuous(breaks = c(1, 10, 20, 31))
    p <- p + theme_minimal(base_size = 12)
    p <- p + theme(
      panel.grid = element_blank(), panel.background = element_rect(fill = "#393c42", linetype = "solid"),
      axis.ticks.y = element_blank(), axis.ticks.x = element_blank(), axis.text.x = element_text(color = "white"),
      axis.text.y = element_text(color = "white"), axis.title.x = element_text(color = "white"),
      axis.title.y = element_text(color = "white"), axis.line = element_blank(),
      legend.title=element_text(size=10,color="white"), 
      legend.text=element_text(size=10,color="white"), 
      plot.background = element_rect(fill = "#393c42"), strip.text.x = element_text(size = 16, colour = "white"))+
      scale_fill_carto_c(palette = "SunsetDark",na.value = "transparent", limits=c(0,x2))+
      labs(y="Time",x="Day (Bottom)")

    return(p)
  })
  output$plot5 <- renderCachedPlot({
    plt4()
  }, cacheKeyExpr = {input$year4 })
}
shinyApp(ui = ui, server = server)
