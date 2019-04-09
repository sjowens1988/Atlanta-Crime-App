

library(shiny)
library(shinythemes)
library(ggplot2)
library(RColorBrewer)
library(colorRamps)
col6 <- primary.colors(11)

ui <- 
  shinyUI(fluidPage(
    theme = shinytheme("slate"), navbarPage(""),
    tabPanel(
      "Plot",
      
      mainPanel(
        tabsetPanel(
          type = "tabs",
          
          tabPanel(
            h4("Time Series of Total Crimes by Neighborhood"),
            fluidRow(
              column(6, selectInput("nhood",
                                    "Neighborhood",
                                    choices = c(
                                      unique(as.character(df1$Neighborhood))
                                    ), multiple = TRUE,
                                    selected = NULL
              ))
            ),
            column(
              6,
              verbatimTextOutput("results")
            ),
            fluidRow(
              column(
                12,
                plotOutput("plot1", height = 800, width = 1600)
              )
            )
          ),
          tabPanel(
            h4("Crimes by Neighborhood and UCR Type"),
            fluidRow(
              column(4, selectInput("year1",
                                    "Year",
                                    choices = c(
                                      unique(as.character(df1$rpt_yr))
                                    ),
                                    selected = NULL
              )),
              column(8, selectInput("type",
                                    "Crime Type",
                                    choices = c(
                                      unique(as.character(df1$UCR.Literal))
                                    ),multiple = TRUE, selected = NULL
              )),
              column(8, selectInput("negh2",
                                    "Neghborhood",
                                    choices = c(
                                      unique(as.character(df1$Neighborhood))
                                    ),
                                    selected = NULL
              ))),
            fluidRow(
              column(
                12,
                plotOutput("plot2", height = 800, width = 600)
              )
            )
          ),
          tabPanel(
            h4("Density Map of Crime Type by Neighborhood"),
            fluidRow(
              column(4, selectInput("year3",
                                    "Year",
                                    choices = c(
                                      unique(as.character(df1$rpt_yr))
                                    ),
                                    selected = NULL
              )),
              column(8, selectInput("type3",
                                    "Type",
                                    choices = c(
                                      unique(as.character(df1$UCR.Literal))
                                    ),
                                    selected = NULL
              )),
              column(8, selectInput("negh",
                                    "Neghborhood",
                                    choices = c(
                                      unique(as.character(df1$Neighborhood))
                                    ),
                                    selected = NULL
              ))
            ),
            fluidRow(
              column(
                12,
                plotOutput("plot4", height = 800, width = 600)
              )
            )
          ),
          tabPanel(
            h4("Crime by Time and Month"),
            fluidRow(
              column(4, selectInput("year4",
                                    "Year",
                                    choices = c(
                                      unique(as.character(df1$rpt_yr))
                                    ),
                                    selected = NULL
              )),
              
              fluidRow(
                column(
                  12,
                  plotOutput("plot5", height = 800, width = 1600)
                )
              )
            )
          )
        )
      )
    )
  ))



