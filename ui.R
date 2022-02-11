library(shiny)
library(CausalImpact)
library(ggplot2)
library(dplyr)
library(shinycssloaders)
library(zoo)
library(stringr)

# user interface (frontend)
shinyUI(
  
  # Navigation bar for the app
  navbarPage(title = div(img(src="https://raw.githubusercontent.com/carviagu/impact_effect_dashboard/main/img/IMPACTEFFECTLOGO_transparent.png",
                             style="margin-top: -14px;
                               padding-bottom:10px",
                             height = 59, width = 200)),
             id = "nav",
             
             # Welcome pane 
             tabPanel("Welcome", fluid = TRUE,
                      br(),
                      h1(strong("Welcome to the"), 
                         style = "font-size:50px;", align = "center"),
                      div(align = "center", style="margin-top:10px",
                          img(src="https://raw.githubusercontent.com/carviagu/impact_effect_dashboard/main/img/IMPACTEFFECTLOGO.PNG",
                              height = 150)),
                      p("Discover more about Causal Impact and see how it works", 
                        style="font-size:25px; margin-top:30px", align = "center"),
                      div(align = "center", style = "margin-top:50px",
                          actionButton(inputId = "goToSim", label = "Start Simulating", 
                                       style="color: #fff; background-color: #337ab7; 
                                       border-color: #2e6da4; align:center;
                                       padding:4px; font-size:150%"),
                          actionButton(inputId = "goToAb", label = "About",
                                       style = "align:center; padding:4px; font-size:150%")),
                      p("Impact Effect Dashboard is an app created to understad how Causal Impact
                        library works with different examples where you can study the effects on 
                        different circumstances.", 
                        style="font-size:20px; margin-top:20px", align = "center"),
                      div(align = "center",style = "margin-top:100px",
                          p("Created with"),
                          img(src="https://raw.githubusercontent.com/carviagu/impact_effect_dashboard/main/img/Rstudio_logo.png",
                              height = 50),
                          img(src="https://raw.githubusercontent.com/carviagu/impact_effect_dashboard/main/img/Shiny-logo.png",
                              height = 100))
                          
                      ),
             
             # Simulation pane
             tabPanel("Simulator", fluid = TRUE, icon = icon("chart-area"),
                      
                      titlePanel("Causal Impact Effect Simulation"),
               # Main panel
               sidebarLayout(
                 
                 # Options section
                 sidebarPanel(
                   p("Select an example dataset and stablish the start and end conditions of the impact, to
          see Causal Impact in action:"),
                   
                   selectInput(
                     inputId = "selData",
                     label = h4('Dataset'),
                     choices = c("views", 
                                 "BTS", 
                                 "olivia rodriguez", 
                                 "Travis Scott", 
                                 "disney"),
                     selected = "example"
                   ),
                   
                   uiOutput(
                     outputId = "datePlace"
                   ),
                   
                   br(),
                   h4("End condition"),
                   
                   selectInput(
                     inputId = "selEnd",
                     label = 'Criteria',
                     choices = list(
                       "Fixed Interval" = 0,
                       "Maximum" = 1,
                       "Minimum" = 2
                     ),
                     selected = "Fixed Interval"
                   ),
                   
                   conditionalPanel(
                     condition = "input.selEnd == 0",
                     numericInput(
                       inputId = "numDays",
                       label = 'Days',
                       value = 10,
                       min = 0
                     )
                   ),
                   
                   conditionalPanel(
                     condition = "input.selEnd == 1",
                     numericInput(
                       inputId = "maxNum",
                       label = 'Upper limit',
                       value = 110
                     )
                   ),
                   
                   conditionalPanel(
                     condition = "input.selEnd == 2",
                     numericInput(
                       inputId = "minNum",
                       label = 'Lower limit',
                       value = 50
                     )
                   )
                 ),
                 
                 # Plots area
                 mainPanel(
                   plotOutput(
                     outputId = 'mainPlot',
                     click = 'onClick'
                   ) %>% withSpinner(type = 8, size = 0.5, color = '#7D7C7C'),
                   
                   # Add information about impact (quantity, dates and days)
                   fluidRow( align="center",
                     column(width=4, align="left",
                           textOutput(
                             outputId = "totalImp"
                               ),
                           textOutput(
                             outputId = "startDay"
                               )
                            ),
                    
                     column(width=4, align="left",
                            textOutput(
                              outputId = 'daysRec'
                                    ),
                            textOutput(
                              outputId = 'endDay'
                                    )
                            ),
                     
                   ),
                   
                   p(
                     "\n"
                   ),
                   
                   wellPanel(
                     fluidRow(
                       column(2, 
                              h4('More details:')),
                       
                       column(2,
                              checkboxInput(
                                inputId ="showAcum",
                                label = "Acumulated Impact",
                                value = TRUE
                              )),
                       column(4,
                              checkboxInput(
                                inputId ="showPunc",
                                label = "Punctual / Relative Impact",
                                value = FALSE
                              )),
                       column(4, align="right",
                              actionButton(
                                inputId ="showSum",
                                label = strong("Show Report"),
                                icon = icon("fas fa-file-alt")
                              ))
                     )
                   ),
                   
                   plotOutput(
                     outputId = 'subPlot'
                     ) %>% withSpinner(type = 8, size = 0.5, color = '#7D7C7C')
                   )
                 )
              ),
      tabPanel("About",icon = icon("book"),
        includeMarkdown("resources/about.md")
      )
      
    )
   
    
) # UI

