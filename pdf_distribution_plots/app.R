#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Rshiny ideas from on https://gallery.shinyapps.io/multi_regression/
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
library(ggplot2)
library(shiny) 
require(LearnBayes)
library(tidyverse)
library(shinyWidgets)
library(shinythemes)  # more funky looking apps
# library(shinydashboard)
options(max.print=1000000)
fig.width <- 1375
fig.height <- 550
fig.width2 <- 1375  
fig.height2 <- 730
fig.width3 <- 1375 
fig.height3 <- 800
p1 <- function(x) {formatC(x, format="f", digits=1)}
p2 <- function(x) {formatC(x, format="f", digits=2)}
options(width=140)
set.seed(12345) # reproducible

pop=1e6 # this is the population size we take sample from
is.even <- function(x){ x %% 2 == 0 } # function to id. odd maybe useful
# Always remember that the purpose of a parallel-group randomized trial is to compare the parallel groups, 
# NOT to look at change from baseline.  Baseline should always be an adjustment covariate (only).

xwidth <- 50
xwidth2 <- 3


 
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
ui <- fluidPage(theme = shinytheme("journal"), #https://www.rdocumentation.org/packages/shinythemes/versions/1.1.2 
                # paper
                
                
                setBackgroundColor(
                    color = c( "#2171B5", "#F7FBFF"),
                    gradient = "linear",
                    direction = "bottom"
                ),
                
                h2("Estimates and tests of proportions Bayesian style"),
                
                h4(" xxxxxxxxxxxxxxxxxxx [1].'
              "), 
                
                h3("  "), 
                # shinyUI(pageWithSidebar(
                #     titlePanel("Hello Shiny!"),
                
                sidebarLayout(
                    
                    sidebarPanel( width=3 ,
                                  
                                  tags$style(type="text/css", ".span8 .well { background-color: #00FFFF; }"),
                                  #wellPanel(style = "background: #2171B5",),
                                  #The first slider sets the power and the next alpha level, so we can power the trial as we wish. 
                                  h4("xxxxxxxxxxxxxxxxxx."),
                                  
                                  actionButton(inputId='ab1', label="R code",   icon = icon("th"), 
                                               onclick ="window.open('https://raw.githubusercontent.com/eamonn2014/responder-non-responder-fallacy-in-RCTs/master/app.R', '_blank')"),    
                                  actionButton("resample", "Simulate a new sample"),
                                  br(), # br(),
                                  tags$style(".well {background-color:#b6aebd ;}"), ##ABB0B4AF
                                  
                                  h4("An appropriate prior to use for a proportion is a Beta prior. We help you select a Beta prior"),
                                  div(
                                      
                                      tags$head(
                                          tags$style(HTML('#ab1{background-color:orange}'))
                                      ),
                                      
                                      tags$head(
                                          tags$style(HTML('#resample{background-color:orange}'))
                                      ),
                                      
                                      div(h5("Enter Normal mean and standard deviation")),
                                      fluidRow(
                                          column(width = xwidth2,
                                                 textInput('m1', 
                                                           div(h5("Mean")), value= "0", width=xwidth),
                                           ), 
                                          column(width = xwidth2,
                                                 textInput('s1', 
                                                           div(h5("SD")), value= "3.5", width=xwidth),
                                          )),
                                      fluidRow(
                                          column(width = xwidth2,
                                                 textInput('m2', 
                                                           div(h5("Mean")), value= "", width=xwidth),
                                          ), 
                                          column(width = xwidth2,
                                                 textInput('s2', 
                                                           div(h5("SD")), value= "", width=xwidth),
                                          )),
                                      fluidRow(
                                          column(width = xwidth2,
                                                 textInput('m3', 
                                                           div(h5("Mean")), value= "", width=xwidth),
                                          ), 
                                          column(width = xwidth2,
                                                 textInput('s3', 
                                                           div(h5("SD")), value= "", width=xwidth),
                                          )),
                                      div(h5("Enter Cauchy location and scale")),
                                      fluidRow(
                                      column(width = xwidth2,
                                             textInput('cm1', 
                                                       div(h5("Mean")), value= "0", width=xwidth),
                                      ), 
                                      column(width = xwidth2,
                                             textInput('cs1', 
                                                       div(h5("SD")), value= "3.5", width=xwidth),
                                      )),
                                  fluidRow(
                                      column(width = xwidth2,
                                             textInput('cm2', 
                                                       div(h5("Mean")), value= "", width=xwidth),
                                      ), 
                                      column(width = xwidth2,
                                             textInput('cs2', 
                                                       div(h5("SD")), value= "", width=xwidth),
                                      )),
                                  fluidRow(
                                      column(width = xwidth2,
                                             textInput('cm3', 
                                                       div(h5("Mean")), value= "", width=xwidth),
                                      ), 
                                      column(width = xwidth2,
                                             textInput('cs3', 
                                                       div(h5("SD")), value= "", width=xwidth),
                                      )),
                                  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                                  div(h5("Enter t distribution df, location and scale")),
                                  fluidRow(
                                    column(width = xwidth2,
                                         textInput('t1a', 
                                                   div(h5("df")), value= "3", width=xwidth),
                                  ), 
                                  column(width = xwidth2,
                                         textInput('t1b', 
                                                   div(h5("Location")), value= "0", width=xwidth),
                                  ),
                    
                        column(width = xwidth2,
                               textInput('t1c', 
                                         div(h5("Scale")), value= "3", width=xwidth),
                        )),
                               
                               
                               
                        
                        fluidRow(
                            column(width = xwidth2,
                                   textInput('t2a', 
                                             div(h5("df")), value= "", width=xwidth),
                            ), 
                            column(width = xwidth2,
                                   textInput('t2b', 
                                             div(h5("Location")), value= "", width=xwidth),
                            ),
                            
                            column(width = xwidth2,
                                   textInput('t2c', 
                                             div(h5("Scale")), value= "", width=xwidth),
                            )),
                        
                        
                        
                        
                        fluidRow(
                            column(width = xwidth2,
                                   textInput('t3a', 
                                             div(h5("df")), value= "", width=xwidth),
                            ), 
                            column(width = xwidth2,
                                   textInput('t3b', 
                                             div(h5("Location")), value= "", width=xwidth),
                            ),
                            
                            column(width = xwidth2,
                                   textInput('t3c', 
                                             div(h5("Scale")), value= "", width=xwidth),
                            )),
                        
                        
                        
                        
                        
                        
                        
                        
                        
                                      div(h5("References:")),  
                                      
                                      tags$a(href = "https://a-little-book-of-r-for-bayesian-statistics.readthedocs.io/en/latest/src/bayesianstats.html", "[1] Using R for Bayesian Statistics"),
                                      div(p(" ")),
                                      tags$a(href = "https://www.ncbi.nlm.nih.gov/pmc/articles/PMC524113/pdf/bmj32900966.pdf", "[2] Individual response to treatment: is it a valid assumption?"),
                                      div(p(" ")),
                                      tags$a(href = "https://www.youtube.com/watch?v=uiCd9m6tmt0&feature=youtu.be", "[3] Professor George Davey Smith - Some constraints on the scope and potential of personalised medicine"),
                                      div(p(" ")),
                                      tags$a(href = "https://physoc.onlinelibrary.wiley.com/doi/epdf/10.1113/EP085070", "[4] True and false interindividual differences in the physiological response to an intervention"),
                                      div(p(" ")),
                                      tags$a(href = "https://twitter.com/f2harrell/status/1220700181496320001", "[5] Purpose of RCT"),
                                      div(p(" ")),
                                      tags$a(href = "https://www.nature.com/magazine-assets/d41586-018-07535-2/d41586-018-07535-2.pdf", "[6] Statistical pitfalls of personalized medicine"),
                                      div(p(" ")),
                                  )
                    
                    
                    ),
                    
                    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~tab panels
                    mainPanel(width=9,
                              
                              #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                              #    tabsetPanel(type = "tabs", 
                              navbarPage(       
                                  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~  
                                  tags$style(HTML("
                            .navbar-default .navbar-brand {color: orange;}
                            .navbar-default .navbar-brand:hover {color: blue;}
                            .navbar { background-color: #b6aebd;}
                            .navbar-default .navbar-nav > li > a {color:black;}
                            .navbar-default .navbar-nav > .active > a,
                            .navbar-default .navbar-nav > .active > a:focus,
                            .navbar-default .navbar-nav > .active > a:hover {color: pink;background-color: purple;}
                            .navbar-default .navbar-nav > li > a:hover {color: black;background-color:yellow;text-decoration:underline;}
                            .navbar-default .navbar-nav > li > a[data-value='t1'] {color: red;background-color: pink;}
                            .navbar-default .navbar-nav > li > a[data-value='t2'] {color: blue;background-color: lightblue;}
                            .navbar-default .navbar-nav > li > a[data-value='t3'] {color: green;background-color: lightgreen;}

                   ")),
                                  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~end of section to add colour     
                                  tabPanel("1 xxxxxxxxxxxxxxx", value=6, 
                                           h4(" "), 
                                           div(plotOutput("plot", width=fig.width3, height=fig.height3)),
                                           h4("Figure 1 The best Beta distibution for the selected beliefs."),
                                  ) ,
                                  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~end of section to add colour     
                                  tabPanel("2 xxxxxxxxxxxxxxxxxx", 
                                          # div(plotOutput("plot", width=fig.width3, height=fig.height3))
                                           
                                  ) 
                                  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                                  
                                  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                              )
                              #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                    )
                    
                ) #
                #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~end tab panels 
                
) 

server <- shinyServer(function(input, output   ) {
    
    # --------------------------------------------------------------------------
    # ---------------------------------------------------------------------------
    # --------------------------------------------------------------------------
    # This is where a new sample is instigated 
    output$plot <- renderPlot({  
        
        # Dummy line to trigger off button-press
        m1 <- as.numeric(input$m1 )
        s1 <- as.numeric(input$s1)
        m2 <- as.numeric(input$m2)
        s2 <- as.numeric(input$s2)
        m3 <- as.numeric(input$m3)
        s3 <- as.numeric(input$s3)
        cm1 <-as.numeric(input$cm1)
        cs1 <- as.numeric(input$cs1)
        cm2 <- as.numeric(input$cm2)
        cs2 <- as.numeric(input$cs2)
        cm3 <- as.numeric(input$cm3)
        cs3 <- as.numeric(input$cs3)
        t1a <- as.numeric(input$t1a)
        t1b <- as.numeric(input$t1b)
        t1c <- as.numeric(input$t1c )
        t2a <- as.numeric(input$t2a)
        t2b <- as.numeric(input$t2b)
        t2c <- as.numeric(input$t2c)
        t3a <- as.numeric(input$t3a)
        t3b <- as.numeric(input$t3b)
        t3c <- as.numeric(input$t3c )
         
    
        
        
        
        
        
        xs <- seq(-20,20, by=2)
        
        xs2 <- seq(-4,4, by=2)
        prob <- print(exp(xs2)/(1+exp(xs2)), digits=2)
        prob <- round(prob,2)
        #p2 <- function(x) {formatC(x, format="f", digits=2)}
        
        
        ##if (n =1) {
        x_values <- seq(-20,20, length.out = 1000)
        
        data.frame(x_values) %>%
            ggplot(aes(x_values) ) +  #  ggplot(aes(x_values,size=.8) )
            #scale_size_manual( values = c(4,2,1) ) +
            stat_function(fun = dnorm,   args=list(mean=m1        ,sd=s1, log = FALSE), aes(colour = "N(mean=0, sd=2.5)"),  size=2) + 
            stat_function(fun = dnorm,   args=list(mean=m2        ,sd=s2, log = FALSE), aes(colour = "N(mean=0, sd=2.5)"),  size=2) +  
            stat_function(fun = dnorm,   args=list(mean=m3        ,sd=s3, log = FALSE), aes(colour = "N(mean=0, sd=2.5)"),  size=2) + 
            stat_function(fun = dcauchy, args=list(location = cm1, scale = cs1),     aes(colour = "Cauchy(location=0, scale=2.5)"),  size=2) + 
            stat_function(fun = dcauchy, args=list(location = cm2, scale = cs2),     aes(colour = "Cauchy(location=0, scale=2.5)"),  size=2) + 
            stat_function(fun = dcauchy, args=list(location = cm3, scale = cs3),     aes(colour = "Cauchy(location=0, scale=2.5)"),  size=2) + 
            stat_function(fun = dst,     args=list(nu=t1a,mu=t1b     ,sigma=t1c),        aes(colour = "t(3df, mean=0, scale=2.5)"),  size=2) + 
            stat_function(fun = dst,     args=list(nu=t2a,mu=t2b     ,sigma=t2c),        aes(colour = "t(3df, mean=0, scale=2.5)"),  size=2) + 
            stat_function(fun = dst,     args=list(nu=t3a,mu=t3b     ,sigma=t3c),        aes(colour = "t(3df, mean=0, scale=2.5)"),  size=2) + 
         
            scale_colour_manual("", values = c("red", "blue", "green", "black","darkgreen", "yellow","brown","orange","grey"))  +
            labs(title="Distributions", 
                 x = "log odds",
                 y = "",
                 subtitle =paste0(c("Note probabilites", prob," are equivalent to log odds: -4,-2, 0 ,2, 4 "), collapse=", "),
                 caption = "") +
            guides(fill=FALSE) +
            theme_bw() +
            theme(legend.justification=c(1,0), legend.position=c(.96,.6)) +
            scale_x_continuous("log odds", breaks=xs, labels=xs, limits=c(-20,20)) +
            theme(legend.position="none") +
            theme(#panel.background=element_blank(),
                # axis.text.y=element_blank(),
                # axis.ticks.y=element_blank(),
                # https://stackoverflow.com/questions/46482846/ggplot2-x-axis-extreme-right-tick-label-clipped-after-insetting-legend
                # stop axis being clipped
                plot.title=element_text(size = 18), plot.margin = unit(c(5.5,12,5.5,5.5), "pt"),
                legend.text=element_text(size=14),
                legend.title=element_text(size=14),
                legend.position="none",
                axis.text.x  = element_text(size=15),
                axis.text.y  = element_text(size=15),
                axis.line.x = element_line(color="black"),
                axis.line.y = element_line(color="black"),
                plot.caption=element_text(hjust = 0, size = 7),
                strip.text.x = element_text(size = 16, colour = "black", angle = 0),
                axis.title.y = element_text(size = rel(1.5), angle = 90),
                axis.title.x = element_text(size = rel(1.5), angle = 0),
                panel.grid.major.x = element_line(color = "grey80", linetype="dotted", size = 1),
                panel.grid.major.y = element_line(color = "grey80", linetype="dotted", size = 1),
                strip.background = element_rect(colour = "black", fill = "#ececf0"),
                panel.background = element_rect(fill = '#ececf0', colour = '#ececf0'),
                plot.background = element_rect(fill = '#ececf0', colour = '#ececf0')
            )
        
        
       
    })
        
        
        
        
        
        
        
        
        
        
 
    
  
    # --------------------------------------------------------------------------
    # ---------------------------------------------------------------------------
})

# Run the application 
shinyApp(ui = ui, server = server) 