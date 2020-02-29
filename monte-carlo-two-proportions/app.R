#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Rshiny ideas from on https://gallery.shinyapps.io/multi_regression/
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
library(ggplot2)
library(shiny) 
require(LearnBayes)
library(tidyverse)
library(shinyWidgets)
library(shinythemes)  # more funky looking apps
library(rstan)
library(DT)
# library(shinydashboard)
options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)
options(max.print=1000000)
fig.width <- 400
fig.height <- 300
fig.width2 <- 1400
fig.height2 <- 350
fig.width3 <- 1300  
fig.height3 <- 545
p1 <- function(x) {formatC(x, format="f", digits=1)}
p2 <- function(x) {formatC(x, format="f", digits=2)}
options(width=200)
set.seed(12345) # reproducible

pop=1e6 # this is the population size we take sample from
is.even <- function(x){ x %% 2 == 0 } # function to id. odd maybe useful
# Always remember that the purpose of a parallel-group randomized trial is to compare the parallel groups, 
# NOT to look at change from baseline.  Baseline should always be an adjustment covariate (only).


# findBeta <- function(quantile1,quantile2,quantile3)
# {
#   # find the quantiles specified by quantile1 and quantile2 and quantile3
#   quantile1_p <- quantile1[[1]]; quantile1_q <- quantile1[[2]]
#   quantile2_p <- quantile2[[1]]; quantile2_q <- quantile2[[2]]
#   quantile3_p <- quantile3[[1]]; quantile3_q <- quantile3[[2]]
#   
#   # find the beta prior using quantile1 and quantile2
#   priorA <- beta.select(quantile1,quantile2)
#   priorA_a <- priorA[1]; priorA_b <- priorA[2]
#   
#   # find the beta prior using quantile1 and quantile3
#   priorB <- beta.select(quantile1,quantile3)
#   priorB_a <- priorB[1]; priorB_b <- priorB[2]
#   
#   # find the best possible beta prior
#   diff_a <- abs(priorA_a - priorB_a); diff_b <- abs(priorB_b - priorB_b)
#   step_a <- diff_a / 100; step_b <- diff_b / 100
#   if (priorA_a < priorB_a) { start_a <- priorA_a; end_a <- priorB_a }
#   else                     { start_a <- priorB_a; end_a <- priorA_a }
#   if (priorA_b < priorB_b) { start_b <- priorA_b; end_b <- priorB_b }
#   else                     { start_b <- priorB_b; end_b <- priorA_b }
#   steps_a <- seq(from=start_a, to=end_a, length.out=1000)
#   steps_b <- seq(from=start_b, to=end_b, length.out=1000)
#   max_error <- 10000000000000000000
#   best_a <- 0; best_b <- 0
#   for (a in steps_a)
#   {
#     for (b in steps_b)
#     {
#       # priorC is beta(a,b)
#       # find the quantile1_q, quantile2_q, quantile3_q quantiles of priorC:
#       priorC_q1 <- qbeta(c(quantile1_p), a, b)
#       priorC_q2 <- qbeta(c(quantile2_p), a, b)
#       priorC_q3 <- qbeta(c(quantile3_p), a, b)
#       priorC_error <- abs(priorC_q1-quantile1_q) +
#         abs(priorC_q2-quantile2_q) +
#         abs(priorC_q3-quantile3_q)
#       if (priorC_error < max_error)
#       {
#         max_error <- priorC_error; best_a <- a; best_b <- b
#       }
#     }
#   }
#   return(list(a= best_a , b=best_b ))
#   #print(paste("The best beta prior has a=",best_a,"b=",best_b))
# }

# n1 = 52 # men
# y1 = 9  # left-handed men
# n2 = 48 # women
# y2 = 4  # left-handed women

#52, 9
#48, 4


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
                                               onclick ="window.open('https://github.com/eamonn2014/Bayesian-proportions-logistic-regression/blob/master/monte-carlo-two-proportions/app.R', '_blank')"),    
                                  actionButton("resample", "Rerun the Monte Carlo simulations"),
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
                                      
                                      #div(strong("Select the parameters using the sliders below"),p(" ")),
                                      # div((" ")),
                                      #br(),
                                      # 
                                      
                                      
                                      # sliderInput("prob1",
                                      #             h5("This percentile"),
                                      #             min=0, max=1, step=.01, value=.1, ticks=FALSE),
                                      
                                      #https://a-little-book-of-r-for-bayesian-statistics.readthedocs.io/en/latest/src/bayesianstats.html
                                      # sliderTextInput("prob1","This percentile:",
                                      #                 
                                      #                 choices=c( 0.00001, seq(.1:.99, by=.01), 0.9999),
                                      #                 
                                      #                 selected=.1, grid = T, width = '100%'),
                                      
                                      
                                      # if you want to estimate the proportion of x, you might have a rough idea that the most likely value is around 0.85, 
                                      # but that the proportion is unlikely to be smaller than 0.60 or bigger than 0.95.
                                      
                                      #                  textInput('vec1', 
                                      #                            div(h5("Select 3 percentiles:")), "0.00001, 0.5, 0.99999"),
                                      #                  
                                      #                  textInput('vec2', 
                                      #                            div(h5("Now the values. for example the default is, that the most likely value is around 0.85 but the proportion 
                                      # is highly unlikely to be smaller than 0.60 or bigger than 0.95")), "0.60, 0.85, 0.95"),
                                      #                  
                                      
                                      # textInput('vec3', 
                                      #           div(h5("Beta parameters for treatment prior")), "1, 1"),
                                      # 
                                      # textInput('vec4', 
                                      #           div(h5("Beta parameters for control prior")), "10, 43"),
                                      # 
                                      # textInput('n1y1', 
                                      #           div(h5("Treatment sample size and responders")), "25, 14"),
                                      # 
                                      # textInput('n2y2', 
                                      #           div(h5("Control sample size and responders")), "25, 4"),
                                      
                                      textInput('vec3', 
                                                div(h5("Beta parameters for treatment prior")), "1, 1"),
                                      
                                      textInput('vec4', 
                                                div(h5("Beta parameters for control prior")), "10, 43"),
                                      
                                      textInput('n1y1', 
                                                div(h5("Treatment sample size and no of events")), "52, 9"),
                                      
                                      textInput('n2y2', 
                                                div(h5("Control sample size and no of events")), "48, 4"),
                                      
                                      # textInput('prio', 
                                      #           div(h5("type in prior for intercept...examples\nnormal(0,5)\ncauchy(0,2.5)\n")), "student_t(3,0,2.5)"),
                                      # 
                                      div(h5("References:")),  
                                      
                                      tags$a(href = "https://lingpipe-blog.com/2009/10/13/bayesian-counterpart-to-fisher-exact-test-on-contingency-tables/", "[1] Ling Pipe"),
                                      div(p(" ")),
                                      tags$a(href = "https://statmodeling.stat.columbia.edu/2009/10/13/what_is_the_bay/", "[2] Gelman"),
                                      div(p(" ")),
                                      tags$a(href = "https://en.wikipedia.org/wiki/Beta_distribution", "[3] Beta distribution"),
                                       div(p(" ")),
                                       tags$a(href = "https://www.tjmahr.com/bayesian-fisher-exact-test/", "[4] Blog article"),
                                       div(p(" ")),
                                      # tags$a(href = "https://twitter.com/f2harrell/status/1220700181496320001", "[5] Purpose of RCT"),
                                      # div(p(" ")),
                                      # tags$a(href = "https://www.nature.com/magazine-assets/d41586-018-07535-2/d41586-018-07535-2.pdf", "[6] Statistical pitfalls of personalized medicine"),
                                      # div(p(" ")),
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
                                  # 
                                  # tabPanel("4 Results", value=3,
                                  #         # div( verbatimTextOutput("reg.summary2")),
                                  #          #    div(plotOutput("res.plot3", width=fig.width2, height=fig.height2)), 
                                  #          h4("Figure 4 Bayesian Monte Carlo Estimates"),    
                                  #       #  div( verbatimTextOutput("stats")),
                                  # ) ,
                                  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                                  tabPanel("1 Prior and posterior analytic distributions", value=7, 
                                           #  h4("Priors"),
                                           
                                           fluidRow(
                                               #  column(width = 5, offset = 0, style='padding:1px;',
                                               #      div(plotOutput("trt.plot", width=fig.width, height=fig.height))),  
                                               
                                               column(width = 7, offset = 0, style='padding:0px;',
                                                      
                                                      h4("Prior distributions"), 
                                                      div(plotOutput("trt.plot", width=fig.width2, height=fig.height2)), 
                                                     # div( verbatimTextOutput("reg.summary2")),
                                                      # h4("STAN logistic regression modelling"),
                                                      
                                                      h4("Posterior distributions updated with observed data"), 
                                                      div(plotOutput("trt.plot1", width=fig.width2, height=fig.height2)),       
                                                      
                                                      #  div( verbatimTextOutput("reg.summary4"))
                                                      
                                               ))
                                           # ), 
                                  ) ,
                                  
                                  tabPanel("2 Quantities of interest generated from posteriors", value=7, 
                                         #  h4("Priors"),
                                           
                                           fluidRow(
                                               column(width = 5, offset = 0, style='padding:1px;',
                                            
                                               h4("Posterior distributions summaries"), 
                                               div( verbatimTextOutput("reg.summary2")))),
                                               
                                              # column(width = 7, offset = 0, style='padding:0px;',
                                          
                                                  
                                                     # h4("STAN logistic regression modelling"),
                                                      
                                                     h4(paste("Posterior distributions : 1 risk difference (trt-ctrl);","2 relative risk (trt/ctrl); 3 odds ratio [odds(trt)/odds(ctrl)]")), 
                                                     div(plotOutput("diff", width=fig.width3, height=fig.height3)),       
                                         
                                         h6(paste("Blue vertical lines demark 95% credible intervals, red dashed lines are population values of interest")), 
                                                    #  div( verbatimTextOutput("reg.summary4"))
                                                      
                                              # )
                                               #)
                                           # ), 
                                  ) ,
                                  
                                  
                                  
                                  
                                  tabPanel("2 Results using datatable", value=3, 
                                           #  h4("Data listing"),
                                           h6("Sort and filter on the fly."),
                                           DT::dataTableOutput("tablex"),
                                           
                                           
                                  )# ,
                                  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                                  # tabPanel("3 Diagnostics", value=6, 
                                  #          # div(plotOutput("reg.plot4", width=fig.width, height=fig.height)), 
                                  #          h4("xxxxxxxxxxxxxxxxxxxxxxx."),         
                                  #        #  div(plotOutput("trt.plot", width=fig.width, height=fig.height)),  
                                  #          
                                  #          # fluidRow(
                                  #          #   column(12,
                                  #          #          sliderInput("senn",
                                  #          #                      strong("Clinical relevant difference"),
                                  #          #                      min=-10, max=10, step=.1, value=-2, ticks=FALSE))
                                  #          # ),
                                  #          #h4("Figure 5 xxxxxxxxxxxxxxxxxx."),         
                                  #          
                                  #          p(strong(" ")),
                                  #          p(strong("xxxxxxxxxxxxxxxxx.")),
                                  #          
                                  #          p(strong("xxxxxxxxxxxxxxxxxxxxxx"
                                  #          )),
                                  #          
                                  #          
                                  #          h4("xxxxxxxxxxxxxxxxxxxxxxx"),
                                  #          #    div( verbatimTextOutput("senn.est")),
                                  #          h4("xxxxxxxxxxxxxxxxxxxx"),
                                  #          #    div( verbatimTextOutput("senn.est2"))
                                  #          
                                  # ) ,
                                  # #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                                  # #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                                  # tabPanel("4 Caterpillar plots", value=3, 
                                  #         # div(plotOutput("dplot2", width=fig.width, height=fig.height)),  
                                  #          #  DT::dataTableOutput("table1"),
                                  #          
                                  # ) # ,
                                  # 
                                  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                                  # tabPanel("7 xxxxxxxxxxxxxxxxxxxx", 
                                  #                                                
                                  #   textOutput("foo"),
                                  #       tags$style(type="text/css", "textx {white-space: pre-wrap;}"),
                                  #        htmlOutput("textx"))
                                  
                                  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                                  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                              )
                              #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                    )
                    
                ) #
                #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~end tab panels 
                
)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#                             tabPanel("5 Model1", value=3, 
#                                      div( verbatimTextOutput("mod1")), 
#                                      #  DT::dataTableOutput("table1"),
#                                      
#                             ) ,
#                             #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#                             tabPanel("6 xxxxxxxxxxxxxxxxxxxx", 
#                                   
#                                      h4(htmlOutput("textWithNumber3",) ) ,
#                                      
#                                      htmlOutput("textx"),
#                             ),
#                                      
#                                    #  width = 12 ),
#                           
#                             
#                             tabPanel("7 xxxxxxxxxxxxxxxxxxxx", 
#                                      
#                                      textOutput("foo"),
#                                      tags$style(type="text/css", "textx {white-space: pre-wrap;}"),
#                             
#                             
#                             htmlOutput("textx"))
#                             #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#                           #)
#                           #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#                # )
#                 
#               ) #
#               #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~end tab panels 
#               
# )
# 
#                 ) )

server <- shinyServer(function(input, output   ) {
    
    # --------------------------------------------------------------------------
    # ---------------------------------------------------------------------------
    # --------------------------------------------------------------------------
    # This is where a new sample is instigated 
    random.sample <- reactive({
        
        foo <- input$resample
        
        # i <- as.numeric(unlist(strsplit(input$vec1,",")))
        # j <- as.numeric(unlist(strsplit(input$vec2,",")))
        # 
        trt <- as.numeric(unlist(strsplit(input$vec3,",")))
        ctr <- as.numeric(unlist(strsplit(input$vec4,",")))
        
        n1y1 <- as.numeric(unlist(strsplit(input$n1y1,","))) #trt
        n2y2 <- as.numeric(unlist(strsplit(input$n2y2,",")))
        
       # prio <- as.character(input$prio)
        #
        
        
        return(list( #prob1=i[1],prob2=j[1],prob3=i[2],prob4=j[2],prob5=i[3],prob6=j[3],
            trt.alpha=trt[1], trt.beta=trt[2],
            ctr.alpha=ctr[1], ctr.beta=ctr[2],
            n1=n1y1[1],y1=n1y1[2],
            n2=n2y2[1],y2=n2y2[2]#, prio=prio
        )) 
        
    })
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    mcmc <- reactive({
        
        sample <- random.sample()
        
        a  <- trt.alpha<- sample$trt.alpha
        b  <- trt.beta<-sample$trt.beta
        a1 <- sample$ctr.alpha
        b1 <- sample$ctr.beta
        
        n1 <- sample$n1  #trt
        y1 <- sample$y1
        n2 <- sample$n2
        y2 <- sample$y2
        
        
        I = 500000                               # simulations
        
        theta1 = rbeta(I, y1+a, n1-y1+b)        # incorp. prior for trt
        theta2 = rbeta(I, y2+a1, n2-y2+b1)      # incorp. prior for placebo
        diff =   theta1-theta2                    # simulated differences
        ratio =  theta1/theta2 
        or <-   (theta1/ (1-theta1)) / (theta2/(1-theta2))
        ptrt <-  theta1>theta2
        prob.ratio <- (theta1/theta2 )>1
        prob.or <-  ((theta1/ (1-theta1)) / (theta2/(1-theta2))) > 2
        
        f <- data.frame(cbind( diff, ratio, or ,ptrt, prob.ratio, prob.or , theta1, theta2))
        
        rnamez <- c( "risk difference trt-ctrl","relative risk trt/ctrl", "odds ratio trt:ctrl", 
                     "p(trt > ctrl)", "pr(risk ratio > 1)", "pr(odds ratio > 1)",
                     "p(y=1|trt)","p(y=1|ctrl)" )
        
        f <- data.frame(cbind( diff, ratio, or ,ptrt,  theta1, theta2))
        
        rnamez <- c( "risk difference trt-ctrl","relative risk trt/ctrl", "odds ratio trt:ctrl", 
                     "p(trt > ctrl)", #"pr(risk ratio > 1)", "pr(odds ratio > 1)",
                     "p(y=1|trt)","p(y=1|ctrl)" )
        
        cnamez <- c("Mean","2.5%","25%","50%","75%","97.5%")
        
        clean <- function(x) { c(mean(x), quantile(x,c(.025, 0.25, 0.5, 0.75, 0.975))) }
        
        x <- apply(f, 2, clean)
        
        z <- t(x)
        
        rownames(z) <- rnamez
        colnames(z) <- cnamez
        
        
   
        return(list(f1= z , diff = diff,   theta1 = theta1,
                    theta2 = theta2  , 
                    ratio = ratio ,
                    or = or,
                    ptrt = theta1>theta2  )) 
        
    })
    
    
    output$diff <- renderPlot({         
        
        z <- mcmc()$diff
       
        q <- quantile(z,c(.025, 0.25, 0.5, 0.75, 0.975))
        par(bg = 'lightgoldenrodyellow') 
        par(mfrow=c(1,3))
         plot(density(z),
              xlab="risk differnece trt - ctrl",
              ylab="p(trt - ctrl | y, n)",
              main="",
              ylim=c(0,max(density(z)$y)),
              frame.plot=FALSE,cex.lab=1.5,lwd=3,yaxt="no")
         abline(v=q[1], col="blue") #95% credible interval
         abline(v=q[5], col="blue")
         abline(v=0, col="red", lty='dashed')
         
         z <- (mcmc()$ratio)
         
         q <- quantile(z,c(.025, 0.25, 0.5, 0.75, 0.975))
         
         plot(density(z), log="x",
              xlab="relative risk trt / ctrl",
              ylab="p(trt / ctrl | y, n)",
              main="",
              ylim=c(0, max(density(z)$y)),##
              frame.plot=FALSE,cex.lab=1.5,lwd=3,yaxt="no")
         abline(v=q[1], col="blue") #95% credible interval
         abline(v=q[5], col="blue")
         abline(v=1, col="red", lty='dashed')
         
         z <- mcmc()$or
         
         q <- quantile(z,c(.025, 0.25, 0.5, 0.75, 0.975))
         
         plot(density(z),   log="x",
              xlab="odds ratio",
              ylab="p(odds trt / odds ctrl | y, n)",
              main="",
              ylim=c(0, max(density(z)$y)),
              frame.plot=FALSE,cex.lab=1.5,lwd=3,yaxt="no")
         abline(v=q[1], col="blue") #95% credible interval
         abline(v=q[5], col="blue")
         abline(v=1, col="red", lty='dashed')
         captio=("xxxx")
         
         
         
         par(mfrow=c(1,1))
         
         
         
         
         
         
         
         
        
        
    })
    
   
 
    
    
    output$trt.plot <- renderPlot({         
        
        sample <- random.sample()
       
        x<- seq(0,1, length.out=10000)
        
        trt.alpha<- sample$trt.alpha
        trt.beta<-  sample$trt.beta
        ctr.alpha<- sample$ctr.alpha
        ctr.beta<-  sample$ctr.beta
        
  
        tmp1 <- max(c(dbeta(x, trt.alpha, trt.beta)  ) )
        tmp2 <- max(c(dbeta(x, ctr.alpha, ctr.beta)))
        tmp <- max(tmp1, tmp2)
         
        par(bg = 'lightgoldenrodyellow')
       
        curve(dbeta(x, trt.alpha, trt.beta),col = "blue", xlab = c("Probabiity"), 
              main=paste0("The Beta distribution for treatment in blue with shape parameters (",p2(trt.alpha),",",p2(trt.beta),") and control in black (",p2(ctr.alpha),",",p2(ctr.beta),")             "  
              ),
              ylab = "Density", xlim=c(0.0,1),  ylim=c(0, (tmp)*1.1) #ylim=c(0, max(
        )
        curve(dbeta(x, ctr.alpha, ctr.beta),col = "black", xlab = c("Probabiity"), 
                
              ylab = "Density",  add=TRUE
        )
        
    })
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    output$trt.plot1 <- renderPlot({         
        
        x<- seq(0,1, length.out=10000)
        
        sample <- random.sample()
        
        a  <- sample$trt.alpha
        b  <- sample$trt.beta
        a1 <- sample$ctr.alpha
        b1 <- sample$ctr.beta
        
        n1 <- sample$n1   
        y1 <- sample$y1
        n2 <- sample$n2
        y2 <- sample$y2
        
        tmp1 <- max(c(dbeta(x,y1+a,  n1-y1+b)  ) )
        tmp2 <- max(c(dbeta(x,y2+a1, n2-y2+b1)))
        tmp <- max(tmp1, tmp2)
        
        par(bg = 'lightgoldenrodyellow') 
        
        curve(dbeta(x, y1+a, n1-y1+b),col = "blue", xlab = c("Probabiity"), 
              main=paste0("The Beta distribution for treatment in blue with shape parameters (",p2(y1+a),",",p2(n1-y1+b),") and control in black (",p2(y2+a1),",",p2(n2-y2+b1),")"  
              ),
              ylab = "Density", xlim=c(0.0,1),  ylim=c(0, (tmp)*1.1) #ylim=c(0, max(tmp1)),
              
              
        )
        
        curve(dbeta(x, y2+a1,  n2-y2+b1),col = "black", xlab = c("Probabiity"), 
           
              ylab = "Density" , add=TRUE
              
              
              
              
        )
        
        
        
    })
    
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # 
    # stan <- reactive({
    #     
    #     sample <- random.sample()
    #     a  <- trt.alpha<- sample$trt.alpha
    #     b  <- trt.beta<-sample$trt.beta
    #     a1 <- sample$ctr.alpha
    #     b1 <- sample$ctr.beta
    #     n <- n1 <- s <- s1 <- NULL
    #     n1 <- sample$n1  #trt
    #     y1 <- sample$y1
    #     n2 <- sample$n2
    #     y2 <- sample$y2
    #     
    #     
    #     alpha1 <- a
    #     beta1 <- b
    #     alpha2 <- a1
    #     beta2 <- b1
    #     
    #     # The Stan model as a string.
    #     
    #     model_string <- paste0(" 
    #     data {
    #     
    #       int n;   // Number of trials
    #       int s;   // Number of successes
    #       int n1;  // Number of trials
    #       int s1;  // Number of successes
    #     
    #       }
    #     
    #     // Here we define what 'unknowns' aka parameters we have.
    #     
    #     parameters {
    #     
    #       real<lower=0, upper=1> rate;
    #       real<lower=0, upper=1> rate1;
    #     
    #     }
    #     
    #     // The generative model
    #     
    #     model {
    #     
    #       rate ~ beta(",alpha1,",", beta1,") ;    // prior on treatment
    #       s ~ binomial(n, rate);  
    #     
    #       rate1 ~ beta(",alpha2,",", beta2,");    // prior on placebo
    #       s1 ~ binomial(n1, rate1);
    #     
    #     }
    #     
    #     // Variables have to be defined before they are assigned to
    #     
    #     generated quantities {
    #     
    #        real d;
    #        real r;
    #        real oddsratio;
    #        real prob;
    #     
    #        prob = rate>rate1;
    #        r = rate/rate1;
    #        d = rate-rate1;
    #        oddsratio = (rate/(1-rate)) / (rate1/(1-rate1));
    #     
    #     }", collapse = "<br>")
    #     
    #     
    #     
    #     
    #     mod <- stan_model(model_code = model_string, verbose = FALSE)
    #     fit <- sampling(mod, data = list(n = n1, s = y1, n1=n2, s1=y2), refresh=0, verbose = FALSE)   
    #     names(fit) <- c("proportion resp Trt", "proportion resp Ctrl","Trt-Ctrl","Trt/Ctrl","Odds Ratio Trt:Ctrl" , "p(Trt>Ctrl)", "lp")
    #     
    #     f <- as.matrix(fit)
    #     
    #     f <- f[,c(3,4,5,6,1,2)]
    #     
    #     rnamez <- c("trt-ctrl","trt/ctrl", "odds ratio trt:ctrl", "p(trt>ctrl)",
    #                 "p(y=1|trt)","p(y=1|ctrl)")
    #     cnamez <- c("Mean","2.5%","25%","50%","75%","97.5%")
    #     
    #     clean <- function(x) { c(mean(x), quantile(x,c(.025, 0.25, 0.5, 0.75, 0.975))) }
    #     
    #     x <- apply(f, 2, clean)
    #     
    #     z <- t(x)
    #     
    #     rownames(z) <- rnamez
    #     
    #     colnames(z) <- cnamez
    #     
    #     return(list(f=z, model_string=model_string)) 
    #     
    # })
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # output$reg.summary <- renderPrint({
    #     
    #     return(print(stan()$f, digits=4))
    #     
    # })
    output$reg.summary2 <- renderPrint({
        
        return(print(mcmc()$f1, digits=4))
        
    })
    # output$reg.summary3 <- renderPrint({
    #     
    #     return(stan2()$fitx2)
    #     
    # })
    # output$reg.summary4 <- renderPrint({
    #     
    #     return(print(stan2()$res, digits=4))
    #     
    # })
    
    # output$dplot1 <- renderPlot({  
    #     
    #     return(stan2()$dplot1)
    #     
    # })
    
    # output$mod1 <- renderPrint({  
    #     
    #     return(stan()$model_string)
    #     
    # })
    # 
    
    # output$textWithNumber3 <- renderText({ 
    #   
    #   A <-  stan()$model_string
    #       
    #   HTML(paste0("Stan model 1 " 
    #               , tags$span(style="color:red", (A)) ,
    #               "  ",
    #               "<br><b><br><b> ",
    #               "<br><b><br><b> ",
    #               
    #              
    #               ""
    #   )) 
    #   
    # })
    # 
    
    
    # output$textx <- renderUI({
    #   
    #   A <-  stan()$model_string
    #   
    #   HTML(paste0("Stan model 1 " 
    #               , tags$span(style="color:green", (A)) ,
    #               "  ",
    #               "<br><b><br><b> ",
    #               "<br><b><br><b> ",
    #               
    #               
    #               ""
    #   )) 
    #   
    # })
    
    #--------------------------------------------------------------------
    # --------------------------------------------------------------------------
    # --------------------------------------------------------------------------
    # tab 1 plot trt and plot ctrl
    
#     stan2 <- reactive({      
#         
#         sample <- random.sample()
#         a  <- trt.alpha<- sample$trt.alpha
#         b  <- trt.beta<-sample$trt.beta
#         a1 <- sample$ctr.alpha
#         b1 <- sample$ctr.beta
#         n <- n1 <- s <- s1 <- NULL
#         n1 <- sample$n1
#         y1 <- sample$y1
#         n2 <- sample$n2
#         y2 <- sample$y2
#         prio <- sample$prio
#         
#         alpha1 <- a
#         beta1 <- b
#         alpha2 <- a1
#         beta2 <- b1
#         
#         
#         library(reshape2)
#         
#         Table <- matrix(c(y1,y2,n1-y1,n2-y2), 2, 2, byrow=TRUE)
#         rownames(Table) <- c('y', 'n')
#         colnames(Table) <- c('Drug', 'Placebo')
#         melt(Table)
#         
#         d = as.data.frame(as.table(as.matrix(Table)))
#         # from stack exchange
#         
#         countsToCases <- function(x, countcol = "Freq") {
#             # Get the row indices to pull from x
#             idx <- rep.int(seq_len(nrow(x)), x[[countcol]])
#             # Drop count column
#             x[[countcol]] <- NULL
#             # Get the rows from x
#             x[idx, ]
#         }
#         
#         
#         dd <- countsToCases(d)
#         rownames(dd)<-NULL
#         head(dd)
#         
#         dd$y <- ifelse(dd$Var1 %in% "y",1,0)
#         dd$x <- ifelse(dd$Var2 %in% "Drug",1,0)
#         dd$Var1 <- dd$Var2 <-NULL
#         
#         names(dd)[names(dd)=="Var2"] <- "trt"
#         head(dd)
#         with(dd, table(dd$x, dd$y))
#         y <- dd$y; x <- dd$x
#         
#         #https://raw.githubusercontent.com/danilofreire/r-scripts/master/stan-logistic-regression.R
#         # guidance on logistic regression priors
#         #https://github.com/stan-dev/stan/wiki/Prior-Choice-Recommendations
#         
#         # prio  <-   "student_t(3,0,2.5)"  #eval(parse(text=prior1))
#         m1 <- paste0('data {
#         int N;  // number of items
#         int y[N];  // binary outcome for item n
#         real x[N];  // predictive feature for item n
#           }
#         parameters {
#         real alpha;  // intercept
#         real beta;  // slope
#           }
#         model {
#         alpha ~ ',prio,';  // weakly informative normal(0,5);cauchy(0,2.5); 
#         for (n in 1:N)
#         y[n] ~ bernoulli(inv_logit(alpha + beta * x[n]));
#         }
#         
#         generated quantities {
#         
#            real d;
#            real r;
#            real oddsratio;
#            real prob;
#            real p1;
#            real p2;
#         
#            p2 = 1/(1+exp(-alpha));
#            p1 = 1/(1+exp(-(alpha+beta)));
#            prob = p1>p2;
#            r = p1/p2;
#            d = p1-p2;
#            oddsratio = (p1/(1-p1)) / (p2/(1-p2));
#         
#         
# }')
        
        # Create a list with the chosen variables
        
        # data.list <- list(N = nrow(dd), y = dd$y, x = dd$x )
        # 
        # # Estimate the model
        # 
        # mod <- stan_model(model_code = m1, verbose = FALSE)
        # fitx <- sampling(mod, data = data.list , refresh=0)
        # 
        # rnamez <- c("alpha","beta","trt-ctrl","trt/ctrl", "odds ratio trt:ctrl", "p(trt>ctrl)",
        #             "p(y=1|trt)","p(y=1|ctrl)","drop")
        # cnamez <- c("Mean","2.5%","25%","50%","75%","97.5%")
        # 
        # f <- as.matrix(fitx)
        # 
        # clean <- function(x) { c(mean(x), quantile(x,c(.025, 0.25, 0.5, 0.75, 0.975))) }
        # 
        # x <- apply(f, 2, clean)
        # 
        # z <- t(x)
        # 
        # rownames(z) <- rnamez
        # 
        # colnames(z) <- cnamez
        # 
        # z <- z[!rownames(z) %in%  "drop",]
        # 
        
        
        # stan_diag(fitx, info = 'sample') # shows three plots together
        # 
        # 
        # stan_par(fitx, par = "alpha")
    #     
    #     require(bayesplot)
    #     
    #     dplot1 <- mcmc_pairs(fitx,   pars = c("alpha", "beta","d","r","oddsratio","prob"),
    #                          off_diag_args = list(size = 0.75))
    #     
    #     
    #     # dplot2 <- color_scheme_set("mix-brightblue-gray") 
    #     #  mcmc_trace(fitx, pars = c("alpha","beta") ) +
    #     #    xlab("Post-warmup iteration")
    #     
    #     return(list(res= z , fitx2=fitx , dplot1=dplot1 )) 
    #     
    # })
    # 
    # --------------------------------------------------------------------------
    # --------------------------------------------------------------------------
    # --------------------------------------------------------------------------
    # tab 2
    # output$dplot2  <- renderPlot({       
    #     
    #     f <- stan2()$fitx2
    #     
    #     dplot2 <- color_scheme_set("mix-brightblue-gray") 
    #     mcmc_trace(f, pars = c("alpha","beta") ) +
    #         xlab("Post-warmup iteration")
    #     
    # })
    # # --------------------------------------------------------------------------
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 
    # listing of simulated data
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 
    output$tablex <- DT::renderDataTable({
        
        mc <- mcmc()$f1
        # s1 <- stan()$f
        # s2 <- stan2()$res
        
        A <- (mc)
        # B <- (s1)
        # C <- (s2)
        
        A <- data.frame(A)
        # B <- data.frame(B)
        # C <- data.frame(C)
        
        A$model <- "Bayesian Monte Carlo"
        # B$model <- "Bayes proportions"
        # C$model <- "Bayes logistic reg."
        # 
        A$parameter = rownames(A)
        # B$parameter = rownames(B)
        # C$parameter = rownames(C)
        
        x <-A# rbind(A,B,C)
        #x$parameter = rownames(x)
        
        names(x) <- c("Mean","p2.5","p25","p50","p75","p975","Model","parameter")
        x <- x[,c("Model","parameter","Mean","p2.5","p25","p50","p75","p975")]
        
        
        rownames(x) <- NULL
        
        foo <- x
        datatable(x,
                  
                  rownames = FALSE,
                  
                  options = list(
                      searching = TRUE,
                      #pageLength = 20,
                      paging=FALSE,
                      lengthMenu = FALSE ,
                      lengthChange = FALSE,
                      autoWidth = FALSE
                      # colReorder = TRUE,
                      # deferRender = TRUE,
                      # scrollY = 200,
                      # scroller = T
                  ))  %>%
            formatRound(
                columns= c("Model","parameter","Mean","p2.5","p25","p50","p75","p975"), digits=c(0,0,3,3,3,3,3,3)  )
        
    })
    
    # --------------------------------------------------------------------------
    # ---------------------------------------------------------------------------
})

# Run the application 
shinyApp(ui = ui, server = server)