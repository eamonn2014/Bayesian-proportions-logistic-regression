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
fig.width3 <- 800  
fig.height3 <- 545
p1 <- function(x) {formatC(x, format="f", digits=1)}
p2 <- function(x) {formatC(x, format="f", digits=2)}
options(width=140)
set.seed(12345) # reproducible

pop=1e6 # this is the population size we take sample from
is.even <- function(x){ x %% 2 == 0 } # function to id. odd maybe useful
# Always remember that the purpose of a parallel-group randomized trial is to compare the parallel groups, 
# NOT to look at change from baseline.  Baseline should always be an adjustment covariate (only).


findBeta <- function(quantile1,quantile2,quantile3)
{
  # find the quantiles specified by quantile1 and quantile2 and quantile3
  quantile1_p <- quantile1[[1]]; quantile1_q <- quantile1[[2]]
  quantile2_p <- quantile2[[1]]; quantile2_q <- quantile2[[2]]
  quantile3_p <- quantile3[[1]]; quantile3_q <- quantile3[[2]]
  
  # find the beta prior using quantile1 and quantile2
  priorA <- beta.select(quantile1,quantile2)
  priorA_a <- priorA[1]; priorA_b <- priorA[2]
  
  # find the beta prior using quantile1 and quantile3
  priorB <- beta.select(quantile1,quantile3)
  priorB_a <- priorB[1]; priorB_b <- priorB[2]
  
  # find the best possible beta prior
  diff_a <- abs(priorA_a - priorB_a); diff_b <- abs(priorB_b - priorB_b)
  step_a <- diff_a / 100; step_b <- diff_b / 100
  if (priorA_a < priorB_a) { start_a <- priorA_a; end_a <- priorB_a }
  else                     { start_a <- priorB_a; end_a <- priorA_a }
  if (priorA_b < priorB_b) { start_b <- priorA_b; end_b <- priorB_b }
  else                     { start_b <- priorB_b; end_b <- priorA_b }
  steps_a <- seq(from=start_a, to=end_a, length.out=1000)
  steps_b <- seq(from=start_b, to=end_b, length.out=1000)
  max_error <- 10000000000000000000
  best_a <- 0; best_b <- 0
  for (a in steps_a)
  {
    for (b in steps_b)
    {
      # priorC is beta(a,b)
      # find the quantile1_q, quantile2_q, quantile3_q quantiles of priorC:
      priorC_q1 <- qbeta(c(quantile1_p), a, b)
      priorC_q2 <- qbeta(c(quantile2_p), a, b)
      priorC_q3 <- qbeta(c(quantile3_p), a, b)
      priorC_error <- abs(priorC_q1-quantile1_q) +
        abs(priorC_q2-quantile2_q) +
        abs(priorC_q3-quantile3_q)
      if (priorC_error < max_error)
      {
        max_error <- priorC_error; best_a <- a; best_b <- b
      }
    }
  }
  return(list(a= best_a , b=best_b ))
  #print(paste("The best beta prior has a=",best_a,"b=",best_b))
}

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
                                  
                                  textInput('vec1', 
                                            div(h5("Select 3 percentiles:")), "0.00001, 0.5, 0.99999"),
                                  
                                  textInput('vec2', 
                                            div(h5("Now the values. for example the default is, that the most likely value is around 0.85 but the proportion 
                 is highly unlikely to be smaller than 0.60 or bigger than 0.95")), "0.60, 0.85, 0.95"),
                                  
                                  
                                  textInput('vec3', 
                                            div(h5("Beta parameters for treatment prior")), "1, 1"),
                                  
                                  textInput('vec4', 
                                            div(h5("Beta parameters for control prior")), "10, 43"),
                                  
                                  textInput('n1y1', 
                                            div(h5("grp1 sample size and successess")), "25, 14"),
                                  
                                  textInput('n2y2', 
                                            div(h5("grp2 sample size and successess")), "25, 4"),
 
                                  textInput('prio', 
                                            div(h5("type in prior for intercept...examples\nnormal(0,5)\ncauchy(0,2.5)\n")), "student_t(3,0,2.5)"),
                                  
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
                                       h4("An appropriate prior to use for a proportion is a Beta prior. 
                                     For example, if you want to estimate the proportion of people who like chocolate,
 you might have a rough idea that the most likely value is around 0.85, 
 but that the proportion is unlikely to be smaller than 0.60 or bigger than 0.95. You can find the best Beta prior to use in this case by specifying that the median 
                                        (50% percentile) of the prior is 0.85, that the 99.999% percentile is 0.95, 
                                        and that the 0.001% percentile is 0.60 [1]."), 
                                       
                                       fluidRow(
                                         #  column(width = 5,
                                         #         div( verbatimTextOutput("reg.summary2")),
                                         #         h4("95% CIs"),
                                         #        div( verbatimTextOutput("reg.summary3"))
                                         # ), 
                                         column(width = 5,
                                                div(plotOutput("ancova.plot", width=fig.width3, height=fig.height3))
                                         )),
                                       h4("Figure 1 The best Beta distibution for the selected beliefs."),
                              ) ,
                              #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~end of section to add colour     
                              tabPanel("2 xxxxxxxxxxxxxxxxxx", 
                                       div(plotOutput("trt.plot", width=fig.width, height=fig.height)),  
                                       h4("Figure 2 xxxxxxxxxxxxxx."),
                                       
                                       h3(" "),
                                       
                                       p(strong("xxxxxxxxxxxx.")),
                                       
                                       p(strong("xxxxxxxxxxxxxxx.")),
                                       
                                       p(strong("xxxxxxxxxxxxxxxx.")),
                                       p(strong("xxxxxxxxxxxxxxxx.")),
                                       
                              ) ,
                              #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                              tabPanel("3 Bayesian Stan Model",
                                       #div( verbatimTextOutput("reg.summary")),
                                       h4("Figure 3 xxxxxxxxxxxxxxxxxxxxx."),         
                                       
                                       
                                       p(strong("Bayesian Monte Carlo Estimates")),
                              ),
                              #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                              tabPanel("4 Monte Carlo", value=3, 
                                      # div( verbatimTextOutput("reg.summary2")),
                                       #    div(plotOutput("res.plot3", width=fig.width2, height=fig.height2)), 
                                       h4("Figure 4 Bayesian Monte Carlo Estimates"),         
                              ) ,
                              #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                              tabPanel("5 xxxxxxxxxxxxxxxxx", value=7, 
                                       h4("STAN Bayesian modelling proportions"),
                                       
                                       fluidRow(
                                         column(width = 8,
                                                div( verbatimTextOutput("reg.summary")),
                                                h4("Monte Carlo approach, base R"),
                                                div( verbatimTextOutput("reg.summary2")),
                                                #   div( verbatimTextOutput("reg.lmm1")),
                                                # div( verbatimTextOutput("reg.lmm2")),
                                      # ),
                                         # 
                                       #fluidRow(
                                        #column(width = 8,
                                                  h4("STAN logistic regression modelling"),
                                               #  div( verbatimTextOutput("reg.summary3")),
                                                  # div( verbatimTextOutput("reg.lmm1")),
                                              #   h4("STAN logistic regression probabilities"),
                                                 div( verbatimTextOutput("reg.summary4"))
                                                  #           div( verbatimTextOutput("reg.lmm2")),
                                          ))
                                      # ),
                              ) ,
                              #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                              tabPanel("6 xxxxxxxxxxxxxxxxxxxxxxx", value=6, 
                                       # div(plotOutput("reg.plot4", width=fig.width, height=fig.height)), 
                                       h4("xxxxxxxxxxxxxxxxxxxxxxx."),         
                                       div(plotOutput("dplot1", width=fig.width, height=fig.height)),  
                                       
                                       fluidRow(
                                         column(12,
                                                sliderInput("senn",
                                                            strong("Clinical relevant difference"),
                                                            min=-10, max=10, step=.1, value=-2, ticks=FALSE))
                                       ),
                                       #h4("Figure 5 xxxxxxxxxxxxxxxxxx."),         
                                       
                                       p(strong(" ")),
                                       p(strong("xxxxxxxxxxxxxxxxx.")),
                                       
                                       p(strong("xxxxxxxxxxxxxxxxxxxxxx"
                                       )),
                                       
                                       
                                       h4("xxxxxxxxxxxxxxxxxxxxxxx"),
                                       #    div( verbatimTextOutput("senn.est")),
                                       h4("xxxxxxxxxxxxxxxxxxxx"),
                                       #    div( verbatimTextOutput("senn.est2"))
                                       
                              ) ,
                              #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                              tabPanel("7 Data", value=3, 
                                       div(plotOutput("dplot2", width=fig.width, height=fig.height)),  
                                       #  DT::dataTableOutput("table1"),
                                       
                              ) 
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
  random.sample <- reactive({
    
    # Dummy line to trigger off button-press
    # prob1 <-    input$prob1
    # prob2 <-    input$prob2
    # prob3 <-    input$prob3
    # prob4<-     input$prob4
    # prob5<-     input$prob5
    # prob6 <-    input$prob6
    i <- as.numeric(unlist(strsplit(input$vec1,",")))
    j <- as.numeric(unlist(strsplit(input$vec2,",")))
    
    trt <- as.numeric(unlist(strsplit(input$vec3,",")))
    ctr <- as.numeric(unlist(strsplit(input$vec4,",")))
    
    n1y1 <- as.numeric(unlist(strsplit(input$n1y1,","))) #trt
    n2y2 <- as.numeric(unlist(strsplit(input$n2y2,",")))
    
    prio <- as.character(input$prio)
    #
  
    
    return(list( prob1=i[1],prob2=j[1],prob3=i[2],prob4=j[2],prob5=i[3],prob6=j[3],
                 trt.alpha=trt[1], trt.beta=trt[2],
                 ctr.alpha=ctr[1], ctr.beta=ctr[2],
                 n1=n1y1[1],y1=n1y1[2],
                 n2=n2y2[1],y2=n2y2[2], prio=prio
                 )) 
    
  })
  
  # --------------------------------------------------------------------------
  
  make.data <- reactive({
    
    sample <- random.sample()
    #  
    prob1 <-    sample$prob1
    prob2 <-    sample$prob2
    prob3 <-    sample$prob3
    prob4<-     sample$prob4 
    prob5<-     sample$prob5
    prob6 <-    sample$prob6
    prio <- sample$prio
    
    quantile1 <- list(p=prob1, x=prob2)    # we believe the median of the prior is 0.85
    quantile2 <- list(p=prob3, x=prob4)    # we believe the 99.999th percentile of the prior is 0.95
    quantile3 <- list(p=prob5, x=prob6)    #
    
    B <- findBeta(quantile1,quantile2,quantile3)
    
    return(list(a=B$a, b= B$b)) 
    
  })
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  output$summaryx3 <- renderPrint({
    #  print(make.data()$N)
  }) 
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 
  
  output$ancova.plot <- renderPlot({         
    
    d  <- make.data()
    a <- d$a
    b <- d$b
    
    sample <- random.sample()
    
    prob1 <-    sample$prob1
    prob2 <-    sample$prob2
    prob3 <-    sample$prob3
    prob4<-     sample$prob4
    prob5<-     sample$prob5
    prob6 <-    sample$prob6
   # prio <- sample$prio
    
    #actual quantiles
    q <- qbeta(c(prob1, prob3, prob5), a,b)  
    
    par(bg = 'grey')
    curve(dbeta(x, a, b),col = "blue", xlab =paste0( "Prior based on belief that the ",prob1," quantile is ", prob2,
                                                     " the ",prob3," quantile is ", prob4,
                                                     " and the ",prob5," quantile is ", prob6), 
          main=paste0("We have found the best Beta distribution with parameters (",p2(a),",",p2(b),") , the ",prob1 ,", ",  prob3," and "
                      ,prob5," quantiles \nof this distribution are: ", p2(q[1]),", " ,p2(q[2])," and ",p2(q[3])  
          ),
          ylab = "Density", xlim=c(0.0,1), #ylim=c(0,5),
    )
  
  })
  
  ##
  output$trt.plot <- renderPlot({         
    
    
    
    sample <- random.sample()
    trt.alpha<- sample$trt.alpha
    trt.beta<-sample$trt.beta
    ctr.alpha<- sample$ctr.alpha
    ctr.beta<-sample$ctr.beta
    #prio <- sample$prio
    
    par(bg = 'grey')
    curve(dbeta(x, trt.alpha, trt.beta),col = "blue", xlab = c("Prior fot treatment, proportion of successess"), 
          main=paste0("The Beta distribution in black with parameters (",p2(trt.alpha),",",p2(trt.beta),") and in blue (",p2(ctr.alpha),",",p2(ctr.beta),")             "  
          ),
          ylab = "Density", xlim=c(0.0,1), #ylim=c(0,5),
          
          
    )
    
    curve(dbeta(x, ctr.alpha, ctr.beta),col = "black", xlab = c("Prior fot treatment, proportion of successess"), 
          #  main=paste0("The Beta distribution with parameters (",p2(ctr.alpha),",",p2(ctr.beta),")"  
          #  ),
          ylab = "Density", xlim=c(0.0,1), add=TRUE#ylim=c(0,5),
          
          
    )
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
      
    
      I = 100000                               # simulations
      
      theta1 = rbeta(I, y1+a, n1-y1+b)        # incorp. prior for trt
      theta2 = rbeta(I, y2+a1, n2-y2+b1)      # incorp. prior for placebo
      diff = theta1-theta2                    # simulated differences
      ratio = theta1/theta2 
      or <- (theta1/ (1-theta1)) / (theta2/(1-theta2))
      ptrt <- theta1>theta2
      
        
      f <- data.frame(cbind( diff, ratio, or ,ptrt, theta1, theta2))
      
      rnamez <- c( "trt-ctrl","trt/ctrl", "odds ratio trt:ctrl", "p(trt>ctrl)",
                  "p(y=1|trt)","p(y=1|ctrl)" )
      cnamez <- c("Mean","2.5%","25%","50%","75%","97.5%")
      
           clean <- function(x) { c(mean(x), quantile(x,c(.025, 0.25, 0.5, 0.75, 0.975))) }
      
      x <- apply(f, 2, clean)
      
      z <- t(x)
      
      rownames(z) <- rnamez
      colnames(z) <- cnamez
      
    
      
      
      
      
      #res
      # VISUALIZATION
      # plot(density(diff),
      #      xlab="theta1 - theta2",
      #      ylab="p(theta1 - theta2 | y, n)",
      #      main="Posterior Simulation of active - placebo",
      #      ylim=c(0,8),
      #      frame.plot=FALSE,cex.lab=1.5,lwd=3,yaxt="no")
      # abline(v=quantiles[1], col="blue") #95% credible interval
      # abline(v=quantiles[5], col="blue")
      # #dev.off()
      # 
    #  cat("Probability theta1 > theta2")
    #  print(mean(theta1>theta2),digits=2)  
      #print(res,digits=3)
      return(list(f1= z )) 
      
    })
    
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    
    
 
  
  
  stan <- reactive({
    
    sample <- random.sample()
    a  <- trt.alpha<- sample$trt.alpha
    b  <- trt.beta<-sample$trt.beta
    a1 <- sample$ctr.alpha
    b1 <- sample$ctr.beta
    n <- n1 <- s <- s1 <- NULL
    n1 <- sample$n1  #trt
    y1 <- sample$y1
    n2 <- sample$n2
    y2 <- sample$y2
    
    
    alpha1 <- a
    beta1 <- b
    alpha2 <- a1
    beta2 <- b1
    
   
    library(rstan)
    
    
    # The Stan model as a string.
    
    model_string <- paste0("// Here we define the data we are going to pass into the model

        data {
        
          int n;   // Number of trials
          int s;   // Number of successes
          int n1;  // Number of trials
          int s1;  // Number of successes
        
          }
        
         
        
        // Here we define what 'unknowns' aka parameters we have.
        
        parameters {
        
          real<lower=0, upper=1> rate;
          real<lower=0, upper=1> rate1;
        
        }
        
        // The generative model
        
        model {
        
          rate ~ beta(",alpha1,",", beta1,") ;    //prior this on trt
          s ~ binomial(n, rate);  #14/25
        
          rate1 ~ beta(",alpha2,",", beta2,");  //prior this on placebo
          s1 ~ binomial(n1, rate1);
        
        }
        
        // Variables have to be defined before they are assigned to
        
        generated quantities {
        
           real d;
           real r;
           real oddsratio;
           real prob;
        
           prob = rate>rate1;
           r = rate/rate1;
           d = rate-rate1;
           oddsratio = (rate/(1-rate)) / (rate1/(1-rate1));
        
        }")

    
   
    
    mod <- stan_model(model_code = model_string, verbose = FALSE)
    fit <- sampling(mod, data = list(n = n1, s = y1, n1=n2, s1=y2), refresh=0, verbose = FALSE)   
    names(fit) <- c("proportion resp Trt", "proportion resp Ctrl","Trt-Ctrl","Trt/Ctrl","Odds Ratio Trt:Ctrl" , "p(Trt>Ctrl)", "lp")
   # fit1 <- print(fit, digits=3)
    
    
    f <- as.matrix(fit)
    
    f <- f[,c(3,4,5,6,1,2)]
    
    rnamez <- c("trt-ctrl","trt/ctrl", "odds ratio trt:ctrl", "p(trt>ctrl)",
                "p(y=1|trt)","p(y=1|ctrl)")
    cnamez <- c("Mean","2.5%","25%","50%","75%","97.5%")
    
    clean <- function(x) { c(mean(x), quantile(x,c(.025, 0.25, 0.5, 0.75, 0.975))) }
    
    x <- apply(f, 2, clean)
    
    z <- t(x)
    
    rownames(z) <- rnamez
    
    colnames(z) <- cnamez
    
    return(list(f=z)) 
 
  })
  
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  output$reg.summary <- renderPrint({
    
    return(print(stan()$f, digits=4))
    
  })
  output$reg.summary2 <- renderPrint({
    
       return(print(mcmc()$f1, digits=4))
    
  })
  output$reg.summary3 <- renderPrint({
    
        return(stan2()$fitx2)
    
  })
  output$reg.summary4 <- renderPrint({
    
    return(print(stan2()$res, digits=4))
    
  })
  
  
  
  
  output$dplot1 <- renderPlot({  
    
    return(stan2()$dplot1)
    
  })
  
  output$dplot2 <- renderPlot({  
    
    return(stan2()$dplot2)
    
  })
  # --------------------------------------------------------------------------
  # --------------------------------------------------------------------------
  # --------------------------------------------------------------------------
  # tab 1 plot trt and plot ctrl
  
  stan2 <- reactive({      
    
   
    sample <- random.sample()
    a  <- trt.alpha<- sample$trt.alpha
    b  <- trt.beta<-sample$trt.beta
    a1 <- sample$ctr.alpha
    b1 <- sample$ctr.beta
    n <- n1 <- s <- s1 <- NULL
    n1 <- sample$n1
    y1 <- sample$y1
    n2 <- sample$n2
    y2 <- sample$y2
    prio <- sample$prio
    
    
    alpha1 <- a
    beta1 <- b
    alpha2 <- a1
    beta2 <- b1
    
    
    library(reshape2)
    
    Table <- matrix(c(y1,y2,n1-y1,n2-y2), 2, 2, byrow=TRUE)
    rownames(Table) <- c('y', 'n')
    colnames(Table) <- c('Drug', 'Placebo')
    melt(Table)
    
    d = as.data.frame(as.table(as.matrix(Table)))
      # from stack exchange
    
    countsToCases <- function(x, countcol = "Freq") {
      # Get the row indices to pull from x
      idx <- rep.int(seq_len(nrow(x)), x[[countcol]])
      # Drop count column
      x[[countcol]] <- NULL
      # Get the rows from x
      x[idx, ]
    }
    

    dd <- countsToCases(d)
    rownames(dd)<-NULL
    head(dd)
    
    dd$y <- ifelse(dd$Var1 %in% "y",1,0)
    dd$x <- ifelse(dd$Var2 %in% "Drug",1,0)
    dd$Var1 <- dd$Var2 <-NULL
    
    names(dd)[names(dd)=="Var2"] <- "trt"
    head(dd)
    with(dd, table(dd$x, dd$y))
    y <- dd$y; x <- dd$x
      
    #https://raw.githubusercontent.com/danilofreire/r-scripts/master/stan-logistic-regression.R
    # guidance on logistic regression priors
    #https://github.com/stan-dev/stan/wiki/Prior-Choice-Recommendations
     
# prio  <-   "student_t(3,0,2.5)"  #eval(parse(text=prior1))
 m1 <- paste0('data {
        int N;  // number of items
        int y[N];  // binary outcome for item n
        real x[N];  // predictive feature for item n
          }
        parameters {
        real alpha;  // intercept
        real beta;  // slope
          }
        model {
        alpha ~ ',prio,';  // weakly informative normal(0,5);cauchy(0,2.5); 
        for (n in 1:N)
        y[n] ~ bernoulli(inv_logit(alpha + beta * x[n]));
        }
        
        generated quantities {
        
           real d;
           real r;
           real oddsratio;
           real prob;
           real p1;
           real p2;
        
           p2 = 1/(1+exp(-alpha));
           p1 = 1/(1+exp(-(alpha+beta)));
           prob = p1>p2;
           r = p1/p2;
           d = p1-p2;
           oddsratio = (p1/(1-p1)) / (p2/(1-p2));
        
        
}')

  

# Create a list with the chosen variables

  data.list <- list(N = nrow(dd), y = dd$y, x = dd$x )

# Estimate the model

    mod <- stan_model(model_code = m1, verbose = FALSE)
    fitx <- sampling(mod, data = data.list , refresh=0)
    
   # sampling(mod, data = data.list, iter = 1000, chains = 4, refresh=0)
    #names(fitx) <- c("intercept (log odds of control resp.)", "log odds ratio Trt:Ctrl" , "lp")
 #   fitx2 <- print(fitx, digits=3)
 
    #https://cran.r-project.org/web/packages/rstan/vignettes/stanfit-objects.html
    
    ## extract alpha and beta with 'permuted = TRUE'
    # fit_ss <- extract(fitx, permuted = TRUE) # fit_ss is a list
    # 
    # ## list fit_ss should have elements with name 'alpha', 'beta', 'lp__'
    # alpha <- fit_ss$alpha
    # beta <-  fit_ss$beta
    # 
    # ## or extract alpha by just specifying pars = 'alpha'
    # #alpha2 <- extract(fit, pars = 'alpha', permuted = TRUE)$alpha
    # 
    # placebo.prob <- 1/(1+exp(-alpha))
    # quantiles1 = quantile(placebo.prob,c(0.025,0.25,0.5,0.75,0.975))
    # 
    # trt.prob <- 1/(1+exp(-(alpha+beta)))
    # # trt.prob <- exp(alpha+beta)/(1+exp(alpha+beta)) # same as above
    # quantiles2 = quantile(trt.prob,c(0.025,0.25,0.5,0.75,0.975))
    # 
    # d <- trt.prob - placebo.prob
    # quantiles3 = quantile(d,c(0.025,0.25,0.5,0.75,0.975))
    # 
    # ratio   <- trt.prob / placebo.prob
    # quantiles4 = quantile(ratio,c(0.025,0.25,0.5,0.75,0.975))
    # 
    # or   <- (trt.prob/(1-trt.prob)) /  (placebo.prob /(1-placebo.prob ))
    # quantiles5 = quantile(or,c(0.025,0.25,0.5,0.75,0.975))
    # 
    # res  <- rbind(quantiles1, quantiles2, quantiles3, quantiles4, quantiles5 )
    # rownames(res) <-c( "Ctrl probability of resp","Trt probability of resp",
    #                    "Trt-Ctrl probability of resp"  ,
    #                    "Trt/Ctrl rel risk of resp",
    #                    "Odds ratio Trt  v Ctrl")
    
    rnamez <- c("alpha","beta","trt-ctrl","trt/ctrl", "odds ratio trt:ctrl", "p(trt>ctrl)",
                "p(y=1|trt)","p(y=1|ctrl)","drop")
    cnamez <- c("Mean","2.5%","25%","50%","75%","97.5%")
    
    f <- as.matrix(fitx)
    
    clean <- function(x) { c(mean(x), quantile(x,c(.025, 0.25, 0.5, 0.75, 0.975))) }
    
    x <- apply(f, 2, clean)
    
    z <- t(x)
    
    rownames(z) <- rnamez
    
    colnames(z) <- cnamez
    
    z <- z[!rownames(z) %in%  "drop",]
    
     
    
    
    
    

    
    
    
    # stan_diag(fitx, info = 'sample') # shows three plots together
    # 
    # 
    # stan_par(fitx, par = "alpha")

    require(bayesplot)
    
    dplot1 <- mcmc_pairs(fitx,   pars = c("alpha", "beta","d","r","oddsratio","prob"),
               off_diag_args = list(size = 0.75))
    
 
     # dplot2 <- color_scheme_set("mix-brightblue-gray") 
     #  mcmc_trace(fitx, pars = c("alpha","beta") ) +
     #    xlab("Post-warmup iteration")
 
      return(list(res= z , fitx2=fitx2 , dplot1=dplot1 )) 
      
 })
  
  # --------------------------------------------------------------------------
  # --------------------------------------------------------------------------
  # --------------------------------------------------------------------------
  # tab 2
  output$dplotchain  <- renderPlot({       
  #   
    f <- stan2()$fitx2
  #   # beta.treatment <-  sample$trt 
  #   # trial <- make.data()$trial
  #   # stats <- stats()
  #   
  #   
  #   require(bayesplot)
  #   # 
  #   # dplot1 <- mcmc_pairs(f,   pars = c("alpha", "beta","d","r","oddsratio","prob"),
  #   #                      off_diag_args = list(size = 0.75))
  #   
  #   
    
    
    dplot2 <- color_scheme_set("mix-brightblue-gray") 
    mcmc_trace(f, pars = c("alpha","beta") ) +
      xlab("Post-warmup iteration")
  #   # par(mfrow=c(1,2))
  # 
  #   # par(mfrow=c(1,1))
  #   
 })
  # --------------------------------------------------------------------------
  # --------------------------------------------------------------------------
  # --------------------------------------------------------------------------   
  # 3rd tab
  output$res.plot3 <- renderPlot({       
    
    #   sample <- random.sample()
    #   
    #   trial <- make.data()$trial
    #   
    #   N <- make.data()$N
    #   
    #   stats <- stats()
    #   A=stats()$A
    #   AT=stats()$AT 
    #   C=stats()$C    
    #   CT=stats()$CT
    #   AN=stats()$AN
    #   CN=stats()$CN
    #   
    #   diff <- trial$y.1observed - trial$y.0observed
    #   mi <-  min( diff)*1.2
    #   ma <-  max(diff)*1.2
    #   beta.treatment <-  sample$trt 
    #   # ---------------------------------------------------------------------------
    #   par(mfrow=c(2,2))
    #   # par(bg = 'ivory')
    #   
    #   xup <-  max(table(trial$treat))  # new
    #   trt <- trial[trial$treat==1,]
    #   trt$diff <- trt$y.1observed - trt$y.0observed
    #   
    #   foo <- sort(trt[,"diff"])
    #   A <- mean(foo < input$trt)*length(foo)   # shown in red
    #   
    #   
    #   foo <- data.frame(foo, col1=NA, col2=NA)
    #   
    #   foo$col1 =   ifelse(foo$foo <=    trt$beta.treatment, "blue" , "black")         
    #   foo$col2 =   ifelse(foo$foo >    trt$beta.treatment, "blue" , "black")   
    #   
    #   if (trt$beta.treatment <  0) {foo$colz = foo$col1} else {foo$colz = foo$col2}
    #   # 
    #   # tex <- "Individual changes in response in treated arm
    #   #      Suggested individual differences due entirely to regression to the mean
    #   #      and random error (within subject and measurement error)"
    #   # tex <- paste0("Treated patients: N= ",AN,", No of responders= ",A," (",AT,"%)")
    #   
    #   if ( beta.treatment <  0) {
    #     foo$colz = foo$col1
    #     tex <- paste0("Treated patients, responders coloured blue \n N= ",AN,", No of responders= ",A," (",AT,"%), non responders=",AN-A," (",100-AT,"%)")
    #   } else {
    #     foo$colz = foo$col2
    #     tex <- paste0("Treated patients, responders coloured blue \n N= ",AN,", No of responders= ",AN-A," (",100-AT,"%), non responders=",A," (",AT,"%)")
    #   }
    # 
    #   plot(foo$foo, main=tex, 
    #        ylab= "follow up - baseline", xlab="Individual subjects order by observed response", 
    #        xlim=c(0, xup), ylim=c(mi,ma), #length(trt[,"diff"])
    #        col=  foo$colz)
    #   
    #   grid(nx = NULL, ny = NULL, col = "lightgray", lty = "dotted")
    #   with(trt, abline(v=A, col="black", lty="dashed"))
    #   with(trt, abline(h=0, col="black", lty=1))
    #   with(trt, abline(h=(beta.treatment), col=c("forestgreen"), lty=c(2), lwd=c(1) ) )
    #   # ---------------------------------------------------------------------------
    #   trt <- trial[trial$treat==0,]
    #   trt$diff <- trt$y.1observed - trt$y.0observed
    #   
    #   foo <- sort(trt[,"diff"])
    #   C <- mean(foo < input$trt)*length(foo)   # shown in red
    #   
    #   foo <- data.frame(foo, col1=NA, col2=NA)
    #   
    #   foo$col1 =   ifelse(foo$foo <=    trt$beta.treatment, "blue" , "black")          
    #   foo$col2 =   ifelse(foo$foo >    trt$beta.treatment, "blue" , "black")   
    #   
    #   #if (trt$beta.treatment <  0) {foo$colz = foo$col1} else {foo$colz = foo$col2}
    # 
    #   if ( beta.treatment <  0) {foo$colz = foo$col1
    #   tex <- paste0("Control patients, responders coloured blue\n N= ",CN,", No of responders= ",C," (",CT,"%), non responders=",CN-C," (",100-CT,"%)")
    #   } else {
    #     foo$colz = foo$col2
    #     tex <- paste0("Control patients, responders coloured blue\n N= ",CN,", No of responders= ",CN-C," (",CT,"%), non responders=",C," (",100-CT,"%)") 
    #   }
    #   
    #   plot(foo$foo, main=tex,
    #        ylab= "follow up - baseline", xlab="Individual subjects order by observed response", 
    #        xlim=c(0, xup), ylim=c(mi,ma), #length(trt[,"diff"])
    #        col=  foo$colz)
    #   
    #   grid(nx = NULL, ny = NULL, col = "lightgray", lty = "dotted")
    #   with(trt, abline(v=C, col="black", lty="dashed"))
    #   with(trt, abline(h=0, col="black", lty=1))
    #   with(trt, abline(h=(beta.treatment), col=c("forestgreen"), lty=c(2), lwd=c(1) ) )
    #   
    #   #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    #   trial <- make.data()$trial
    #   
    #   diff <- trial$y.1observed - trial$y.0observed
    #   mi <-  min( diff)*1.2
    #   ma <-  max(diff)*1.2
    #   
    #   x <- trial$y.0observed
    #   mix <-  min( x) 
    #   max <-  max(x) 
    #   
    #   trt <- trial[trial$treat==1,]
    #   trt$diff <- trt$y.1observed - trt$y.0observed
    #   
    #   cr <- with(trt, cor.test( diff,   y.0observed, method="pearson"))
    #   cr$estimate[1][[1]]
    #   cr$conf.int[1:2]
    #   cr <- paste0( p2(cr$estimate),", 95%CI (",p2(cr$conf.int[1]),", " ,p2(cr$conf.int[2]), " )")
    #   
    #   # Due to floating point arithmetic we will the values will slightly differ and will get a val so setting this to NA
    #   if (input$noise==0) {  cr <- "NA, 95%CI (NA, NA)"  }
    #   
    #   trt$col1 =   ifelse(trt$diff <  (sample$trt), "blue" , "black")         
    #   trt$col2 =   ifelse(trt$diff >  (sample$trt), "blue" , "black")           
    #   
    #   if ( beta.treatment <  0) {
    #     foo$colz = foo$col1
    #     tex <- paste0("Treatment arm: Individual changes against baseline, \nPearson's correlation ",cr,"\n N= ",AN,", No of responders= ",A," (",AT,"%), non responders=",AN-A," (",100-AT,"%)")
    #   } else {
    #     foo$colz = foo$col2
    #     tex <- paste0("Treatment arm: Individual changes against baseline, \nPearson's correlation ",cr,"\n N= ",AN,", No of responders= ",AN-A," (",100-AT,"%), non responders=",A," (",AT,"%)")
    #   }
    # 
    #   with(trt, plot(diff ~  y.0observed,
    #                  
    #                  col=  ifelse(beta.treatment <=  0, trt$col1 , 
    #                               ifelse(beta.treatment >  0, trt$col2 ,    NA )) ,
    #                  
    #                  
    #                  pch=16
    #                  , xlab="observed baseline",  ylab="follow up - baseline"  ,
    #                  
    #                  main=tex, #paste0("Treatment arm: observed responders in blue\nPearson's correlation ",cr),
    #                  
    #                  cex.main =1.25,
    #                  ylim=c(mi,ma), xlim=c(mix,max) ))
    #   
    #   grid(nx = NULL, ny = NULL, col = "lightgray", lty = "dotted")
    #   with(trt, abline(h=0, col="black", lty=1))
    #   with(trt, abline(lm(diff ~  y.0observed), col=c("red"), lty=c(1), lwd=c(1) ) )
    #   with(trt, abline(h=(beta.treatment), col=c("forestgreen"), lty=c(2), lwd=c(1) ) )
    #   #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    #   
    #   ctr <- trial[trial$treat==0,]
    #   ctr$diff <- ctr$y.1observed - ctr$y.0observed
    #   cr <- with(ctr, cor.test( diff,   y.0observed, method="pearson"))
    #   cr$estimate[1][[1]]
    #   cr$conf.int[1:2]
    #   cr <- paste0( p2(cr$estimate),", 95%CI (",p2(cr$conf.int[1]),", " ,p2(cr$conf.int[2]), " )")
    #   
    #   # Due to floating point arithmetic we will the values will slightly differ and will get a val so setting this to NA
    #   if (input$noise==0) {  cr <- "NA, 95%CI (NA, NA)"  }
    #   
    #   ctr$col1 =   ifelse(ctr$diff <  (sample$trt), "blue" , "black")         
    #   ctr$col2 =   ifelse(ctr$diff >  (sample$trt), "blue" , "black")   
    #   
    #   if ( beta.treatment <  0) {
    #     ctr$colz = foo$col1
    #     tex <- paste0("Control arm: Individual changes against baseline, \nPearson's correlation ",cr,"\n N= ",CN,", No of responders= ",C," (",CT,"%), non responders=",CN-C," (",100-CT,"%)")
    #   } else {
    #     ctr$colz = foo$col2
    #     tex <- paste0("Control arm: Individual changes against baseline, \nPearson's correlation ",cr,"\n N= ",CN,", No of responders= ",CN-C," (",CT,"%), non responders=",C," (",100-CT,"%)")
    #   }
    #   
    #   with(ctr, plot(diff ~  y.0observed, 
    #                  
    #                  col=  ifelse(beta.treatment <=  0, ctr$col1 , 
    #                               ifelse(beta.treatment >  0, ctr$col2 ,    NA )) ,
    #                  
    #                  pch=16
    #                  , xlab="observed baseline",  ylab="follow up - baseline"  ,
    #                  
    #                  main=tex, #paste0("Treatment arm: observed responders in blue\nPearson's correlation ",cr),
    #                  
    #                  cex.main =1.25,
    #                  ylim=c(mi,ma), xlim=c(mix,max) ))
    #   
    #   grid(nx = NULL, ny = NULL, col = "lightgray", lty = "dotted")
    #   with(ctr, abline(h=0, col="black", lty=1))
    #   with(ctr, abline(lm(diff ~  y.0observed), col=c("red"), lty=c(1), lwd=c(1) ) )
    #   with(ctr, abline(h=(beta.treatment), col=c("forestgreen"), lty=c(2), lwd=c(1) ) )
    #   #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    #   par(mfrow=c(1,1))
    #   
    # })
    # # --------------------------------------------------------------------------
    # # --------------------------------------------------------------------------
    # # --------------------------------------------------------------------------
    # # fith tab
    # output$reg.plot4 <- renderPlot({         
    #   
    #   trial <- make.data()$trial
    #    sample <- random.sample()
    #   N <- make.data()$N
    # 
    #   diff <- trial$y.1observed - trial$y.0observed
    #   mi <-  min( diff)*1.2
    #   ma <-  max(diff)*1.2
    #   
    #   stats <- stats()
    #   A=stats()$A
    #   AT=stats()$AT 
    #   C=stats()$C    
    #   CT=stats()$CT
    #   AN=stats()$AN
    #   CN=stats()$CN
    #   T.SENN =stats()$T.SENN
    #   C.SENN =stats()$C.SENN
    #   TC.SENN =stats()$TC.SENN
    #   CT.SENN =stats()$CT.SENN
    # 
    #   xup <-  max(table(trial$treat))  # new
    # 
    #   trt <- trial[trial$treat==1,]
    #   trt$diff <- trt$y.1observed - trt$y.0observed
    # 
    #   foo <- sort(trt[,"diff"])
    # 
    #   foo <- data.frame(foo, col1=NA, col2=NA)
    # 
    #   foo$col1 =   ifelse(foo$foo <=    input$senn, "blue" , "black")
    #   foo$col2 =   ifelse(foo$foo >     input$senn, "blue" , "black")
    # 
    #   if ( input$senn <  0) {
    #     foo$colz = foo$col1
    #   tex <- paste0("Treated patients \n N= ",AN,", No of responders= ",T.SENN," (",TC.SENN,"%), non responders=",AN-T.SENN," (",100-TC.SENN,"%)")
    #   } else {
    #     foo$colz = foo$col2
    #   tex <- paste0("Treated patients \n N= ",AN,", No of responders= ",AN-T.SENN," (",100-TC.SENN,"%), non responders=",T.SENN," (",TC.SENN,"%)")
    #   }
    #   par(mfrow=c(1,2))
    #   plot(foo$foo, main=tex,
    #        ylab= "follow up - baseline", xlab="Individual subjects ordered by observed response",
    #        xlim=c(0, xup), ylim=c(mi,ma), #length(trt[,"diff"])
    #        col=  foo$colz)
    #   grid(nx = NULL, ny = NULL, col = "lightgray", lty = "dotted")
    # 
    #   abline(h=0)
    #   abline(h=input$trt, lty=2)
    #   abline(h=input$senn, lty=2, col="blue")
    #   title(main = "", sub = "Patients observed to respond coloured blue, otherwise black; blue dashed line denotes clincal relevant difference",  
    #         adj=0,cex.sub = 0.75, font.sub = 1, col.sub = "black"
    #         
    #   )
    #   # ---------------------------------------------------------------------------
    # 
    #   trt <- trial[trial$treat==0,]
    #   trt$diff <- trt$y.1observed - trt$y.0observed
    #   foo <- sort(trt[,"diff"])
    # 
    #   foo <- data.frame(foo, col1=NA, col2=NA)
    # 
    #   foo$col1 =   ifelse(foo$foo <=     input$senn, "blue" , "black")
    #   foo$col2 =   ifelse(foo$foo >     input$senn, "blue" , "black")
    # 
    #   if ( input$senn <  0) {foo$colz = foo$col1
    #   tex <- paste0("Control patients \n N= ",CN,", No of responders= ",C.SENN," (",CT.SENN,"%), non responders=",CN-C.SENN," (",100-CT.SENN,"%)")
    #   } else {
    #     foo$colz = foo$col2
    #   tex <- paste0("Control patients \n N= ",CN,", No of responders= ",CN-C.SENN," (",100-CT.SENN,"%), non responders=",C.SENN," (",CT.SENN,"%)")
    #   }
    # 
    #   plot(foo$foo, main=tex,
    #        ylab= "follow up - baseline", xlab="Individual subjects ordered by observed response",
    #        xlim=c(0, xup), ylim=c(mi,ma), #length(trt[,"diff"])
    #        col=  foo$colz)
    #   grid(nx = NULL, ny = NULL, col = "lightgray", lty = "dotted")
    # 
    #   abline(h=0)
    #   abline(h=input$trt, lty=2)
    #   abline(h=input$senn, lty=2, col="blue")
    #   title(main = "", sub = "Patients observed to respond coloured blue, otherwise black; black dashed line the true trt effect, which should only manifest in the treated only",  
    #         adj=0,cex.sub = 0.75, font.sub = 1, col.sub = "black"
    #         
    #   )
    #   
    #   par(mfrow=c(1,1))
    # ---------------------------------------------------------------------------
  })
  # --------------------------------------------------------------------------
  # --------------------------------------------------------------------------
  # --------------------------------------------------------------------------
  senn2 <- reactive({
    
    # sample <- random.sample()
    # 
    # noise <-  sample$noise        
    # beta.treatment <-  sample$trt   
    # senn <- (input$senn)
    # 
    # 
    # if (beta.treatment < 0 & senn > 0 ) {
    #   
    #     res <-  pnorm( (beta.treatment-senn)/ sqrt(noise^2+noise^2)    )
    #     res2 <- pnorm( (0-senn)/ sqrt(noise^2+noise^2)    )
    # 
    # }  else if (beta.treatment < 0 & senn <  0 ) {
    #   
    #     res <-  1- pnorm( (beta.treatment-senn)/ sqrt(noise^2+noise^2)    )
    #     res2 <- 1- pnorm( (0-senn)/ sqrt(noise^2+noise^2)    )
    #   
    # }  else  if (beta.treatment > 0 & senn < 0 ) {
    #   
    #     res <-  1- pnorm( (beta.treatment-senn)/ sqrt(noise^2+noise^2)    )
    #     res2 <- 1- pnorm( (0-senn)/ sqrt(noise^2+noise^2)    )
    #   
    #    # beta.treatment > 0 & senn > 0 
    #   } else  {
    #     
    #     res <-   pnorm( (beta.treatment-senn)/ sqrt(noise^2+noise^2)    )
    #     res2 <-  pnorm( (0-senn)/ sqrt(noise^2+noise^2)    )
    # }  
    #   
    #   
    # 
    # return(list(res=res , res2=res2))
    
  })     
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  output$senn.est <- renderPrint({
    
    # return(senn2()$res)
    
  })
  
  output$senn.est2 <- renderPrint({
    
    # return(senn2()$res2)
    
  })
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # ---------------------------------------------------------------------------
  # get some counts and percentage for observed resp and non resp
  stats <- reactive({
    
    #    sample <- random.sample()
    #    
    #    trial <- make.data()$trial
    #    
    #    
    #    if (sample$trt < 0) {    
    #    
    #        N <- nrow(trial)
    #    # ---------------------------------------------------------------------------treated
    #       # trt rel diff -ve
    #        trt <- trial[trial$treat==1,]
    #        trt$diff <- trt$delta.observed      # trt effect          
    #        foo <- sort(trt[,"diff"])                         # sorted treatment effect
    #        A <- mean(foo <= sample$trt)*length(foo)          # proportion at follow up less than or equal to trt effect
    #        AT <- round(A/length(foo)*100,1)                  # %
    #        AN <- length(foo)                                 # count
    #        
    #        T.SENN <-   mean(foo < input$senn)*length(foo)   # proportion at follow up less than clin rel diff
    #        TC.SENN <- round(T.SENN/length(foo)*100,1)        # %
    #        # ---------------------------------------------------------------------------ctrl
    #        trt <- trial[trial$treat==0,]                     # same for ctrl
    #        trt$diff <- trt$delta.observed 
    #        foo <- sort(trt[,"diff"])
    #        C <- mean(foo <= sample$trt)*length(foo)   # 
    #        CT <- round(C/length(foo)*100,1)
    #        CN = length(foo)
    #        
    #        C.SENN <- mean(foo < input$senn)*length(foo)
    #        CT.SENN <- round(C.SENN/length(foo)*100,1)
    #    
    #        
    #    } else { 
    #   
    #         N <- nrow(trial)
    #         trt <- trial[trial$treat==1,]
    #         trt$diff <- trt$delta.observed 
    #         foo <- sort(trt[,"diff"])
    #         A <- mean(foo > sample$trt)*length(foo)   # 
    #         AT <- round(A/length(foo)*100,1)
    #         AN <- length(foo)
    #         
    #         T.SENN <- mean(foo < input$senn)*length(foo)
    #         TC.SENN <- round(T.SENN/length(foo)*100,1)
    #         # ---------------------------------------------------------------------------
    #         trt <- trial[trial$treat==0,]
    #         trt$diff <-trt$delta.observed 
    #         foo <- sort(trt[,"diff"])
    #         C <- mean(foo > sample$trt)*length(foo)   # 
    #         CT <- round(C/length(foo)*100,1)
    #         CN = length(foo)
    #         
    #         C.SENN <-mean(foo < input$senn)*length(foo)
    #         CT.SENN <- round(C.SENN/length(foo)*100,1)
    # }
    #        
    
    
    
    
    
    
    
    
    
    
    
    # Z <- data.frame(AN=AN, A=A, AT=AT, CN=CN, C=C, CT= CT)
    # names(Z) <- c("N trt","Observed responders trt",  "%" , "N ctrl","Observed responders ctrl" , "%")
    # rownames(Z) <- NULL
    # ---------------------------------------------------------------------------
    # return(list(A=A, AT=AT, C=C, CT= CT,  AN=AN, CN=CN, T.SENN=T.SENN, TC.SENN=TC.SENN, #Z=Z,
    #             C.SENN=C.SENN , CT.SENN=CT.SENN)) 
    
  })
  # ---------------------------------------------------------------------------
  lmm <- reactive({
    
    #   sample <- random.sample()
    #   
    #   d <- make.data()$d
    # 
    #  require(nlme)
    # # LMM approach
    # m1 <- lme(delta.observed~ treat + y.0observed,
    #           random=~1|treat , data=d, method="REML",
    #           weights = varIdent(form = ~1 | treat))
    # 
    # m0 <-lme(delta.observed~ treat + y.0observed,
    #          random=~1|treat , data=d, method="REML")
    # 
    # # print(m1)
    # m2 <- anova(m1,m0) # are the trt ctr interindividual variation in response different?
    # # 
    # # c.grp <- m1$sigma
    # # t.grp <- coef(m1$modelStruct$varStruct, uncons = FALSE)[[1]]*m1$sigma
    # 
    # # true individual response to the intervention estimate
    # sqrt(t.grp^2 - c.grp^2) 
    # 
    # # truth
    # sd(sample()$beta.treatment )
    # 
    # return(list(m1=m1, m0=m0, m2=m2)) 
    
  })
  
  # ---------------------------------------------------------------------------
  output$reg.lmm0 <- renderPrint({
    
    # return(lmm()$m0)
    
  })
  
  # ---------------------------------------------------------------------------
  
  output$reg.lmm1 <- renderPrint({
    
    # return(lmm()$m1)
    
  })
  # ---------------------------------------------------------------------------
  
  output$reg.lmm2 <- renderPrint({
    
    # return(lmm()$m2)
    
  })
  # ---------------------------------------------------------------------------
  
  output$A <- renderPrint({
    # stats()$A
  }) 
  # ---------------------------------------------------------------------------
  
  output$C <- renderPrint({
    # stats()$C
  }) 
  
  # ---------------------------------------------------------------------------
  
  output$xx <- renderPrint({ 
    
    # m  <- stats()$Z
    # 
    #     return(m )
  })
  
  # ---------------------------------------------------------------------------
  
  output$tablex <- DT::renderDataTable({
    
    # foo<- stats()$Z
    # 
    #  rownames(foo) <- NULL
    # library(DT)
    # 
    # datatable(foo,
    # 
    #           rownames = TRUE,
    #           #
    #           options = list(
    #             searching = TRUE,
    #             pageLength = 15,
    #             paging=TRUE,
    #             lengthMenu = FALSE ,
    #             lengthChange = FALSE,
    #             autoWidth = FALSE,
    #             #  colReorder = TRUE,
    #             #deferRender = TRUE,
    #             # scrollY = 200,
    #             scroller = T
    #           ))  %>%
    # 
    #   formatRound(
    #     columns= namez,
    #     digits=c(2,2,2,2)  )
  })
  
  # ---------------------------------------------------------------------------
  output$table1 <- DT::renderDataTable({
    
    # foo<- make.data()$d
    # 
    # namez <- c("true baseline","observed baseline","eligible","treatment group","true treatment effect\n in treated only","
    #           true response","observed response","delta observed")
    # names(foo) <- namez
    #  rownames(foo) <- NULL
    # library(DT)
    #  
    # datatable(foo,   
    #             
    #            rownames = TRUE,
    # #           
    #            options = list(
    #                searching = TRUE,
    #                pageLength = 15,
    #                paging=TRUE,
    #                lengthMenu = FALSE ,
    #                lengthChange = FALSE,
    #                autoWidth = FALSE,
    #             #  colReorder = TRUE,
    #              #deferRender = TRUE,
    #                # scrollY = 200,
    #               scroller = T
    #            ))  %>%
    #   
    #      formatRound(
    #          columns= namez,   
    #                     digits=c(2,2,0,0,1,2,2,2)  )
  })
  # --------------------------------------------------------------------------
  # ---------------------------------------------------------------------------
})

# Run the application 
shinyApp(ui = ui, server = server)