#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Rshiny ideas from on https://gallery.shinyapps.io/multi_regression/
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
rm(list=ls())
library(ggplot2)
library(shiny) 
require(LearnBayes)
library(tidyverse)
library(shinyWidgets)
library(shinythemes)  # more funky looking apps
library(rstan)
library(DT)
require(rms) # freq logistic regression
library(shinyalert)

options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)
options(max.print=1000000)
fig.width <- 400
fig.height <- 300
fig.width2 <- 1400
fig.height2 <- 300#50
fig.width3 <- 1300  
fig.height3 <- 350#400
fig.width4 <- 1380
fig.height4 <- 300
p1 <- function(x) {formatC(x, format="f", digits=1)}
p2 <- function(x) {formatC(x, format="f", digits=2)}
options(width=200)
set.seed(12345) # reproducible

is.even <- function(x){ x %% 2 == 0 } # function to id. odd maybe useful


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
ui <- fluidPage(theme = shinytheme("journal"), #https://www.rdocumentation.org/packages/shinythemes/versions/1.1.2
                # paper
                useShinyalert(),  # Set up shinyalert
                setBackgroundColor(
                    color = c( "#2171B5", "#F7FBFF"), 
                    gradient = "linear",
                    direction = "bottom"
                ),
                
                h2("Estimates of and tests of proportions Bayesian style"),
                
                h4("Imagine planning a randomised clinical trial with equal recruitment to a treatment and a control arm. From a previous clinical trial studying 
                the disease we observed
                clinical events on the placebo arm but we have no knowledge for the new drug. Here clincial events are a good clinical outcome. 
                We plan a new study, using the information
                from the previous study via an appropriate Beta prior for the placebo. As we have no data regarding the expected event rate for the treatment 
                we will use a reference, uniform prior, try others for example Beta(0.5,0.5) or Beta(0,0). 
                There is no such thing as a noninformative prior. It is hoped that we will see a 30% event rate with the new treatment. 
                We fix the sample size and investigate the posterior distribtution of the difference in proportions, the ratio and the odds ratio.
                We observed 10 clincial events in 50 patients (20%) in the placebo arm of the earlier trial, we will therefore use a Beta(11, 41) for the placebo prior
                to plan our new study. With a sample size of 50 per arm, can we expect p(efficacy) > 0.95?
              "), 
                
                h3("  "), 
         
                
                sidebarLayout(
                    
                    sidebarPanel( width=3 ,
                                  
                                  tags$style(type="text/css", ".span8 .well { background-color: #00FFFF; }"),
                                
                                  
                                  actionButton(inputId='ab1', label="R code",   icon = icon("th"),   
                                               onclick ="window.open('https://raw.githubusercontent.com/eamonn2014/Bayesian-proportions-logistic-regression/master/monte-carlo-two-proportions/app.R', '_blank')"),    
                                  actionButton("resample", "Rerun the Monte Carlo simulations"),
                                  br(), # br(), 
                                  tags$style(".well {background-color:#b6aebd ;}"), ##ABB0B4AF
                                  
                                  h4("You can change the beta priors, the (planned) sample size and the number of events in the boxes below."),
                                  div(
                                      
                                      tags$head(
                                          tags$style(HTML('#ab1{background-color:orange}'))
                                      ),
                                      
                                      tags$head(
                                          tags$style(HTML('#resample{background-color:orange}'))
                                      ),
                                  
                                      
                                      textInput('vec3', 
                                                div(h5("Beta shape parameters for treatment prior")), "1, 1"),
                                      
                                      textInput('vec4', 
                                                div(h5("Beta shape parameters for control prior")), "11, 41"),
                                      
                                      textInput('n1y1', 
                                                div(h5("Treatment sample size and no. of events")), "50, 15"),
                                      
                                      textInput('n2y2', 
                                                div(h5("Control sample size and no. of events")), "50, 10"),
                                      
                                   
                                      div(h5("References:")),  
                                      
                                      tags$a(href = "https://lingpipe-blog.com/2009/10/13/bayesian-counterpart-to-fisher-exact-test-on-contingency-tables/", "[1] Ling Pipe"),
                                      div(p(" ")),
                                      tags$a(href = "https://statmodeling.stat.columbia.edu/2009/10/13/what_is_the_bay/", "[2] Gelman"),
                                      div(p(" ")),
                                      tags$a(href = "https://en.wikipedia.org/wiki/Beta_distribution", "[3] Beta distribution"),
                                       div(p(" ")),
                                       tags$a(href = "https://www.tjmahr.com/bayesian-fisher-exact-test/", "[4] Blog article"),
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
                           
                                  tabPanel("1 Prior and posterior analytic distributions", value=7, 
                                      
                                           
                                           fluidRow(
                                       
                                               
                                               column(width = 7, offset = 0, style='padding:0px;',
                                                      
                                                      h4("Prior distributions"), 
                                                      div(plotOutput("trt.plot", width=fig.width2, height=fig.height2)), 
                                                   
                                                      
                                                      h4("Posterior distributions updated with the observed data"), 
                                                      div(plotOutput("trt.plot1", width=fig.width2, height=fig.height2)),       
                                                      
                                                      
                                                      
                                               ))
                                           
                                  ) ,
                                  
                                  tabPanel("2 Quantities of interest generated from posteriors", value=7, 
                                       
                                           
                                           fluidRow(
                                               column(width = 5, offset = 0, style='padding:1px;',
                                            
                                               h4("Posterior distributions summaries, P(efficacy) will be judged by P(trt>ctrl)"), 
                                               div( verbatimTextOutput("reg.summary2")))),
                                               
                                             
                                                      
                                                     h4(paste("Posterior distributions : 1 risk difference (trt-ctrl);","2 relative risk (trt/ctrl); 3 odds ratio [odds(trt)/odds(ctrl)]")), 
                                                     div(plotOutput("diff", width=fig.width3, height=fig.height3)),       
                                         
                                         h6(paste("Blue vertical lines demark 95% credible intervals, red dashed lines are population values of interest")), 
                                                
                                              
                                  ) ,
                                  
                                  
                                  
                                  
                                  tabPanel("2 Same results using datatable function", value=3, 
                                     
                                           h6("Sort and filter on the fly."),
                                           h4("Posterior distributions summaries, p(efficacy) will be judged by p(trt>ctrl)"), 
                                           DT::dataTableOutput("tablex"),
                                           h4(paste("Posterior distributions : 1 risk difference (trt-ctrl);","2 relative risk (trt/ctrl); 3 odds ratio [odds(trt)/odds(ctrl)]")), 
                                           div(plotOutput("diff2", width=fig.width4, height=fig.height4)),  
                                           h6(paste("Blue vertical lines demark 95% credible intervals, red dashed lines are population values of interest")), 
                                  ),
                                  
                                  tabPanel("3 Frequentist analysis", value=3, 
                                           h3("No prior information is used ! Also don't forget the confidence intervals cannot be interpreted
                                              that there is the stated probability that the true population parameter lies in the interval !"),
                                           h4("Fisher's Exact Test for Count Data"),
                                           div( verbatimTextOutput("fisher")),
                                           h4("2-sample test for equality of proportions without continuity correction"), 
                                           div( verbatimTextOutput("prop")),
                                           h4("Logistic regression odds ratio"), 
                                           div( verbatimTextOutput("logregx")),
                                         
                                           
                                           
                                  )
                                 
                              )
                              #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                    )
                    
                ) 
                #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~end tab panels 
                
)


server <- shinyServer(function(input, output   ) {
    
  shinyalert("Bayesian evaluations of two proportions via Monte Carlo simulation",
             "Use at your own risk",
             type = "info")
    # --------------------------------------------------------------------------
    # ---------------------------------------------------------------------------
    # --------------------------------------------------------------------------
    # This is where a new sample is instigated 
    random.sample <- reactive({
        
        foo <- input$resample
    
        trt <- as.numeric(unlist(strsplit(input$vec3,",")))
        ctr <- as.numeric(unlist(strsplit(input$vec4,",")))
        
        n1y1 <- as.numeric(unlist(strsplit(input$n1y1,","))) #trt
        n2y2 <- as.numeric(unlist(strsplit(input$n2y2,",")))
         
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
        ptrt <- mean( theta1>theta2)  
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
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    
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
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    
    output$trt.plot <- renderPlot({         
        
        sample <- random.sample()
       
        x<- seq(0.001,.999, length.out=10000)
        
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
        
      x<- seq(0.001,.999, length.out=10000)
        
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
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    output$reg.summary2 <- renderPrint({
        
        return(print(mcmc()$f1, digits=4))
        
    })
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    
    output$diff2 <- renderPlot({         
        
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
        x$Model <- NULL
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
                columns= c("parameter","Mean","p2.5","p25","p50","p75","p975"), digits=c(0,3,3,3,3,3,3)  )
        
    })
    
    # --------------------------------------------------------------------------
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    fisher <- reactive({
      
      sample <- random.sample()
      
      n1 <- sample$n1  
      y1 <- sample$y1
      n2 <- sample$n2
      y2 <- sample$y2

      n1 <- 50
      y1 <- 15
      n2 <- 50
      y2 <- 10

      
      #~~~~~~~~~~~~~~~~~~~~~~~~~~
      data <- matrix(c(y1, n1-y1,y2,n2-y2),nr=2,dimnames=list(c("response","nonresponse"), c("trt","ctrl")))
      f <- fisher.test(data)
      
      #~~~~~~~~~~~~~~~~~~~~~~~~~~
      res <- prop.test(x = c(y1, y2), n = c(n1, n2), correct = FALSE)
      pr <- res 
      
      #~~~~~~~~~~~~~~~~~~~~~~~~~~
      # # logistic regression
  
      Table <- t( matrix(c(y1,y2,n1-y1,n2-y2), nc=2))
      f99 <- glm(as.table(Table) ~ c(1,0), family=binomial)
      f100 <- exp(coef(f99))[2][[1]]
  
      #~~~~~~~~~~~~~~~~~~~~~~~~~~
      
      d = as.data.frame(as.table(as.matrix(data)))
      
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
      
      
      dd$y <- ifelse(dd$Var1 %in% "response",1,0)
      dd$x <- ifelse(dd$Var2 %in% "trt",1,0)
      dd$Var1 <- dd$Var2 <- NULL
     
      #y <- dd$y; x <- dd$x
      # summary(glm(y ~ x, family=binomial))
      
      ddd <<- datadist(dd)
      options( datadist = "ddd" )
      harrell <- summary(lrm(y~x,dd))
      
   
      #~~~~~~~~~~~~~~~~~~~~~~~~~~
      return(list(f= f, pr=pr , logreg = harrell  ))   #, logreg = f99
      
  })
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    
    output$fisher <- renderPrint({
      
      return(print(fisher()$f, digits=4))
      
    })
    output$prop <- renderPrint({
      
      return(print(fisher()$pr, digits=4))
      
    })
    
    output$logregx <- renderPrint({
      
      return(print(fisher()$logreg, digits=4))
      
    })
    #~~~~~~~~~~~~~~
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # ---------------------------------------------------------------------------
    
    
})

# Run the application 
shinyApp(ui = ui, server = server)