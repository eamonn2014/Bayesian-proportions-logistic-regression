
 

#/***************************************************************************#

#Filename           :      Bayesian proportions and logistic regression.Rmd

#Author             :      obrieea1

#Date               :      24-May-2008

#R                  :      3.2.3 (2015-12-10)

#Platform           :      x86_64-pc-linux-gnu (64-bit)

#Project/Study      :      Development

#Description        :      Demonstrating test of proportions and logistic regression

#Assumptions:  

#Input              :      Knitr, Hmisc and Haven are the r pacakage required  

#Output             :      A Rmd and html file is outputed to MODESIM

#Macros used        :      none

#---------------------------------------------------------------------------

#MODIFICATION HISTORY:

#    <DD-MON-YYYY>,

#    <Description>

#   

#***************************************************************************/

 

        rm(list=ls())

 

        set.seed(123)

        startTime<-proc.time()

        library(knitr)

        options(width=120)

        opts_chunk$set(comment = "", warning = FALSE, message = FALSE,

                       echo = FALSE, tidy = FALSE, size="tiny",  cache=FALSE,

                       progress=TRUE,

                         fig.width=7, fig.height=3.5,

                       cache.path = 'program_Cache/',

                       fig.path='figure/')

        

        knitr::knit_hooks$set(inline = function(x) {

          knitr:::format_sci(x, 'md')

        })

        

        

        # create an R file of the code!

        # https://stackoverflow.com/questions/26048722/knitr-and-tangle-code-without-execution

       

         knit_hooks$set(purl = function(before, options) {

           if (before) return()

           input  = current_input()  # filename of input document

           output = paste(tools::file_path_sans_ext(input), 'R', sep = '.')

           if (knitr:::isFALSE(knitr:::.knitEnv$tangle.start)) {

           assign('tangle.start', TRUE, knitr:::.knitEnv)

           unlink(output)

         }

           cat(options$code, file = output, sep = '\n', append = TRUE)

         })

 

          # function to calculate the mode    

          getmode <- function(v) {

               uniqv <- unique(v)

               uniqv[which.max(tabulate(match(v, uniqv)))]

          }

 



      cat("\nThree ways to perform Bayesian analysis of 2by2 table. Modeling proportions using STAN, simulation in base R and logistic regression in STAN. I also use rms frequentis logistic regression.\n")

 


 

      # MODESIM directories

     #wd <- Rhome <-'/home/obrieea1/Documents/CAIN457 RISK CALCULATOR/'

      wd <- Rhome <-'C:/Users/User/Documents/GIT/Bayesian-proportions-logistic-regression'

      path.script <-wd.code <- wd #paste0(Rhome,'CODE DEVELOPMENT')                  # note this new dir

      wd.data <- wd # paste0(Rhome,'DATA')


      # GPS directories - not used here

      #gps <- '/view/obrieea1_view/vob/CIGE025E/CIGE025E3401/pub_2/'

      #path.script <- paste0(gps, '/pgm/eff/')

      #path.import <- paste0(gps, '/analysis_data/')

      #path.export <- paste0(gps, '/reports/eff/')

      #wd.code <- paste0(Rhome,'CODE')                

      #wd.data <- paste0(Rhome,'DATA')


      # GPS directories - not used here

      #gps <- '/view/obrieea1_view/vob/CAIN457/pool/pool_002/'

      #path.script <- paste0(gps, '/report/pgm_a/') 

      #path.import <- paste0(gps, '/report/data_a/')  # data is here

      #path.export <- paste0(gps, '/report/misc/')    # outputs here?


      # rounding functions

      p2 <- function(x) {formatC(x, format="f", digits=2)}

      p3 <- function(x) {formatC(x, format="f", digits=3)}

      p4 <- function(x) {formatC(x, format="f", digits=4)}

    


 

     

###

 

#https://stats.stackexchange.com/questions/136744/how-to-select-hyperprior-distribution-for-beta-distribution-parameter

#http://www.sumsar.net/files/posts/2017-bayesian-tutorial-exercises/modeling_exercise2_old.html

# https://stats.stackexchange.com/questions/58564/help-me-understand-bayesian-prior-and-posterior-distributions/58792#58792 ref for beta distributions

# https://rpubs.com/JanpuHou/294788 plot Beta dist

 


 

# choosing a prior of the treatment response rate,

# actually used this site to find aporpriate alpha and beta (LearnBayes package)

# http://a-little-book-of-r-for-bayesian-statistics.readthedocs.io/en/latest/src/bayesianstats.html

# we believe the median of the prior is 0.25

# we believe the 90th percentile of the prior is 0.45

# we believe the 10th percentile of the prior is 0.2

# here are my alpha and beta shape parameters, almost match our requirements

 

  a=7.17; b= 15.15

  qbeta(c(0.10, 0.5, 0.90), a,b) #0.1996753 0.3158126 0.4500998

 

# here is a plot of the chosen beta distribution

 

  curve(dbeta(x,a,b),col = "blue", xlab = "prior for trt", ylab = "Probability", xlim=c(0.0,0.8), ylim=c(0,5))

 

 

 


 

# choosing a prior of the placebo response rate,

 

 

# we believe the median of the prior is 0.20

# we believe the 90th percentile of the prior is 0.40

# we believe the 10th percentile of the prior is 0.15

# here are my alpha and beta shape parameters

 

a1=5.38;b1= 14.60

qbeta(c(0.10, 0.5, 0.90), a1,b1)  #0.1495854 0.2615579 0.3997546

 

curve(dbeta(x,a1, b1),col = "blue", xlab = "prior for trt", ylab = "Probability", xlim=c(0.0,0.8), ylim=c(0,5))

 

 

 

 


 

library(rstan)

 

# The Stan model as a string.

model_string <- "// Here we define the data we are going to pass into the model

 

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

  rate ~ beta(7.17, 15.15) ;    //prior this on trt

  s ~ binomial(n, rate);

 

  rate1 ~ beta(5.38, 14.60);  //prior this on placebo

  s1 ~ binomial(n1, rate1);

}

 

// Variables have to be defined before they are assigned to

 

generated quantities {

  real d;

  d = rate-rate1;

}"

 

 

# trial data

data_list <- list(n = 100, s = 56, n1=51, s1=9 )

 

# Compiling and producing posterior samples from the model.

stan_samples <- stan(model_code = model_string, data = data_list)

print(stan_samples, digits = 3)

 

 


 

    n1 = 100 # trt n

    y1 = 56  # trt observed responders

    n2 = 51  # placebo n

    y2 = 9   # placebo observed responders

   


   

    I = 100000                              # simulations

    theta1 = rbeta(I, y1+a, n1-y1+b)        # incorp. prior for trt

    theta2 = rbeta(I, y2+a1, n2-y2+b1)      # incorp. prior for placebo

    diff = theta1-theta2                    # simulated differences

   


   

    quantiles = quantile(diff,c(0.025,0.25,0.5,0.75,0.975))

    print(quantiles,digits=2)  

 

 


 

library(reshape2)

Table <- matrix(c(56,9,100-56,51-9), 2, 2, byrow=TRUE)

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

 

m1 <- '

data {                         

int<lower=0> N;                   // number of observations

int<lower=0,upper=1> y[N];        // setting the dependent variable (y) as binary

vector[N] x;                      // independent variable 1

}


parameters {

real alpha;                       // intercept

real b_x;                         // beta for x, etc

}

model {

alpha ~ student_t(3,0,2.5);              // you can set priors for all betas

b_x ~   student_t(3,0,2.5);              // if you prefer not to, uniform priors will be used

y ~ bernoulli_logit(alpha + b_x * x  ); // model

}'

 

# Create a list with the chosen variables

data.list <- list(N = nrow(dd), y = dd$y, x = dd$x )

str(data.list)

 

# Estimate the model

fit <- stan(model_code = m1, data = data.list, iter = 1000, chains = 4)

print(fit, digits = 3)

 

require(rms)

lrm(y~x,dd)

 

 


 

#https://cran.r-project.org/web/packages/rstan/vignettes/stanfit-objects.html

## extract alpha and beta with 'permuted = TRUE'

fit_ss <- extract(fit, permuted = TRUE) # fit_ss is a list

## list fit_ss should have elements with name 'alpha', 'beta', 'lp__'

alpha <- fit_ss$alpha

beta <- fit_ss$b_x

## or extract alpha by just specifying pars = 'alpha'

#alpha2 <- extract(fit, pars = 'alpha', permuted = TRUE)$alpha

 

  placebo.prob <- 1/(1+exp(-alpha))

  quantiles = quantile(placebo.prob,c(0.025,0.25,0.5,0.75,0.975))

  print(quantiles,digits=2)  

  

  

  trt.prob <- 1/(1+exp(-(alpha+beta)))

# trt.prob <- exp(alpha+beta)/(1+exp(alpha+beta)) # same as above

  quantiles = quantile(trt.prob,c(0.025,0.25,0.5,0.75,0.975))

  print(quantiles,digits=2)

 

  

 

 


 

      options(width=70)

      opts_knit$set(root.dir = wd.code)   ##THIS SETS YOUR WORKING DIRECTORY

      sessionInfo()

      print(getwd())

      stopTime<-proc.time()

 


 

    # move stangle R file to a folder in GPS

    # put this at bottom and give it the same name as the RMD file , replace any blanks with underscore

    # https://amywhiteheadresearch.wordpress.com/2014/11/12/copying-files-with-r/

 

    # https://stackoverflow.com/questions/21101573/need-the-filename-of-the-rmd-when-knitr-runs

    x <- knitr::current_input()

    x

    rcode <-  gsub(' ','_', trimws(x))                       # replace blank with underscore, this is needed

    file.copy(rcode, path.script,  overwrite=TRUE)           # make a copy of the rcode in a folder of choice

   

    

    # saving the html to GPS!

    # https://stackoverflow.com/questions/28894515/rmarkdown-directing-output-file-into-a-directory

    # x1 <- gsub('\\.Rmd','\\.html', trimws(x))

    # rcode <-  gsub(' ','_', trimws(x1))  

    # knit: (function(inputFile, encoding) {

    #   out_dir <- path.script;

    #   rmarkdown::render(inputFile,

    #                     encoding=encoding,

    #                     output_file=file.path(dirname(x), out_dir, rcode)) })

    #

    

    

 

