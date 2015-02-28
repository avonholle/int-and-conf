library(shiny)
library(stargazer)
library(contrast)
library(Epi)
library(reshape)
library(ggplot2)
library(reshape2)

# Parameters for function below..................
# ns: number of people in sample
# beta0: coefficient for intercept
# beta1: coefficient for x term
# beta2: coefficient for z term
# beta3: coefficient for interaction term between x and z
# confound: indicator variable (F=no, T=yes) for confounding
 
created = function(ns, beta0, beta1, beta2, beta3, confound=F, interact=F){
  # Get exposure, x, and covariate, z.
  # if confounder, it is dependent on x with some random error
  x.1 <- sample(c(0,1), ns, replace=T, prob=c(0.5,0.5)) 
  ez <- rnorm(ns, mean=0, sd=0.01) # random error term for z 
  
  z.1 <- ifelse(rep(confound,ns),
                  rbinom(ns, 1,  expit(-(round(3*x.1 + ez, 1)))),
                  sample(c(0,1), ns, replace=T, prob=c(0.5,0.5))
  )                                           # return confounder if confound=T,
  # otherwise return a dichotomous variable with 50/50 distirbution

  e4 <- rnorm(ns, mean=0, sd=0.01) # random error term
  
  lp <- ifelse(rep(interact, ns),
               beta0 + beta1*x.1 + beta2*z.1 + beta3*x.1*z.1 + e4,
               beta0 + beta1*x.1 + beta2*z.1 + e4)
               # linear predictor (lp)
  pr <- expit(lp)        # apply inverse logit function to lp to get probability
  y  <- rbinom(ns, 1, pr)     # generate bernoulli response variable with probability pr.4 for n1 number of observations.
  
  df <- data.frame(y=y, x=x.1, z=z.1)
  return(df)
}

expit <- function(x) 1/(1+exp(-x))

shinyServer(function(input, output) {

  withMathJax()

  # take parameters entered with the slider bar and use to simulate data
  ldata <- reactive({
    created(input$n1, input$beta0, input$beta1, input$beta2, input$beta3, input$conf, input$interact)
  })
  
# get a summary of the simulated data
output$table1 <- renderTable(print(ldata()[1:10,]))

# get 2x2 tables by strata
# This method doesn't work in putting the object into the server.R
# #########################################################

# tbl.1 <- reactive({
#   by(ldata(), ldata()$z, function(x) twoby2(table(x$y, x$x)))
# })
# 
# output$twobytwo.z0 <- renderTable({ tbl.1()$"0"$table })
# output$twobytwo.z1 <- renderTable({ tbl.1()$"1"$table })

# alternate way of getting two by two table
output$table1alt <- renderText ({ 
  (with(ldata(), table(z, y, x)))
}) # doesn't work so well

# alternate way of getting two by two table
output$table2alt <- renderTable ({ 
  dcast(ldata(), z + y ~ x, value.var="z", fun.aggregate=length)
})

# Use simulated data to run logistic regression
results1 <- reactive({
     glm( y ~ x*z, family="binomial", data=ldata() )
   })

# use simulated data to run logistic regression without interaction -- just crude estimate
# ##################################################################

results.none <- reactive({
  glm( y ~ x , family="binomial", data=ldata() )
})

res.none.check <- reactive({
  res.none <- glm( y ~ x , family="binomial", data=ldata() )
  
  o.x.crude <- contrast(res.none, 
                           list(x = 1), 
                           list(x = 0)) # difference in log odds of x=1 and x=0
  
  return(list(results.none=res.none, odds.x.crude=o.x.crude))
})

output$check.odds.crude.2 <-  renderText({
  stargazer(res.none.check()$odds.x.crude$Contrast, type="html")
})

output$check.odds.crude.3 <-  renderText({
  stargazer(res.none.check()$results.none, type="html")
})

compare <- reactive({
  odds.x.crude <- contrast(results.none(), 
                           list(x = 1), 
                           list(x = 0)) # difference in log odds of x=1 and x=0
  
  odds.x.z0 <- contrast(results1(), 
                        list(x=1, z=0), 
                        list(x=0, z=0))
  
  odds.x.z1 <- contrast(results1(), 
                        list(x=1, z=1), 
                        list(x=0, z=1))
  return(list(crude.odds=odds.x.crude, z0.odds=odds.x.z0, z1.odds=odds.x.z1))
})

output$compare.odds.crude <- renderText(
                                paste("Crude odds is:", 
                                     round(exp(compare()$crude.odds$Contrast), digits=2))
                                )

output$check.odds.crude <-  renderText({
  stargazer(results.none(), type="html")
})


output$compare.odds.z0 <- renderText(
                                paste("Odds at strata z=0 is:", 
                                      round(exp(compare()$z0.odds$Contrast), 2))
                                )
                                
output$compare.odds.z1 <- renderText(
                                paste("Odds at strata z=1 is:", 
                                      round(exp(compare()$z1.odds$Contrast), 2))
                                )

# Output results to a table
# ############################
output$summary <- renderTable({
  summary(results1())
})

output$summary.2 <- renderText({
  stargazer(results1(), type="html")
})

# plot odds
# ########################

# extract data
odds <- reactive({
  
  odds.4.00 <- contrast(results1(), a = list(x = 0, z=0))
  odds.4.01 <- contrast(results1(), a = list(x = 0, z=1))
  odds.4.10 <- contrast(results1(), a = list(x = 1, z=0))
  odds.4.11 <- contrast(results1(), a = list(x = 1, z=1))
  
  do <- cbind.data.frame(
    type=c("00", "01", "10", "11"), 
    rbind.data.frame( odds.4.00[c(3:6)],
                      odds.4.01[c(3:6)],
                      odds.4.10[c(3:6)],
                      odds.4.11[c(3:6)]))
  do$type <- factor(do$type, labels=c("x=0 and z=0",
                                      "x=0 and z=1",
                                      "x=1 and z=0",
                                      "x=1 and z=1"))
  return(do)
})



# Make plot
output$oddsplot <- renderPlot({
  ggplot(odds(), aes(y=Contrast, x=type)) +
        geom_point() +
        geom_errorbar(aes(ymin=Lower, ymax=Upper), width=0.2) +
        # scale_y_log10() +
        geom_hline(yintercept = 0, linetype=2) +
        coord_flip() +
        labs(list(
          title = 'log(odds) by group',
          x = "Groups", 
          y = "log(odds)")) +
        theme_bw()
})

# extract data for 2nd iteration of plot -- with odds ratios
# .........................................................


# Make a plot of marginal odds ratios and crude odds ratio
# ..............................................

# extract data
odds.2 <- reactive({
  
  # get estimate of odds ratios with se for intervals in plot
  odds.crude <-  contrast(results.none(), 
                          list(x = 1), 
                          list(x = 0)) # difference in log odds of x=1 and x=0
  odds.z0 <-  contrast(results1(), 
                       list(x = 1, z=0), 
                       list(x = 0, z=0)) # difference in log odds of x=1 and x=0
  odds.z1 <-  contrast(results1(), 
                       list(x = 1, z=1), 
                       list(x = 0, z=1)) # difference in log odds of x=1 and x=0
  odds.2 <- cbind.data.frame(
    type=c( "Strata, z=0", "Crude", "Strata, z=1"), 
    rbind.data.frame( odds.z0[c(2:5)],
                      odds.crude[c(1:4)],
                      odds.z1[c(2:5)])
  )
  
  odds.2 = within(odds.2, {
    OR.lower = exp(Contrast-1.96*SE)
    OR = exp(Contrast)
    OR.upper = exp(Contrast+1.96*SE)
  })
  
  odds.2$type = factor(odds.2$type, levels=c("Strata, z=0",
                                             "Crude",
                                             "Strata, z=1")) # change order of type 
  return(odds.2)
})

# Make 2nd plot

output$oddsplot.2 <- renderPlot({
  ggplot(odds.2(), aes(y=OR, x=type)) +
    geom_point() +
    geom_errorbar(aes(ymin=OR.lower, ymax=OR.upper), width=0.2) +
    scale_y_log10() +
    geom_hline(yintercept = 0, linetype=2) +
    coord_flip() +
    labs(list(
      title = 'Odds ratios by strata and crude in model with multiplicative interaction.',
      x = "Groups", 
      y = "Odds ratios")) +
    theme_bw()
})

# output odds ratios to table by strata of x and z
# ###########################################################
## odds and 95% CI
# ########################

tableodds <- reactive({
  
    odds.4.00 <- contrast(results1(), a = list(x = 0, z=0))
    odds.4.01 <- contrast(results1(), a = list(x = 0, z=1))
    odds.4.10 <- contrast(results1(), a = list(x = 1, z=0))
    odds.4.11 <- contrast(results1(), a = list(x = 1, z=1))
    
    odds <- cbind.data.frame(
      type=c("00", "01", "10", "11"), 
      rbind.data.frame( 
        odds.4.00[c(3:6)],
        odds.4.01[c(3:6)],
        odds.4.10[c(3:6)],
        odds.4.11[c(3:6)])) # this is estimate of log odds
    
    odds$divide <- exp(odds$Contrast -odds$Contrast[1]) # this is odds ratio (log difference in log odds relative to odds of 00, considered baseline)
    # make groups for or
    odds$xvar <- ifelse(odds$type %in% c("10", "11"), 1 ,0)
    odds$zvar <- ifelse(odds$type %in% c("01", "11"), 1 ,0)
    # both estimates are differences in log odds. take exp of qty to get odds ratio
    
    # make a 2 by 2 table of odds ratios
    # ##################################
    dcast(odds, xvar~zvar, value.var="divide", fun.aggregate=mean)
})

output$to.1 <- renderTable({
  tableodds()
})


# ###############################################
# Get an icr value based on odds ratios
# make ICR
# #####################
icr <- reactive({
  or.01 <- contrast(results1(), list(x = 0, z=1),
                      list(x = 0, z=0))
  or.10 <- contrast(results1(), list(x = 1, z=0),
                      list(x = 0, z=0))
  or.11 <- contrast(results1(), list(x = 1, z=1),
                      list(x = 0, z=0))
  icr.1 = exp(or.11$Contrast) - exp(or.10$Contrast) - exp(or.01$Contrast) + 1
  icr.1
  expected.11 <- exp(or.10$Contrast) + exp(or.01$Contrast) - 1
  return(list(icr.1=icr.1, expected.11 = expected.11, or.11=or.11$Contrast))
})

output$texticr <- renderText({
  paste("The ICR is: ", round(icr()$icr.1,digits=2))
})

output$texticr.2 <- renderText({
  paste("The expected OR for '11' group is: ", round(icr()$expected.11, digits=2), 
        ", and the observed OR for '11' group (with no confounding) is: ", round(exp(icr()$or.11), digits=2))
})

# Output parameter values
# ##############################

output$textn <- renderText({ 
  paste("The sample size is: ", input$n1)
})

output$text0 <- renderUI({
  x<-input$beta0
  y<-input$beta1
  z<-input$beta2
  z2 <- input$beta3
  withMathJax(
    sprintf("\\( 
            \\beta_0 = %.02f , 
            \\beta_1 = %.02f ,  
            \\beta_2 = %.02f , 
            \\beta_3 = %.02f \\)", x, y, z, z2)
  )
})

output$text0i <- renderUI({
  x <- exp(input$beta0)
  y <- exp(input$beta1)
  z <- exp(input$beta2)
  z2 <- exp(input$beta3)
  withMathJax(
    sprintf("\\(
            exp(\\beta_0) = %.02f = \\text{odds of y at x=0 and z=0,}
            \\)",
            x))
  
})

output$text1i <- renderUI({
  y <- exp(input$beta1)
  withMathJax(
    sprintf("\\(
            exp(\\beta_1) = %.02f = \\text{odds ratio for x=1 vs x=0 with no interaction,}
            \\)",
            y))
  
})

output$text2i <- renderUI({
  z <- exp(input$beta2)
  withMathJax(
    sprintf("\\(
            exp(\\beta_2) = %.02f = \\text{odds ratio for z=1 vs z=0 with no interaction,}
            \\)",
            z))
  
})

output$text3i <- renderUI({
  z2 <- exp(input$beta3)
  withMathJax(
    sprintf("\\(
            exp(\\beta_3) = %.02f = \\text{interaction term}
            \\)",
            z2))
  
})


output$textc <- renderText({ 
  paste("Confounding status is: ", input$conf)
})

output$texti <- renderText({ 
  paste("Interaction status is: ", input$interact)
})

#output$eqn2 <- renderText("y = &#946;_1*x")

# see http://shiny.rstudio.com/gallery/mathjax.html
# have to be careful with font sizes.
output$eqn1 <- renderUI({
  withMathJax(
      helpText('\\( \\text{logit(p) = } \\left(\\frac{p}{1-p}\\right) \\text{ = } \\beta_0 + \\beta_1 \\cdot x + \\beta_2 \\cdot z + \\beta_3 \\cdot x \\cdot z\\)')
  )
})

output$textconf <- renderUI({
  withMathJax(
    helpText('\\( \\text{logit(z) = 0 + 3}\\cdot\\text{x} \\)'
             ))
})

})