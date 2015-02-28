# ui.R

library(shiny)

shinyUI(pageWithSidebar(
  
  headerPanel("Demonstration of statistical interaction and confounding"),
    sidebarPanel(
      h2("Select parameters for simulation"),
      p("Regression coefficients for logistic regression:"),
      sliderInput("n1","sample size", min=50, max=5000, value=500, step=1, format="###", animate=FALSE),
      sliderInput("beta0","intercept:", min=-3, max=3, value=0.5, step=0.1, format="#.#", animate=FALSE),
      sliderInput("beta1","covariable x:", min=-3, max=3, value=0.1, step=0.1, format="#.#", animate=FALSE),
      sliderInput("beta2","covariable z", min=-3, max=3, value=0.0, step=0.1, format="#.#", animate=FALSE),
      sliderInput("beta3","interaction between x and z", min=-3, max=3, value=1, step=0.1, format="#.#", animate=FALSE),
      br(),
      checkboxInput(inputId = "conf", label = "Confounding", value=F),
      checkboxInput(inputId = "interact", label = "Interaction", value=F)
    ),
    
    mainPanel(
        withMathJax(),
        h3("Model for simulation"),
        h3(uiOutput("eqn1")),
        h4("Model for confounding between x and z"),
        h3(uiOutput("textconf")),
        h3("Selected parameters"),
        textOutput("textn"),
        h3(uiOutput("text0")),
        h3(uiOutput("text0i")),
        h3(uiOutput("text1i")),
        h3(uiOutput("text2i")),
        h3(uiOutput("text3i")),
        textOutput("textc"),
        textOutput("texti"),
        br(),
        h4("Estimated values"),
        h3("Compare crude odds ratio for x to strata estimates by z (to assess confounding)"),
        textOutput("compare.odds.crude"),
#        htmlOutput("check.odds.crude"),
#        htmlOutput("check.odds.crude.2"),
#        htmlOutput("check.odds.crude.3"),
        textOutput("compare.odds.z0"),
        textOutput("compare.odds.z1"),
        br(),
        h3("Table of odds ratios of y (vs the x=0 and z=0 group) by x and z (to assess interaction)"),
        tableOutput("to.1"),
        br(),
        h3("ICR"),
        textOutput("texticr"),
        textOutput("texticr.2"),
        h3("Sample of simulated data"),
        tableOutput("table1"),
        br(),
        h3("Frequencies of y and x by the z strata"),
        tableOutput("table2alt"),
        br(),
        h3("Summary of regression"),
        tableOutput("summary"),
        br(),
        h3("Plot of log odds by groups"),
        plotOutput("oddsplot"),
        br(),
        h3("Plot of crude and stratified odds ratios"),
        plotOutput("oddsplot.2")
        #htmlOutput("summary.2")
        
        
        # now need to add plots of param values and a stargazer across different model fits
    )
    
    ))