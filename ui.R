library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
    
    # Application title
    titlePanel("PvE Online"),
    
    # Sidebar with a slider input for the number of bins
    fluidRow(
        column(3,
               strong("Step 1: Your Data"),
               br(),
               checkboxInput("has_header", "My data has a header row", value = FALSE), 
               HTML('<textarea id="raw_dat" rows="15" cols="30" 
                    placeholder="Paste your data here"></textarea>'),
               br(),
               helpText("NB: Even if you have a header row, irradiance (E) must be in the first column and photosynthesis (P) must be in the second column.")
               ),
        column(3,
            selectInput("model_type", "Step 2: Select a model", 
                        c("Jassby and Platt 1976 (tanh)" = "tanh", 
                          "Linear Model" = "linear")),
            uiOutput("model_formula"),
            hr(),
            strong("Step 3: Estimate Initial Values"),
            br(), br(),
            uiOutput("coef_guess_ui")
            ),
        
        column(6,
            strong("Step 4: Fit the model"),
            br(),
            checkboxInput("optimise", "Check here to optimise.", FALSE),
            br(), br(),
            imageOutput("dat_plot", height = "300px"),
            conditionalPanel(
                condition = "input.optimise",
                helpText(strong("Fitted coefficients")),
                helpText(htmlOutput("fit_print"))
            ))
    ),
    
    fluidRow(
        column(6,
        h4("About This Page"),
        HTML("This page was created by <a href='http://www.pritchard.co/research'>Daniel Pritchard</a> to support undergraduate teaching. It uses, <a href='http://www.r-project.org'>R</a>, <a href='http://www.rstudio.com'>RStudio</a> and <a href='http://shiny.rstudio.com'>Shiny</a> to present a simplified interface for non-linear curve fitting, with specific applications in algal ecophysiology. It is not intended for use in research applications. For that, please <a href='http://www.pritchard.co/contact'>contact Daniel directly</a>.")
        ),
        column(6,
        h4("Useful References"),
        HTML("Jassby, A. and Platt, T.  1976. Mathematical formulation of relationship between photosynthesis and light for phytoplankton.  <em>Limnology and Oceanography</em>, 21: 540--547.<br>")
        )
    )
))