library(shiny)
library(DemoTools)
library(dplyr)
library(ggplot2)
library(shinythemes)

ui <- fluidPage(
  theme = shinytheme("simplex"),
  #shinythemes::themeSelector(),
  
  #Set Slider Colours
  tags$style(HTML(".js-irs-0 .irs-single, .js-irs-0 .irs-bar-edge, .js-irs-0 .irs-bar {background: firebrick}")),
  tags$style(HTML(".js-irs-1 .irs-single, .js-irs-1 .irs-bar-edge, .js-irs-1 .irs-bar {background: firebrick}")),
  
  tags$style(HTML(".js-irs-2 .irs-single, .js-irs-2 .irs-bar-edge, .js-irs-2 .irs-bar {background: steelblue}")),
  tags$style(HTML(".js-irs-3 .irs-single, .js-irs-3 .irs-bar-edge, .js-irs-3 .irs-bar {background: steelblue}")),
  tags$style(HTML(".js-irs-4 .irs-single, .js-irs-4 .irs-bar-edge, .js-irs-4 .irs-bar {background: steelblue}")),
  tags$style(HTML(".js-irs-5 .irs-single, .js-irs-5 .irs-bar-edge, .js-irs-5 .irs-bar {background: steelblue}")),
  
  
  tags$style(HTML(".js-irs-6 .irs-single, .js-irs-6 .irs-bar-edge, .js-irs-6 .irs-bar {background: forestgreen}")),
  tags$style(HTML(".js-irs-7 .irs-single, .js-irs-7 .irs-bar-edge, .js-irs-7 .irs-bar {background: forestgreen}")),
  tags$style(HTML(".js-irs-8 .irs-single, .js-irs-8 .irs-bar-edge, .js-irs-8 .irs-bar {background: forestgreen}")),
  tags$style(HTML(".js-irs-9 .irs-single, .js-irs-9 .irs-bar-edge, .js-irs-9 .irs-bar {background: forestgreen}")),
  
  tags$style(HTML(".js-irs-10 .irs-single, .js-irs-10 .irs-bar-edge, .js-irs-10 .irs-bar {background: orange}")),
  tags$style(HTML(".js-irs-11 .irs-single, .js-irs-11 .irs-bar-edge, .js-irs-11 .irs-bar {background: orange}")),
  
  tags$style(HTML(".js-irs-12 .irs-single, .js-irs-12 .irs-bar-edge, .js-irs-12 .irs-bar {background: black}")),
  tags$style(HTML(".js-irs-13 .irs-single, .js-irs-13 .irs-bar-edge, .js-irs-13 .irs-bar {background: firebrick}")),
  
  tags$head(tags$style(
    type = 'text/css',
    'form.well { max-height: 750px; overflow-y: auto;}'
  )),
  
  titlePanel("Rogers-Castro Migration Schedules"),
  
  sidebarLayout(
    sidebarPanel(
      
      #PRE-WORKING AGE INPUTS
      h1(id="heading1", "Pre-Working Age"),
      tags$style(HTML("#heading1{font-size: 20px;}")),
      checkboxInput(inputId = "pre_working_box",
                    label = "Include Component",
                    value = TRUE),
      conditionalPanel(
        condition = "input.pre_working_box",
                       sliderInput(inputId = "a1",
                                   label = HTML("a<sub>1</sub> for height of peak:"),
                                   min = 0,
                                   max = 1,
                                   value = 0.09),
                       sliderInput(inputId = "alpha1",
                                   label = HTML("&alpha;<sub>1</sub> for shape of component:"),
                                   min = 0,
                                   max = 1,
                                   value = 0.1)
      ),
      hr(),

      #WORKING AGE
      h1(id="heading2", "Working Age"),
      tags$style(HTML("#heading2{font-size: 20px;}")),
      checkboxInput(inputId = "working_box",
                    label = "Include Component",
                    value = TRUE),
      conditionalPanel(condition = "input.working_box",
                       sliderInput(inputId = "a2",
                                   label = HTML("a<sub>2</sub> for height of peak:"),
                                   min = 0,
                                   max = 1,
                                   value = 0.2),
                       sliderInput(inputId = "alpha2",
                                   label = HTML("&alpha;<sub>2</sub> for shape of component:"),
                                   min = 0,
                                   max = 1,
                                   value = 0.1),
                       sliderInput(inputId = "mu2",
                                   label = HTML("&mu;<sub>2</sub> for age of labour force peak:"),
                                   min = 0,
                                   max = 100,
                                   value = 21),
                       sliderInput(inputId = "lambda2",
                                   label = HTML("&lambda;<sub>2</sub> for shape of component:"),
                                   min = 0,
                                   max = 1,
                                   value = 0.4)
      ),
      hr(),
      
      #RETIREMENT AGE
      h1(id="heading3", "Retirement Age"),
      tags$style(HTML("#heading3{font-size: 20px;}")),
      checkboxInput(inputId = "retirement_box",
                    label = "Include Component",
                    value = TRUE),
      conditionalPanel(condition = "input.retirement_box",
                       sliderInput(inputId = "a3",
                                   label = HTML("a<sub>3</sub> for height of peak:"),
                                   min = 0,
                                   max = 1,
                                   value = 0.02),
                       
                       sliderInput(inputId = "alpha3",
                                   label = HTML("&alpha;<sub>3</sub> for shape of component:"),
                                   min = 0,
                                   max = 1,
                                   value = 0.25),
                       
                       sliderInput(inputId = "mu3",
                                   label = HTML("&mu;<sub>3</sub> for age of retirement peak:"),
                                   min = 0,
                                   max = 100,
                                   value = 67),
                       
                       sliderInput(inputId = "lambda3",
                                   label = HTML("&lambda;<sub>3</sub> for shape of component:"),
                                   min = 0,
                                   max = 1,
                                   value = 0.6)
      ),
      hr(),
      
      #POST-RETIREMENT AGE
      h1(id="heading4", "Post-Retirement Age"),
      tags$style(HTML("#heading4{font-size: 20px;}")),
      checkboxInput(inputId = "post_retirement_box",
                    label = "Include Component",
                    value = TRUE),
      conditionalPanel(condition = "input.post_retirement_box",
      sliderInput(inputId = "a4",
                  label = HTML("a<sub>4</sub> for height of peak:"),
                  min = 0,
                  max = 1,
                  value = 0.01),
      
      sliderInput(inputId = "lambda4",
                  label = HTML("&lambda;<sub>4</sub> for shape of component:"),
                  min = -0.1,
                  max = 0.05,
                  value = 0)
      ),
      hr(),
      
      #OVERALL
      h1(id="heading5", "Overall Migration Level"),
      tags$style(HTML("#heading5{font-size: 20px;}")),
      sliderInput(inputId = "c",
                  label = "c for overall migration level:",
                  min = 0,
                  max = 1,
                  value = 0.01),
      
      hr(),
      
      #MAX AGE
      h1(id="heading6", "Maximum Age"),
      tags$style(HTML("#heading6{font-size: 20px;}")),
      sliderInput(inputId = "max_age",
                  label = "Maximum age displayed on graph:",
                  min = 80,
                  max = 120,
                  value = 100)
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Histogram ----
      plotOutput(outputId = "distPlot")
      
    )
  )
)

server <- function(input, output) {
  
  output$distPlot <- renderPlot({
    
    npars = 1
    a1= input$a1; alpha1= input$alpha1
    a2= input$a2; alpha2= input$alpha2; mu2= input$mu2; lambda2= input$lambda2
    a3= input$a3; alpha3= input$alpha3; mu3= input$mu3; lambda3= input$lambda3
    a4= input$a4; lambda4 = input$lambda4
  
    if (input$pre_working_box){
      npars = npars + 2
    } else {
      a1 = 0; alpha1 = 0
    }
    if (input$working_box){
      npars = npars + 4
    } else {
      a2=0; alpha2=0; mu2=0; lambda2=0
    }
    if (input$retirement_box){
      npars = npars + 4
    } else {
      a3=0; alpha3=0; mu3=0; lambda3=0
    }
    if (input$post_retirement_box){
      npars = npars + 2
    } else {
      a4=0; lambda4=0
    }
    
    pars <- c(a1=a1, alpha1=alpha1,
              a2=a2, alpha2=alpha2, mu2=mu2, lambda2=lambda2,
              a3=a3, alpha3=alpha3, mu3=mu3, lambda3=lambda3,
              a4=a4, lambda4 =lambda4,
              c=input$c)
    
    ages <- 0:input$max_age
    mx <- mig_calculate_rc(ages = ages, pars = pars)
    
    df <- tibble(age = ages, mx = mx)
    df %>%
      ggplot(aes(age, mx)) +
      geom_line(color="steelblue", size=1.5) +
      ggtitle(paste0(npars, "-Parameter Model")) + 
      theme_minimal() +
      theme(text=element_text(size=16)) +
      xlab("Age") + ylab("Migration Rate (mx)")
    
  })
  
}


shinyApp(ui = ui, server = server)
