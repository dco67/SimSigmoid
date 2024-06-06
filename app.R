library(shiny)
library(ggplot2)
library(shinyWidgets)

# library(broom)
# library(nlshelper)


# Define the Gompertz, Logistic, and Weibull functions
gompertz <- function(b, x) {
  y <- b[1] * exp(-exp(b[2] - b[3] * x))
  out <- data.frame(x, y)
  return(out)
}

logistic <- function(b, x) {
  y <- b[1] / (1 + exp(b[2] - b[3] * x))
  out <- data.frame(x, y)
  return(out)
}

weibull <- function(b, x) {
  y <- b[1] * (1 - exp(-b[3] * x^b[2]))
  out <- data.frame(x, y)
  return(out)
}

# Default parameters and initial data
default_params <- c(100, 3, 0.2)

# Define the Shiny UI
ui <- fluidPage(
  withMathJax(),
  titlePanel("Modèles de Croissance"),
  sidebarLayout(
    sidebarPanel(
      selectInput("model", "Modèle", choices = c("Gompertz", "Logistic", "Weibull"), selected = "Gompertz"),
      sliderInput("b1", 
                  "Paramètre b1", 
                  min = 1, 
                  max = 200, 
                  value = default_params[1],
                  step = 0.1),
      sliderInput("b2", 
                  "Paramètre b2", 
                  min = .01, 
                  max = 3, 
                  value = default_params[2],
                  step = 0.1),
      sliderInput("b3", "Paramètre b3", 
                  min = 0.01, 
                  max = 1, 
                  value = default_params[3],
                  step = 0.01),
      sliderInput("error", 
                  "Erreur", 
                  min = 0, 
                  max = 10, 
                  value = 2, 
                  step = 0.1),
      
      actionBttn("generate", 
                 "Générer",
                 style = "stretch",
                 color = "danger",
                 icon = icon("sliders")
      ),
      downloadBttn(
        label = "Sauvegarder",
        outputId = "downloadData",
        style = "stretch",
        color = "success",
        icon = shiny::icon("download")
        ),
      wellPanel(style = "background: lightblue",
                fluidRow(
                  column(4,
                         a(h5("Par Daniel Coulombe, Ph.D.")),
                         p("2023")
                  ),
                  column(4,
                         tags$a(
                           href="https://isteah.org", 
                           tags$img(src="ISTEAH_LOGO.png", 
                                    title="ISTEAH", 
                                    width="120",
                                    height="100")
                         )
                  )
                )
      )
      ),
    mainPanel(
      uiOutput("equation"),
      plotOutput("growth_curve_plot"),
      verbatimTextOutput("regression_table"),
      tableOutput("regrTidy")
    )
  )
)

# Define the Shiny server
server <- function(input, output) {
  generating_data <- reactiveVal(NULL)
  generating_model <- reactiveVal(NULL)
  regression_coefs <- reactiveVal(NULL)
  regression_tidy <- reactiveVal(NULL)
  regression_full_results <- reactiveVal(NULL)
  
  observe({
    b <- c(input$b1, input$b2, input$b3)
    x <- seq(0, 40, length.out = 100)
    
    if (input$model == "Gompertz") {
      dat <- gompertz(b, x)
    } else if (input$model == "Logistic") {
      dat <- logistic(b, x)
    } else {
      dat <- weibull(b, x)
    }
    
    generating_data(dat)
    generating_model(dat)
  })
  
    observeEvent(input$generate, {
      b <- c(input$b1, input$b2, input$b3)
      x <- seq(0, 40, length.out = 100)
      
      if (input$model == "Gompertz") {
        dat <- gompertz(b, x)
      } else if (input$model == "Logistic") {
        dat <- logistic(b, x)
      } else {
        dat <- weibull(b, x)
      }
      
      # Add random error
      error <- rnorm(n = length(x), mean = 0, sd = input$error)
      dat$y <- dat$y + error
      generating_data(dat)
      
      # Perform nonlinear regression using nls
      model_function <- switch(input$model,
                               Gompertz = "b1 * exp(-exp(b2 - b3 * x))",
                               Logistic = "b1 / (1 + exp(b2 - b3 * x))",
                               Weibull = "b1 * (1 - exp(-b3 * x^b2))"
      )
      
      model_formula <- as.formula(paste("y ~", model_function))
      
      fit <- nls(model_formula, start = list(b1 = input$b1, b2 = input$b2, b3 = input$b3), data = dat)
      
      # Store regression results for the table
      regression_coefs(coefficients(fit))
      regression_full_results(summary(fit))
      regression_tidy(tidy(fit, conf.int=TRUE))
    })
    

  output$equation <- renderUI({
    ifelse(is.null(regression_coefs()), b <- c(input$b1, input$b2, input$b3), b <- regression_coefs())
    equation <- switch(input$model,
                       Gompertz = paste0("\\widehat{y} = ", round(b[1], 3), "e^{-e^{(", round(b[2], 3), " - ", round(b[3], 3), "x)}}"),
                       Logistic = paste0("\\widehat{y} = \\frac{", round(b[1], 3), "}{1+e^{(-", round(b[2], 3), " - ", round(b[3], 3), "x)}}"),
                       Weibull = paste0("\\widehat{y} = ", round(b[1], 3), "\\left [1 - e^{-", round(b[2], 3), "x^{", round(b[3], 3), "}}\\right ]")
    )
    
   h3(withMathJax(paste0("$$", equation, "$$")))
  })
  
  output$growth_curve_plot <- renderPlot({
    dat <- generating_data()
    model <- generating_model()
    
    if (!is.null(dat)) {
      p <- ggplot() +
        geom_line(data = model, aes(x, y), color = "blue") +
        geom_point(data = dat, aes(x, y), color = "red") +
        labs(title = "Modèle de Croissance", x = "X", y = "Y")
      print(p)
    }
  })
  
  
  output$regression_table <- renderPrint({
  regression_full_results()
  })
  
  output$regrTidy <- renderTable({
    regression_tidy()
  })
  
  output$downloadData <- downloadHandler(
    filename = function() {
      "growth_curve.csv"
    },
    content = function(file) {
      write.csv(generating_data(), file)
    }
  )
  
}

shinyApp(ui, server)
