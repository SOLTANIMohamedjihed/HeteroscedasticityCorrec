library(shiny)
library(MASS)
library(car)

# Define UI
ui <- fluidPage(
  titlePanel("Heteroscedasticity Correction "),
  sidebarLayout(
    sidebarPanel(
      selectInput("dataset", "Select a dataset", choices = c("mtcars", "iris")),
      checkboxGroupInput("methods", "Select methods to use:",
                         choices = c("WLS", "GLS", "Transformations", "Robust SE"),
                         selected = c("WLS", "GLS"))
    ),
    mainPanel(
      plotOutput("plot"),
      tableOutput("table")
    )
  )
)

# Define server
server <- function(input, output) {

  # Load dataset
  data <- reactive({
    switch(input$dataset,
           "mtcars" = mtcars,
           "iris" = iris)
  })

  # Apply selected methods
  model <- reactive({
    if ("WLS" %in% input$methods) {
      # Weighted Least Squares
      fit <- glm(vs ~ wt + mpg, data = data(), family = binomial())
      w <- 1/resid(fit)^2
      fit_wls <- glm(vs ~ wt + mpg, data = data(), family = binomial(), weights = w)
    }
    if ("GLS" %in% input$methods) {
      # Generalized Least Squares
      fit_gls <- gls(vs ~ wt + mpg, data = data(), method = "ML")
    }
    if ("Transformations" %in% input$methods) {
      # Transformations
      fit_log <- glm(vs ~ log(wt) + log(mpg), data = data(), family = binomial())
      fit_sqrt <- glm(vs ~ sqrt(wt) + sqrt(mpg), data = data(), family = binomial())
    }
    if ("Robust SE" %in% input$methods) {
      # Robust Standard Errors
      fit_robust <- glm(vs ~ wt + mpg, data = data(), family = binomial())
      fit_robust_se <- coeftest(fit_robust, vcov = vcovHC(fit_robust, type = "HC1"))
    }
    list(fit_wls = fit_wls, fit_gls = fit_gls, fit_log = fit_log, fit_sqrt = fit_sqrt, fit_robust_se = fit_robust_se)
  })

  # Generate plot
  output$plot <- renderPlot({
    plot(data()$wt, data()$mpg, pch = ifelse(data()$vs == 0, 19, 3), col = ifelse(data()$vs == 0, "black", "red"), xlab = "Weight", ylab = "Miles per gallon")
    abline(v = mean(data()$wt), lty = 2)
    abline(h = mean(data()$mpg), lty = 2)
  })

  # Generate table
  output$table <- renderTable({
    model_list <- model()
    df <- data.frame(Method = character(), Coefficients = numeric(), SE = numeric(), pvalue = numeric(), stringsAsFactors = FALSE)
    if ("WLS" %in% input$methods) {
      wls_coef <- coef(model_list$fit_wls)
      wls_se <- sqrt(diag(vcov(model_list$fit_wls)))
      wls_pvalue <- summary(model_list$fit_wls)$coefficients[,4]
      df <- rbind(df, data.frame(Method = "WLS", Coefficients = wls_coef, SE = wls_se, pvalue = wls_pvalue))
    }
    if ("GLS" %in% input$methods) {
      gls_coef <- coef(model_list$fit_gls)
      gls_se <- sqrt(diag(vcov(model_list$fit_gls)))
      gls_pvalue <- summary(model_list$fit_gls)$tTable[, "Pr(>|t|)"]
      df <- rbind(df, data.frame(Method = "GLS", Coefficients = gls_coef, SE = gls_se, pvalue = gls_pvalue))
    }
    if ("Transformations" %in% input$methods) {
      log_coef <- coef(model_list$fit_log)
      log_se <- sqrt(diag(vcov(model_list$fit_log)))
      log_pvalue <- summary(model_list$fit_log)$coefficients[,4]
      df <- rbind(df, data.frame(Method = "Log Transformation", Coefficients = log_coef, SE = log_se, pvalue = log_pvalue))
      sqrt_coef <- coef(model_list$fit_sqrt)
      sqrt_se <- sqrt(diag(vcov(model_list$fit_sqrt)))
      sqrt_pvalue <- summary(model_list$fit_sqrt)$coefficients[,4]
      df <- rbind(df, data.frame(Method = "Square Root Transformation", Coefficients = sqrt_coef, SE = sqrt_se, pvalue = sqrt_pvalue))
    }
    if ("Robust SE" %in% input$methods) {
      robust_coef <- coef(model_list$fit_robust_se)
      robust_se <- sqrt(diag(vcov(model_list$fit_robust_se)))
      robust_pvalue <- summary(model_list$fit_robust_se)$coefficients[,4]
      df <- rbind(df, data.frame(Method = "Robust Standard Errors", Coefficients = robust_coef, SE = robust_se, pvalue = robust_pvalue))
    }
    df
  })
}

# Run app
shinyApp(ui = ui, server = server)