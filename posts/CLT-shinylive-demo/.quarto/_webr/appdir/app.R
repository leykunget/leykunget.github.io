library(shiny)
library(bslib)

theme <- bs_theme(font_scale = 1.5)

# Define UI for app that draws a histogram ----
ui <- page_sidebar(theme = theme,
  sidebar = sidebar(open = "open",
    numericInput("n", "Sample size", 100),
    checkboxInput("pause", "Pause", FALSE),
  ),
  plotOutput("plot", width=1100)
)

server <- function(input, output, session) {
  data <- reactive({
    input$resample
    if (!isTRUE(input$pause)) {
      invalidateLater(1000)
    }
    rnorm(input$n)
  })
  
  output$plot <- renderPlot({
    hist(data(),
         breaks = 40,
         xlim = c(-3, 3),
         ylim = c(0, 0.5),
         col = 'skyblue',
         border = 'white',
         xlab = "Value",
         freq = FALSE,
         main = "Central Limit Theorem Demonstration",
         cex.lab = 1.2,
         cex.axis = 1.1,
         cex.main = 1.4
    )
    
    x <- seq(from = -3, to = 3, length.out = 500)
    y <- dnorm(x)
    lines(x, y, lwd = 2, col = 'darkgreen')
    
    lwd <- 3
    abline(v = 0, col = "red", lwd = lwd, lty = 2)
    abline(v = mean(data()), col = "blue", lwd = lwd, lty = 1)
    
    legend("topright", legend = c("Normal Distribution", "Population Mean", "Sample Mean"),
           col = c("darkgreen", "red", "blue"),
           lty = c(1, 2, 1),
           lwd = c(2, lwd, lwd),
           bty = "n",
           cex = 1.1,
           text.font = 1.5
    )
    
    grid(nx = NULL, ny = NULL, col = "lightgray", lty = "dotted")
  }, res = 140)
}

# Create Shiny app ----
shinyApp(ui = ui, server = server)
