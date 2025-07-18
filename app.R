library(shiny)

ui <- fluidPage(
  titlePanel("Multi-tab Example: y = x² and z = log(y)"),
  tabsetPanel(
    tabPanel("Tab 1: y = x²",
             sidebarLayout(
               sidebarPanel(
                 numericInput("x_input", "Enter a value for x:", value = 1)
               ),
               mainPanel(
                 plotOutput("plot_y")
               )
             )
    ),
    tabPanel("Tab 2: z = log(y)",
             mainPanel(
               plotOutput("plot_z"),
               textOutput("y_value"),
               tableOutput("xy_table")  # <- added table output
             )
    )
  )
)

server <- function(input, output, session) {
  # Shared reactive values
  shared <- reactiveValues(y = NULL)
  
  # Compute y = x^2 when x is input
  observe({
    shared$y <- input$x_input^2
  })
  
  # Plot y = x^2
  output$plot_y <- renderPlot({
    x <- input$x_input
    y <- x^2
    plot(x, y, pch = 16, col = "blue",
         main = "y = x²",
         xlab = "x", ylab = "y")
  })
  
  # Plot z = log(y)
  output$plot_z <- renderPlot({
    req(shared$y)  # Make sure y is available
    y <- shared$y
    z <- log(y)
    plot(y, z, pch = 16, col = "green",
         main = "z = log(y)",
         xlab = "y", ylab = "z")
  })
  
  output$y_value <- renderText({
    req(shared$y)
    paste("Current value of y:", round(shared$y, 2))
  })
  
  # Render x and y in a table
  output$xy_table <- renderTable({
    req(shared$y)
    data.frame(
      x = input$x_input,
      y = shared$y
    )
  })
}

shinyApp(ui = ui, server = server)
