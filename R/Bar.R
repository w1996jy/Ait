# Bar_ui ------------------------------------------------------------------

Bar_ui <- function(id) {
  ns <- NS(id)
  tabPanel(
    title = "Bar",
    sidebarLayout(
      sidebarPanel(
        id = ns("Sidebar"),
        # 只有一个 Run 按钮
        actionButton(ns("run_button"), "Run")
      ),
      mainPanel(
        plotOutput(ns("plot_output"))  # Output area for the plot
      )
    )
  )
}

# Bar_server --------------------------------------------------------------

Bar_server <- function(input, output, session) {
  # Reactive value to store the plot object
  plot_data <- reactiveVal(NULL)
  
  # Observe the Run button click event
  observeEvent(input$run_button, {
    # Generate the plot when the button is clicked
    new_plot <- ggplot(mtcars, aes(x = hp, y = mpg)) +
      geom_point() +
      labs(title = "马力与每加仑英里数的关系",
           x = "马力 (hp)",
           y = "每加仑英里数 (mpg)") +
      theme_minimal()
    
    # Update the reactive value with the new plot
    plot_data(new_plot)
  })
  
  # Render the plot based on the reactive value
  output$plot_output <- renderPlot({
    plot_data()
  })
}
