#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
         sliderInput("eps",
                     "epsilon exponent",
                     min = -6,
                     max = -1,
                     step=0.2,
                     
                     value = 0), width = 2
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
         plotlyOutput("atan"),
         plotlyOutput("bottleneck"), width = 6
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   
   output$atan <- renderPlotly({
      
      range = seq(-5, 5, by = 0.01)
      v_2   = atan(1.5 / sqrt(10 ^ input$eps))
      v_0   = atan(-0.5 / sqrt( 10 ^ input$eps))
      v     = atan((range - 0.5) / sqrt( 10 ^ input$eps))
      plot_ly(d = data.frame(v), x =~ range, y =~ v, type = "scatter", mode = "line") %>%
         add_markers(x = 0, y = v_0, marker = list(color = "#FF0000")) %>%
         add_markers(x = 2, y = v_2, marker = list(color = "#FF0000")) %>%
         layout(showlegend = FALSE)
   })
   output$bottleneck <- renderPlotly({
      d <- mandel_r(10^(input$eps))
      #length(d[[2]])
      #plot(y=d[[1]],x=d[[2]][1:3140])
      
      range = d[[2]]
      #range = sort(c(range,range))
      #range
      par <- range ^ 2 + 1/4 + 10^(input$eps)
      #length(par)
      #tail(par)
      plot_ly(data = data.frame(d), y =~ par, x =~ d[[2]], type = 'scatter', mode = 'lines') %>% 
         add_trace(y =~ range, line = list(color = "009900")) %>%
         add_trace(y =~ d[[1]], line = list(color = "#FF0000")) %>%
         layout(showlegend = FALSE)
      
      
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

