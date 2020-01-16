library(shiny)
library(ggplot2)
library(dplyr)

data <- mtcars
list_axis <- list('Miles per gallon'='mpg',
                  'Displacement'='disp',
                  'Gross horsepower'='hp',
                  'Rear axle ratio'='drat',
                  'Weight'='wt', 
                  '1/4 mile time'='qsec')
list_color <- list('Number of cylinders'='f_cyl',
                   'Engine(0=V-shaped,1=straight)'='f_vs',
                   'Transmission'='f_am',
                   'Number of forward gears'='f_gear',
                   'Number of carburators'='f_carb')

data$f_cyl <- factor(data$cyl)
data$f_vs <- factor(data$vs)
data$f_am <- factor(data$am)
data$f_gear <- factor(data$gear)
data$f_carb <- factor(data$carb)

ui <- fluidPage(
  sidebarLayout (
    sidebarPanel(
      textInput( inputId='plotName',
                label='Plot title:',
                value='',
                placeholder='Enter the title'),
      selectInput( inputId = 'x',
                   label = 'X-axis',
                   choices = list_axis,
                   selected = 'mpg'),
      selectInput( inputId = 'y',
                   label = 'Y-axis',
                   choices = list_axis,
                   selected = 'disp'),
      selectInput( inputId = 'color',
                   label = 'Colored by',
                   choices = list_color,
                   selected = 'cyl'),
      sliderInput( inputId = 'alpha',
                   label = 'Alpha',
                   min=0, max=1,
                   value=0.5),
      numericInput(inputId='size',
                   label='Dot size:',
                   value=3,
                   min=1, max=9),
      checkboxInput( inputId = 'show_d',
                     label = 'Show data?',
                     value = FALSE),
      checkboxInput( inputId = 'show_s',
                     label = 'Show summary?',
                     value = FALSE),
      submitButton('Apply')
    ),
    
    mainPanel(
      plotOutput(outputId = 'scatter'),
      fluidRow(
        column(width = 6, tableOutput(outputId = 'data_selected')),
        column(width = 6, tableOutput(outputId = 'summary'))
      )
      
      
    )
  )  
)

server <- function(input, output) {
  output$scatter <- renderPlot({
    req(input$size)
    
    ggplot(data, aes_string(x=input$x, y=input$y,color=input$color)) + 
      geom_point(alpha=input$alpha, size=input$size)  + 
      ggtitle(tools::toTitleCase(isolate(input$plotName))) +
      scale_colour_discrete() 
  })
  new_data <- reactive({
    data %>% select(input$x, input$y, input$color)
  })
  output$data_selected <- renderTable({
    if (input$show_d) {
      new_data()
    }
  })
  output$summary <- renderTable({
    if (input$show_s) {
      new_data() %>% 
        group_by_(input$color) %>% 
        summarise_all(funs(mean,sd))
    }
  })
}

shinyApp(ui=ui, server=server)