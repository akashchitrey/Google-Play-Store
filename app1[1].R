library(shiny)
library(DT)
library(plotly)
library(crosstalk)
library(ggplot2)
library(stringi)
library(shinyWidgets)
library(shinydashboard)
library(shinythemes)

playstore <- read.csv(file.choose())

m <-  playstore %>% 
  tibble::rownames_to_column()

nms <- names(playstore)

ui <- fluidPage(
  theme = shinytheme("united"),
  setBackgroundColor(color = "slategrey", gradient = "linear"),
  
  
  headerPanel("Google App Store"),
  sidebarPanel(
    sliderInput('sampleSize', 'Sample Size', min = 1, max = nrow(playstore),
                value = 1000, step = 500, round = 0),
    selectInput('x', 'X', choices = nms, selected = "App"),
    selectInput('y', 'Y', choices = nms, selected = "Rating"),
    selectInput('color', 'Color', choices = nms, selected = "Category"),
    
    selectInput('facet_row', 'Facet Row', c(None = '.', nms)),
    selectInput('facet_col', 'Facet Column', c(None = '.', nms), selected = "Type"),
    sliderInput('plotHeight', 'Height of plot (in pixels)', 
                min = 100, max = 2000, value = 1000)
  ),
  
  mainPanel(
    tabsetPanel(type = "tabs",
                tabPanel(title = "Overview", 
                         plotlyOutput(outputId = 'trendPlot', height = "900px")),
                tabPanel(title = "Datatable", 
                         plotlyOutput(outputId = "x2"),
                         br(),
                         DT::dataTableOutput(outputId = "x1"))
                
                
    )
  ))

server <- function(input, output) {
  
  d <- SharedData$new(m, ~rowname)
  dataset <- reactive({
    playstore[sample(nrow(playstore), input$sampleSize),]
  })
  #Output of overview page  
  output$trendPlot <- renderPlotly({
    
    
    p <- ggplot(dataset(), aes_string(x = input$x, y = input$y, color = input$color)) + 
      geom_point()
    
    
    facets <- paste(input$facet_row, '~', input$facet_col)
    if (facets != '. ~ .') p <- p + facet_grid(facets)
    
    ggplotly(p) %>% 
      layout(height = input$plotHeight, autosize=TRUE)
  })
  # highlight selected rows in the scatterplot of plotly and DT
  output$x2 <- renderPlotly({
    
    s <- input$x1_rows_selected
    
    if (!length(s)) {
      p <- d %>%
        plot_ly(x = ~Category, y = ~Rating, mode = "markers", color = I('black'), name = 'Unfiltered') %>%
        layout(showlegend = T) %>% 
        highlight("plotly_selected", color = I('red'), selected = attrs_selected(name = 'Filtered'))
    } else if (length(s)) {
      pp <- m %>%
        plot_ly() %>% 
        add_trace(x = ~Category, y = ~Rating, mode = "markers", color = I('black'), name = 'Unfiltered') %>%
        layout(showlegend = T)
      
      # selected data
      pp <- add_trace(pp, data = m[s, , drop = F], x = ~Category, y = ~Rating, mode = "markers",
                      color = I('red'), name = 'Filtered')
    }
    
  })
  
  # highlight selected rows in the table
  output$x1 <- DT::renderDataTable({
    m2 <- m[d$selection(),]
    dt <- DT::datatable(m)
    if (NROW(m2) == 0) {
      dt
    } else {
      DT::formatStyle(dt, "rowname", target = "row",
                      color = DT::styleEqual(m2$rowname, rep("white", length(m2$rowname))),
                      backgroundColor = DT::styleEqual(m2$rowname, rep("black", length(m2$rowname))))
    }
  })
  
  # download the filtered data
  output$x3 = downloadHandler(playstore, content = function(file) {
    s <- input$x1_rows_selected
    if (length(s)) {
      write.csv(m[s, , drop = FALSE], file)
    } else if (!length(s)) {
      write.csv(m[d$selection(),], file)
    }
  })
  
}

shinyApp(ui, server)
