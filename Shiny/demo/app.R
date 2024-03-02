pacman::p_load(shiny, sf, tidyverse, bslib, tmap)

hunan <- st_read(dsn = "data/geospatial",
                 layer = "Hunan")
data <- read_csv("data/aspatial/Hunan_2012.csv")
hunan_data <- left_join(hunan, data,
                        by = c("County"= "COUNTY"))

ui <- fluidPage(
  titlePanel("Economic Development Indicators Choropleth Mapping of Hunan, China"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput(inputId="variable",
                  label= "Development Indicator",
                  choices= list("Gross Domestic Product, GDP"= "GDP",
                                "Gross Domestic Product Per Capita" = "GDPPC",
                                "Gross Industry Output" = "GIO",
                                "Output Value of Agriculture" = "OVA",
                                "Output Value of Service" = "OVS"),
                  selected = "GDPPC"),
      sliderInput(inputId ="classes",
                 label = "Number of Classes",
                 min = 5,
                 max = 15,
                 value = c(10))
    ),
    mainPanel(plotOutput("mapPlot",
                         width = "100%",
                         height = 700)
              )
  )
)

server <- function(input, output) {
  output$mapPlot <- renderPlot({
    tmap_options(check.and.fix = TRUE) +
      tm_shape(hunan_data)+
      tm_fill(input$variable,
              n = input$classes,
              style = "quantile",
              palette = "plasma") +
      tm_borders(lwd= 0.3, alpha=1)+
      tm_layout(
        legend.outside=TRUE,
        legend.title.size = 1.7,
        legend.text.size = 1.2,
        frame=FALSE)
  })
}

shinyApp(ui = ui, server = server)