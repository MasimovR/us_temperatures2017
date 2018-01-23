#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinythemes)

# Define UI for application that draws a histogram
ui <- fluidPage(theme = shinytheme("sandstone"),
  
  tags$style("body {background-color: grey;}"),
   # Application title
   HTML('<div class="panel panel-default">
  <div class="panel-heading"><font size="6"><b>US daily temperatures in 2017</b></font></div></div>'),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
        sliderInput("slider", "Time", min = as.Date("2017-01-01"),max =as.Date("2017-12-31"),value=as.Date("2017-01-01"),timeFormat="%d %b %Y"),
        textOutput("SliderText"),
        HTML("Data: &nbsp; <a href='http://www.noaa.gov'><img src='NOAA_emblem.png' height='40' width='190'></a>")
        
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
        tabsetPanel(
          tabPanel("Counties", plotOutput("counties")),
          tabPanel("Voronoi Tessellations", plotOutput("voronoi"))
        )
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   output$counties <- renderPlot({
     require(ggplot2)
     data <- feather::read_feather(paste0(input$slider, "_county.feather"))
     ggplot(data, aes(x=long, y=lat, group=group,
                                 fill = TAVG)) + 
       geom_polygon() + 
       coord_fixed() + 
       guides(fill = guide_colorbar(title = expression(paste("Average temperature [",degree,"F]")), 
                                    title.position = "top",
                                    title.theme = element_text(size = 14, color = "white",face = "bold", family = "mono", angle = 0) )) +
       theme_void() + 
       scale_fill_gradientn(colours = rev(c("#a50026", "#d73027", "#f46d43", "#fdae61", "#fee090", "#ffffbf", "#e0f3f8", "#abd9e9", "#74add1", "#4575b4", "#313695")),
                            limits = c(-32, 132),
                            breaks = c(0, 50, 100)) +
       theme(
         panel.background = element_rect(fill = "transparent",colour = NA),
         plot.background = element_rect(fill = "transparent",colour = NA),
         legend.position = "bottom",
         plot.margin = unit(c(0,0,0,0), "cm"),
         legend.text = element_text(size = 12, family = "mono", color = "white"),
         legend.key = element_rect(fill = "black"),
         legend.key.height = unit(10, "pt"),
         legend.key.width = unit(45, "pt"),
         legend.box.margin = unit(c(0, 0, 0.1, 0), 'cm')) 
   }, bg="transparent", height = 500)
   output$voronoi <- renderPlot({
     require(ggplot2)
     data <- feather::read_feather(paste0(input$slider, "_voronoi.feather"))
     ggplot(data) + 
       geom_polygon(aes(x=long, y=lat, group=ID, fill = TAVG)) + 
       guides(fill = guide_colorbar(title = expression(paste("Average temperature [",degree,"F]")), 
                                    title.position = "top",
                                    title.theme = element_text(size = 14, color = "white",face = "bold", family = "mono", angle = 0) )) +
       
       theme_void() + 
       scale_fill_gradientn(colours = rev(c("#a50026", "#d73027", "#f46d43", "#fdae61", "#fee090", "#ffffbf", "#e0f3f8", "#abd9e9", "#74add1", "#4575b4", "#313695")),
                            limits = c(-32.81, 132.81),
                            breaks = c(0, 50, 100)) +
       theme(
         panel.background = element_rect(fill = "transparent",colour = NA),
         plot.background = element_rect(fill = "transparent",colour = NA),
         legend.position = "bottom",
         plot.margin = unit(c(0,0,0,0), "cm"),
         legend.text = element_text(size = 12, family = "mono", color = "white"),
         legend.key = element_rect(fill = "black"),
         legend.key.height = unit(10, "pt"),
         legend.key.width = unit(45, "pt"),
         legend.box.margin = unit(c(0, 0, 0.1, 0), 'cm')) +
       coord_fixed() 
       
   }, bg="transparent", height = 500)
  
}

# Run the application 
shinyApp(ui = ui, server = server)


