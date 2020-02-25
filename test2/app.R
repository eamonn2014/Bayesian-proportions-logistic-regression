#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
    titlePanel("Ja-Anteil von Abstimmungen"),
    
    sidebarLayout(
        sidebarPanel(
            helpText("Create maps with information from ballot outcomes."),
            ui <- fluidPage(theme = shinytheme("journal"), #ht
                            selectInput("var", 
                                        label = "Choose a variable to display",
                                        choices = c("Epidemiegesetz",
                                                    "BG",
                                                    "1:12",
                                                    "Familien",
                                                    "Nationalstrassenabgabegesetz"),
                                        selected = "Epidemiegesetz"),
                            
                            sliderInput("range", 
                                        label = "Range of interest:",
                                        min = 0, max = 100, value = c(0, 100))
                            
            ),
            
            mainPanel(plotOutput("map"))
        )
    ))
    
)

server <- shinyServer(function(input, output   ) {
     
        output$map <- renderPlot({
            data <- switch(input$var, 
                           "Epidemiegesetz" = "Epidemiegesetz",
                           "BG" = "BG",
                           "1:12" = "Loehne",
                           "Familien" = "Familien",
                           "Nationalstrassenabgabegesetz" = "Nationalstrassenabgabegesetz")
            
            color <- switch(input$var, 
                            "Epidemiegesetz" = "darkgreen",
                            "BG" = "red",
                            "1:12" = "darkorange",
                            "Familien" = "darkviolet",
                            "Nationalstrassenabgabegesetz" = "darkblue")
            
            legend <- switch(input$var,
                             "Epidemiegesetz" = "Epidemiegesetz",
                             "BG" = "BG",
                             "1:12" = "Sozis",
                             "Familien" = "Familien",
                             "Nationalstrassenabgabegesetz" = "blablabla")
            
            percent_map(var = data, color = color, max = input$range[2], min = input$range[1], legend = legend)
        })
    }
)

percent_map <- function(var, color, legend, min = 0, max = 100) {
    
    # constrain gradient to percents that occur between min and max
    abst$tmp_var <- abst[[var]]
    abst$tmp_var <- pmax(abst$tmp_var, min)
    abst$tmp_var <- pmin(abst$tmp_var, max)
    
    #plot
    aha <- ggplot(abst, aes(long, lat, group=group))+
        geom_polygon(aes(fill = tmp_var))+
        coord_fixed()+
        scale_fill_gradient(low = "lightskyblue", high = color, 
                            space = "Lab", na.value = "lightblue")+
        labs(title=var, x="", y="")+
        theme(axis.text=element_blank(),
              axis.ticks=element_blank(),
              panel.grid.minor = element_blank(),
              panel.background = element_blank()
        )
    print(aha)
    abst$tmp_var <- NULL
}
# Run the application 
shinyApp(ui = ui, server = server)
