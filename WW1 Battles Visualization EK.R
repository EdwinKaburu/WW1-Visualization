
# WW1 Battles Visualization
# Developed by Edwin Kaburu
# Github : https://github.com/EdwinKaburu/


# Uncomment Below - Make sure to install the following packages or refer to their respective documentations

# install.packages("shiny")

# https://shiny.rstudio.com

# install.packages("leaflet")

# https://rstudio.github.io/leaflet/

#Clear out Varibles in Environment
rm(list = ls())


library(shiny)
library(ggplot2)
library(dplyr)
library(leaflet)


# Refer to your working directory (getwd())- file location might defer.
# Get the Data from Csv file
data1 = read.csv("WorldWarOneCleaned.csv",TRUE,",")

#UI Component
ui <- navbarPage("WWI VISUALIZATION",
                 tabPanel("WW1 MAP BY YEAR", titlePanel("Yearly Battles Changes"),
                          sidebarLayout(
                            sidebarPanel(
                              sliderInput(inputId = "slider", label = "years", min = 1914, max = 1918, value = 1914)
                            ),
                            mainPanel(
                              leafletOutput("yearly_Map",height = 800)
                            )
                          )
                 ), 
                 tabPanel("WW1 BATTLES BY COUNTRY",
                          sidebarLayout(
                            sidebarPanel(
                              selectInput("Country","Countries Battles in:", choices = unique(data1$Country)),
                              hr(),
                              helpText("Data from Edwin's WW1 Sheet.")
                            ),
                            mainPanel(
                              plotOutput("battles_Plot")
                            )
                          )
                 ),
                 tabPanel("WW1 OVERVIEW MAP",  titlePanel("WW1 Map: 1914 to 1918"),
                          leafletOutput("overview_leaf",height=800)
                 )
)

#Backend process
server <- function(input, output) {
  
  output$yearly_Map <- renderLeaflet({
    leaflet(data1) %>% 
      setView(lat = data1[1,5], lng = data1[1,6], zoom = 3) %>% 
      addProviderTiles("CartoDB.Positron")
  }) 
  
  observe({
    
    ## filter data
    df_filtered <- reactive({
      data1[data1$Year == input$slider, ]
    })
    
    
    leafletProxy(mapId = "yearly_Map", data = df_filtered()) %>%
      clearMarkers() %>% 
      addMarkers()
  })
  
  output$battles_Plot <- renderPlot({
    
    df <- data1 %>%
      filter(data1$Country == input$Country ) %>%
      group_by(Year) %>%
      summarise(total_battles = n())
    
    ggplot(df, aes(x = Year, y = total_battles)) + scale_color_gradient(low="green", high="red") +
      geom_point(aes(color = total_battles),size = 4 , stat = "identity" ) + xlab('Year') + ylab('# of Battles')+
      ggtitle('Yearly # of Battles') +
      theme(plot.title = element_text(size = 22,hjust=0.5),axis.title.x = element_text(size = 18),axis.title.y = element_text(size =18 , angle = 360, vjust = 0.5), axis.text = element_text(size = 15))
  })
  
  output$overview_leaf <- renderLeaflet({
    leaflet(data1) %>%
      setView(lat = data1[1,5], lng = data1[1,6], zoom = 10) %>%
      addProviderTiles("CartoDB.Positron") %>%
      addMarkers(lng = data1[,6], lat = data1[,5], popup = data1[,1])
  })
  
}
shinyApp(ui, server)



