library(shiny)
library(tidyverse)
library(gganimate)
library(ggpubr)
library(jpeg)
library(grid)
library(RCurl)
library(imager)


player2019 = read_csv("2020DataFiles\\2020DataFiles\\2020-Mens-Data/MPlayers.csv")
Season19 = read_csv("2020DataFiles\\2020DataFiles\\2020-Mens-Data\\MEvents2019.csv")


court <- rasterGrob(readJPEG("court.jpeg"),
                    width=unit(1,"npc"), height=unit(1,"npc"))

findID = function(First_Name, Last_Name) {
    player = player2019%>%
        filter(FirstName == First_Name & LastName == Last_Name)
    return(player[["PlayerID"]])
}
ui = fluidPage(
    
    titlePanel("NCAA 2018-19 Stat"),
    sidebarLayout(
        sidebarPanel(
            textInput("FirstName", h3("First Name"), 
                  value = "Zion"),
            textInput("LastName", h3("Last Name"), 
                  value = "Williamson"), width = 2),
            
            mainPanel(
                tabsetPanel(
                    tabPanel("Court", plotOutput("map")),
                    tabPanel("Stat", tableOutput("stat"))
            )))
)

server <- function(input, output) {
    output$map <- renderPlot({
        Season19 %>%
            filter(Area != 0 & (EventType == "made1" | EventType == "miss1" | EventType == "made2" | EventType == "miss2" | EventType == "made3" | EventType == "miss3")) %>%
            filter(EventPlayerID == findID(input$FirstName, input$LastName)) %>%
            mutate(X = ifelse(X > 50, 100-X, X)) %>%
            ggplot(aes(X, Y, color = EventType, fill = EventType)) +
            annotation_custom(court, xmin=-0, xmax=50, ymin=-0, ymax=100) + 
            xlim(0, 50)+
            ylim(0,100)+
            geom_point(size = 4) +
            theme_void() +
            theme(legend.title = element_blank())
        
    })
    
    output$stat <- renderTable({
        Season19 %>%
            filter(EventPlayerID == findID(input$FirstName, input$LastName)) %>%
            select(EventPlayerID:Area) %>%
            group_by(EventType) %>%
            summarise(n = n()) %>%
            spread(EventType, n) %>%
            mutate(Points = made3*3+made2*2+made1) %>%
            select(-foul, -fouled)
    })
}


shinyApp(ui = ui, server = server)
