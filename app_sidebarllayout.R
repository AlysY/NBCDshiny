
library(shiny)
library(shinydashboard)
library(leaflet)


####################
## Colour palette ## -------------------------------------------------------------
####################

## Palette options
NBCD_palette <- list(
    
    ## For heat map
    `priority`  = c(
        `yellow` = "#FFF100",
        `orange`        = "#fc8105", 
        `red`      = "#C00004")
)
pal_num <- colorQuantile(NBCD_palette$priority, domain = 1:3, n=3)
# pie(rep(1, 3), col = pal_num(1:3))

####################
## User interface ## -------------------------------------------------------------
####################

# Define UI for application that draws a histogram
ui <- fluidPage(
    
    tags$head(tags$style(HTML("
                           .navbar-nav {float: none !important;}
                           .navbar {font-size: 15px;}
                           .navbar-nav > li:nth-child(4) {float: right;}
                           
                            "))),
    
    
    navbarPage(title ="", 
               theme = shinytheme("cerulean"), # cerulean (Light blue), flatyly (dark blue), readable (simple with light blue)
               
               # Tab 1 - Map, landing page -----------------------------------------------------------------------
               tabPanel("Map", icon = icon("map"),
                        titlePanel("Nature-Based Coastal Defence"),
                        h4("Identifying opportunities for nature-based coastal defence using a shoreline suitability analysis"),
                        sidebarLayout(
                            sidebarPanel(
                                p("some inputs here"),
                                selectInput(inputId = "Taxa",
                                            label = "Taxa:",
                                            choices = c("All", "Dune", "Mangrove", "Saltmarsh", "Seagrass", "Shellfish")),
                                br(),
                                p("e.g. only showing the highest priority/erosion areas")
                            ),
                            mainPanel(
                                leafletOutput("leaflet_map", height=700)
                                )
                            )
                        ),
               tabPanel("Case studies", icon = icon("search"),
                        # tabs on the main panel
                        tabsetPanel(id = "tab_plot",
                                    
                                    tabPanel(h4("Case 1"),
                                             h4("Case study location 1"),
                                             p("Reason and description of case study"),
                                             
                                             leafletOutput("leaflet_case1")
                                    ),
                                    
                                    tabPanel(h4("Case 2"),
                                             h4("Case study location 2"),
                                             p("Reason and description of case study"),
                                             leafletOutput("leaflet_case2")
                                    ),
                                    
                                    tabPanel(h4("Case 3"),
                                             h4("Case study location 3"),
                                             p("Reason and description of case study"),
                                             leafletOutput("leaflet_case3")
                                    ),
                                    
                                    tabPanel(h4("Case 4"),
                                             h4("Case study location 4"),
                                             p("Reason and description of case study"),
                                             leafletOutput("leaflet_case4")
                                    )
                                   
                        )),
               
               navbarMenu(("More information"), icon = icon("info"), # change icon
                          tabPanel(("About Nature-based Coastal Defence")), 
                          tabPanel(("Methods")),
                          tabPanel(("Contact us"))), 
               
               
               ## tab, Share  ----------------------------------------------------------------
               navbarMenu(("Share"), icon = icon("share-alt"), # consider adding a hashtag
                          
                          ## Twitter  ----------------------------------------------------------------
                          tabPanel(tags$a(href = 'https://twitter.com/intent/tweet?url=http%3A%2F%2Famrhotspots.com.au%2Faci%2Fadminpanel%2Fmanage%2Flogin &text=Check%20out%20this%20tool%20to%20track%20antibiotic%20resistance', icon("twitter"), "Twitter" )),
                          
                          ## LinkedIn  ----------------------------------------------------------------
                          tabPanel(tags$a(href = 'http://www.linkedin.com/shareArticle?mini=true&url=http%3A%2F%2Famrhotspots.com.au%2Faci%2Fadminpanel%2Fmanage%2Flogin &title=Check%20out%20this%20tool%20to%20track%20antibiotic%20resistance', icon("linkedin"), "LinkedIn" )),
                          
                          ## Facebook  ----------------------------------------------------------------
                          tabPanel(tags$a(href = 'https://www.facebook.com/sharer/sharer.php?u=http%3A%2F%2Famrhotspots.com.au%2Faci%2Fadminpanel%2Fmanage%2Flogin', icon("facebook"), "Facebook" ))
                          
                          
               ) # close navbar2 
    )  # close navbar
) # Close fluidpage


############
## Server ## -----------------------------------------------------------------------
############

server <- function(input,output, session){
    output$leaflet_map <- renderLeaflet({
        
        leaflet() %>% # create a leaflet map
            setView(lng = 144.946457, lat = -37.840935, zoom = 7) %>%
            addTiles(options = providerTileOptions(minZoom = 3))
        
    })
    
    output$leaflet_case1 <- renderLeaflet({
        
        leaflet() %>% # create a leaflet map
            setView(lng = 144.3167, lat = -38.3333, zoom = 11) %>%
            addTiles(options = providerTileOptions(minZoom = 2)) 
        
    })
    
    
    output$leaflet_case2 <- renderLeaflet({
        
        leaflet() %>% # create a leaflet map
            setView(lng = 146.3333, lat = -38.7833, zoom = 11) %>%
            addTiles(options = providerTileOptions(minZoom = 2))
        
    })
    
    
    output$leaflet_case3 <- renderLeaflet({
        
        leaflet() %>% # create a leaflet map
            setView(lng = 147.5, lat = -38, zoom = 10) %>%
            addTiles(options = providerTileOptions(minZoom = 2)) 
        
    })
    
    
    output$leaflet_case4 <- renderLeaflet({
        
        leaflet() %>% # create a leaflet map
            setView(lng = 149.7500, lat = -37.5500, zoom = 10) %>%
            addTiles(options = providerTileOptions(minZoom = 2))
        
    })
}

###############
## Shiny app ## -------------------------------------------------------------
###############
shinyApp(ui = ui, server = server)
