
citation <- "National Centre for Coasts and Climate. In prep. Identifying opportunities for nature-based coastal defence using a shoreline suitability analysis."

ui <- bootstrapPage(
  navbarPage(theme = shinytheme("flatly"), collapsible = TRUE,
             HTML('<a style="text-decoration:none;cursor:default;color:#FFFFFF;" class="active" href="#">Nature-Based Coastal Defence</a>'),
             id="nav",
             #windowTitle = "NBCD",
             
             tabPanel("NBCD Victoria",
                          leafletOutput("leaflet_map", width="auto", height=950),
                          
                     
                          absolutePanel(id = "controls", class = "panel panel-default",
                                        top = 170, left = 25, width = 400, fixed=TRUE,
                                        draggable = TRUE, height = "auto", style = "opacity: 0.8; padding: 20px 20px 20px 20px",
                                        
                                        
                                        h3("Nature-based Coastal Defence"),
                                        h4("Identifying opportunities for nature-based coastal defence using a shoreline suitability analysis for Victoria, Australia"),
                                        br(),
                                        p("some inputs or other info here"),
                                      
                                        selectInput(inputId = "taxa",
                                                    label = "Taxa:",
                                                    choices = c("All", "Dune", "Mangrove", "Saltmarsh", "Seagrass", "Shellfish")),
                                        
                                        checkboxGroupInput(inputId = "suitability", label = "Filter the areas by suitability:",
                                                           choices = c("High", "Medium", "Low"),
                                                           selected = c("High", "Medium", "Low")),
                                        checkboxGroupInput(inputId = "priority", label = "Filter the areas by priority:",
                                                           choices = c("High", "Medium", "Low"),
                                                           selected = c("High", "Medium", "Low")),
                                        
                                        br(),
                                        p("e.g. only showing the highest priority/erosion areas"),
                                        br(),
                                        span(tags$i(h6("This map provides a guide only. Areas should be investigated on-ground prior to deciding on a management option.")), style="color:#045a8d"),
                                        br(),
                                        p(paste("How to cite:", citation))
                                       
                          )
                      
             ),
             tabPanel("Case studies", icon = icon("search"),
                      # tabs on the main panel
                      tabsetPanel(id = "tab_plot",
                                  
                                  tabPanel(h4("Case 1"),
                                           h3("Case study 1: Torquay"),
                                           p("Reason and description of case study"),
                                           p("Paragraph descriptions"),
                                           br(),
                                           br(),
                                           p("Images of the area"),
                                           p("Paragraph descriptions"),
                                           br(),
                                           br(),
                                           leafletOutput("leaflet_case1", height = 500),
                                           p(paste("How to cite:", citation))
                                  ),
                                  
                                  tabPanel(h4("Case 2"),
                                           h3("Case study 2: Corner Inlet"),
                                           p("Reason and description of case study"),
                                           p("Paragraph descriptions"),
                                           br(),
                                           br(),
                                           p("Images of the area"),
                                           p("Paragraph descriptions"),
                                           br(),
                                           br(),
                                           leafletOutput("leaflet_case2", height = 500),
                                           p(paste("How to cite:", citation))
                                  ),
                                  
                                  tabPanel(h4("Case 3"),
                                           h3("Case study 3: Lakes Entrance"),
                                           p("Reason and description of case study"),
                                           p("Paragraph descriptions"),
                                           br(),
                                           br(),
                                           p("Images of the area"),
                                           p("Paragraph descriptions"),
                                           br(),
                                           br(),
                                           leafletOutput("leaflet_case3", height = 500),
                                           p(paste("How to cite:", citation))
                                  ),
                                  
                                  tabPanel(h4("Case 4"),
                                           h4("Case study location 4"),
                                           p("Reason and description of case study"),
                                           p("Paragraph descriptions"),
                                           br(),
                                           br(),
                                           p("Images of the area"),
                                           p("Paragraph descriptions"),
                                           br(),
                                           br(),
                                           leafletOutput("leaflet_case4", height = 500),
                                           p(paste("How to cite:", citation))
                                  )
                                  
                      )),
             
             navbarMenu(("More information"), icon = icon("info"), # change icon
                        tabPanel(("About Nature-based Coastal Defence"),
                                 h4("Issue of coastal erosion"),
                                 p("Text"),
                                 br(),
                                 br(),
                                 h4("Opportunity for nature-based solutions"),
                                 p("Text"),
                                 br(),
                                 br(),
                                 h4("Examples"),
                                 p("Text"),
                                 br(),
                                 br()
                                 ), 
                        
                        tabPanel(("Methods"),
                                 h4("Species investigated"),
                                 p("Why?"),
                                 br(),
                                 br(),
                                 h4("SDMs"),
                                 p("Data sources, model outputs "),
                                 br(),
                                 br(),
                                 h4("Prioritisation"),
                                 p("Text"),
                                 br(),
                                 br()
                                 ),
                        
                        tabPanel(("Contact us"),
                                 h4("Contact details"),
                                 p("E.g. Ask the uni to set up an email for this app"),
                                 p("Or put Becki's"),
                                 br(),
                                 br(),
                                 h4("Sponsors and funding"),
                                 p("Who funded the project?"),
                                 p("Images of their logos"),
                                 br(),
                                 br(),
                                 )), 
             
             
             ## tab, Share  ----------------------------------------------------------------
             navbarMenu(("Share"), icon = icon("share-alt"), # consider adding a hashtag
                        
                        ## Twitter  ----------------------------------------------------------------
                        tabPanel(tags$a(href = 'https://twitter.com/intent/tweet?url=http%3A%2F%2Fnccc.edu.au &text=Nature%20Based%20Coastal%20Defence%20opportunities%20in%20Victoria', icon("twitter"), "Twitter" )),
                        
                        ## LinkedIn  ----------------------------------------------------------------
                        tabPanel(tags$a(href = 'http://www.linkedin.com/shareArticle?mini=true&url=http%3A%2F%2Fnccc.edu.au &title=Nature%20Based%20Coastal%20Defence%20opportunities%20in%20Victoria', icon("linkedin"), "LinkedIn" )),
                        
                        ## Facebook  ----------------------------------------------------------------
                        tabPanel(tags$a(href = 'https://www.facebook.com/sharer/sharer.php?u=http%3A%2F%2nccc.edu.au', icon("facebook"), "Facebook" ))
                        
                        
             ) # close navbar2 
  ) # close navbar1
)
  
  
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
