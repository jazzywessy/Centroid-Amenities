library(leaflet)
library(shinythemes)

navbarPage("Centroid-Amenities", id="nav",
  theme = shinythemes::shinytheme("united"),
  tabPanel("Interactive map",
    div(class="outer",

      tags$head(
        # Include our custom CSS
        includeCSS("styles.css"),
        includeScript("gomap.js")
      ),

      # If not using custom CSS, set height of leafletOutput to a number instead of percent
      leafletOutput("map", width="100%", height="100%"),

      # Shiny versions prior to 0.11 should use class = "modal" instead.
      absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
        draggable = TRUE, top = 60, left = "auto", right = 20, bottom = "auto",
        width = 600, height = "auto",
        fluidRow(
          column(12,
            h2("Centroid-Amenities Explorer")
          ),
          column(12,
            selectInput("selectAmenities", "Select Amenities:",  choices = c("Childcare","Eldercare") , selected = "Childcare")
          ),
          column(12,
            numericInput("supplyInput", "Amenity Capacity:", 
                              min = 10, max = 150,
                              value = 20)
          ),
          column(6,
            uiOutput("selectSubzoneOuput")
          ),
          column(6,
            numericInput("clusterInput", 
                         "Number of Suggested Amenities:", 
                         min = 3, max = 20,
                         value = 3)
          ),
          column(12,
            verbatimTextOutput("value")
          ),
          column(12,
            #checkboxGroupInput("checkLayers", "Toggle layers to be displayed:",c("Childcare Centres","Eldercare Centres"),""),
            conditionalPanel( condition = "output.subzoneCheck",
                              fluidRow(
                      #          column(6,
                      #            plotOutput("SOS", height = 225)
                      #          ),
                      #          column(6,
                      #            plotOutput("K_Means", height = 225)
                      #          ),
                                column(6, 
                                #       h4("Current Accessibility Index"),
                                  plotOutput("CurrentHist", height = 225)
                                ),
                                column(6,
                                #       h4("Improved Accessibility Index"),
                                  plotOutput("AfterHist", height = 225)
                                ),
                                column(6,
                                       h4("Current Avg. Distance (m)"),
                                  verbatimTextOutput("CurrentAvg")
                                ),
                                column(6,
                                       h4("Improved Avg. Distance (m)"),
                                  verbatimTextOutput("AfterAvg")
                                )
                              )
            )
          )
        )
      )

      #tags$div(id="cite",
      #  'Data compiled for ', tags$em('Coming Apart: The State of White America, 1960–2010'), ' by Charles Murray (Crown Forum, 2012).'
      #)
    )
  ),

  tabPanel("Data explorer",
    fluidRow(
      column(12,
        selectInput("selectTable", "Select Table:",  c("Aged 0 to 4 living in HDB by postal code" = "children_per_HDB",
                                                       "Child Care Services" = "childcare",
                                                       "Singapore Residents by Subzone and Type of Dwelling, June 2016" = "subzone_dwelling_type") , selected = "children_per_HDB")
      )
    ),
    hr(),
    DT::dataTableOutput("tableShow")
  ),

  conditionalPanel("false", icon("crosshair"))
)
