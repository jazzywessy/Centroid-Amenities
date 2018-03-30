library(leaflet)
library(shinythemes)
vars <- c(
  "Is SuperZIP?" = "superzip",
  "Centile score" = "centile",
  "College education" = "college",
  "Median income" = "income",
  "Population" = "adultpop"
)

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
        width = 350, height = "auto",

        h2("Centroid-Amenities Explorer"),
        
        selectInput("selectSubzone", "Select Subzone:",  choices = list_subzone$SUBZONE_N , selected = ""),
        sidebarPanel(id = "slidercontrols", class = "panel panel-default", fixed = TRUE,
                      draggable = FALSE, bottom = "auto",
                      width = 300, height = "auto", sliderInput("sliderBuffer",
                    "Distance to buffer (m):",
                    min = 0,
                    max = 500,
                    value = 50,
                    step = 25)),
        #checkboxGroupInput("checkLayers", "Toggle layers to be displayed:",c("Childcare Centres","Eldercare Centres"),""),
        conditionalPanel( condition = "output.subzoneCheck",
                          #checkboxGroupInput("checkStats", "Toggle statistics to be displayed:",c("K Means","L Function"),""),
                          plotOutput("SOS", height = 225),
                          plotOutput("K_Means", height = 225),
                          plotOutput("L_Function", height = 225)
                          )
        
        #selectInput("color", "Color", vars),
        #selectInput("size", "Size", vars, selected = "adultpop"),
        #conditionalPanel("input.color == 'superzip' || input.size == 'superzip'",
          # Only prompt for threshold when coloring or sizing by superzip
        #  numericInput("threshold", "SuperZIP threshold (top n percentile)", 5)
        #),
        
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
