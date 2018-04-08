
library(rgdal)
library(maptools)
library(spatstat)
library(raster)
library(tidyverse)
library(sf)
library(leaflet)
library(dplyr)
library(rgeos)
library(ClusterR)
library(scales)
library(lattice)
library(flexclust)
library(SpatialAcc)

function(input, output, session) {
  ## Interactive Map ###########################################
  
  output$map <- renderLeaflet({
    leaflet() %>%
      addProviderTiles("MapBox", options = providerTileOptions(
        id = "mapbox.streets",
        accessToken = token),
        group = "Streets") %>%
      addProviderTiles("MapBox", options = providerTileOptions(
        id = "mapbox.light",
        accessToken = token),
        group = "Light") %>%
      addProviderTiles("MapBox", options = providerTileOptions(
        id = "mapbox.outdoors",
        accessToken = token),
        group = "Outdoors") %>%
      setView(lng = 103.8509, lat = 1.3800, zoom = 12) %>%
      addScaleBar(position = "bottomleft") %>%
      addLayersControl(position = "topleft",
        baseGroups = c("Streets", "Light", "Outdoors")
      )
  })
  
  output$selectSubzoneOuput <- renderUI({
    if (input$selectAmenities == "Childcare"){
      selectInput("selectSubzone", "Select Subzone:",  choices = list_subzone$SUBZONE_N , selected = "")
    } else if(input$selectAmenities == "Eldercare"){
      selectInput("selectSubzone", "Select Subzone:",  choices = eldercare_geo_unfiltered$SUBZONE_N , selected = "")
    }
  })
  
  observeEvent(c(input$selectSubzone,input$clusterInput), {
    
    output$subzoneCheck <- reactive({
      input$selectSubzone
    })
    outputOptions(output, "subzoneCheck", suspendWhenHidden = FALSE) 
    
    # Filter
      subzone_pl <- subzone_pl_unfiltered %>% filter(`SUBZONE_N` == input$selectSubzone)
      
    # Create icon
      icon_eldercare <- awesomeIcons(
        icon = 'fa-building-o',
        iconColor = 'black',
        library = 'fa',
        markerColor = 'beige'
      )
      
      icon_childcare <- awesomeIcons(
        icon = 'fa-graduation-cap',
        iconColor = 'black',
        library = 'fa',
        markerColor = 'beige'
      )
      
      icon_centriods_eldercare <- awesomeIcons(
        icon = 'fa-building-o',
        iconColor = 'black',
        library = 'fa',
        markerColor = 'red'
      )
      
      icon_centriods_childcare <- awesomeIcons(
        icon = 'fa-graduation-cap',
        iconColor = 'black',
        library = 'fa',
        markerColor = 'red'
      )
      
      icon_hdb <- awesomeIcons(
        icon = 'fa-home',
        iconColor = 'black',
        library = 'fa',
        markerColor = 'cadetblue'
      )
      
      icon_hdb_elder <- awesomeIcons(
        icon = 'fa-home',
        iconColor = 'black',
        library = 'fa',
        markerColor = 'lightblue'
      )
      
      if (input$selectAmenities == "Childcare"){
        
        # Filter
        subzone_hdb_postal_presch_clean <- subzone_hdb_postal_presch_clean_unfiltered %>% filter(`SUBZONE_N` == input$selectSubzone)
        childcare_geo <- childcare_geo_unfiltered %>% filter(`SUBZONE_N` == input$selectSubzone)
        childcare_geo$supply <- sample(10:150, nrow(childcare_geo))
        
        
        ## Heatmap ###########################################
        # Transform 
        geometryTest <- st_as_sf(subzone_hdb_postal_presch_clean, coords = c("lng", "lat"),crs = 4326)
        subzone_sp <-  sf:::as_Spatial(subzone_pl$geometry) 
        childcare_sp <-  sf:::as_Spatial(geometryTest$geometry) 
        
        childcare_sp <- as(childcare_sp, "SpatialPoints") 
        subzone_sp <- as(subzone_sp, "SpatialPolygons") 
        
        # ppp object 
        owin <- as(subzone_sp, "owin") 
        childcare_ppp <- as(childcare_sp, "ppp") 
        childcare_ppp$window <- owin 
        
        # Create raster 
        childcare_bw <- density(childcare_ppp, sigma=bw.diggle, edge=TRUE, kernel="gaussian", weights = geometryTest$Total_PreSch_HDB) 
        childcare_bw_raster <- raster(childcare_bw) 
        raster::projection(childcare_bw_raster) <- sp::CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0") 
        
        ## Hansen ###########################################
        # Extract 
        childcare_pl_geo <- childcare_geo %>% 
          extract(col = geometry, c('lng', 'lat'), '\\(([^,]+), ([^)]+)\\)')
        
        GR.C_Coords <-cbind(as.numeric(as.character(childcare_pl_geo$lng)),as.numeric(as.character(childcare_pl_geo$lat)))
        GR.H_Coords <-cbind(subzone_hdb_postal_presch_clean$lng, subzone_hdb_postal_presch_clean$lat)
        
        GR.d <- distance(GR.H_Coords, GR.C_Coords)
        GR.d100 <- GR.d / 100000
        
        # set limit to 50m no any further than
        GR.acc <- ac(subzone_hdb_postal_presch_clean$Total_PreSch_HDB,
                     childcare_pl_geo$supply,
                     GR.d100, d0 = 50,
                     power = 2, family = "Hansen")
        
        print(GR.acc)
        
        
        GR.acc1 <- data.frame(subzone_hdb_postal_presch_clean[,c(1,8,9)],
                              GR.acc)
        ## K Means ###########################################
        # CRS
        getCRS <- st_crs(childcare_geo)
        
        test <- subzone_hdb_postal_presch_clean[, c(8,9)]
        
        if(input$clusterInput > 20 || input$clusterInput < 3){
          
          output$value <- renderText("Number not in range, default 3 clusters will be used")
          km1 <- cclust(test, k=3, save.data=FALSE,weights = subzone_hdb_postal_presch_clean$Total_PreSch_HDB,method="hardcl")
          
        }else{
          
          km1 <- cclust(test, k=as.numeric(as.character(input$clusterInput)), save.data=FALSE,weights = subzone_hdb_postal_presch_clean$Total_PreSch_HDB,method="hardcl")
        }
        
        
        sayangPlot <- (km1@centers)
        sayang <- (km1@centers)
        
        sayang <- data.frame(sayang)
        
        sayang <- st_as_sf(sayang,
                           coords = c("lng", "lat"),
                           crs = 4326)
        
        ## Leaflet ###########################################
        
        output$map <- renderLeaflet({
          leaflet() %>%
            addProviderTiles("MapBox", options = providerTileOptions(
              id = "mapbox.streets",
              accessToken = token),
              group = "Streets") %>%
            addProviderTiles("MapBox", options = providerTileOptions(
              id = "mapbox.light",
              accessToken = token),
              group = "Light") %>%
            addProviderTiles("MapBox", options = providerTileOptions(
              id = "mapbox.outdoors",
              accessToken = token),
              group = "Outdoors") %>%
            setView(as.numeric(as.character(subzone_hdb_postal_presch_clean$lng[1])), as.numeric(as.character(subzone_hdb_postal_presch_clean$lat[1])), zoom = 16) %>%
            addAwesomeMarkers(data=subzone_hdb_postal_presch_clean, icon=icon_hdb,
                              popup = paste("Planning Area: ", subzone_hdb_postal_presch_clean$`PLN_AREA_N`, "<br>",
                                            "Subzone: ", subzone_hdb_postal_presch_clean$`SUBZONE_N`, "<br>",
                                            "Total Population in Subzone: ", subzone_hdb_postal_presch_clean$`TOTAL.x`, "<br>",
                                            "Total Population living in HDB in Subzone: ", subzone_hdb_postal_presch_clean$`HDB` , "<br>",
                                            "Total Population of aged 0-4 in Subzone: ", subzone_hdb_postal_presch_clean$`Preschool`, "<br>",
                                            "Total Population of aged 0-4 living in HDB in Subzone: ", subzone_hdb_postal_presch_clean$`PreSch_HDB` , "<br>",
                                            "Postal Code: ", subzone_hdb_postal_presch_clean$`POSTAL` , "<br>",
                                            "Pre-Sch living at ",subzone_hdb_postal_presch_clean$`POSTAL`," in 1 and 2 Rooms: ", subzone_hdb_postal_presch_clean$`1&2Room_PreSch_HDB`, "<br>",
                                            "Pre-Sch living at ",subzone_hdb_postal_presch_clean$`POSTAL`," in 3 Rooms: ", subzone_hdb_postal_presch_clean$`3Room_PreSch_HDB`, "<br>",
                                            "Pre-Sch living at ",subzone_hdb_postal_presch_clean$`POSTAL`," in 4 Rooms: ", subzone_hdb_postal_presch_clean$`4Room_PreSch_HDB`, "<br>",
                                            "Pre-Sch living at ",subzone_hdb_postal_presch_clean$`POSTAL`," in 5 Rooms and Executive: ", subzone_hdb_postal_presch_clean$`5Room_PreSch_HDB`),
                              group = "HDB Kids") %>%
            addAwesomeMarkers(data=childcare_geo$geometry, icon=icon_childcare,
                              popup = paste("Planning Area: ", childcare_geo$`PLN_AREA_N`, "<br>",
                                            "Subzone: ", childcare_geo$`SUBZONE_N`, "<br>",
                                            "Childcare Centre Name: ", childcare_geo$`NAME`, "<br>"),
                              group = "Child Care Centres") %>%
            addPolygons(data=subzone_pl$geometry, weight = 3, fillColor = "brown",popup = paste("Planning Area: ", subzone_pl$`PLN_AREA_N`, "<br>",
                                                                                                "Subzone: ", subzone_pl$`SUBZONE_N`),
                        group = "Subzone") %>%
            addAwesomeMarkers(data=sayang, icon=icon_centriods_childcare,
                              group = "Centriod") %>%
            addRasterImage(x = childcare_bw_raster, opacity = 0.5, project = FALSE, group = "Heatmap") %>%
            addCircles(data = GR.acc1, lng= ~lng, lat= ~lat, weight= 1,
                       radius = ~sqrt(GR.acc) * 10,
                       popup = ~POSTAL, group = "Hansen") %>%
            addScaleBar(position = "bottomleft") %>%
            addLayersControl(position = "topleft",
                             baseGroups = c("Streets", "Light", "Outdoors"),
                             overlayGroups = c("HDB Kids", "Child Care Centres","Subzone","Centriod","Heatmap","Hansen"),
                             options = layersControlOptions(collapsed = FALSE)
            )
        })
        
        
        output$SOS <- renderPlot({
          
          # Cluster Points
          plot(test, col =(km1@cluster +1) , main = paste(input$selectSubzone, " Cluster Points"), pch=3, cex=2)
          
        })
        
        output$K_Means <- renderPlot({
          
          #Centriods
          plot(sayangPlot, col = palette() , main = paste(input$selectSubzone, " Cluster Centriods"), pch=3, cex=2)
          
        })
        
      } else if (input$selectAmenities == "Eldercare"){
        
        
        # Filter
        subzone_hdb_postal_elder_clean <- subzone_hdb_postal_elder_clean_unfiltered %>% filter(`SUBZONE_N` == input$selectSubzone)
        eldercare_geo <- eldercare_geo_unfiltered %>% filter(`SUBZONE_N` == input$selectSubzone)
        
        test2 <- subzone_hdb_postal_elder_clean[, c(8,9)]
        
        if(input$clusterInput > 20 || input$clusterInput < 3){
          
          output$value <- renderText("Number not in range, default 3 clusters will be used")
          km2 <- cclust(test2, k=3, save.data=FALSE,weights = subzone_hdb_postal_elder_clean$Total_Elder_HDB,method="hardcl")
          
        }else{
          
          km2 <- cclust(test2, k=as.numeric(as.character(input$clusterInput)), save.data=FALSE,weights = subzone_hdb_postal_elder_clean$Total_Elder_HDB,method="hardcl")
        }
        
        sayangPlot2 <- (km2@centers)
        sayang2 <- (km2@centers)
        
        sayang2 <- data.frame(sayang2)
        
        sayang2 <- st_as_sf(sayang2,
                            coords = c("lng", "lat"),
                            crs = 4326)
        
        ## Leaflet ###########################################
        
        output$map <- renderLeaflet({
          leaflet() %>%
            addProviderTiles("MapBox", options = providerTileOptions(
              id = "mapbox.streets",
              accessToken = token),
              group = "Streets") %>%
            addProviderTiles("MapBox", options = providerTileOptions(
              id = "mapbox.light",
              accessToken = token),
              group = "Light") %>%
            addProviderTiles("MapBox", options = providerTileOptions(
              id = "mapbox.outdoors",
              accessToken = token),
              group = "Outdoors") %>%
            setView(as.numeric(as.character(subzone_hdb_postal_elder_clean$lng[1])), as.numeric(as.character(subzone_hdb_postal_elder_clean$lat[1])), zoom = 16) %>%
            addAwesomeMarkers(data=subzone_hdb_postal_elder_clean , icon=icon_hdb_elder,
                              popup = paste("Planning Area: ", subzone_hdb_postal_elder_clean$`PLN_AREA_N`, "<br>",
                                            "Subzone: ", subzone_hdb_postal_elder_clean$`SUBZONE_N`, "<br>",
                                            "Total Population in Subzone: ", subzone_hdb_postal_elder_clean$`TOTAL.x`, "<br>",
                                            "Total Population living in HDB in Subzone: ", subzone_hdb_postal_elder_clean$`HDB` , "<br>",
                                            "Total Population of Elderly in Subzone: ", subzone_hdb_postal_elder_clean$`Elder`, "<br>",
                                            "Total Population of Elderly living in HDB in Subzone: ", subzone_hdb_postal_elder_clean$`Elder_HDB` , "<br>",
                                            "Postal Code: ", subzone_hdb_postal_elder_clean$`POSTAL` , "<br>",
                                            "Elderly living at ",subzone_hdb_postal_elder_clean$`POSTAL`," in 1 and 2 Rooms: ", subzone_hdb_postal_elder_clean$`1&2Room_Elder_HDB`, "<br>",
                                            "Elderly living at ",subzone_hdb_postal_elder_clean$`POSTAL`," in 3 Rooms: ", subzone_hdb_postal_elder_clean$`3Room_Elder_HDB`, "<br>",
                                            "Elderly living at ",subzone_hdb_postal_elder_clean$`POSTAL`," in 4 Rooms: ", subzone_hdb_postal_elder_clean$`4Room_Elder_HDB`, "<br>",
                                            "Elderly living at ",subzone_hdb_postal_elder_clean$`POSTAL`," in 5 Rooms and Executive: ", subzone_hdb_postal_elder_clean$`5Room_Elder_HDB`),
                              group = "HDB Elderly") %>%
            addAwesomeMarkers(data=eldercare_geo$geometry, icon=icon_eldercare,
                              popup = paste("Planning Area: ", eldercare_geo$`PLN_AREA_N`, "<br>",
                                            "Subzone: ", eldercare_geo$`SUBZONE_N`, "<br>",
                                            "Eldercare Centre Name: ", eldercare_geo$`NAME`, "<br>"),
                              group = "Elder Care Centres") %>%
            addPolygons(data=subzone_pl$geometry, weight = 3, fillColor = "brown",popup = paste("Planning Area: ", subzone_pl$`PLN_AREA_N`, "<br>",
                                                                                                "Subzone: ", subzone_pl$`SUBZONE_N`),
                        group = "Subzone") %>%
            addAwesomeMarkers(data=sayang2, icon=icon_centriods_eldercare,
                              group = "Centriod") %>%
            addHeatmap(data= subzone_hdb_postal_elder_clean, lng = ~lng, lat = ~lat, intensity = ~Total_Elder_HDB_Scale,
                       blur = 20, radius = 15, group = "Heatmap") %>%
            addScaleBar(position = "bottomleft") %>%
            addLayersControl(position = "topleft",
                             baseGroups = c("Streets", "Light", "Outdoors"),
                             overlayGroups = c("HDB Elderly","Elder Care Centres","Subzone","Centriod","Heatmap"),
                             options = layersControlOptions(collapsed = FALSE)
            )
        })
        
        output$SOS <- renderPlot({
          
          # Cluster Points
          plot(test2, col =(km2@cluster +1) , main = paste(input$selectSubzone, " Cluster Points"), pch=3, cex=2)
          
        })
        
        output$K_Means <- renderPlot({
          
          #Centriods
          plot(sayangPlot2, col = palette() , main = paste(input$selectSubzone, " Cluster Centriods"), pch=3, cex=2)
          
        })
        
      }
      
      
  }, ignoreInit = TRUE, ignoreNULL = TRUE)


  ## Data Explorer ###########################################
  
  observeEvent(input$selectTable, {
    if(input$selectTable == "children_per_HDB"){
      
      subzone_hdb_postal_presch_clean_table <- subzone_hdb_postal_presch_clean_unfiltered %>%
        dplyr:::select(`POSTAL`,`SUBZONE_N`, `PLN_AREA_N`,`1&2Room_PreSch_HDB`,`3Room_PreSch_HDB`,`4Room_PreSch_HDB`,`5Room_PreSch_HDB`)
      
      output$tableShow = DT::renderDataTable({
        DT::datatable(subzone_hdb_postal_presch_clean_table)
      })
      
    }else if(input$selectTable == "childcare"){
      
      childcareTable <- childcare %>%
        dplyr:::select(`OBJECTID`,`ADDRESSPOS`, `ADDRESSSTR`,`DESCRIPTIO`,`HYPERLINK`,`NAME`,`FMEL_UPD_D`)
      
      output$tableShow = DT::renderDataTable({
        DT::datatable(childcareTable,
                      escape  = F,
        options = list(columnDefs = list(list(targets = 8,render = JS("function(data, type, row, meta) {","return type === 'display' && data.length > 10 ?","'<span title=\"' + data + '\">' + data.substr(0, 8) + '...</span>' : data;","}"))))
        )
      })
    }else if(input$selectTable == "subzone_dwelling_type"){
      
      subzone_dwelling_type_table <- subzone_dwelling_type %>% st_set_geometry(NULL) %>%
        dplyr:::select(`OBJECTID`,`SUBZONE_N`, `PLN_AREA_N`,`TOTAL`,`HDB`,`ONE_TO_TWO`,`THREE_RM`,`FOUR_RM`,`FIVE_RM_EX`,`CONDOS_OTH`,`LANDED_PRO`,`OTHERS`,`FMEL_UPD_D`)
      
      output$tableShow = DT::renderDataTable({
        DT::datatable(subzone_dwelling_type_table,
                      escape  = F,
                      options = list(columnDefs = list(list(targets = 8,render = JS("function(data, type, row, meta) {","return type === 'display' && data.length > 10 ?","'<span title=\"' + data + '\">' + data.substr(0, 8) + '...</span>' : data;","}"))))
        )
      })
    }
  }, ignoreInit = TRUE, ignoreNULL = TRUE)
  
  output$tableShow = DT::renderDataTable({
    subzone_hdb_postal_presch_clean_table <- subzone_hdb_postal_presch_clean_unfiltered %>%
      dplyr:::select(`POSTAL`,`SUBZONE_N`, `PLN_AREA_N`,`1&2Room_PreSch_HDB`,`3Room_PreSch_HDB`,`4Room_PreSch_HDB`,`5Room_PreSch_HDB`)
    
    output$tableShow = DT::renderDataTable({
      DT::datatable(subzone_hdb_postal_presch_clean_table)
    })
  })
}
