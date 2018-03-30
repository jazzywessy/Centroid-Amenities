
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
  
  observeEvent(input$selectSubzone, {
      # Filter
      subzone_hdb_postal_presch_clean <- subzone_hdb_postal_presch_clean_unfiltered %>% filter(`SUBZONE_N` == input$selectSubzone)
      childcare_geo <- childcare_geo_unfiltered %>% filter(`SUBZONE_N` == input$selectSubzone)
      subzone_pl <- subzone_pl_unfiltered %>% filter(`SUBZONE_N` == input$selectSubzone)
      
      ## K Means ###########################################
      # CRS
      getCRS <- st_crs(childcare_geo)
      
      # Geometry
      childcare_geometry <- childcare_geo %>%
        tidyr:::extract(geometry, c("lng", "lat"), "\\(([^,]+), ([^)]+)\\)")

      childcare_drop_geo <- st_drop_geometry(childcare_geo)

      #Matrix
      childcare_matrix <- with(childcare_geometry, cbind( as.numeric(lng) , as.numeric(lat)))
      row.names(childcare_matrix)<- 1:nrow(childcare_matrix)

      hdb_matrix <- with(subzone_hdb_postal_presch_clean, cbind(lng, lat))
      row.names(hdb_matrix)<- 1:nrow(hdb_matrix)


      plot_coord_childcare <- SpatialPointsDataFrame(coords= childcare_matrix, data = childcare_drop_geo, proj4string = CRS(getCRS[['proj4string']]))
      plot_coord_hdb <- SpatialPointsDataFrame(coords= hdb_matrix, data = subzone_hdb_postal_presch_clean, proj4string = CRS(getCRS[['proj4string']]))

      plot_coord_hdb <- spTransform(plot_coord_hdb,CRS("+init=epsg:4326"))
      plot_coord_childcare <- spTransform(plot_coord_childcare,CRS("+init=epsg:4326"))

      # Calculate Distance
      distance.matrix <- matrix(0,nrow(childcare_geo),3,dimnames=list(c(),c("Lat","Lon","DistC")))
      for(i in 1:nrow(plot_coord_childcare)){
        sub <- plot_coord_childcare[i,]
        dist.v <- gDistance(sub,plot_coord_hdb)
        distance.matrix[i,] <- matrix(c(sub@coords,dist.v),ncol=3)
      }

      distDF <- as.data.frame(distance.matrix)
      subzone_childcare_data <-  scale(distDF[,3])
      
      
      
      ## L Function ###########################################
      # Transform
      subzone_sp <-  sf:::as_Spatial(subzone_pl$geometry)
      childcare_sp <-  sf:::as_Spatial(childcare_geo$geometry)
      
      childcare_sp <- as(childcare_sp, "SpatialPoints")
      subzone_sp <- as(subzone_sp, "SpatialPolygons")
      
      # ppp object
      owin <- as(subzone_sp, "owin")
      childcare_ppp <- as(childcare_sp, "ppp")
      childcare_ppp$window <- owin
      
      # Create raster
      childcare_bw <- density(childcare_ppp, sigma=bw.diggle, edge=TRUE, kernel="gaussian")
      childcare_bw_raster <- raster(childcare_bw)
      raster::projection(childcare_bw_raster) <- sp::CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0")
      
      # Create icon
      icon_childcare <- awesomeIcons(
        icon = 'fa-graduation-cap',
        iconColor = 'black',
        library = 'fa',
        markerColor = 'beige'
      )
      
      icon_hdb <- awesomeIcons(
        icon = 'fa-home',
        iconColor = 'black',
        library = 'fa',
        markerColor = 'cadetblue'
      )
      
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
                            group = "HDB") %>%
          addAwesomeMarkers(data=childcare_geo$geometry, icon=icon_childcare,
                            popup = paste("Planning Area: ", childcare_geo$`PLN_AREA_N`, "<br>",
                                          "Subzone: ", childcare_geo$`SUBZONE_N`, "<br>",
                                          "Childcare Centre Name: ", childcare_geo$`NAME`, "<br>"),
                            group = "Child Care Centres") %>%
          addRasterImage(x = childcare_bw_raster, opacity = 0.5, project = FALSE, group = "Kernel Density") %>%
          addPolygons(data=subzone_pl$geometry, weight = 3, fillColor = "brown",popup = paste("Planning Area: ", subzone_pl$`PLN_AREA_N`, "<br>",
                                                                                               "Subzone: ", subzone_pl$`SUBZONE_N`),
                      group = "Subzone") %>%
          addCircleMarkers(
            data=childcare_geo$geometry,
            radius = input$sliderBuffer,
            color = "navy",
            stroke = FALSE, fillOpacity = 0.1,
            group = "Buffer"
          ) %>%
          addScaleBar(position = "bottomleft") %>%
          addLayersControl(position = "topleft",
            baseGroups = c("Streets", "Light", "Outdoors"),
            overlayGroups = c("HDB", "Child Care Centres","Kernel Density","Subzone","Buffer"),
            options = layersControlOptions(collapsed = FALSE)
          )
      })
      
      output$subzoneCheck <- reactive({
        input$selectSubzone
      })
      outputOptions(output, "subzoneCheck", suspendWhenHidden = FALSE) 
      
      output$L_Function <- renderPlot({
        childcare_L <- Lest(childcare_ppp, correction="Ripley")
        plot(childcare_L, main = paste(input$selectSubzone, " Childcare L Funtion"))
      })
      
      output$SOS <- renderPlot({

        wss <- (nrow(subzone_childcare_data)-1)*sum(apply(subzone_childcare_data,2,var))
        for (i in 2:3) wss[i] <- sum(kmeans(subzone_childcare_data,
                                            centers=i)$withinss)
        plot(1:3, wss, type="b", xlab="Number of Clusters",
             ylab="Within groups sum of squares", main = paste(input$selectSubzone, " Sum of square of errors"))

      })

      output$K_Means <- renderPlot({

        clust2 <- kmeans(subzone_childcare_data,3)
        distDF$Clusters <- clust2$cluster

        plot(subzone_childcare_data, col = distDF$Clusters, main = paste(input$selectSubzone, " K Means"))

      })
      
  }, ignoreInit = TRUE, ignoreNULL = TRUE)


  ## Data Explorer ###########################################

  observe({
    if (is.null(input$goto))
      return()
    isolate({
      map <- leafletProxy("map")
      map %>% clearPopups()
      dist <- 0.5
      zip <- input$goto$zip
      lat <- input$goto$lat
      lng <- input$goto$lng
      showZipcodePopup(zip, lat, lng)
      map %>% fitBounds(lng - dist, lat - dist, lng + dist, lat + dist)
    })
  })
  
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
