library(dplyr)
library(sf)
library(rgdal)

token <- "pk.eyJ1IjoianNjczI2MDciLCJhIjoicFhiM1hJTSJ9.VBD0_KEXWrrEbW21L0uYGw"

## Read Layers ###########################################
subzone_age_gender <- readOGR(dsn="data", layer="SUBZONE_AGE_GENDER_2016")
subzone_dwelling_type <- st_read(dsn = "data", layer = "SUBZONE_DWELLING_TYPE_2016")
childcare <- st_read(dsn = "data", layer = "CHILDCARE")
eldercare <- st_read(dsn = "data", layer = "ELDERCARE")

subzone_ogr <- readOGR(dsn = "data", layer = "MP14_SUBZONE_NO_SEA_PL")
subzone_pl <- st_read(dsn = "data", layer = "MP14_SUBZONE_NO_SEA_PL")
subzone_pl_unfiltered <- st_transform(subzone_pl, 4326)

# Change Lat and Lng

eldercare_geo <- st_transform(eldercare, 4326)

childcare_geo <- st_transform(childcare, 4326)
subzone_dwelling_type_geo <- st_transform(subzone_dwelling_type, 4326)

childcare_geo_unfiltered <- st_intersection(subzone_dwelling_type_geo, childcare_geo)

eldercare_geo_unfiltered <- st_intersection(subzone_dwelling_type_geo, eldercare_geo)

# Left Join
subzone_age_gender_dwelling_type  <- left_join(subzone_dwelling_type, subzone_age_gender@data, by = c("SUBZONE_N" = "SUBZONE_N"))

# Inner Join
list_subzone  <- inner_join(childcare_geo_unfiltered, subzone_ogr@data, by = c("SUBZONE_N" = "SUBZONE_N"))

## Childcare ###########################################

# Data Preparation
subzone_age_gender_dwelling_type_cleaned <- subzone_age_gender_dwelling_type %>%
  mutate(Preschool = `BET0TO4`) %>%
  mutate(`PercentagePreSch` = (`Preschool` / `TOTAL.y`) * 100) %>%
  mutate(`Percentage_HDB` = (`HDB` / `TOTAL.x`) * 100) %>%
  mutate(`PreSch_HDB` = floor((`Percentage_HDB` / 100) * Preschool)) %>%
  mutate("1&2Room_Percentage" = (`ONE_TO_TWO` / `HDB`) * 100) %>%
  mutate("3Room_Percentage" = (`THREE_RM` / `HDB`) * 100) %>%
  mutate("4Room_Percentage" = (`FOUR_RM` / `HDB`) * 100) %>%
  mutate("5Room_Percentage" = (`FIVE_RM_EX` / `HDB`) * 100) %>%
  mutate("1&2Room_PreSch" = (`1&2Room_Percentage` / 100) * PreSch_HDB) %>%
  mutate("3Room_PreSch" = (`3Room_Percentage` / 100) * PreSch_HDB) %>%
  mutate("4Room_PreSch" = (`4Room_Percentage` / 100) * PreSch_HDB) %>%
  mutate("5Room_PreSch" = (`5Room_Percentage` / 100) * PreSch_HDB) %>%
  mutate("1&2Room_PreSch" = if_else(is.na(`1&2Room_PreSch`), 0, floor(`1&2Room_PreSch` ) )) %>%
  mutate("3Room_PreSch" = if_else(is.na(`3Room_PreSch`), 0, floor(`3Room_PreSch` ) )) %>%
  mutate("4Room_PreSch" = if_else(is.na(`4Room_PreSch`), 0, floor(`4Room_PreSch` ) )) %>%
  mutate("5Room_PreSch" = if_else(is.na(`5Room_PreSch`), 0, floor(`5Room_PreSch` ) ))

subzone_hdb_postal <- readOGR(dsn="data",layer="SUBZONE_HDB_POSTAL")
subzone_hdb_postal_presch  <- left_join(subzone_hdb_postal@data, subzone_age_gender_dwelling_type_cleaned, by = c("SUBZONE_N" = "SUBZONE_N"))

subzone_hdb_postal_presch_clean_unfiltered <- subzone_hdb_postal_presch %>%
  mutate(`X1room` = as.numeric(as.character(`X1room`))) %>%
  mutate(`X2room` = as.numeric(as.character(`X2room`))) %>%
  mutate(`X3room` = as.numeric(as.character(`X3room`))) %>%
  mutate(`X4room` = as.numeric(as.character(`X4room`))) %>%
  mutate(`X5room` = as.numeric(as.character(`X5room`))) %>%
  mutate(`EF` = as.numeric(as.character(`EF`))) %>%
  mutate(`X1X2Room` =  rowSums(.[2:3]) ) %>%
  mutate(`X5ExRoom` = rowSums(.[6:7]) ) %>%
  group_by(`SUBZONE_N`) %>%
  mutate(TotalPopX1X2 = sum(`X1X2Room`)) %>%
  mutate(TotalPopX3 = sum(`X3room`)) %>%
  mutate(TotalPopX4 = sum(`X4room`)) %>%
  mutate(TotalPopX5Ex = sum(`X5ExRoom`)) %>%
  mutate(PercentageX1X2 = (`X1X2Room` / `TotalPopX1X2`) * 100) %>%
  mutate(PercentageX3 = (`X3room` / `TotalPopX3`) * 100) %>%
  mutate(PercentageX4 = (`X4room` / `TotalPopX4`) * 100) %>%
  mutate(PercentageX5Ex = (`X5ExRoom` / `TotalPopX5Ex`) * 100) %>%
  mutate("1&2Room_PreSch_HDB" = (`PercentageX1X2` / 100) * `1&2Room_PreSch`) %>%
  mutate("3Room_PreSch_HDB" = (`PercentageX3` / 100) * `3Room_PreSch`) %>%
  mutate("4Room_PreSch_HDB" = (`PercentageX4` / 100) * `4Room_PreSch`) %>%
  mutate("5Room_PreSch_HDB" = (`PercentageX5Ex` / 100) * `5Room_PreSch`) %>%
  mutate("1&2Room_PreSch_HDB" = if_else(is.na(`1&2Room_PreSch_HDB`), 0, round(`1&2Room_PreSch_HDB` ) )) %>%
  mutate("3Room_PreSch_HDB" = if_else(is.na(`3Room_PreSch_HDB`), 0, round(`3Room_PreSch_HDB` ) )) %>%
  mutate("4Room_PreSch_HDB" = if_else(is.na(`4Room_PreSch_HDB`), 0, round(`4Room_PreSch_HDB` ) )) %>%
  mutate("5Room_PreSch_HDB" = if_else(is.na(`5Room_PreSch_HDB`), 0, round(`5Room_PreSch_HDB` ) ))

subzone_hdb_postal_presch_clean_unfiltered <- subzone_hdb_postal_presch_clean_unfiltered %>%
  mutate("Total_PreSch_HDB" = (`1&2Room_PreSch_HDB` + `3Room_PreSch_HDB` + `4Room_PreSch_HDB` + `5Room_PreSch_HDB`) )%>%
  mutate("Total_PreSch_HDB" = as.numeric(as.character(`Total_PreSch_HDB`)) ) %>%
  mutate("Total_PreSch_HDB_Scale" = scale(`Total_PreSch_HDB`) ) %>%
  dplyr:::select(`POSTAL`,`SUBZONE_N`, `PLN_AREA_N`,`1&2Room_PreSch_HDB`,`3Room_PreSch_HDB`,`4Room_PreSch_HDB`,`5Room_PreSch_HDB`,`lng`,`lat`,`Total_PreSch_HDB`,`Total_PreSch_HDB_Scale`,`TOTAL.x`,`HDB`,`Preschool`,`PreSch_HDB`)

# geometryTest <- subzone_hdb_postal_presch_clean_unfiltered[, c(8,9)]
# start <- which(colnames(subzone_hdb_postal_presch_clean_unfiltered) == "lat")
# end <- which(colnames(subzone_hdb_postal_presch_clean_unfiltered) == "lng")
# print(start)
# print(end)

## Eldercare ###########################################

# Data Preparation
subzone_age_gender_dwelling_type_elder <- subzone_age_gender_dwelling_type %>%
  mutate(`Elder`= `BET65TO69` + `BET70TO74` + `BET75TO79` + `BET80TO84` + `OVER85` ) %>%
  mutate(`PercentageElder` = (`Elder` / `TOTAL.y`) * 100) %>%
  mutate(`Percentage_HDB` = (`HDB` / `TOTAL.x`) * 100) %>%
  mutate(`Elder_HDB` = floor((`Percentage_HDB` / 100) * Elder)) %>%
  mutate("1&2Room_Percentage" = (`ONE_TO_TWO` / `HDB`) * 100) %>%
  mutate("3Room_Percentage" = (`THREE_RM` / `HDB`) * 100) %>%
  mutate("4Room_Percentage" = (`FOUR_RM` / `HDB`) * 100) %>%
  mutate("5Room_Percentage" = (`FIVE_RM_EX` / `HDB`) * 100) %>%
  mutate("1&2Room_Elder" = (`1&2Room_Percentage` / 100) * Elder_HDB) %>%
  mutate("3Room_Elder" = (`3Room_Percentage` / 100) * Elder_HDB) %>%
  mutate("4Room_Elder" = (`4Room_Percentage` / 100) * Elder_HDB) %>%
  mutate("5Room_Elder" = (`5Room_Percentage` / 100) * Elder_HDB) %>%
  mutate("1&2Room_Elder" = if_else(is.na(`1&2Room_Elder`), 0, floor(`1&2Room_Elder` ) )) %>%
  mutate("3Room_Elder" = if_else(is.na(`3Room_Elder`), 0, floor(`3Room_Elder` ) )) %>%
  mutate("4Room_Elder" = if_else(is.na(`4Room_Elder`), 0, floor(`4Room_Elder` ) )) %>%
  mutate("5Room_Elder" = if_else(is.na(`5Room_Elder`), 0, floor(`5Room_Elder` ) ))


subzone_hdb_postal_elder  <- left_join(subzone_hdb_postal@data, subzone_age_gender_dwelling_type_elder, by = c("SUBZONE_N" = "SUBZONE_N"))

subzone_hdb_postal_elder_clean_unfiltered <- subzone_hdb_postal_elder %>%
  mutate(`X1room` = as.numeric(as.character(`X1room`))) %>%
  mutate(`X2room` = as.numeric(as.character(`X2room`))) %>%
  mutate(`X3room` = as.numeric(as.character(`X3room`))) %>%
  mutate(`X4room` = as.numeric(as.character(`X4room`))) %>%
  mutate(`X5room` = as.numeric(as.character(`X5room`))) %>%
  mutate(`EF` = as.numeric(as.character(`EF`))) %>%
  mutate(`X1X2Room` =  rowSums(.[2:3]) ) %>%
  mutate(`X5ExRoom` = rowSums(.[6:7]) ) %>%
  group_by(`SUBZONE_N`) %>%
  mutate(TotalPopX1X2 = sum(`X1X2Room`)) %>%
  mutate(TotalPopX3 = sum(`X3room`)) %>%
  mutate(TotalPopX4 = sum(`X4room`)) %>%
  mutate(TotalPopX5Ex = sum(`X5ExRoom`)) %>%
  mutate(PercentageX1X2 = (`X1X2Room` / `TotalPopX1X2`) * 100) %>%
  mutate(PercentageX3 = (`X3room` / `TotalPopX3`) * 100) %>%
  mutate(PercentageX4 = (`X4room` / `TotalPopX4`) * 100) %>%
  mutate(PercentageX5Ex = (`X5ExRoom` / `TotalPopX5Ex`) * 100) %>%
  mutate("1&2Room_Elder_HDB" = (`PercentageX1X2` / 100) * `1&2Room_Elder`) %>%
  mutate("3Room_Elder_HDB" = (`PercentageX3` / 100) * `3Room_Elder`) %>%
  mutate("4Room_Elder_HDB" = (`PercentageX4` / 100) * `4Room_Elder`) %>%
  mutate("5Room_Elder_HDB" = (`PercentageX5Ex` / 100) * `5Room_Elder`) %>%
  mutate("1&2Room_Elder_HDB" = if_else(is.na(`1&2Room_Elder_HDB`), 0, round(`1&2Room_Elder_HDB` ) )) %>%
  mutate("3Room_Elder_HDB" = if_else(is.na(`3Room_Elder_HDB`), 0, round(`3Room_Elder_HDB` ) )) %>%
  mutate("4Room_Elder_HDB" = if_else(is.na(`4Room_Elder_HDB`), 0, round(`4Room_Elder_HDB` ) )) %>%
  mutate("5Room_Elder_HDB" = if_else(is.na(`5Room_Elder_HDB`), 0, round(`5Room_Elder_HDB` ) )) %>%
  mutate("Total_Elder_HDB" = `1&2Room_Elder_HDB` + `3Room_Elder_HDB` + `4Room_Elder_HDB` + `5Room_Elder_HDB` ) %>%
  mutate("Total_Elder_HDB" = as.numeric(as.character(`Total_Elder_HDB`)) ) %>%
  mutate("Total_Elder_HDB_Scale" = scale(`Total_Elder_HDB`) )%>%
  dplyr:::select(`POSTAL`,`SUBZONE_N`, `PLN_AREA_N`,`1&2Room_Elder_HDB`,`3Room_Elder_HDB`,`4Room_Elder_HDB`,`5Room_Elder_HDB`,`lng`,`lat`,`Total_Elder_HDB`,`Total_Elder_HDB_Scale`,`TOTAL.x`,`HDB`,`Elder`,`Elder_HDB`)

# Function
st_drop_geometry <- function(x) {
  if(inherits(x,"sf")) {
    x <- st_set_geometry(x, NULL)
    class(x) <- 'data.frame'
  }
  return(x)
}