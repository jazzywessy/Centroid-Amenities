library(dplyr)
library(sf)
library(rgdal)

token <- "pk.eyJ1IjoianNjczI2MDciLCJhIjoicFhiM1hJTSJ9.VBD0_KEXWrrEbW21L0uYGw"
allzips <- readRDS("data/superzip.rds")
allzips$latitude <- jitter(allzips$latitude)
allzips$longitude <- jitter(allzips$longitude)
allzips$college <- allzips$college * 100
allzips$zipcode <- formatC(allzips$zipcode, width=5, format="d", flag="0")
row.names(allzips) <- allzips$zipcode

cleantable <- allzips %>%
  dplyr::select(
    City = city.x,
    State = state.x,
    Zipcode = zipcode,
    Rank = rank,
    Score = centile,
    Superzip = superzip,
    Population = adultpop,
    College = college,
    Income = income,
    Lat = latitude,
    Long = longitude
  )


## Read Layers ###########################################
subzone_age_gender <- readOGR(dsn="data", layer="SUBZONE_AGE_GENDER_2016")
subzone_dwelling_type <- st_read(dsn = "data", layer = "SUBZONE_DWELLING_TYPE_2016")
childcare <- st_read(dsn = "data", layer = "CHILDCARE")
subzone_ogr <- readOGR(dsn = "data", layer = "MP14_SUBZONE_NO_SEA_PL")
subzone_pl <- st_read(dsn = "data", layer = "MP14_SUBZONE_NO_SEA_PL")
subzone_pl_unfiltered <- st_transform(subzone_pl, 4326)

# Change Lat and Lng
childcare_geo <- st_transform(childcare, 4326)
subzone_dwelling_type_geo <- st_transform(subzone_dwelling_type, 4326)

childcare_geo_unfiltered <- st_intersection(subzone_dwelling_type_geo, childcare_geo)

# Left Join
subzone_age_gender_dwelling_type  <- left_join(subzone_dwelling_type, subzone_age_gender@data, by = c("SUBZONE_N" = "SUBZONE_N"))

# Inner Join
list_subzone  <- inner_join(childcare_geo_unfiltered, subzone_ogr@data, by = c("SUBZONE_N" = "SUBZONE_N"))

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

# Function
st_drop_geometry <- function(x) {
  if(inherits(x,"sf")) {
    x <- st_set_geometry(x, NULL)
    class(x) <- 'data.frame'
  }
  return(x)
}