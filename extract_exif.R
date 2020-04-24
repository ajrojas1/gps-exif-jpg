# Extract EXIF Data using R
# Alfredo Rojas 
# 04/11/2020

library(exifr)
library(dplyr)
library(tmaptools)
library(tidyverse)
library(leaflet)

# create path to files and list JPGs

photo_files <- list.files(pattern = "_GPS")
path_to_file <- file.path(photo_files, "")
path_to_gps <- file.path("GPS_Points", "")

extract_jpg_transect <- function(jpg_path, tran_path){
  
  # ADD ERROR CHECKING FOR FUTURE
  # list jpg files and gps files for iteration
  jpg_files <- list.files(path_to_file, pattern = "*.jpg")
  gps_files <- list.files(path_to_gps, pattern = "*.gpx")
  
  # create data frame from EXIF data
  exif_data <- read_exif(paste0(path_to_file, jpg_files))
  
  # get relevant variables
  # https://www.r-bloggers.com/extracting-exif-data-from-photos-using-r/
  exif_data2 <- exif_data %>%
    select(SourceFile, 
           GPSLatitude, 
           GPSLongitude, 
           GPSTimeStamp)
  
  # see how many duplicates
  duplicates <- exif_data[c("GPSLongitude", "GPSLatitude")] %>%
    unique() %>%
    count()
  
  # separate geometry into lon/lat: https://github.com/r-spatial/sf/issues/231
  waypoints_sel <- gps_files %>%
    st_geometry() %>% # creates list
    do.call(rbind, .) %>% # do.call stores each list element for rbind, 
    as_tibble() %>%       # rbind converts c("X", "Y") into a matrix and combines all rows
    setNames(c("lon", "lat")) # creates names for new tibble
  
  # bind columns
  waypoint_bind <- bind_cols(gps_files , waypoints_sel) %>%
    select(time, lon, lat)
  
  rm(waypoints_sel)
  
  waypoint_bind$lon <- round(waypoint_bind$lon, 6) # find a way to generalize decimal places 
  waypoint_bind$lat <- round(waypoint_bind$lat, 5)
  
  # for loop to extract JPEGS taken near the transect waypoints
  start_time <- Sys.time()
  output_list <- list()
  for(i in 1:NROW(waypoint_bind)){
    gps_compare <- exif_data2 %>% # handle different decimal point places
      filter(.$GPSLongitude < (waypoint_bind$lon[i] + 0.000002), 
             .$GPSLongitude > (waypoint_bind$lon[i] - 0.000002),
             .$GPSLatitude < (waypoint_bind$lat[i] + 0.00002),
             .$GPSLatitude > (waypoint_bind$lat[i] - 0.00002)
             )
    output_list[[i]] <- gps_compare
  }
  output_df <- output_list %>%
    do.call(rbind, .) %>% 
    as_tibble() %>%
    unique()
  
  # write.csv(output_df, paste0(getwd(), "/output_csv/bar.csv"), row.names = T)
  
  end_time <- Sys.time()
  end_time - start_time # compare to indexing and modifying data frame. . . 

}

library(tidyverse)
look_csv <- read_csv("output_csv/bark.csv")
new_folder <- file.path(getwd(), "output_csv")
file.copy(look_csv$SourceFile, new_folder)


