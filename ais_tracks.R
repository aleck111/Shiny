library(sf)

read_combine_ais_ribs <- function(){
  w2s1 <- read_sf(here("data/AIS_tracks/w2s1.geojson"))
  w2s2 <- read_sf(here("data/AIS_tracks/w2s2.geojson")) 
  w2s3 <- read_sf(here("data/AIS_tracks/w2s3.geojson")) 
  w2s4 <- read_sf(here("data/AIS_tracks/w2s4.geojson")) 
  ah1 <- read_sf(here("data/AIS_tracks/ah1.geojson")) 
  yam <- read_sf(here("data/AIS_tracks/yam.geojson")) 
  
  ribs <- rbind(w2s1, w2s2, w2s3, w2s4, ah1, yam) |>
    spTransform(CRS("+proj=longlat +datum=WGS84 +no_defs"))
}
