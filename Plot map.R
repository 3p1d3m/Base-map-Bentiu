# Import the data 

# Load packages -----------------------------------------------------------
pacman::p_load(
  rio,           ## Import and export data 
  here,          ## Relative file path 
  janitor,       ## Automated data cleaning 
  sf,            ## for working with geospatial data
  ggspatial,     ## for basemaps and north arrows
  raster,        ## for spatial formatting 
  prettymapr,    ## for basemaps
  tidyverse      ## data managemnet and vsisulaisation 
)


## read in shapefile by sous_prefecture--------------------------------------------------

map_poc_raw <- read_sf(here::here("bentiu", "BentiuPoC_sector_bloc_191128.shp")) %>% 
  clean_names()

map_poc_huts <- read_sf(here::here("1-Initial Data", "bentiu_huts_houses.shp")) %>% 
  
  clean_names()

map_poc_secblock <- read_sf(here::here("1-Initial Data", "BentiuPoC_sector_bloc_191128.shp")) %>% 
  
  clean_names()

map_poc <- read_sf(here::here("2-Clean Data", "bentiu_sector_bloc_join.shp")) %>% 
  
  # puts all column names to lower case
  janitor::clean_names() 


map_poc_shelters <- read_sf(here::here("2-Clean Data", "bentiu_shelters.shp")) %>% 
  
  # puts all column names to lower case
  janitor::clean_names() 




#  Clean map data-----------------------------------------------------------------------
map_poc_cl <-  map_poc_raw %>%      ## convert all values to lowercase
  mutate_at(c("is_in_sect", "name"), str_to_lower) %>%  
  mutate(is_in_sect = str_replace(is_in_sect, " ", "_"),  # replace space with _
         name = str_replace(name, " ", "_"),              # replace space with _   
         bloc = name,                                     # create bloc variable to match linelist 
         sector_bloc_name = paste0(is_in_sect, " ", bloc)) %>%      # create variable that combines both sector and bloc number 
  mutate(is_in_sect = case_when(
    name == "unmiss_bentiu" & is.na(is_in_sect) ~ "Unmiss logbase",
    TRUE ~ is_in_sect
  ))

## check the coordinate reference system (CRS)
# st_crs(map_poc_cl)

## if CRS not WGS84, reset it
# map <- st_set_crs(map, value = 4326) # Sets to WGS84

# get the bounding box for the shapefile 
bounding_box <- map_poc_cl %>% 
  st_bbox()


# plot a base map including scale bar 
basemap <- ggplot() +
  # change the bounding box to an sf object
  # this defines the area to download map tiles for
  geom_sf(data = st_as_sfc(bounding_box)) +
  # download map tiles and add to the plot
  annotation_map_tile(
    # define what map tiles to use
    type =  "cartolight",
    # define folder to store tile images 
    cachedir = here::here("data", "map_tiles"),
    # define if should download tiles each time
    forcedownload = FALSE,
    # hide messages about download status and zoom
    progress = "none" )


# show basemap
basemap

# plot cases on top of basemap
basemap + 
  
  annotation_map_tile(zoomin = 2) +
  ## add the shapefile on top
  geom_sf(data = map_poc_cl, 
          # no fill
          fill = NA,
          # black borders
          colour = "red") + 

  geom_sf(data = map_poc_huts,
          fill = NA,
          color = "grey40") +
  geom_text( aes(label = 'Sector 1',
                  x = 29.785,
                  y = 9.339),
              angle = 10, 
              vjust = 1) +
geom_text( aes(label = 'Sector 2', 
               x = 29.7895, 
               y = 9.34),
           angle = 10,
           hjust =  0.8) +
  geom_text( aes(label = 'Sector 3',
                 x = 29.80,
                 y = 9.339,
                 angle = -70,
                 hjust = 1,
                 vjust =  0.7)) +
  geom_text(aes(label = 'Sector 4',
                x = 29.8011, 
                y = 9.3348, 
                angle = -70,
                hjust = 1,
                vjust = - 0.1)) +
  geom_text(aes(label = 'Sector 5',
                x = 29.8025, 
                y = 9.33,
                angle = -70,
                hjust = 1, 
                vjust = - 0.90)) +
  
  geom_text(aes(label = 'UNMISS humanitarian logbase',
                x = 29.7870,
                y = 9.33,
                angle = -50,
                hjust = 0.3,
                vjust = 0.2)) +
  # spatial-aware automagic scale bar
  annotation_scale(location = "bl") +
  
  # spatial-aware automatic north arrow
  annotation_north_arrow(location = "tr", 
                         which_north = "true") +
  
 
  labs(
       captions = "This map was generated using freely availabe shapefiles from 
                           https://data.world/datasets/south-sudan?page=4.
                          DISCLAIMER:This map doesn't represent any political boundaries or 
                     affilitions other than showing the service map of the IDP camp",
       title = "Map: Sectors and blocks of Bentiu Internaly Displaced Population camp,
                August 2021") +
  theme_void()






