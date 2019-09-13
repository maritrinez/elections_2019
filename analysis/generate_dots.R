#------- DESCRIPTION ------
# Generate coordinates for each party seat: 
# create a data frame with the number of dots we want plotted 
# according to the number of seats they got in the elections, 
# inside the correspondent municipality geography.


#------- REFERENCE ------
# https://www.cultureofinsight.com/blog/2018/05/02/2018-04-08-multivariate-dot-density-maps-in-r-with-sf-ggplot2/
# https://cultureofinsight.shinyapps.io/dotmap/


#------- LIBRARIES & WD ------
library(tidyverse)
library(sf)
library(readxl)
library(geojsonio)

library(maptools) # automatic dot density

library(rmapshaper)
library(mapview)
library(geojsonio)
library(tmap)


setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#------- LOAD DATA ------
# Elections data
elections <- read_csv('data/muni2019_parties_colors.csv')

# Municipalities IGN 
# http://centrodedescargas.cnig.es/CentroDescargas/catalogo.do?Serie=CAANE 
# download zip from 'líneas limite municipales'

p_canarias <- 'raw_data/ign_shapefiles/recintos_municipales_inspire_canarias_wgs84/'
p_peninsula <- 'raw_data/ign_shapefiles/recintos_municipales_inspire_peninbal_etrs89/'

muni_sf <- sf::read_sf(p_peninsula) %>% 
  st_transform(4326) %>%
  rbind(sf::read_sf(p_canarias)) %>% 
  mutate(MUNICODE_INE = str_sub(NATCODE, start = 7, end = 11)) %>% 
  filter(!grepl('^53', MUNICODE_INE))

rm(p_canarias, p_peninsula)

# plot to confirm projection
mapview::mapview(filter(muni_sf, grepl('^31', MUNICODE_INE)))

# Some municipalities results are missing
# The final data is not available yet (9-9-2019)
missing <- muni_sf %>% 
  filter(MUNICODE_INE %in% setdiff(muni_sf$MUNICODE_INE, elections$location_id))
rm(missing)

# ---- Duplicated rows
# test that each municipality has only one multipoligon
duplicated_munis <- muni_sf %>% 
  group_by(MUNICODE_INE) %>% 
  summarise(count = n()) %>% 
  ungroup() %>% 
  filter(count > 1) %>% 
  .$MUNICODE_INE

# there are 7 shapefiles have two rows of multipolygons
# after plotting them we see that:
# all of them have a bigger main polygon and small ones around them
# Only the big polygon will be kept in order to generate the inside points
aux <-  muni_sf %>% filter(MUNICODE_INE %in% duplicated_munis)
mapview::mapview(aux)
mapview::mapview(aux[1:2,])
rm(duplicated_munis, aux)

# Add an area variable and select the big polygons
muni_sf <- muni_sf %>% 
  mutate(area = st_area(.)) %>% 
  group_by(MUNICODE_INE) %>% 
  top_n(1, area) %>% 
  ungroup()


#------- GENERATE RANDOM POINTS -----

# Ddd a coordinates inside the muni polygon for every given seat
# (the seats_available variable )
muni_sf_seats <- elections %>% 
  group_by(location_id) %>% 
  mutate(seats_given = sum(party_seats)) %>% 
  distinct(location_id, muni_name, seats_given) %>% 
  right_join(muni_sf, by = c('location_id' = 'MUNICODE_INE')) %>% 
  mutate(muni_name = if_else(!is.na(muni_name), muni_name, NAMEUNIT)) %>% 
  filter(!is.na(seats_given)) %>%
  select(location_id, muni_name, seats_given, geometry) %>% 
  ungroup()

# Function to generate random points, uniform, avoiding overlapping
# The trick was to create a bigger sample than needed & remove the overlapped afterwards.
sample_uniform_points <- function (geometry, seats, municode) {
  print(municode)
  
  # - - Constants
  ss <- 150 # Sample size = 150 works on both, small an big polygons.
  multistring <- st_cast(geometry, 'MULTILINESTRING') %>% st_set_crs(4326) # Multistring boundary
  
  # - - Raw points
  pts <- st_sample(geometry, ss) %>% # (funciona en cáceres, que es la mayor de todas y en emperador que es la más pequeña)
    st_cast("POINT") %>%
    st_coordinates() %>%
    as_tibble() %>%
    setNames(c("lon","lat")) %>% 
    st_as_sf(coords = c("lon","lat")) %>% 
    st_set_crs(4326)
  
  # - - Remove the points that are closest to the boundary (un porcentaje en función del espacio disponible para cada punto?)
  pts <- pts %>% 
    mutate(dist_real = as.numeric(st_distance(geometry, multistring))) %>%
    filter(dist_real > pmin(450, quantile(.$dist_real, 0.5))) %>% # boundary threshold
    mutate(id = row_number(), MUNICODE_INE = municode)
  
  # # Plot pts
  # plot(geometry, main = municode)
  # plot(pts, add = T, col = alpha('red', 0.6) , pch = 19)
  
  # - - Remove the closest points
  # while the sample is bigger than the one needed (seats)
  # Look for the closest pairs of points,
  # and from that pair, remove the point which have
  # a third point closer.
  i <- 1
  while (length(pts$id) > seats) {
    # Get distances among points
    distances <- st_distance(pts, pts) %>%
      as_tibble() %>%
      mutate(p1 = row_number()) %>%
      gather(p2, value, -p1) %>%
      mutate(p2 = str_replace(p2, 'V', '')) %>%
      mutate_all(as.numeric) %>%
      filter(value > 0)
    
    # Calculate the mean of the bottom 2 distances
    distances_mins <- distances %>% 
      group_by(p1) %>% 
      top_n(-2, value) %>% 
      mutate(min_mean = mean(value)) %>%
      ungroup() %>% 
      distinct(p1, min_mean) %>% 
      right_join(distances)
    
    # Closests pairs
    # For each closest pair, remove the point with the lowest min_mean
    # The 'lowest' min_mean are ones in the lower quantile
    # The lower quantile is based on the number of points
    # So it is bigger when ther are many points and more accurate when the points are close to the number seats
    
    prob <- length(pts$id) ^ -1.2
    
    # Get the closest points 
    closest <- distances_mins %>% 
      filter(value < quantile(.$value, prob)) %>% 
      group_by(value) %>% 
      top_n(-1, min_mean) %>% 
      ungroup() %>% 
      distinct(p1, min_mean)
    
    # print(i)
    # print(length(pts$id) - length(unique(closest$p1)))
    # print(municode)
    
    # -- Filter out the closest points from the 'pts' data frame
    # If removing the closest the data frame is bigger than the sample needed (seats), remove them
    closest_l <- length(unique(closest$p1))
    if (closest_l == 0) {
      closest <- distances_mins %>% 
        slice(1) %>% 
        .$p1
    } else if (length(pts$id) - closest_l >= seats) {
      closest <- closest %>% 
        .$p1 %>% 
        unique()
    } else {
      # Remove as much as needed so the data frame will be the same size
      # than the sample needed
      closest <- closest %>% 
        top_n(length(pts$id) - seats, min_mean) %>% 
        .$p1 %>% 
        unique()
    }
    
    pts <- pts %>% 
      filter(!(id %in% closest)) %>% 
      mutate(id = row_number())
    i <- i+1
  }
  return (pts)
}

# Run the function (it takes about one hour)
start <- Sys.time()
muni_dots <- pmap(muni_sf_seats, # pmap_df throws an error
                ~ sample_uniform_points(st_sfc(..4), seats = ..3, municode = ..1))
muni_dots <- do.call(rbind, muni_dots) %>% 
  select(-dist_real)
Sys.time() - start



# Write the dots.
# Use write_sf as write_csv fails when reading the file;
# because read_csv doesn't find a valid geometry column
write_sf(muni_dots, 'data/muni_dots.csv', layer_options = "GEOMETRY=AS_WKT", delete_dsn=TRUE)

# # If needed, read the muni_dots sf again
# muni_dots <- read_sf('data/muni_dots.csv', geometry_column = 'geometry') %>% 
#   select(-WKT) %>% 
#   st_set_crs(4326)


# Assign a party and therefore a color, to every point
elections_long <- elections %>% 
  uncount(party_seats) %>% 
  group_by(location_id) %>% 
  arrange(rank) %>% 
  mutate(seat_index = row_number()) %>% 
  ungroup() %>% 
  arrange(location_id)


muni_dots_colors <- muni_dots %>%
  rename(
    seat_index = id,
    location_id = MUNICODE_INE
  ) %>% 
  right_join(elections_long) %>% 
  st_as_sf() %>%
  st_set_crs(4326) %>% 
  select(-seats_available)

write_sf(muni_dots_colors, 'data/muni_dots.csv', layer_options = "GEOMETRY=AS_WKT", delete_dsn=TRUE)

# ------- MUNI POLYGONS WITH ADITIONAL INFO --------------

# - - Party seats by party
# - - Parties count
# - - Majority vs simple majority municipalities


# Function to store the party seats as unique variable
seats2list <- function (group) {
  tmp <- group %>% 
    select(party_acronym, political_party_long_name, party_seats, color, rank) %>% 
    list()
  print(tmp)
  return (tmp)
}


muni_sf_data <- muni_sf_seats %>%
  left_join(elections) %>% 
  select(-seats_available) %>% 
  group_by(location_id) %>% 
  do(mutate(., party_seats_list = seats2list(.))) %>% 
  summarise(
    muni_name = first(muni_name), 
    seats_given = first(seats_given),
    party_count = n(),
    winner = political_party_slug[rank == 1],
    majority = party_seats[rank == 1] >= floor(first(seats_given) / 2) + 1,
    party_seats_list = first(party_seats_list),
    geometry = first(geometry)
  ) %>%
  ungroup() %>% 
  mutate(id = row_number()) %>% 
  st_as_sf() %>% 
  st_set_crs(4326)



#------- TODO::SAVE POINTS & POLYGONS AS GEOJSON ------

#  Polygons 
geojson_write(muni_sf_data, file = '../development/data/municipalities.json')

# Dots
muni_dots_colors %>% 
  rename(p_slug = political_party_slug, p_name = political_party_long_name) %>% 
  mutate(
    p_slug = if_else(!is.na(p_slug), p_slug, str_replace_all(tolower(party_acronym_original), "[[:space:]]", "")),
    p_name = if_else(!is.na(p_name), p_name, toupper(party_acronym_original)),
    id = row_number(),
    valid = st_dimension(st_sfc(geometry)) # Removes empty geometries
  ) %>% 
  filter(!is.na(valid)) %>% 
  select(id, location_id, p_slug, p_name, seat_index, color, color_hex) %>% 
  geojson_write(file = '../development/data/seats.json')


class(muni_dots_colors)


