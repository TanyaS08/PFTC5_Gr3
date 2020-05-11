#' PFTC5 - Perspective piece
#' Script to produce support figures
#' 
#' Jonathan von Oppen, Aarhus University
#' 01-04-2020
#' 
#' #########################################################################

### 0) Preamble ----
### >> a) Dependencies ----
if (!require('pacman')) install.packages('pacman', repos="https://cloud.r-project.org")
pacman::p_load(
  # general
  tidyverse,
  stringr,
  #sf,
  # for maps
  rgeos,
  rworldmap,
  maps,
  geosphere,
  #raster,
  #spData,
  #spDataLarge,
  #rworldxtra,
  # In addition, it uses the following visualisation packages:
  #tmap,      # for static and interactive maps
  #grid,      # for layout of inset maps
  #leaflet,   # for interactive maps
  #mapview,   # for interactive maps
  #shiny,     # for web applications
  #cartogram, # for continuous and non-contiguous area cartograms
  update = FALSE)

### >> b) Colour scheme ----
# ( from https://color.adobe.com/SRADDET-Sud-2-color-theme-14318632 )
theme_red <- "#F96654"
theme_blue <- "#4088C7"
theme_green <- "#34B362"
theme_darkblue <- "#1D5799"
theme_yellow <- "#FABC55"

### >> c) Functions ----
capwords <- function(s, strict = FALSE) {
  cap <- function(s) paste(toupper(substring(s, 1, 1)),
                           {s <- substring(s, 2); if(strict) tolower(s) else s},
                           sep = "", collapse = " " )
  sapply(strsplit(s, split = " "), cap, USE.NAMES = !is.null(names(s)))
}

### 1) Map with countries of origin vs residence ----
# >> prepare data ----
# get countries' centroid coordinates from https://worldmap.harvard.edu/data/geonode:country_centroids_az8
centroids <- getMap(resolution = "high") %>% 
  as.data.frame(gCentroid(byid = TRUE)) %>% 
  dplyr::select(NAME, LON, LAT)

# import participant countries
locations <- read.csv("//uni.au.dk/Users/au630524/Documents/Jonathan/Courses&workshops/200309_PFTC5/Perspectives piece/PFTC5_Peru_participants.csv") %>% 
# prepare dataset
  # rename all to lower case
  rename_all(tolower) %>% 
  # add unique participant ID
  mutate(partic_id = c(1:n())) %>% 
  # split country column into origin & residence
  mutate(origin = str_extract(country, "/.*")) %>% 
  # remove slash
  mutate(origin = str_remove(origin, ".*/")) %>% 
  # remove origin string from country
  mutate(country = str_remove(country, "/.*")) %>% 
  # fill origin col with countries for all other cases
  mutate(origin = coalesce(origin, country)) %>% 
  # rename country to residence
  rename(residence = country) %>% 
  # recode country names, for UK, US, CZ
  mutate_at(vars(residence, origin), list(~recode(., 
                          "UK" = "United Kingdom",
                          "US" = "United States",
                          "Czech Republic" = "Czech Rep.")))

# df for residences  
residences <- locations %>% 
  # summarise by country
  group_by(residence) %>% 
  summarise(n_people_res = n()) %>% 
  ungroup() %>% 
  # order by n_people
  arrange(desc(n_people_res)) %>% 
  # add centroids of residence countries
  left_join(centroids, by = c("residence" = "NAME")) %>% 
  # specify lat & lon to residence
  rename(lat_res = LAT, lon_res = LON) %>% 
  # reorder columns
  dplyr::select(contains("res"), n_people_res)

# df for origins
origins <- locations %>% 
  # summarise by country
  group_by(origin) %>% 
  summarise(n_people_ori = n()) %>% 
  ungroup() %>% 
  # order by n_people
  arrange(desc(n_people_ori)) %>% 
  # add centroids of origin countries
  left_join(centroids, by = c("origin" = "NAME")) %>% 
  # specify lat & lon to origin
  rename(lat_ori = LAT, lon_ori = LON) %>% 
  # reorder columns
  dplyr::select(contains("ori"), n_people_ori)

# re-join both dfs to original df
pftc5_locations <- locations %>% 
  # add residences
  left_join(residences, by = "residence") %>% 
  # add origins
  left_join(origins, by = "origin") %>% 
  # drop no. people columns
  select(-starts_with("n_people"))

# compute connecting lines
# create empty list
pftc5_lines <- NULL
# combine ori_lon&ori_lat and res_lon&res_lat into vectors
for(ID in 1:max(locations$partic_id)){
  # take values from lat_ori & lon_ori and put them into one vector
  pftc5_lines$line[[ID]] <- gcIntermediate(c(pftc5_locations$lon_ori[ID], 
                                             pftc5_locations$lat_ori[ID]),
                                           c(pftc5_locations$lon_res[ID], 
                                             pftc5_locations$lat_res[ID]),
                                          n = 100, 
                                          breakAtDateLine = FALSE)
}
names(pftc5_lines$line) <- paste("pftc5", locations$partic_id, sep = "_")

# create vector of IDs, each ID replicated for the number of line points
id_vector <- NULL
for(ID in 1:47){
  new_id <- c(rep(names(pftc5_lines$line)[ID], nrow(pftc5_lines$line[[ID]])))
  id_vector <- c(id_vector, new_id)
}

# stack all line point coordinates and add ID column
pftc5_lines_df <- pftc5_lines$line %>% 
  do.call(rbind, .) %>% 
  as.data.frame() %>% 
  mutate(id = id_vector) #%>% View

# >> make map ----
# create world base map
worldmap <- map_data("world")
# filter PFTC5 countries
# extract vector of unique country names
pftc5_countries <- locations %>% 
  select(origin, residence) %>% 
  unlist() %>% 
  unique() %>% 
  # recode country names back, for UK, US, CZ
  recode(., 
         "United Kingdom" = "UK",
         "United States" = "USA",
         "Czech Rep." = "Czech Republic")
# extract polygons  
pftc5_polygons <- map_data("world") %>%
  filter(region %in% pftc5_countries)

# # alternative source
# world_map <- getMap(resolution = "high")
# pftc5_countries1 <- world_map[world_map@data$ADMIN %in% c(residences$residence, origins$origin), ]

# make map
#pftc5_map <- 
  ggplot() +
    # base world map
    geom_polygon(data = worldmap, aes(x = long, y = lat, group = group), fill = "grey", alpha = .4) +
    # top map layer with PFTC5 countries
    geom_polygon(data = pftc5_polygons, aes(x = long, y = lat, group = group), fill = "darkgrey") +
    # residence points, weighted by no. people
    geom_point(data = residences, aes(x = lon_res+1.5, y = lat_res, size = n_people_res), color = theme_darkblue, alpha = .6) +
    # origin points, weighted by no. people
    geom_point(data = origins, aes(x = lon_ori-1.5, y = lat_ori, size = n_people_ori), color = theme_yellow, alpha = .6) +
    # connecting lines between respective origins and residences
    #geom_line(data = pftc5_lines_df, aes(x = lon, y = lat, group = id)) +
    # map appearance parameters
    labs(size = "No. people") +
    coord_quickmap() +
    theme_bw() +
    theme(panel.grid = element_blank())


### 2) Barplot for institutional support ----
# create dummy data 
university <- c(rep("UiB", 7),
                 rep("UA", 6),
                 rep("UBC", 2),
                 rep("Oxford", 3),
                 rep("Cusco", 6),
                 rep("Lima", 2),
                 rep("Oslo", 2),
                 rep("Cordoba", 3),
                 "Aarhus",
                 "Copenhagen",
                 "Waterloo",
                 "UCD",
                 "Purdue",
                 "Stockholm",
                 "Ås",
                 "Edinburgh",
                 "UAustral Chile",
                 "Charles University",
                 "GVSU",
                 "Newcastle",
                 "La Paz",
                 "UoM",
                 "CUNY",
                 "UWS")
response <- factor(c(rep("high effort", 17),
              rep("supportive", 5),
              rep("unmindful", 7),
              rep("no response", 18)),
            levels = c("high effort", "supportive", "unmindful", "no response"))

support <- data.frame(university, response)
levels(support$response) <- c("high effort", "supportive", "unmindful", "no response")

# make plot
ggplot(support, aes(x = response, fill = response)) +
  geom_histogram(stat = "count") +
  scale_fill_manual(values = c(theme_green, theme_blue, theme_yellow, theme_red)) +
  theme_bw() +
  theme(panel.grid = element_blank())
