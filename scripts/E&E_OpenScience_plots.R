#' PFTC5 - Perspective piece
#' Script to produce support figures
#' 
#' Jonathan von Oppen, Aarhus University
#' 01-04-2020
#' 
#' #########################################################################

rm(list = ls())

### 0) Preamble ----
### >> a) Dependencies ----
if (!require('pacman')) install.packages('pacman', repos="https://cloud.r-project.org")
pacman::p_load(
  # general
  tidyverse,
  tidylog,
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
grad_red <- c("#FFCCCB", "#F0AAAD", "#E1888F", "#D16570", "#C24352")
grad_green <- c("#D0EDC6", "#B0E0A0", "#90D27B", "#71C456", "58AA3D")
grad_yellow <- c("#FFFFC2", "#FFFE55", "#FFF000", "#FFE23D", "#FFD700")
grad_blue <- c("#A8D7FA", "#88CBFA", "#78B6FA", "#5AA2F6", "#408AF1")
grad_pink <- c("#ECC3EB", "#DF9DDD", "#D178CF", "#C353C0", "#A73CA4")
  # nord(palette = "silver_mine", n = 50, reverse = TRUE)

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
  dplyr::select(NAME, LON, LAT) %>% 
  mutate(NAME = recode(NAME, "United Kingdom" = "UK",
                             "United States" = "USA",
                             "Czech Rep." = "Czech Republic"))

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
  mutate_at(vars(residence, origin), list(~recode(., "US" = "USA")))

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
  left_join(origins, by = "origin") 

res_freq <- locations %>% group_by(residence) %>% summarise(n())
ori_freq <- locations %>% group_by(origin) %>% summarise(n())

# >> make map ----
# create world base map
worldmap <- map_data("world")
# filter PFTC5 countries
# extract vector of unique country names
pftc5_countries <- locations %>% 
  select(origin, residence) %>% 
  unlist() %>% 
   unique() #%>% 
  # # recode country names back, for UK, US, CZ
  # recode(., 
  #        "United Kingdom" = "UK",
  #        "United States" = "USA",
  #        "Czech Rep." = "Czech Republic")

# extract polygons for residences
pftc5_polygons_res <- worldmap %>%
  filter(region %in% pftc5_countries) %>% 
  select(-contains("ori")) %>% 
  left_join(pftc5_locations, by = c("region" = "residence"))

# # extract polygons for origins
# pftc5_polygons_ori <- worldmap %>%
#   filter(region %in% pftc5_countries) %>% 
#   left_join(pftc5_locations, by = c("region" = "origin"))

# >> a) PFTC1 ----

(pftc1_map <- ggplot() +
  # base world map
  geom_polygon(data = worldmap, aes(x = long, y = lat, group = group), fill = "grey", alpha = .4) +
  # top map layer with PFTC1 countries
  geom_polygon(data = pftc1_polygons_res, #  %>% filter(region %in% pftc1_locations$residence) to not display other origin countries
               aes(x = long, y = lat, group = group, fill = n_people_res)) +
  # plot outline of PFTC country
  geom_polygon(data = pftc1_polygons_res %>% filter(region == "China"), aes(x = long, y = lat), colour = "black", size = 1.4, fill = NA) +
  # add gradient colour fill
  scale_fill_gradientn(colours = grad_blue, 
                       na.value = "grey60",
                       breaks = c(1, 12)) + 
  labs(fill = "No. people\n") +
  coord_quickmap() +
  theme_bw() +
  theme(panel.grid = element_blank(),
        legend.position = "bottom"))


# >> b) PFTC2 ----

(pftc2_map <- ggplot() +
    # base world map
    geom_polygon(data = worldmap, aes(x = long, y = lat, group = group), fill = "grey", alpha = .4) +
    # top map layer with PFTC2 countries
    geom_polygon(data = pftc2_polygons_res, #  %>% filter(region %in% pftc2_locations$residence) to not display other origin countries
                 aes(x = long, y = lat, group = group, fill = n_people_res)) +
    # plot outline of PFTC country
    geom_polygon(data = pftc2_polygons_res %>% filter(region == "China"), aes(x = long, y = lat), colour = "black", size = 1.4, fill = NA) +
    # add gradient colour fill
    scale_fill_gradientn(colours = grad_red, 
                         na.value = "grey60",
                         breaks = c(1, 12)) + 
    labs(fill = "No. people\n") +
    coord_quickmap() +
    theme_bw() +
    theme(panel.grid = element_blank(),
          legend.position = "bottom"))


# >> c) PFTC3 ----

(pftc3_map <- ggplot() +
    # base world map
    geom_polygon(data = worldmap, aes(x = long, y = lat, group = group), fill = "grey", alpha = .4) +
    # top map layer with PFTC3 countries
    geom_polygon(data = pftc3_polygons_res, #  %>% filter(region %in% pftc3_locations$residence) to not display other origin countries
                 aes(x = long, y = lat, group = group, fill = n_people_res)) +
    # plot outline of PFTC country
    geom_polygon(data = pftc3_polygons_res %>% filter(region == "Peru"), aes(x = long, y = lat), colour = "black", size = 1.4, fill = NA) +
    # add gradient colour fill
    scale_fill_gradientn(colours = grad_red, 
                         na.value = "grey60",
                         breaks = c(1, 12)) + 
    labs(fill = "No. people\n") +
    coord_quickmap() +
    theme_bw() +
    theme(panel.grid = element_blank(),
          legend.position = "bottom"))


# >> d) PFTC4 ----

(pftc4_map <- ggplot() +
    # base world map
    geom_polygon(data = worldmap, aes(x = long, y = lat, group = group), fill = "grey", alpha = .4) +
    # top map layer with PFTC4 countries
    geom_polygon(data = pftc4_polygons_res, #  %>% filter(region %in% pftc4_locations$residence) to not display other origin countries
                 aes(x = long, y = lat, group = group, fill = n_people_res)) +
    # plot outline of PFTC country
    geom_rect(data = pftc5_polygons_res %>% filter(subregion == "Svalbard"), 
              aes(xmin = min(long)-3, 
              xmax = max(long), 
              ymin = min(lat)-2, 
              ymax = max(lat)+5), 
              colour = "black", size = 1.4, fill = NA) +
    # add gradient colour fill
    scale_fill_gradientn(colours = grad_red, 
                         na.value = "grey60",
                         breaks = c(1, 12)) + 
    labs(fill = "No. people\n") +
    coord_quickmap() +
    theme_bw() +
    theme(panel.grid = element_blank(),
          legend.position = "bottom"))


# >> e) PFTC5 ----

(pftc5_map <- ggplot() +
    # base world map
    geom_polygon(data = worldmap, aes(x = long, y = lat, group = group), fill = "grey", alpha = .4) +
    # top map layer with PFTC5 countries
    geom_polygon(data = pftc5_polygons_res, #  %>% filter(region %in% pftc5_locations$residence) to not display other origin countries
                 aes(x = long, y = lat, group = group, fill = n_people_res)) +
    # plot outline of PFTC country
    geom_polygon(data = pftc5_polygons_res %>% filter(region == "Peru"), aes(x = long, y = lat), colour = "black", size = 1.4, fill = NA) +
    # add gradient colour fill
    scale_fill_gradientn(colours = grad_red, 
                         na.value = "grey60",
                         breaks = c(1, 12)) + 
    labs(fill = "No. people\n") +
    coord_quickmap() +
    theme_bw() +
    theme(panel.grid = element_blank(),
          legend.position = "bottom"))


