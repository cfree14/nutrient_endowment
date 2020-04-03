
# Theme
my_theme1 <- theme(axis.text=element_text(size=12),
                  axis.title=element_text(size=14),
                  plot.title=element_text(size=16),
                  legend.text=element_text(size=12),
                  legend.title=element_text(size=14),
                  strip.text=element_text(size=14))
my_theme <- my_theme1

# Countries
world <- rnaturalearth::ne_countries(scale="small", returnclass="sf") %>% 
  mutate(iso3_use=countrycode(subunit, "country.name", "iso3c"))

# Country centroids
world_pts <- world %>% 
  # sf::st_transform(crs="+proj=moll") +
  # sf::st_centroid(of_largest_polygon = T) + 
  sf::st_centroid(of_largest_polygon = T)
