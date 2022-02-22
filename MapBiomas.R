library(terra)
library(geobr)
library(tidyverse)

my_rast = rast('brasil_coverage_2020.tif') #file path

#plot(my_rast)

# how many layers?
nlyr(my_rast)

## GeoBR ##
# Read at a given year
mun <- read_municipality(code_muni="RJ", year=2020)
mun_code = mun %>% select(code_muni, geom) %>% vect()

# crop
cropped = crop(my_rast, mun_code)

#mask
masked = mask(cropped, mun_code)

# extracting data from raster
extraction = terra::extract(masked, mun_code)
save(extraction, file = 'extraction.rda') 

# Get forest coverage of municipalities
  # forest = c(3, 4, 5, 49)
n_pixels_mun = extraction %>%
  group_by(ID) %>%
  summarise(n_pixels = n())
n_floresta_mun = extraction %>%
  filter(brasil_coverage_2020 %in% c(3,4,5,49) ) %>%
  group_by(ID) %>%
  summarise(n_floresta = n())
mun_cobertura = left_join(n_pixels_mun, n_floresta_mun, by = 'ID') %>%
  mutate(percentual = (n_floresta/n_pixels))

# back to shapefile
mun_cobertura = mun %>%
  mutate(ID = seq_len(nrow(.))) %>%  #define ID
  left_join( mun_cobertura, by = "ID") #merge by ID

# plot
ggplot(mun_cobertura) +
  geom_sf(aes(fill = percentual) ) +
  scale_fill_distiller(type = 'seq', palette = 'Greens', 
                       direction = 1)+
  theme_classic()+
  labs(title = 'Cobertura Vegetal por município',
       caption = 'Mapa de municípios do estado do Rio de Janeiro', 
       fill = 'Cobertura vegetal')
ggsave('mapa cobertura vegetal.png')

# Top 10 cobertura vegetal
top_10 = mun_cobertura %>%
  as.data.frame() %>%
  select(name_muni, percentual) %>%
  arrange(desc(percentual)) %>%
  head(10) %>%
  mutate(percentual = round(percentual, 2)) %>%
  setNames(c('Município', 'Cobertural vegetal'))

write_csv(top_10, 'top_10.csv')


