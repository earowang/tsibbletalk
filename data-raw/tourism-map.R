library(tidyverse)
library(sf)

# data src:
# https://www.abs.gov.au/AUSSTATS/abs@.nsf/DetailsPage/1270.0.55.003July%202016?OpenDocument
aust_tr <- st_read("data-raw/tr_2016_aust_shape/TR_2016_AUST.shp") %>% 
  select(TR_NAME16)
aust_tr_s <- st_simplify(aust_tr, dToleranc = .05)
plot(aust_tr_s)

library(tsibbletalk)
tourism_monthly

ggplot(aust_tr_s) +
  geom_sf()
