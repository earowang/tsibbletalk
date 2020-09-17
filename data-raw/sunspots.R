library(tidyverse)

sunspots2019 <- read_delim("data-raw/SN_y_tot_V2.0.csv", delim = ";",
  col_names = c("year", "number")) %>%
  mutate(year = as.integer(year), number = as.double(number)) %>%
  tsibble::as_tsibble(index = year)
usethis::use_data(sunspots2019, overwrite = TRUE)
