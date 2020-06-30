## code to prepare `tourism_monthly` dataset goes here
library(tidyverse)
library(tsibble)

tourism_raw <- read_csv("data-raw/tourism-monthly.csv", skip = 5)

tourism_meta <- tourism_raw %>% 
  slice(1:2) %>% 
  select(3:(ncol(tourism_raw) - 1)) %>% 
  t() %>% 
  as_tibble(.name_repair = ~ c("Region", "Purpose")) %>% 
  fill(Region, .direction = "down") %>% 
  mutate(region_purpose = paste(Region, Purpose, sep = "_"))

tourism_monthly <- tourism_raw %>% 
  fill(X1, .direction = "down") %>% 
  filter(X1 != "Total") %>% 
  slice(4:(n() - 1)) %>% 
  mutate(Month = yearmonth(paste(X1, `Summation Options`, sep = "-"))) %>% 
  select(-c(1:2, 311)) %>% 
  mutate(across(where(is.character), as.numeric)) %>% 
  rename_with(~ tourism_meta$region_purpose, where(is.numeric)) %>% 
  pivot_longer(
    -c("Month"), names_sep = "_", 
    names_to = c("Region", "Purpose"), values_to = "Trips"
  ) %>% 
  left_join(tourism %>% distinct(State, Region), by = "Region") %>% 
  relocate(State, .after = Month) %>% 
  mutate(State = if_else(is.na(State), "Queensland", State)) %>% 
  as_tsibble(index = Month, key = c(State, Region, Purpose))

usethis::use_data(tourism_monthly, overwrite = TRUE)
