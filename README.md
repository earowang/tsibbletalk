
<!-- README.md is generated from README.Rmd. Please edit that file -->

# tsibbletalk

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
<!-- badges: end -->

The goal of tsibbletalk is to …

## Installation

You can install the development version of tsibbletalk from
[GitHub](https://github.com/) with:

``` r
# install.packages("remotes")
remotes::install_github("earowang/tsibbletalk")
```

## Get started

``` r
library(plotly)
library(feasts)
library(tsibble)
library(tsibbletalk)

tourism_shared <- tourism %>%
  as_shared_tsibble(spec = (State / Region) * Purpose)

tourism_feat <- tourism_shared %>%
  features(Trips, feat_stl)

p0 <- plotly_key_tree(tourism_shared, height = 900, width = 600)
p1 <- tourism_shared %>%
  ggplot(aes(x = Quarter, y = Trips)) +
  geom_line(aes(group = Region), alpha = 0.5) +
  facet_wrap(~ Purpose, scales = "free_y")
p2 <- tourism_feat %>%
  ggplot(aes(x = trend_strength, y = seasonal_strength_year)) +
  geom_point(aes(group = Region))

subplot(p0,
  subplot(
    ggplotly(p1, tooltip = "Region", width = 900),
    ggplotly(p2, tooltip = "Region", width = 900),
    nrows = 2),
  widths = c(.4, .6)) %>%
  highlight(dynamic = TRUE)
```

![](man/figures/tourism-crosstalk.gif)

``` r
library(shiny)
ped2015 <- pedestrian %>%
  filter_index(~ "2015") %>% 
  fill_gaps()
ui <- fluidPage(tsibbleDiceUI("tswrap"))
server <- function(input, output, session) {
  p <- ped2015 %>%
    ggplot(aes(x = Date_Time, y = Count, colour = Sensor)) +
    geom_line(size = .2) +
    facet_wrap(~ Sensor, scales = "free_y") +
    theme(legend.position = "none")
  tsibbleDiceServer("tswrap", p, period = "1 day")
}
shinyApp(ui, server)
```

![](man/figures/shiny-wrap.gif)
