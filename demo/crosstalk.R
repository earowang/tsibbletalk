library(plotly)
library(tsibble)
library(feasts)
library(crosstalk)

SharedTsibbleData <- R6::R6Class(
  classname = "SharedTsibbleData",
  inherit = crosstalk::SharedData,
  public = list(
    print = function(...) {
      print(self$origData(), ...)
    }
  )
)

as_shared_tsibble <- function(x) {
  SharedTsibbleData$new(data = x, key = parse_key_val(x))
}

parse_key_val <- function(data, key = NULL) {
  if (is_null(key)) {
    key <- tsibble::key_vars(data)
  }
  vec_c(!!!pmap(as.list(data)[key], paste, sep = "/"))
}

features.SharedTsibbleData <- function(.tbl, .var, features, ...) {
  out <- fabletools::features(.tbl$origData(), {{ .var }}, features, ...)
  reconstruct_shared_tsibble(out, .tbl)
}

reconstruct_shared_tsibble <- function(data, template) {
  SharedTsibbleData$new(data,
    key = parse_key_val(data, key_vars(template$origData())),
    group = template$groupName())
}

tour <- tourism %>%
  group_by(State, Purpose) %>%
  summarise(Trips = sum(Trips)) %>%
  ungroup() %>%
  as_shared_tsibble()

tour_feat <- tour %>%
  features.SharedTsibbleData(Trips, feat_stl)

g1 <- ggplotly({tour %>%
    ggplot(aes(x = Quarter, y = Trips, group = State)) +
    geom_line() +
    facet_wrap(~ Purpose)})
g2 <- ggplotly({tour_feat %>%
    ggplot(aes(x = trend_strength, y = seasonal_strength_year)) +
    geom_point() +
    facet_wrap(~ Purpose)})

subplot(g1, g2) %>% highlight("plotly_selected")

ped <- pedestrian %>%
  # filter(Sensor %in% c("Bourke Street Mall (North)", "Southern Cross Station")) %>%
  filter(
    as.Date('2015-02-17') <= Date,
    Date <= as.Date('2015-02-21')
  )

ped_feat <- ped %>%
  features(Count, feat_stl)

ped_shared <- highlight_key(ped, ~ Sensor, group = 'ped')
g1 <- ggplotly({ped_shared %>%
    ggplot(aes(x = Date_Time, y = Count, colour = Sensor)) +
    geom_line()})
ped_feat_shared <- highlight_key(ped_feat, ~ Sensor, group = 'ped')
g2 <- ggplotly({ped_feat_shared %>%
    ggplot(aes(x = trend_strength, y = seasonal_strength_day)) +
    geom_point()})

subplot(g1, g2) %>% highlight("plotly_selected")

ped_full <- ped %>%
  dplyr::left_join(ped_feat)

ped_shared <- highlight_key(ped_full, ~ Sensor)
g1 <- ggplotly({ped_shared %>%
  ggplot(aes(x = Date_Time, y = Count, colour = Sensor)) +
  geom_line()})
g2 <- ggplotly({ped_shared %>%
  ggplot(aes(x = trend_strength, y = seasonal_strength_day)) +
  geom_point()})

subplot(g1, g2) %>% highlight("plotly_selected")

ped_feat_shared <- highlight_key(ped_feat, ~ Sensor)
g1 <- ggplotly({ped_feat_shared %>%
    ggplot(aes(x = spikiness, y = linearity)) +
    geom_point()})
g2 <- ggplotly({ped_feat_shared %>%
    ggplot(aes(x = trend_strength, y = seasonal_strength_day)) +
    geom_point()})

subplot(g1, g2) %>% highlight("plotly_selected")
