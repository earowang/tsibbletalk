library(plotly)
library(tsibble)
library(feasts)
library(dplyr)
library(crosstalk)

tourism_shared <- tourism %>%
  as_shared_tsibble(spec = (State / Region) * Purpose)

tourism_feat <- tourism_shared %>%
  features(Trips, feat_stl)

p0 <- plotly_key_tree(tourism_shared, height = 800, width = 600)
p1 <- tourism_shared %>%
  ggplot(aes(x = Quarter, y = Trips)) +
  geom_line(aes(group = Region), alpha = 0.5) +
  facet_wrap(~ Purpose, scales = "free_y")
p2 <- tourism_feat %>%
  ggplot(aes(x = trend_strength, y = seasonal_strength_year)) +
  geom_point(aes(group = Region))

subplot(p0,
  subplot(
    ggplotly(p1, tooltip = "Region", width = 800),
    ggplotly(p2, tooltip = "Region", width = 800),
    nrows = 2),
  widths = c(.4, .6)) %>%
  highlight(dynamic = TRUE)

tour <- tourism %>%
  group_by(State, Region) %>%
  summarise(Trips = sum(Trips)) %>%
  ungroup() %>%
  as_shared_tsibble(spec = State/ Region)

tour_feat <- tour %>%
  features(Trips, feat_stl)

g0 <- plotly_key_tree(tour)
g1 <- tour %>%
  ggplot(aes(x = Quarter, y = Trips, group = Region)) +
  geom_line()
g2 <- tour_feat %>%
  ggplot(aes(x = trend_strength, y = seasonal_strength_year)) +
  geom_point()

subplot(ggplotly(g1), ggplotly(g2), nrows = 2) %>%
  subplot(g0) %>%
  layout(width = 800) %>%
  highlight(dynamic = TRUE)

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

data <- tsibble::as_tsibble(hts::htseg2) %>%
  dplyr::distinct(`Level 1`, `Level 2`, `Level 3`)
plot_dendro2(new_dendrogram(data, c("Level 1", "Level 2", "Level 3")), data)

ped_sensor <- pedestrian %>% distinct(Sensor)
plot_dendro2(new_dendrogram(ped_sensor, "Sensor"), ped_sensor)

library(treemap)
data(GNI2014)
aa <- as_tibble(GNI2014)[c(3, 1)] %>%
  filter(continent %in% c("Oceania", "Asia"))
dendro <- new_dendrogram(aa, c("continent", "iso3"))
plot(dendro)
plot(dendro, type = "triangle")
plot_dendro2(dendro, data = aa, height = 670)

library(tsibble)
vctrs::vec_rbind(pedestrian, new_data(pedestrian))
