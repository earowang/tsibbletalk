library(plotly)
library(tsibble)
library(feasts)
library(crosstalk)

tour <- tourism %>%
  group_by(State, Region) %>%
  summarise(Trips = sum(Trips)) %>%
  ungroup() %>%
  as_shared_tsibble()

tour_feat <- tour %>%
  features.SharedTsibbleData(Trips, feat_stl)

g0 <- plotly_key_tree(tour)
g1 <- ggplotly({tour %>%
    ggplot(aes(x = Quarter, y = Trips, group = Region)) +
    geom_line()})
g2 <- ggplotly({tour_feat %>%
    ggplot(aes(x = trend_strength, y = seasonal_strength_year)) +
    geom_point()})

subplot(g1, g2, nrows = 2) %>%
  subplot(g0) %>%
  print()
  # highlight(persistent = TRUE, dynamic = TRUE)

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

library(plotly)
dend <- USArrests %>%
  dist() %>%
  hclust() %>%
  as.dendrogram() %>%
  plot_dendro(set = "A", xmin = -100, height = 900, width = 1100)
