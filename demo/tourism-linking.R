library(ggplot2)
library(plotly)
library(tsibble)
library(tsibbletalk)
library(feasts)

# nesting and crossing
tourism_shared <- tourism %>%
  as_shared_tsibble(spec = (State / Region) * Purpose)

tourism_feat <- tourism_shared %>%
  features(Trips, feat_stl) %>%
  mutate(strong_trend = trend_strength > .5)

p0 <- plotly_key_tree(tourism_shared, height = 900, width = 600)
p1 <- tourism_shared %>%
  ggplot(aes(x = Quarter, y = Trips)) +
  geom_line(aes(group = Region), alpha = 0.5) +
  facet_wrap(~ Purpose, scales = "free_y")
p2 <- tourism_feat %>%
  ggplot(aes(x = trend_strength, y = seasonal_strength_year)) +
  geom_point(aes(group = Region))
p4 <- tourism_feat %>%
  plot_ly() %>%
  add_histogram(x = ~ strong_trend)

subplot(p0,
  subplot(
    ggplotly(p1, tooltip = "Region", width = 900),
    ggplotly(p2, tooltip = "Region", width = 900),
    nrows = 2),
  widths = c(.4, .6)) %>%
  highlight(dynamic = TRUE)

subplot(p0,
        subplot(
          ggplotly(p1, tooltip = "Region", width = 900),
          p4,
          nrows = 2),
        widths = c(.4, .6)) %>%
  layout(barmode = "overlay") %>%
  highlight(dynamic = TRUE)

# nesting only
library(dplyr)
library(crosstalk)
library(DT)
tourism_hier <- tourism %>%
  group_by(State, Region) %>%
  summarise(Trips = sum(Trips)) %>%
  ungroup() %>%
  as_shared_tsibble(spec = State / Region)

tourism_hier_feat <- tourism_hier %>%
  features(Trips, feat_stl)

p0 <- plotly_key_tree(tourism_hier, height = 900, width = 600)
p1 <- tourism_hier %>%
  ggplot(aes(x = Quarter, y = Trips, group = Region)) +
  geom_line()
p2 <- tourism_hier_feat %>%
  ggplot(aes(x = trend_strength, y = seasonal_strength_year)) +
  geom_point()
t1 <- datatable(tourism_hier_feat)

bscols(
  subplot(p0,
    subplot(
      ggplotly(p1, tooltip = "Region", width = 900),
      ggplotly(p2, tooltip = "Region", width = 900),
      nrows = 2),
    widths = c(.4, .6)) %>%
    highlight(dynamic = TRUE),
  t1
)

# model()
tourism2 <- tourism %>%
  filter(Region %in% c("Melbounre", "Sydney")) %>%
  as_shared_tsibble()
dcmp <- tourism2 %>%
  model(STL(Trips ~ season(window = Inf)))
components(dcmp)

library(fable)
tourism_f <- tourism2 %>%
  model(ets = ETS(Trips)) %>%
  forecast(h = "2 years")
tourism_f %>%
  ggplot(aes(x = Quarter, y = Trips))
