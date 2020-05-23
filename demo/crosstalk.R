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

paste_data <- function(data) {
  vec_c(!!!pmap(data, paste, sep = "&"))
}

parse_key_val <- function(data, key = NULL) {
  if (is_null(key)) {
    key <- tsibble::key_vars(data)
  }
  paste_data(as.list(data)[key])
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

tour_less <- tourism %>%
  group_by(State, Region) %>%
  summarise(Trips = sum(Trips))

new_dendrogram <- function(x, vars) {
  if (has_length(x, 0)) return(x)
  if (is.data.frame(x)) {
    if (has_length(vars, 1)) {
      height <- length(x)
      members <- vec_size(vec_unique(x[[height]]))
      midpoint <- (members - 1) / 2
      lst <- map(
        vec_split(x, x[vars])$val, function(x)
          structure(as.character(x), label = as.character(x), members = 1,
            height = 0, leaf = TRUE)
      )
      attr(lst, "members") <- members
      attr(lst, "midpoint") <- midpoint
      attr(lst, "height") <- height
      structure(lst, class = "dendrogram")
    } else{
      height <- length(x)
      members <- vec_size(vec_unique(x[[height]]))
      midpoint <- (members - 1) / 2
      use <- vars[1]
      rest <- vars[-1]
      df_lst <- map(
        vec_split(x[names(x) != use], x[use])$val, function(x)
          structure(x, members = members, midpoint = midpoint, height = height)
      )
      attr(df_lst, "members") <- members
      attr(df_lst, "midpoint") <- midpoint
      attr(df_lst, "height") <- height
      new_dendrogram(df_lst, rest)
    }
  } else if (is.list(x)) {
    out <- map(x, new_dendrogram, vars)
    attr(out, "members") <- attr(x, "members")
    attr(out, "midpoint") <- attr(x, "midpoint")
    attr(out, "height") <- attr(x, "height")
    structure(out, class = "dendrogram")
  }
}

data <- tsibble::as_tsibble(hts::htseg2) %>%
  dplyr::distinct(`Level 1`, `Level 2`, `Level 3`)
plotly::plot_dendro(new_dendrogram(data, c("Level 1", "Level 2", "Level 3")),
                    xmin = -1)

ped_sensor <- pedestrian %>% distinct(Sensor)
plot_dendro(new_dendrogram(ped_sensor, "Sensor"))

tour_df <- tourism %>%
  # filter(State %in% c("Victoria", "New South Wales", "Queensland")) %>%
  distinct(State, Region)
plot(new_dendrogram(tour_df[c(2, 1)], c("State", "Region")))
plot_dendro2(new_dendrogram(tour_df[c(2, 1)], c("State", "Region")))

library(treemap)
aa <- as_tibble(GNI2014)[c(3, 1)]
plot(new_dendrogram(aa, c("continent", "iso3")))
plot_dendro2(new_dendrogram(aa, c("continent", "iso3")))

plot_dendro2 <- function(d, set = "A", xmin = -1, height = 500, width = 500, ...) {
  # get x/y locations of every node in the tree
  allXY <- get_xy(d)
  # get non-zero heights so we can split on them and find the relevant labels
  non0 <- allXY[["y"]][allXY[["y"]] > 0]
  # splitting on the minimum height would generate all terminal nodes anyway
  split <- non0[min(non0) < non0]
  # label is a list-column since non-zero heights have multiple labels
  # for now, we just have access to terminal node labels
  labs <- labels(d)
  allXY$label <- vector("list", nrow(allXY))
  allXY$label[[1]] <- labs
  allXY$label[allXY$y == 0] <- labs

  # collect all the *unique* non-trivial nodes
  nodes <- list()
  for (i in split) {
    dsub <- cut(d, i)$lower
    for (j in seq_along(dsub)) {
      s <- dsub[[j]]
      if (is.leaf(s)) next
      if (any(vapply(nodes, function(x) identical(x, s), logical(1)))) next
      nodes[[length(nodes) + 1]] <- s
    }
  }

  heights <- sapply(nodes, function(x) attr(x, "height"))
  labs <- lapply(nodes, labels)

  # NOTE: this won't support nodes that have the same height
  # but that isn't possible, right?
  # for (i in seq_along(heights)) {
  #   allXY$label[[which(allXY$y == heights[i])]] <- labs[[i]]
  # }

  tidy_segments <- dendextend::as.ggdend(d)$segments

  allTXT <- allXY[allXY$y == 0, ]

  blank_axis <- list(
    title = "",
    showticklabels = FALSE,
    zeroline = FALSE
  )

  allXY$members <- sapply(allXY$label, length)
  allTXT$label <- as.character(allTXT$label)

  allXY %>%
    plot_ly(x = ~y, y = ~x, color = I("black"), hoverinfo = "none",
            height = height, width = width) %>%
    add_segments(
      data = tidy_segments, xend = ~yend, yend = ~xend, showlegend = FALSE
    ) %>%
    add_markers(
      data = allXY[allXY$y > 0, ], key = ~label, set = set,
      text = ~paste0("members: ", members), hoverinfo = "text",
      showlegend = FALSE
    ) %>%
    add_text(
      data = allTXT, x = 0, y = ~x, text = ~label, key = ~label, set = set,
      textposition = "middle left", showlegend = FALSE
    ) %>%
    layout(
      dragmode = "select",
      xaxis = c(blank_axis, list(range = c(xmin, extendrange(allXY[["y"]])[2]))),
      yaxis = c(blank_axis, list(range = extendrange(allXY[["x"]])))
    )
}

get_xy <- function(node) {
  m <- dendextend::get_nodes_xy(node)
  colnames(m) <- c("x", "y")
  tibble::as_tibble(m)
}

plot_key_tree <- function(data, key = NULL, xmin = -1,
                          height = 500, width = 500, ...) {
  template <- data
  data <- data$origData()
  key <- enquo(key)
  if (quo_is_null(key)) {
    key <- key(data)
  } else {
    key <- syms(names(data)[tidyselect::eval_select(key, data = data)])
  }
  data <- select(distinct(data, !!!key), !!!key)
  dendro <- new_dendrogram(data, vars = names(data))
  plot_dendro2(dendro, set = template$groupName(), xmin = xmin,
    height = height, width = width, ...)
}
plot_key_tree(as_shared_tsibble(tourism), key = c(State, Region))
plot_key_tree(as_shared_tsibble(tourism))
