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

#' @importFrom plotly plot_ly add_segments add_markers add_text layout
plot_dendro2 <- function(d, data, set = "A", height = 600, width = 500, ...) {
  labs <- vec_c(!!!map(unname(data), vec_unique))
  all_xy <- dplyr::arrange(get_xy(d), -y) %>% 
    dplyr::mutate("label" := c("root", labs))

  tidy_segments <- dendextend::as.ggdend(d)$segments
  all_txt <- dplyr::filter(all_xy, y == 0)

  axis <- list(
    title = "", showticklabels = FALSE, zeroline = FALSE, showgrid = FALSE
  )
  xaxis_rng <- -extendrange(all_xy[["y"]])
  max_nchar <- max(nchar(all_txt[["label"]]))

  all_xy %>%
    plot_ly(x = ~-y, y = ~x, color = I("black"), hoverinfo = "none",
      height = height, width = width) %>%
    add_segments(
      data = tidy_segments, xend = ~-yend, yend = ~xend, showlegend = FALSE
    ) %>%
    add_markers(
      data = dplyr::filter(all_xy, y > 0), key = ~label, set = set,
      text = ~label, hoverinfo = "text",
      showlegend = FALSE
    ) %>%
    add_text(
      data = all_txt, x = xaxis_rng[1], y = ~x, text = ~label, key = ~label,
      set = set, textposition = "middle right", showlegend = FALSE
    ) %>%
    layout(
      dragmode = "select",
      xaxis = c(axis, list(range = c(xaxis_rng[2], xaxis_rng[1] * max_nchar))),
      yaxis = c(axis, list(range = extendrange(all_xy[["x"]])))
    )
}

get_xy <- function(node) {
  m <- dendextend::get_nodes_xy(node)
  colnames(m) <- c("x", "y")
  tibble::as_tibble(m)
}

plot_key_tree <- function(data, height = 600, width = 500, ...) {
  # TODO: key needs to be linked
  template <- data
  data <- data$origData()
  key <- key(data)
  data <- select(distinct(data, !!!key), !!!key)
  dendro <- new_dendrogram(data, vars = names(data))
  plot_dendro2(dendro, data = data, set = template$groupName(),
    height = height, width = width, ...)
}
