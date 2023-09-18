new_dendrogram <- function(x, cols) {
  if (has_length(x, 0)) return(x)

  height <- length(x)
  members <- vec_size(vec_unique(x[[height]]))
  midpoint <- (members - 1) / 2

  if (is.data.frame(x)) {
    if (has_length(cols, 1)) {
      lst <- map(
        vec_split(x, x[cols])$val, function(x)
          structure(as.character(x), label = as.character(x), members = 1,
            height = 0, leaf = TRUE)
      )
      attr(lst, "members") <- members
      attr(lst, "midpoint") <- midpoint
      attr(lst, "height") <- height
      structure(lst, class = "dendrogram")
    } else{
      use <- cols[1]
      rest <- cols[-1]
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
    out <- map(x, new_dendrogram, cols)
    attr(out, "members") <- attr(x, "members")
    attr(out, "midpoint") <- attr(x, "midpoint")
    attr(out, "height") <- attr(x, "height")
    structure(out, class = "dendrogram")
  }
}

#' @importFrom plotly plot_ly add_segments add_markers add_text layout
#' @importFrom grDevices extendrange
plot_dendro2 <- function(d, data, cols, set, height = NULL, width = NULL, ...) {
  labs <- vec_c(!!!map(unname(data[cols]), vec_unique))
  key_vals <- paste_data(data)
  root_lab <- list(key_vals)
  nlist <- length(cols) - 1
  lab_lst <- vec_init(list(), n = nlist)
  for (i in seq_len(nlist)) {
    indices <- vec_group_loc(data[cols[i]])$loc
    lab_lst[[i]] <- map(indices, function(x) key_vals[x])
  }
  lab_lst <- vec_c(!!!lab_lst) # flat one level
  lab_lst <- vec_c(root_lab, lab_lst,
    vec_split(key_vals, data[tail(cols, 1)])$val)
  all_xy <- get_xy(d)
  all_xy <- vec_slice(all_xy, i=vec_order(all_xy$y, direction = "desc"))
  all_xy$label <- c("", labs)
  all_xy$key <- lab_lst

  tidy_segments <- dendextend::as.ggdend(d)$segments
  all_txt <- vec_slice(all_xy, all_xy$y == 0)

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
      data = all_xy, key = ~key, set = set, text = ~label, hoverinfo = "text",
      showlegend = FALSE
    ) %>%
    add_text(
      data = all_txt, x = xaxis_rng[1], y = ~x, text = ~label, key = ~key,
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
  as_tibble(m)
}

#' Plot nesting structures in shared tsibbles using plotly
#'
#' @param data A shared tsibble.
#' @inheritParams plotly::plot_dendro
#'
#' @export
#' @examples
#' if (interactive()) {
#'   shared_tourism <- as_shared_tsibble(tourism_monthly,
#'     spec = (State / Region) * Purpose)
#'   plotly_key_tree(shared_tourism)
#' }
plotly_key_tree <- function(data, height = NULL, width = NULL, ...) {
  if (!is_shared_tsibble_data(data)) {
    abort("`data` must be a shared tsibble data, created with `as_shared_tsibble()`.")
  }
  template <- data
  data <- as_tsibble(data)
  key <- key_vars(data)
  data <- vec_unique(data[key])[key]
  cols <- template$nesting()
  if (is_empty(cols)) {
    abort(c(
      "No nesting structure found.",
      i = "Please specify `spec` in `as_shared_tsibble()`."))
  }
  dendro <- new_dendrogram(vec_unique(data[cols]), cols)
  plot_dendro2(dendro, data = data, cols = cols, set = template$groupName(),
    height = height, width = width, ...)
}
