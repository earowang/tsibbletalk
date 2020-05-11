clean_plotly_attrs <- function(x) {
  stopifnot(is_formula(x))
  nm <- f_name(x)
  gsub("^~", "", nm)
}

is_faceted <- function(layout) {
  sum(grepl("axis", names(layout))) > 2
}

facet_by <- function(data, plotly_data) {
  nfacets <- vec_size(plotly_data)
  key <- vec_rep_each(
    seq_len(nfacets),
    # NA identifies the grouping info
    map_int(plotly_data, function(z) sum(!is.na(z$x))))
  vec_split(as_tibble(data), key)$val
}

is_aes_mapping_present <- function(plotly) {
  !is_null(plotly$x$attrs[[1]]$colour)
}

plotlyReactData <- function(data, plotly) {
  # TODO: split colour first and then facet
  if (is_aes_mapping_present(plotly)) {
    colour_chr <- clean_plotly_attrs(plotly$x$attrs[[1]]$colour)
    data <- dplyr::arrange(data, !!sym(colour_chr))
  }
  data_lst <- list(data)
  if (is_aes_mapping_present(plotly) || is_faceted(plotly$x$layout)) {
    data_lst <- facet_by(data, plotly$x$data)
  }
  x_chr <- clean_plotly_attrs(plotly$x$attrs[[1]]$x)
  y_chr <- clean_plotly_attrs(plotly$x$attrs[[1]]$y)
  grps <- dplyr::group_vars(plotly_data(plotly))
  if (!is_empty(grps)) {
    data_lst <- map(data_lst, function(x) plotly::group2NA(x, grps))
  }
  # TODO: missing 'text' as tooptip
  map2(
    data_lst,
    plotly$x$data,
    function(x, y) list2(x = x[[x_chr]], y = x[[y_chr]], !!!y[-(1:3)])
  )
}

plotlyReact <- function(outputId, data, plotly,
                        session = shiny::getDefaultReactiveDomain()) {
  plotly::plotlyProxyInvoke(
    plotly::plotlyProxy(outputId, session),
    "react",
    (plotlyReactData(data, plotly)),
    reset_x_range(plotly$x$layout),
    plotly$x$config
  )
}

reset_x_range <- function(layout) {
  # TODO: x as linear type but should be contextual ticktext
  xaxis <- names(layout)[grepl("xaxis", names(layout))]
  for (i in xaxis) {
    layout[[i]]$tickvals <- layout[[i]]$ticktext <- NULL
    layout[[i]]$autorange <- TRUE # ggplotly panel spacing gone
  }
  layout
}

