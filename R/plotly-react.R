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

plotlyReactData <- function(data, plotly) {
  data_lst <- list(data)
  if (is_faceted(plotly$x$layout)) {
    data_lst <- facet_by(data, plotly$x$data)
  }
  x_chr <- clean_plotly_attrs(plotly$x$attrs[[1]]$x)
  y_chr <- clean_plotly_attrs(plotly$x$attrs[[1]]$y)
  grps <- dplyr::group_vars(plotly_data(plotly))
  if (!is_empty(grps)) {
    data_lst <- map(data_lst, function(x) plotly::group2NA(x, grps))
  }
  # TODO: missing 'text' as tooptip and other aesthetics mapping
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
  xaxis <- names(layout)[grepl("xaxis", names(layout))]
  for (i in xaxis) {
    old <- layout[[i]]
    layout[[i]] <- NULL
    layout[[i]]$hoverformat <- old$hoverformat
    layout[[i]]$title <- old$title
    layout[[i]]$gridcolor <- old$gridcolor
    layout[[i]]$anchor <- old$anchor
    layout[[i]]$zeroline <- old$zeroline
    layout[[i]]$gridwidth <- old$gridwidth
    layout[[i]]$showgrid <- old$showgrid
    layout[[i]]$domain <- old$domain
    layout[[i]]$showline <- old$showline
    layout[[i]]$linewidth <- old$linewidth
    layout[[i]]$linecolor <- old$linecolor
    layout[[i]]$tickangle <- old$tickangle
    layout[[i]]$tickfont <- old$tickfont
    layout[[i]]$showticklabels <- old$showticklabels
    layout[[i]]$tickwidth <- old$tickwidth
    layout[[i]]$ticklen <- old$ticklen
    layout[[i]]$tickcolor <- old$tickcolor
    layout[[i]]$ticks <- old$ticks
    layout[[i]]$nticks <- old$nticks
    layout[[i]]$tickmode <- old$tickmode
    layout[[i]]$tickcolor <- old$tickcolor
    layout[[i]]$categoryorder <- old$categoryorder
    layout[[i]]$autorange <- TRUE
  }
  layout
}
