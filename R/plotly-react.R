clean_plotly_attrs <- function(x) {
  stopifnot(is_formula(x))
  nm <- f_name(x)
  gsub("^~", "", nm)
}

plotlyReactData <- function(data, plotly) {
  grps <- dplyr::group_vars(plotly_data(plotly))
  x_chr <- clean_plotly_attrs(plotly$x$attrs[[1]]$x)
  y_chr <- clean_plotly_attrs(plotly$x$attrs[[1]]$y)
  if (!is_empty(grps)) {
    data <- plotly::group2NA(data, grps)
  }
  # missing 'text' as tooptip
  list2(
    x = data[[x_chr]],
    y = data[[y_chr]],
    !!!plotly$x$data[[1]][-(1:3)] # one data layer atm
  )
}

plotlyReact <- function(outputId, data, plotly,
                        session = shiny::getDefaultReactiveDomain()) {
  plotly::plotlyProxyInvoke(
    plotly::plotlyProxy(outputId, session),
    "react",
    list(plotlyReactData(data, plotly)),
    reset_x_range(plotly$x$layout),
    plotly$x$config
  )
}

reset_x_range <- function(layout) {
  old <- layout$xaxis
  layout$xaxis <- NULL
  layout$xaxis$hoverformat <- old$hoverformat
  layout$xaxis$title <- old$title
  layout$xaxis$gridcolor <- old$gridcolor
  layout$xaxis$anchor <- old$anchor
  layout$xaxis$zeroline <- old$zeroline
  layout$xaxis$gridwidth <- old$gridwidth
  layout$xaxis$showgrid <- old$showgrid
  layout$xaxis$domain <- old$domain
  layout$xaxis$showline <- old$showline
  layout$xaxis$linewidth <- old$linewidth
  layout$xaxis$linecolor <- old$linecolor
  layout$xaxis$tickangle <- old$tickangle
  layout$xaxis$tickfont <- old$tickfont
  layout$xaxis$showticklabels <- old$showticklabels
  layout$xaxis$tickwidth <- old$tickwidth
  layout$xaxis$ticklen <- old$ticklen
  layout$xaxis$tickcolor <- old$tickcolor
  layout$xaxis$ticks <- old$ticks
  layout$xaxis$nticks <- old$nticks
  layout$xaxis$tickmode <- old$tickmode
  layout$xaxis$tickcolor <- old$tickcolor
  layout$xaxis$categoryorder <- old$categoryorder
  layout$xaxis$type <- layout$xaxis$type
  layout$xaxis$autorange <- TRUE
  layout
}
