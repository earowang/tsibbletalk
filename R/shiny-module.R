#' A shiny module to easily slice and dice tsibble index for visualising periodicity
#'
#' A pair of UI and server functions: `tsibbleWrapUI()` and `tsibbleWrapServer()`.
#'
#' @param id A unique shiny id.
#' @param plot A `ggplot` or `plotly` object.
#' @param period A string passed to [lubridate::period()] to specify the minimum
#' seasonal period, for example `"1 day"`.
#' @name tsibble-wrap
#' @examples
#' if (interactive()) {
#'   library(tsibble)
#'   library(dplyr)
#'   library(shiny)
#'   library(ggplot2)
#'   p <- tourism %>%
#'     filter(Region %in% c("Melbourne", "Sydney")) %>%
#'     ggplot(aes(x = Quarter, y = Trips, colour = Region)) +
#'     geom_line() +
#'     facet_wrap(~ Purpose, scales = "free_y") +
#'     theme(legend.position = "none")
#'
#'   ui <- fluidPage(tsibbleWrapUI("dice"))
#'   server <- function(input, output, session) {
#'     tsibbleWrapServer("dice", p, period = "1 year")
#'   }
#'   shinyApp(ui, server)
#' }
NULL

#' @importFrom shiny NS tagList uiOutput moduleServer observeEvent renderUI
#' @importFrom shiny sliderInput
#' @importFrom plotly plotlyOutput ggplotly renderPlotly plotly_build
#' @rdname tsibble-wrap
#' @export
tsibbleWrapUI <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns("period")),
    plotlyOutput(ns("plot"))
  )
}

#' @rdname tsibble-wrap
#' @export
tsibbleWrapServer <- function(id, plot, period) {
  moduleServer(
    id,
    function(input, output, session) {
      if (is_ggplot(plot)) {
        data <- plot$data
        plot <- ggplotly(plot)
      } else if (inherits(plot, "plotly")) {
        data <- plotly_data(plot)
        plot <- plotly_build(plot)
      } else {
        abort("`plot` must be a ggplot or plotly object.")
      }
      idx <- data[[tsibble::index_var(data)]]
      period <- parse_period(idx, period)
      output$period <- renderUI({
        ns <- session$ns
        sliderInput(
          ns("unit"), "",
          min = 0, max = period$max, value = period$max, step = period$unit,
          pre = period$label, animate = TRUE, width = "100%"
        )
      })
      output$plot <- renderPlotly(plot)
      observeEvent(input$unit, {
        new_data <- dice_tsibble(data, period$to, input$unit, period$scale)
        plotlyReact("plot", new_data, plot, clear = input$unit == 0)
      })
    }
  )
}

#' @importFrom lubridate period
parse_period <- function(x, period) {
  UseMethod("parse_period")
}

parse_period.POSIXt <- function(x, period) {
  is_week <- is_week_period(period)
  x_period <- period(period)
  scale <- 3600
  if (is_week) {
    to <- yearweek()
    unit <- as.double(gsub("([0-9]+).*$", "\\1", period))
    label <- "week "
  } else if (x_period$day != 0) {
    to <- new_date()
    unit <- x_period$day
    label <- "day "
  } else if (x_period$month != 0) {
    to <- yearmonth()
    unit <- x_period$month
    label <- "month "
  } else { # year
    to <- double()
    unit <- x_period$year
    label <- "year "
  }
  max <- vec_size(vec_unique(date_floor(x, to = to, unit = 1)))
  list(to = to, unit = unit, scale = scale, max = max, label = label)
}

parse_period.yearquarter <- function(x, period) {
  period <- period(period)
  to <- double()
  unit <- period$year
  scale <- 1
  label <- "year "
  max <- vec_size(vec_unique(date_floor(x, to = to, unit = 1))) + 1
  list(to = to, unit = unit, scale = scale, max = max, label = label)
}

parse_period.yearmonth <- parse_period.yearquarter

parse_period.numeric <- parse_period.yearquarter

#' @importFrom dplyr mutate as_tibble
dice_tsibble <- function(data, to, unit, scale) {
  idx <- tsibble::index(data)
  mutate(as_tibble(data),
    ".GROUP" := date_floor(!!idx, to = to, unit = unit),
    !!idx := date_dice(!!idx, .GROUP) / scale,
    ".GROUP" := as.factor(.GROUP)
  )
}

is_week_period <- function(x) {
  grepl("(w)|(week)", x, ignore.case = TRUE)
}
