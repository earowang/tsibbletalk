#' A shiny module to easily slice and dice tsibble index for visualising periodicity
#'
#' A pair of UI and server functions: `tsibbleDiceUI()` and `tsibbleDiceServer()`.
#'
#' @param id A unique shiny id.
#' @param plot A `ggplot` or `plotly` object.
#' @param period A string passed to [lubridate::period()] to specify the minimul
#' seasonal period, for example `"1 day"`.
#' @name tsibble-dice
NULL

#' @importFrom shiny NS tagList uiOutput moduleServer observeEvent renderUI
#' @importFrom shiny sliderInput
#' @importFrom plotly plotlyOutput ggplotly renderPlotly
#' @rdname tsibble-dice
#' @export
tsibbleDiceUI <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns("period")),
    plotlyOutput(ns("plot"))
  )
}

#' @rdname tsibble-dice
#' @export
tsibbleDiceServer <- function(id, plot, period) {
  moduleServer(
    id,
    function(input, output, session) {
      if (is_ggplot(plot)) {
        data <- plot$data
        plot <- ggplotly(plot)
      } else {
        data <- plotly_data(plot)
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
        if (input$unit == period$max) return()
        new_data <- dice_tsibble(data, period$to, input$unit, period$scale)
        plotlyReact("plot", new_data, plot)
      })
    }
  )
}

#' @importFrom lubridate period
parse_period <- function(x, period) {
  period <- period(period)
  if (period$day != 0) {
    to <- new_date()
    unit <- period$day
    scale <- 3600
    label <- "day "
  }  
  max <- vec_size(vec_unique(date_floor(x, to = to, unit = 1))) + 1
  list(to = to, unit = unit, scale = scale, max = max, label = label)
}

#' @importFrom dplyr mutate as_tibble
dice_tsibble <- function(data, to, unit, scale) {
  idx <- tsibble::index(data)
  mutate(as_tibble(data),
    ".GROUP" := date_floor(!!idx, to = to, unit = unit),
    !!idx := date_dice(!!idx, .GROUP) / scale,
    ".GROUP" := as.factor(.GROUP)
  )
}
