library(crosstalk)
library(plotly)
library(lubridate)
library(tsibble)

dice_tsibble <- function(data, unit = NULL) {
  stopifnot(is_tsibble(data))
  mutate(
    data,
    # Date = floor_date(Date, unit = paste(unit, "days")),
    Date = floor_date_anchor(Date_Time, unit = unit, as_date('2014-12-29')),
    Time = as.numeric(dice_date(Date_Time, Date)), # linear scale
    Date = as.factor(Date)
  )
}

floor_date_anchor <- function(x, unit, anchor) {
  diff <- as.double(as_date(x)) - as.double(as_date(anchor))
  anchor + days(floor(diff / unit) * unit)
}

sx <- pedestrian %>%
  filter(Sensor %in% c("Bourke Street Mall (North)", "Southern Cross Station")) %>%
  filter(
    as.Date('2015-02-17') <= Date,
    Date <= as.Date('2015-04-21')
  )

p0 <- ggplotly({sx %>%
    ggplot(aes(x = Time, y = Count, group = Date)) +
    geom_line() +
    facet_wrap(~ Sensor)})
p <- ggplotly({sx %>%
    dice_tsibble(unit = 3) %>%
    ggplot(aes(x = Time, y = Count, colour = Date)) +
    geom_line() +
    facet_wrap(~ Sensor) +
    theme(legend.position = "none")})
 # facet and colour alternate

p$x$data[[1]]$x
p$x$data[[2]]$x
p$x$data[[3]]$x
plotlyReactData(as_tibble(dice_tsibble(sx, unit = 3)), p0)[[3]]$x

data <- as_tibble(dice_tsibble(sx, unit = 3))
p0$x$data[[1]]$showlegend

ui <- fluidPage(
  headerPanel(h1("Dynamic data in Plotly", align = "center")),
  br(),
  div(sliderInput("unit", "Unit:", min = 1, max = 14, value = 1), align = "center"),
  br(),
  div(plotlyOutput("plot"))
)

server <- function(input, output, session) {
  p0 <- ggplotly({
    sx %>%
    ggplot(aes(x = Time, y = Count, group = Date, colour = Sensor)) +
    geom_line()})
  # p0 <- sx %>% group_by(Date) %>%
  #   plot_ly(x = ~ Time, y = ~ Count) %>%
  #   add_lines()
  output$plot <- renderPlotly(p0)
  observeEvent(input$unit, {
    new <- dice_tsibble(sx, input$unit)
    plotlyReact("plot", new, p0)
  })
}

shinyApp(ui, server)

# Testing facet
ped <- pedestrian %>%
  fill_gaps() %>%
  filter(Date <= as.Date('2015-03-01'))

p1 <- ggplotly({
  ped %>%
    dice_tsibble(unit = 3) %>%
    ggplot(aes(x = Time, y = Count, colour = Date)) +
    geom_line() +
    facet_wrap(~ Sensor, nrow = 2, scales = "free_y")})

server <- function(input, output, session) {
  p0 <- ggplotly({
    ped %>%
      ggplot(aes(x = Time, y = Count, group = Date)) +
      geom_line() +
      facet_wrap(~ Sensor, nrow = 2, scales = "free_y")})
  output$plot <- renderPlotly(p0)
  observeEvent(input$unit, {
    new <- dice_tsibble(ped, input$unit)
    plotlyReact("plot", new, p0)
  })
}

shinyApp(ui, server)

sx %>%
  mutate(
    # Date = yearweek(Date),
    Date = yearweek(floor_date(Date, '14 days')),
    Time = dice_date(Date_Time, Date)
  ) %>%
  ggplot(aes(x = Time, y = Count, group = Date)) +
  geom_line() +
  NULL
  # scale_x_time(
  #   breaks = hms::hms(hour = seq(from = 12, by = 24, length.out = 7)),
  #   labels = c("M", "T", "W", "T", "F", "S", "S")
  # )
p
