library(plotly)
library(lubridate)
library(tsibble)
library(shiny)
library(dplyr)

dice_tsibble <- function(data, unit = NULL) {
  stopifnot(is_tsibble(data))
  mutate(
    data,
    # Date = floor_date(Date, unit = paste(unit, "days")),
    Date = floor_date_anchor(Date_Time, unit = unit, as_date('2014-12-29')),
    Time = as.numeric(dice_date(Date_Time, Date)) / 3600, # linear scale
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
    Date <= as.Date('2015-02-21')
  ) %>%
  mutate()

p0 <- ggplotly({sx %>%
    ggplot(aes(x = Time, y = Count, colour = Date)) +
    geom_line() +
    facet_wrap(~ Sensor)})
library(ggplot2)

ui <- fluidPage(
  headerPanel(h1("Dynamic data in Plotly", align = "center")),
  br(),
  div(sliderInput("unit", "Unit:", min = 1, max = 14, value = 1), align = "center"),
  br(),
  div(plotlyOutput("plot"))
)

server <- function(input, output, session) {
  p0 <- sx %>%
    ggplot(aes(x = Time, y = Count, group = Date, colour = Sensor)) +
    geom_line() +
    facet_wrap(~ Sensor)
  # p0 <- sx %>% group_by(Date) %>%
  #   plot_ly(x = ~ Time, y = ~ Count, color = ~ Sensor) %>%
  #   add_lines()
  output$plot <- renderPlotly(ggplotly(p0) %>% layout(xaxis = list(autorange = TRUE)))
  observeEvent(input$unit, {
    new <- dice_tsibble(sx, input$unit)
    plotlyReact("plot", new, p0)
  })
}
server <- function(input, output, session) {
  p0 <- ggplotly({sx %>%
    ggplot(aes(x = Time, y = Count, group = Date, colour = Sensor)) +
    geom_line() +
    facet_wrap(~ Sensor)}) %>% layout(xaxis = list(autorange = TRUE))
  # p0 <- sx %>% group_by(Date) %>%
  #   plot_ly(x = ~ Time, y = ~ Count, color = ~ Sensor) %>%
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

sx <- pedestrian %>%
  filter(Sensor %in% c("Southern Cross Station")) %>%
  filter(Date <= as.Date('2015-06-30'))

sx %>%
  mutate(
    # Date = yearmonth(Date),
    Date = yearmonth(floor_date(Date, '2 months')),
    Time = dice_date(Date_Time, Date),
    Date = as.factor(Date)
  ) %>%
  ggplot(aes(x = Time, y = Count, colour = Date)) +
  geom_line() +
  NULL
  # scale_x_time(
  #   breaks = hms::hms(hour = seq(from = 12, by = 24, length.out = 7)),
  #   labels = c("M", "T", "W", "T", "F", "S", "S")
  # )

sx <- pedestrian %>%
  filter(Sensor %in% c("Southern Cross Station")) %>%
  filter(as.Date('2015-01-05') <=  Date, Date <= as.Date('2015-01-21'))

sx %>%
  mutate(
    # Date = yearmonth(Date),
    Date = yearweek(floor_date(yearweek(Date), '1 week')),
    Time = dice_date.POSIXt.yearweek(Date_Time, Date),
    Date = as.factor(as_date(Date))
  ) %>%
  ggplot(aes(x = Time, y = Count, colour = Date)) +
  geom_line() +
  NULL
