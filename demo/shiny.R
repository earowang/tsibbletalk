library(crosstalk)
library(plotly)
library(lubridate)
library(tsibble)
library(rlang)

dice_tsibble <- function(data, unit = NULL) {
  # TODO: plotly's x and y
  stopifnot(is_tsibble(data))
  idx <- index(data)
  mutate(
    data,
    Date = floor_date(Date, unit = unit),
    Time = dice_date(Date_Time, Date)
  )
}

ggplotly({sx %>%
    dice_tsibble(unit = '3 days') %>%
    ggplot(aes(x = Time, y = Count, group = Date)) +
    geom_line()})

sx <- pedestrian %>%
  filter(Sensor == "Southern Cross Station") %>%
  filter(Date <= as.Date('2015-01-14'))

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
    ggplot(aes(x = Time, y = Count, group = Date)) +
    geom_line()})
  # p0 <- sx %>% group_by(Date) %>%
  #   plot_ly(x = ~ Time, y = ~ Count) %>%
  #   add_lines()
  output$plot <- renderPlotly(p0)
  observeEvent(input$unit, {
    new <- dice_tsibble(sx, paste(input$unit, "days"))
    plotlyReact("plot", new, p0)
  })
}

shinyApp(ui, server)

sx %>%
  mutate(
    # Date = yearweek(Date),
    Date = yearweek(floor_date(Date, '14 days')),
    Time = dice_date.POSIXt.yearweek(Date_Time, Date)
  ) %>%
  ggplot(aes(x = Time, y = Count, group = Date)) +
  geom_line() +
  NULL
  # scale_x_time(
  #   breaks = hms::hms(hour = seq(from = 12, by = 24, length.out = 7)),
  #   labels = c("M", "T", "W", "T", "F", "S", "S")
  # )
p
