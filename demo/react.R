library(plotly)
library(lubridate)
library(tsibble)
library(shiny)
library(dplyr)
library(ggplot2)


period("1 month")$day
period("1 month")$month

sx <- pedestrian %>%
  filter(Sensor %in% c("Bourke Street Mall (North)", "Southern Cross Station")) %>%
  filter(
    Date <= as.Date('2015-02-21')
  )

dice_tsibble(sx, "3 day")$.group[1:10]

ui <- fluidPage(
  headerPanel(h1("Dynamic data in Plotly", align = "center")),
  br(),
  div(sliderInput("unit", "Unit:", min = 1, max = 14, value = 1), align = "center"),
  br(),
  div(plotlyOutput("plot"))
)

server <- function(input, output, session) {
  p0 <- sx %>%
    ggplot(aes(x = Date_Time, y = Count, colour = Sensor)) +
    geom_line() +
    facet_wrap(~ Sensor)
  output$plot <- renderPlotly(ggplotly(p0) %>% layout(xaxis = list(autorange = TRUE)))
  observeEvent(input$unit, {
    new <- dice_tsibble(sx,  paste0(input$unit, "day"))
    plotlyReact("plot", new, p0)
  })
}
shinyApp(ui, server)

server <- function(input, output, session) {
  p0 <- sx %>%
    filter(Sensor %in% c("Southern Cross Station")) %>%
    plot_ly(x = ~ Date_Time, y = ~ Count, color = ~ Sensor) %>%
    add_lines()
  output$plot <- renderPlotly(p0)
  observeEvent(input$unit, {
    new <- dice_tsibble(sx %>% filter(Sensor %in% c("Southern Cross Station")), paste0(input$unit, "day"))
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
