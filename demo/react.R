library(plotly)
library(lubridate)
library(tsibble)
library(shiny)
library(dplyr)
library(ggplot2)

sx <- pedestrian %>%
  filter(Sensor %in% c("Bourke Street Mall (North)", "Southern Cross Station")) %>%
  filter(
    Date <= as.Date('2015-02-21')
  )

ui <- fluidPage(
  tsibbleDiceUI("dice")
)

server <- function(input, output, session) {
  p0 <- sx %>%
    ggplot(aes(x = Date_Time, y = Count, colour = Sensor)) +
    geom_line() +
    facet_wrap(~ Sensor) +
    theme(legend.position = "none")
  tsibbleDiceServer("dice", p0, period = "1 day")
}
shinyApp(ui, server)

sx2 <- sx %>%
  filter(Sensor %in% c("Southern Cross Station"))

server <- function(input, output, session) {
  p0 <- sx2 %>%
    plot_ly(x = ~ Date_Time, y = ~ Count, color = ~ Sensor) %>%
    add_lines()
  tsibbleDiceServer("a", sx2, "1 day", p0)
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
