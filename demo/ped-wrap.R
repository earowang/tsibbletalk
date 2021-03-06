library(plotly)
library(lubridate)
library(tsibble)
library(shiny)
library(dplyr)
library(ggplot2)

# ggplot: daily
sx <- pedestrian %>%
  filter(
    Sensor %in% c("Bourke Street Mall (North)", "Southern Cross Station"),
    Date <= as.Date('2015-12-31')
  )

ui <- fluidPage(
  tsibbleWrapUI("dice")
)

server <- function(input, output, session) {
  p0 <- sx %>%
    ggplot(aes(x = Date_Time, y = Count, colour = Sensor)) +
    geom_line(size = .2) +
    facet_wrap(~ Sensor) +
    theme(legend.position = "none")
  tsibbleWrapServer("dice", p0, period = "1 day")
}
shinyApp(ui, server)

# plotly
sx2 <- sx %>%
  filter(Sensor %in% c("Southern Cross Station"))

server <- function(input, output, session) {
  p0 <- sx2 %>%
    plot_ly(x = ~ Date_Time, y = ~ Count, color = ~ Sensor) %>%
    add_lines()
  tsibbleWrapServer("dice", p0, "1 day")
}
shinyApp(ui, server)

# weekly
server <- function(input, output, session) {
  p0 <- sx %>%
    ggplot(aes(x = Date_Time, y = Count, colour = Sensor)) +
    geom_line(size = .2) +
    facet_wrap(~ Sensor) +
    theme(legend.position = "none")
  tsibbleWrapServer("dice", p0, period = "1 week")
}
shinyApp(ui, server)

# monthly
server <- function(input, output, session) {
  p0 <- sx %>%
    ggplot(aes(x = Date_Time, y = Count, colour = Sensor)) +
    geom_line(size = .2) +
    facet_wrap(~ Sensor) +
    theme(legend.position = "none")
  tsibbleWrapServer("dice", p0, period = "1 month")
}
shinyApp(ui, server)


# annual
server <- function(input, output, session) {
  p0 <- pedestrian %>%
    filter(Sensor %in% c("Southern Cross Station")) %>%
    mutate(year = as.factor(year(Date_Time))) %>%
    ggplot(aes(x = Date_Time, y = Count, colour = year)) +
    geom_line(size = .2)
    theme(legend.position = "none")
  tsibbleWrapServer("dice", p0, period = "1 year")
}
shinyApp(ui, server)
