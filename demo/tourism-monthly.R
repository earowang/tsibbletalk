library(tsibble)
library(tsibbletalk)
library(tidyverse)
library(shiny)

melb_syd <- tourism_monthly %>%
  filter(Region %in% c("Melbourne", "Sydney"))

p0 <- melb_syd %>%
  ggplot(aes(x = Month, y = Trips, colour = Region)) +
  geom_line() +
  facet_wrap(~ Purpose, scales = "free_y") +
  theme(legend.position = "none")

ui <- fluidPage(
  tsibbleWrapUI("dice")
)
server <- function(input, output, session) {
  tsibbleWrapServer("dice", p0, period = "1 year")
}
shinyApp(ui, server)
