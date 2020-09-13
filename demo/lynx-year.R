library(tsibble)
library(ggplot2)
library(shiny)

lynx_tsbl <- as_tsibble(lynx)

ui <- fluidPage(
  tsibbleWrapUI("dice")
)

server <- function(input, output, session) {
  p0 <- lynx_tsbl %>%
    ggplot(aes(x = index, y = value)) +
    geom_line(size = .2)
  tsibbleWrapServer("dice", p0, period = "5 year")
}
shinyApp(ui, server)
