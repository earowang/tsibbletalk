# Explanation of temporal model diagnostics
# Load libraries
library(tidyverse)
library(lubridate)
library(tsibble)
library(sugrrants)
library(broom)

# Read in pre-collected data, and plot it
walk_2018 <- read_rds(here::here("data-raw/walk_2018.rds")) %>%
  filter(
    Sensor %in% c(
      "Southern Cross Station",
      "Melbourne Central",
      "Flinders Street Station Underpass",
      "Birrarung Marr"
    )
  ) %>%
  mutate(
    mday = mday(Date),
    month = month(Date),
    year = year(Date),
    day = yday(Date)
  )

# Add holiday info,and plot again
hols_2018 <- tsibble::holiday_aus(year = 2018, state = "VIC")
walk_2018_hols <- walk_2018 %>%
  mutate(
    weekday = wday(Date_Time, label = TRUE, week_start = 1),
    workday = if_else(
      condition = (Date %in% hols_2018$date) |
        weekday %in% c("Sat", "Sun"),
      true = "non-workday",
      false = "workday"
    )
  )

# Closer look at Flinders
flinders <- walk_2018_hols %>%
  filter(Sensor == "Flinders Street Station Underpass")

# Read weather data, and join to counts
melb_weather_2018 <-
  read_csv(here::here("data-raw/melb_ncdc_2018.csv")) %>%
  mutate(
    high_prcp = if_else(
      condition = prcp > 5,
      true = "rain",
      false = "none"
    ),
    high_temp = if_else(
      condition = tmax > 33,
      true = "hot",
      false = "not"
    ),
    low_temp = if_else(
      condition = tmin < 6,
      true = "cold",
      false = "not"
    )
  )
melb_walk_weather_2018 <- walk_2018_hols %>%
  left_join(melb_weather_2018,
            by = c("Date" = "date"))

# Select set of varibales for modeling
melb_walk_weather_prep_lm <- melb_walk_weather_2018 %>%
  filter(Sensor == "Flinders Street Station Underpass") %>%
  mutate_at(.vars = vars(
    Sensor,
    Time,
    month,
    year,
    workday,
    high_prcp,
    high_temp,
    low_temp
  ),
  as_factor) %>%
  mutate(log_count = log1p(Count))

# Fit a simple  linear model to log count
walk_fit_lm <- lm(
  formula = log_count ~ Time + month + workday + high_prcp + high_temp + low_temp,
  data = melb_walk_weather_prep_lm
)

glance(walk_fit_lm)
peds_aug_lm <- augment(walk_fit_lm,
                       data = melb_walk_weather_prep_lm)
ggplot(peds_aug_lm,
       aes(x = log_count,
           y = .fitted)) +
  geom_point() +
  facet_wrap( ~ Sensor)

# Overall plot is pretty useless! But its because
# there are multiple sensors and these are not in the model
flinders_lm <- peds_aug_lm %>%
  filter(Sensor == "Flinders Street Station Underpass") %>%
  select(Date_Time, Date, Time, Count, .fitted, log_count) %>%
  pivot_longer(
    cols = c(log_count, .fitted),
    names_to = "model",
    values_to = "log_count"
  ) %>%
  mutate(Count = expm1(log_count))

# Closer inspection of fitted and observed in a calendar plot
flinders_lm_cal <- flinders_lm %>%
  frame_calendar(x = Time, y = Count, date = Date)
gg_cal <- ggplot(flinders_lm_cal) +
  geom_line(
    data = filter(flinders_lm_cal,  model == "log_count"),
    aes(
      x = .Time,
      y = .Count,
      colour = model,
      group = Date
    )
  ) +
  geom_line(
    data = filter(flinders_lm_cal, model == ".fitted"),
    aes(
      x = .Time,
      y = .Count,
      colour = model,
      group = Date
    )
  )
prettify(gg_cal) + theme(legend.position = "bottom")

library(plotly)
library(shiny)

floor_date_anchor <- function(x, unit, anchor) {
  diff <- as.double(as_date(x)) - as.double(as_date(anchor))
  anchor + days(floor(diff / unit) * unit)
}

dice_tsibble <- function(data, unit = NULL) {
  mutate(
    data,
    Date = floor_date_anchor(Date_Time, unit = unit, as_date('2018-01-01')),
    Time = as.numeric(dice_date(Date_Time, Date)) / 3600, # linear scale
    Date = as.factor(Date)
  )
}

ui <- fluidPage(
  headerPanel(h1("Diagnostics for temporal data modelling", align = "center")),
  br(),
  div(sliderInput("unit", "Unit:", min = 1, max = 30, value = 1), align = "center"),
  br(),
  div(plotlyOutput("plot"))
)

server <- function(input, output, session) {
  p0 <- ggplotly({
    flinders_lm %>%
      ggplot(aes(x = Time, y = Count, group = Date, colour = model)) +
      geom_line(alpha = 0.7)}) %>% layout(xaxis = list(autorange = TRUE))
  output$plot <- renderPlotly(p0)
  observeEvent(input$unit, {
    new <- dice_tsibble(flinders_lm, input$unit)
    plotlyReact("plot", new, p0)
  })
}

shinyApp(ui, server)

dice_tsibble <- function(data, unit = NULL) {
  mutate(
    data,
    Date = yearmonth(floor_date(Date, paste(unit, 'months'))),
    Time = as.numeric(dice_date(Date_Time, Date)) / 3600,
    Date = as.factor(Date)
  )
}

ui <- fluidPage(
  headerPanel(h1("Diagnostics for temporal data modelling", align = "center")),
  br(),
  div(sliderInput("unit", "Unit:", min = 1, max = 4, value = 1), align = "center"),
  br(),
  div(plotlyOutput("plot"))
)

server <- function(input, output, session) {
  p0 <- ggplotly({
    flinders_lm %>%
      dice_tsibble(unit = 1) %>%
      ggplot(aes(x = Time, y = Count, group = Date, colour = model)) +
      geom_line(alpha = 0.7)}) %>% layout(xaxis = list(autorange = TRUE))
  output$plot <- renderPlotly(p0)
  observeEvent(input$unit, {
    new <- dice_tsibble(flinders_lm, input$unit)
    plotlyReact("plot", new, p0)
  })
}

shinyApp(ui, server)
