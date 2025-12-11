library(dplyr)
library(plotly)
library(gapminder)
library(readr)
library(DT)

data("gapminder")
gap_2007 <- gapminder %>% filter(year == 2007)

scatter_plot <- gap_2007 %>%
  plot_ly(
    x = ~gdpPercap, y = ~lifeExp,
    color = ~continent, size = ~pop,
    type = "scatter", mode = "markers",
    hoverinfo = "text",
    text = ~paste(
      "Country:", country,
      "<br>Continent:", continent,
      "<br>GDP per Capita:", round(gdpPercap, 0),
      "<br>Life Expectancy:", round(lifeExp, 1),
      "<br>Population:", format(pop, big.mark = ",")
    )
  ) %>%
  layout(
    title = "GDP vs Life Expectancy (2007)",
    xaxis = list(title = "GDP per Capita (log scale)", type = "log"),
    yaxis = list(title = "Life Expectancy (years)")
  )

scatter_plot

top20_lifeexp <- gap_2007 %>%
  arrange(desc(lifeExp)) %>%
  slice_head(n = 20) %>%
  mutate(country = factor(country, levels = country))

bar_chart <- top20_lifeexp %>%
  plot_ly(
    x = ~country, y = ~lifeExp,
    type = "bar",
    hoverinfo = "text",
    text = ~paste(
      "Country:", country,
      "<br>Life Expectancy:", round(lifeExp, 1)
    )
  ) %>%
  layout(
    title = "Top 20 Countries by Life Expectancy (2007)",
    xaxis = list(title = "Country"),
    yaxis = list(title = "Life Expectancy (years)")
  )

bar_chart

asia_sel <- gapminder %>%
  filter(
    continent == "Asia",
    country %in% c("India", "China", "Japan", "Indonesia")
  )

line_chart <- asia_sel %>%
  plot_ly(
    x = ~year, y = ~lifeExp,
    color = ~country,
    type = "scatter", mode = "lines+markers",
    hoverinfo = "text",
    text = ~paste(
      "Country:", country,
      "<br>Year:", year,
      "<br>Life Expectancy:", round(lifeExp, 1)
    )
  ) %>%
  layout(
    title = "Life Expectancy in Selected Asian Countries",
    xaxis = list(title = "Year"),
    yaxis = list(title = "Life Expectancy (years)")
  )

line_chart

subplot(
  scatter_plot,
  bar_chart,
  line_chart,
  nrows = 2,
  shareX = FALSE, shareY = FALSE,
  margin = 0.04,
  titleX = TRUE, titleY = TRUE
) %>%
  layout(title = "Gapminder Interactive Dashboard")

gap_summary <- gap_2007 %>%
  group_by(continent) %>%
  summarise(
    countries = n_distinct(country),
    avg_lifeExp = round(mean(lifeExp), 1),
    avg_gdpPercap = round(mean(gdpPercap), 0),
    total_pop = sum(pop),
    .groups = "drop"
  )

datatable(
  gap_summary,
  options = list(pageLength = 5, autoWidth = TRUE),
  caption = "Continent-level summary (Gapminder 2007)."
)

covid_url <- "https://github.com/owid/covid-19-data/raw/master/public/data/owid-covid-data.csv"

covid_data <- read_csv(covid_url)

covid_sel <- covid_data %>%
  filter(location %in% c("India", "United States", "Brazil")) %>%
  select(location, date, new_cases_smoothed_per_million) %>%
  mutate(date = as.Date(date)) %>%
  filter(!is.na(new_cases_smoothed_per_million))

covid_plot <- covid_sel %>%
  plot_ly(
    x = ~date,
    y = ~new_cases_smoothed_per_million,
    color = ~location,
    type = "scatter", mode = "lines",
    hoverinfo = "text",
    text = ~paste(
      "Country:", location,
      "<br>Date:", date,
      "<br>New Cases / million:",
      round(new_cases_smoothed_per_million, 1)
    )
  ) %>%
  layout(
    title = "COVID-19 New Cases (Smoothed per Million)",
    xaxis = list(title = "Date"),
    yaxis = list(title = "New Cases (per million)")
  )

covid_plot

covid_summary <- covid_sel %>%
  group_by(location) %>%
  summarise(
    start_date = min(date),
    end_date = max(date),
    avg_new_cases_per_million =
      round(mean(new_cases_smoothed_per_million), 1),
    max_new_cases_per_million =
      round(max(new_cases_smoothed_per_million), 1),
    .groups = "drop"
  )

datatable(
  covid_summary,
  options = list(pageLength = 3, autoWidth = TRUE),
  caption = "Summary of COVID-19 new cases (smoothed per million)."
)

