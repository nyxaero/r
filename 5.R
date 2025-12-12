library(dplyr)
library(nycflights13)
library(ggplot2)
library(zoo)
data("starwars")

starwars_filtered<-starwars %>%
  select(name,species,height,mass) %>%
  filter(!is.na(species)&!is.na(height)&height>100) %>%
  arrange(desc(height))
head(starwars_filtered)
ggplot(starwars_filtered,aes(x=reorder(name,-height),y=height,fill=species))+
  geom_bar(stat="identity")+
  coord_flip()+
  labs(title="Height of Star Wars  Charecters",x="Character",y="Height(cm)")+
  theme_minimal()

species_summary <- starwars %>%
  group_by(species) %>%
  summarize(
    avg_height = mean(height, na.rm = TRUE),
    avg_mass = mean(mass, na.rm = TRUE),
    count = n()
  ) %>%
  arrange(desc(count))

head(species_summary)

ggplot(species_summary, aes(x = reorder(species, -avg_height), y = avg_height, fill = species)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "Average Height by Species", x = "Species", y = "Average Height (cm)") +
  theme_minimal()

starwars_classified <- starwars %>%
  mutate(height_category = ifelse(height > 180, "Tall", "Short"))

head(starwars_classified)

ggplot(starwars_classified, aes(x = height_category, fill = height_category)) +
  geom_bar() +
  labs(title = "Distribution of Height Categories", x = "Height Category", y = "Count") +
  theme_minimal()

data("flights")
data("airlines")

flights_inner_join <- flights %>%
  inner_join(airlines, by = "carrier")

flights_outer_join <- flights %>%
  full_join(airlines, by = "carrier")

head(flights_inner_join)
head(flights_outer_join)

flights_rolling <- flights %>%
  arrange(year, month, day) %>%
  mutate(
    arr_delay = ifelse(is.na(arr_delay), 0, arr_delay),
    rolling_avg_delay = rollmean(arr_delay, 5, fill = NA),
    cumulative_delay = cumsum(arr_delay)
  )

head(flights_rolling)

ggplot(flights_rolling, aes(x = day)) +
  geom_line(aes(y = rolling_avg_delay, color = "Rolling Average Delay")) +
  geom_line(aes(y = cumulative_delay / 1000, color = "Cumulative Delay (x1000)")) +
  labs(title = "Rolling Average and Cumulative Delay of Flights",
       x = "Day of the Month", y = "Delay (minutes)") +
  scale_color_manual(values = c("Rolling Average Delay" = "blue", 
                                "Cumulative Delay (x1000)" = "red")) +
  theme_minimal()