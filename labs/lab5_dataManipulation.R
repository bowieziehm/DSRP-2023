## imports
library(tibble)
library(dplyr)
library(ggplot2)

## read data set
set <- read.csv("data/imbdDataset.csv")
View(set)

#1. filter() - show only movies of Adventure genre
filter(set, genre == "Adventure")

#2. select() - subset with only movie name and its gross
select(set, title, gross.M.)

#3. mutate() - 2 new columns
set <- mutate(set, rating_to_gross_ratio = trunc(100*(rating / gross.M.)))
set <- mutate(set, rating_on_100_scale = trunc(rating * 10))

#4. summarize() - showing the mean gross of movies released before and after 1971
summarize(set,
    before_1971 = mean(gross.M.[release_year <= 1971]),
    after_1971 = mean(gross.M.[release_year > 1971])
  )

#5. arrange() - random reordering of data set practice
arrange(select(set, gross.M., runtime), desc(runtime))

#6.
ggplot(data = set, aes(x = year, y = rating_on_100_scale)) +
  geom_bar() +
  labs(x = "Rating (0-100)", y = "Genre")