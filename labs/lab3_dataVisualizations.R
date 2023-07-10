# load required packages
library(ggplot2)


readableSetName <- "data/imbdDataset.csv"
set <- read.csv(readableSetName)
View(set)


#1
ggplot(data = set, aes(x = release_year)) +
  geom_histogram()
ggsave("plot1.pdf")

#2
ggplot(data = set, aes(x = genre, y = gross.M., color=genre,fill = genre)) +
  geom_bar(stat = "summary",
           fun = "mean")
ggsave("plot2.pdf")
#3
ggplot(data = set, aes(x = rating, y = gross.M.)) +
  geom_line(stat = "summary",
            fun = "mean")
ggsave("plot3.pdf")