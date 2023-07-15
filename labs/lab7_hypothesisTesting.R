## PRELIMINARY ####

# install needed packages
# install.packages("corrplot")

# load required packages
library(corrplot)
library(tidyverse)

# read in data set
data <- read.csv("data/imbdDataset.csv")




## PART 1 ####

# 1. Test for a significant difference between 2 groups
View(data)

# Independent T Test: compare popularity (measured by movie's gross multiplied by the rating) between Animated and Live-Action movies
# H0: popularity is the same independent of art form used
# HA: popularity is different 

# c_mean = nc_mean
# H0: c_mean - nc_mean = 0
# HA: (difference) != 0

# get appropriate movie groups distinguished by art form
animated <- data |> filter(genre == "Animation")
liveAction <- data |> filter(genre != "Animation")

# find popularity and store it
animatedPopularity <- animated$gross.M. * animated$rating
liveActionPopularity <- liveAction$gross * liveAction$rating

# T Test
t.test(animatedPopularity, liveActionPopularity)


### RESULTS ###

# data:  animatedPopularity and liveActionPopularity
# t = 2.8152, df = 87.556, p-value = 0.006021
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#   93.83756   544.42629
# sample estimates:
#  mean of x    mean of y 
#  802.9670     483.8351 


### CONCLUSION ###

# p-val < 0.05 -> null rejected, HA accepted

## PART 2 ####

# 2. Test for a significant difference between 3+ groups

# ANOVA Test
# Test for difference between genre and average gross
# H0: Testing for Comedy, Adventure, and Horror movies, Movie gross is the same independent of movie genre
# HA: Movie gross is different

# Get the movies we want
filteredData <- data |> 
  filter(genre == c("Horror", "Adventure", "Comedy")) |>
  select(genre, gross.M.)

aov_res <- aov(gross.M. ~ genre, filteredData)

summary(aov_res)
TukeyHSD(aov_res)


### RESULTS ###

#               Df  Sum Sq  Mean Sq  F value    Pr(>F)    
# genre         2   267598    133799   8.652    0.000273 ***
#   Residuals   157 2427958   15465                     
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# > TukeyHSD(aov_res)
# Tukey multiple comparisons of means
#   95% family-wise confidence level
# 
# Fit: aov(formula = gross.M. ~ genre, data = filteredData)
# 
# $genre
#                    diff           lwr        upr     p adj
# Comedy-Adventure  -76.73754 -124.1738 -29.301259 0.0005442
# Horror-Adventure -133.60963 -269.7560   2.536781 0.0556788
# Horror-Comedy     -56.87210 -192.3236  78.579445 0.5820706


### CONCLUSION ###

# #Based on the ANOVA test results, the p-value is 0.000273, indicating a significant difference in average gross among the movie genres (Comedy, Adventure, and Horror).
# Null Hypothesis is therefore rejected

# Tukey HSD test reveals:
#   - Comedy movies have a significantly lower average gross compared to Adventure movies (p-value: 0.0005442)
#   - Difference between Horror and Adventure movies is marginally significant (p-value: 0.0556788)
#   - There is no significant difference between Horror and Comedy movies in terms of average gross (p-value: 0.5820706)
## PART 3 ####

# 3. Test for a significant association between categorical variables

# Chi-Squared Test - check if groups are independent
# Test for relationship between movie genre and movie release year
# H0: There is no significant association between movie genre and release year
# HA: There exists a relationship
t <- table(data$release_year, data$genre)
chi <- chisq.test(t)
chi$p.value


### RESULTS ###

# Pearson's Chi-squared test
# 
# data:  t
# X-squared = 2549.8, df = 2020, p-value = 6.243e-15


### CONCLUSION ###

# Based on the chi-squared test, the p-value is 6.243e-15. 
# This extremely small p-value indicates a significant association between movie genre and movie year
# Therefore, we reject the null hypothesis and conclude that there is a significant relationship between the genre of a movie and the year it was released

