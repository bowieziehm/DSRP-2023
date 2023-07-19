library(tidymodels)
library(dplyr)
library(stringr)

# install.packages("xgboost")

#### 1. Get Data
data <- read.csv("DSRP 2023/data/imbdDataset.csv")


#### 2. Clean Dataset

# convert 0 gross to NA
data$gross.M. <- na_if(data$gross.M., 0)

## remove " min" from runtime column
data <- data %>%
  mutate(runtime = as.numeric(str_remove(runtime, " min")))


## remove NAs
data <- na.omit(data)
View(data)

## Encode categorical data
data <- data |> mutate(title = as.factor(title), genre = as.factor(genre), release_year = as.factor(release_year))

#### 3. Visualize correlation (via PCA for high-dims)
library(reshape2)
library(ggplot2)

## Correlation
cors <- data |> select(runtime, rating, gross.M.) |>
  cor() |> melt() |> as.data.frame()
cors

## Visualize data
ggplot(cors, aes(x=Var1, y=Var2, fill=value)) +
  geom_tile() +
  scale_fill_gradient2(low="darkred", high = "blue", mid= "white",
                       midpoint = 0)

## PCA
data_num <- select(data, -title, -genre, -release_year)
pca <- prcomp(data_num, scale. = T)
summary(pca)

pca$rotatzion^2

pcds <- as.data.frame(pca$x)
pcds$gross.M. <- data$gross.M.

ggplot(pcds, aes(PC1, PC2, color=gross.M.)) + geom_point()

## Other Plots

ggplot(data = data, aes(x=release_year, y=gross.M.)) +
  geom_point()



ggplot(data = data, aes(x = runtime, y = gross.M.)) +
  geom_line(stat = "summary",
            fun = "mean")

ggplot(data = data, aes(x=genre, y=gross.M.)) +
  geom_point()

#### 4. Feature Selection

# Choose type: prediction or classification
# Choose vars: labels and features


# choose variables that account for the most correlation
data <- select(data, genre, release_year, gross.M.)
data

#### 5. Split data
library(rsample)

## Train-Test Split 75:25

# make sampling reproducible
set.seed(479)

# regression
reg_split <- initial_split(data, prop=0.75)
reg_train <- training(reg_split) # 75% of the data, randomly sampled n/o replacement
reg_test <- testing(reg_split) # 25%


#### 6. Fit Data
library(parsnip)
## Select a model (classifier or predictor): e.g. boosted tree for classification

fit <- linear_reg() |>
  set_engine("lm") |>
  set_mode("regression") |>
  fit(gross.M. ~ ., data = reg_train)



## Use for regression
boost_tree_fit <- boost_tree() |>
  set_engine("xgboost") |>
  set_mode("regression") |>
  fit(gross.M. ~ ., data = reg_train)
boost_tree_fit$fit

fit$fit
summary(fit$fit)

boost_tree_fit$fit
summary(boost_tree_fit$fit)

#### 7. Make test predictions
library(yardstick)
# install.packages("Metrics") 
# install.packages("MLmetrics")
library(MLmetrics)
library(Metrics)

# Linear Regression
moviePred <- reg_test
moviePred$linReg <- predict(fit, reg_test)$.pred

# error
yardstick::mae(moviePred, truth = gross.M., estimate = linReg)

yardstick::rmse(moviePred, truth = gross.M., estimate = linReg)


## Boosted Decision Trees

# Regression
boostPred <- reg_test
boostPred$logReg <- predict(boost_tree_fit, reg_test)$.pred
# error
yardstick::mae(boostPred, truth = gross.M., estimate = logReg)

yardstick::rmse(boostPred, truth = gross.M., estimate = logReg)
