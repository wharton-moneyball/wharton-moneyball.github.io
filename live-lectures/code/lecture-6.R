# load librarires
library(tidyverse)
# read in data
load("../data/batting_2014_2015.RData")
# rename
data = batting_2014_2015
# look at data
head(data)
# pivot data
data = data %>%
    pivot_wider(
        names_from = yearID,
        values_from = BA,
        names_prefix = "BA_"
    )
# make scatterplot
ggplot(data = data) +
    geom_point(mapping = aes(x = BA_2014, y = BA_2015)) +
    labs(x = "2014 Batting Avg", y = "2015 Batting Avg")

# aside on how to make relationships in graphs (quadratic)
x = seq(-5, 5, by = 0.1)
y = x^2
sample_data = tibble(x, y)
# plot it
ggplot(data = sample_data) +
    geom_point(x = x, y = y) +
    xlim(-5, 5) +
    ylim(0, 25)

# make our regression line
r = cor(data$BA_2014, data$BA_2015)
sd_y = sd(data$BA_2015)
sd_x = sd(data$BA_2014)

b = r * sd_y / sd_x

y_bar = mean(data$BA_2015)
x_bar = mean(data$BA_2014)

a = y_bar - x_bar * b
# add line
ggplot(data = data) +
    geom_point(mapping = aes(x = BA_2014, y = BA_2015)) +
    geom_abline(slope = b, intercept = a, col = "lightcoral") +
    labs(x = "2014 Batting Avg", y = "2015 Batting Avg")

# using built in r
reg_model = lm(BA_2015 ~ BA_2014, data = data)
# get coefficients
coefficients = reg_model$coefficients
# pull out individual coefficients
intercept = coefficients[1]
slope = coefficients[2]

# using model predictions
data_pred = data |> 
    mutate(pred = predict(reg_model, data))

ggplot(data = data_pred) +
    geom_point(mapping = aes(x = BA_2014, y = BA_2015)) +
    geom_line(aes(x = BA_2014, y = pred), col = "lightcoral") +
    labs(x = "2014 Batting Avg", y = "2015 Batting Avg")

# load modelr
library(modelr)

# add model, residuals
full_data = data %>%
    add_predictions(model = reg_model, type = "response", var = "pred") %>%
    add_residuals(model = reg_model, var = "resid")

ggplot(full_data) +
    geom_point(aes(x = BA_2014, y = BA_2015)) +
    geom_line(aes(x = BA_2014, y = pred), col = "lightcoral") +
    geom_segment(aes(x = BA_2014, xend = BA_2014, y = BA_2015, yend = pred), color = "dodgerblue")

# prediction on new data
new_data = tibble(BA_2014 = c(0.280, 0.274, 0.220, 0.315))
new_preds = predict(reg_model, new_data)

new_final = new_data |> 
    mutate(preds = new_preds)

ggplot(data = data) +
    geom_point(mapping = aes(x = BA_2014, y = BA_2015)) +
    geom_abline(slope = b, intercept = a, col = "lightcoral") +
    geom_point(data = new_final, aes(x = BA_2014, y = preds), color = "dodgerblue")
    labs(x = "2014 Batting Avg", y = "2015 Batting Avg")
