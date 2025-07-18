# set working directory
setwd("~/wsabi/Moneyball/code")

# simple calculator
3 + 4

# variable assignment
x <- 3 + 4
x = 3 + 4
# print variable
x
# different x
x <- 3 + 5
x

# define a vector
x <- c(7, 8, 9)
# print vector
x

# define a sequence of numbers
x <- 1:10
x

# define y
y <- c(3, 5, 6, 7, 10, 14, 15, 18, 20, 21)

# scatterplot
plot(x, y)

# install packages
install.packages("tidyverse")
# load packages
library(tidyverse)

# reading in data
nba_shooting_small <- read_csv("../data/nba_shooting_small.csv")
# preview data
head(nba_shooting_small)

# arrange increasing
arrange(nba_shooting_small, FGA)
# arrange decreasing
arrange(nba_shooting_small, desc(FGA))
# alphabetize
arrange(nba_shooting_small, PLAYER)

# save this
nba_shooting_arranged = arrange(nba_shooting_small, FGA)

# add columns to data frame
nba_shooting_full = mutate(nba_shooting_small, FGP = FGM/FGA, TPP = TPM/TPA, FTP = FTM/FTA)
# look at data two ways
head(nba_shooting_full)
view(nba_shooting_full)
