################
### PACKAGES ###
################

# tidyverse
library(tidyverse)
# install lahman
install.packages("Lahman")
# load Lahman
library(Lahman)

##############
### PIPING ###
##############

# look at pitching
pitching = as_tibble(Pitching)
head(pitching)
# old way
pitching_new = mutate(pitching, IP = IPouts / 3)
# new way
pitching_new = pitching %>%
    # add innings pitched
    mutate(IP = IPouts / 3) %>%
    # filter for IP > 150
    filter(IP >= 150) %>%
    # select columns we want
    select(playerID, yearID, teamID, lgID, IP, ERA)

# pulling ERA column out
pitching_new %>%
    pull(ERA)

###############
### VISUALS ###
###############

# make scatterplot
ggplot(data = pitching_new) +
    geom_point(mapping = aes(x = yearID, y = ERA), size = 0.3)
# explore good pitchers
arrange(pitching_new, ERA) %>%
    slice_head(n = 10)


##########################
### GROUPED OPERATIONS ###
##########################

# standardize our era
std_era = pitching_new %>%
    # by year
    group_by(yearID) %>%
    mutate(z_ERA = scale(ERA))

year_means = pitching_new %>%
    # "for each" _____
    group_by(yearID) %>%
    # select new columns
    reframe(mean_ERA = mean(ERA),
            sd_ERA = sd(ERA)) %>%
    # allows us to do non-grouped operations after
    ungroup()

# multiple groups
league_season = pitching_new %>%
    # understand how each league did in a year
    group_by(yearID, lgID) %>%
    # standardize ERAs
    mutate(std_ERA = scale(ERA))

# make our final plot
pitching_final = pitching_new %>%
    # for each year
    group_by(yearID) %>%
    # get mean ERA
    mutate(mean_ERA = mean(ERA)) %>%
    ungroup()

# make final plot
ggplot(data = pitching_final) +
    # scatter and line
    geom_point(mapping = aes(x = yearID, y = ERA), size = 0.3) +
    geom_line(mapping = aes(x = yearID, y = mean_ERA), col =)
# alternate way
ggplot() +
    geom_point(data = pitching_new, mapping = aes(x = yearID, y = ERA), size = 0.3) +
    geom_line(data = pitching_final, mapping = aes(x = yearID, y = mean_ERA), col = "red")