# load package
library(tidyverse)

# read data
raw_shooting = read_csv("../data/nba_shooting.csv")
# preview data
head(raw_shooting)

# add new columns
full_shooting = mutate(raw_shooting,
                       FGP = FGM / FGA,
                       TPP = TPM / TPA,
                       FTP = FTM/FTA,
                       eFGP = (FGM + 0.5 * TPM) / FGA,
                       PTS = FTM + 2 * FGM + TPM,
                       TSP = PTS / (2 * (FGA + 0.44 * FTA)))
# sort over TSP
arrange(full_shooting, desc(TSP))

# hist of FGP
ggplot(data = full_shooting) +
    geom_histogram(mapping = aes(x = FGP), bins = 25)
# hist with widths
ggplot(data = full_shooting) +
    geom_histogram(mapping = aes(x = FGP), binwidth = 0.02)

# density plot
ggplot(data = full_shooting) +
    geom_density(mapping = aes(x = FGP))

# scatterplot
ggplot(data = full_shooting) +
    geom_point(mapping = aes(x = FGP, y = FTP))

# filter for players with low observations
filtered_shooting = filter(full_shooting, FGA >= 50, FTA >= 50)

# filtered scatterplot
ggplot(data = filtered_shooting) +
    geom_point(mapping = aes(x = FGP, y = FTP))

# increase transparency
ggplot(data = filtered_shooting) +
    geom_point(mapping = aes(x = FGP, y = FTP), alpha = 0.3) +
    theme_bw()

# set as variable
transparent_plot = ggplot(data = filtered_shooting) +
    geom_point(mapping = aes(x = FGP, y = FTP), alpha = 0.3) +
    theme_bw()
# save to computer
ggsave("transparent_plot.png", transparent_plot, width = 5, height = 4)

# 2d histogram (heatmap)
ggplot(data = filtered_shooting) +
    geom_bin2d(aes(x = FGP, y = FTP), bins = 100) +
    scale_fill_viridis_c(option = "plasma", direction = 1)

# zoom in
ggplot(data = filtered_shooting) +
    geom_point(mapping = aes(x = FGP, y = FTP), alpha = 0.3) +
    theme_bw() +
    xlim(0.35, 0.55)

#################
### FILTERING ###
#################

# basic filter
filter(full_shooting, FGA > 100)
# two part filter
filter(full_shooting, FGA >= 100, FTA > 75)
# get steph
filter(full_shooting, PLAYER == "Stephen Curry", TPP > 0.4)
# or statement
filter(full_shooting, PLAYER == "Stephen Curry" | TPP > 0.4)
# multiple years (2015 - 2017)
filter(full_shooting, SEASON >= 2015, SEASON <= 2017)
filter(full_shooting, SEASON == 2015 | SEASON == 2016 | SEASON == 2017)
filter(full_shooting, SEASON %in% c(2015, 2016, 2017))
# not operator
filter(full_shooting, PLAYER != "Stephen Curry")

###########################
### CREATING CATEGORIES ###
###########################

# case when function
nba_shooting = mutate(full_shooting,
                      shooter_quality = case_when(
                          FGP < 0.42 ~ "Below Average",
                          FGP >= 0.42 & FGP <= 0.48 ~ "Average",
                          FGP > 0.48 ~ "Above Average"
                      ))

# select columns
select(full_shooting, FGP, FTP, TPP, TSP, eFGP)


# summarize data: averages for FGP, FTP, TPP, TSP, eFGP
reframe(full_shooting, mean_FGP = mean(FGP), mean_FTP = mean(FTP),
        mean_TPP = mean(TPP), mean_TSP = mean(TSP), mean_eFGP = mean(eFGP))

reframe(full_shooting, sd_FGP = sd(FGP), sd_FTP = sd(FTP),
        sd_TPP = sd(TPP), sd_TSP = sd(TSP), sd_eFGP = sd(eFGP))

# field goal percentage
reframe(full_shooting, mean_FGP = mean(FGP), sd_FGP = sd(FGP))

# save data to csv
write_csv(full_shooting, "full-shooting.csv")
