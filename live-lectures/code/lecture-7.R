# load in libraries
library(tidyverse)

# load in field goal data
fg_train = read_csv("../data/nfl_fg_train.csv")
# preview
head(fg_train)

# overall mean
fg_models = fg_train %>%
    # add global mean
    mutate(global_mean = mean(Success)) %>%
    # for each kicker
    group_by(Kicker) %>%
    mutate(kicker_mean = mean(Success)) %>%
    ungroup() %>%
    # yard line bins
    mutate(dist_5 = cut(Distance, breaks = seq(from = 15, to = 80, by = 5))) %>%
    # for each bin
    group_by(dist_5) %>%
    mutate(dist_5_means = mean(Success)) %>%
    ungroup() %>%
    # no bins
    group_by(Distance) %>%
    mutate(distance_means = mean(Success)) %>%
    ungroup()
    
# visualize our estimates
ggplot(data = fg_models) +
    geom_point(aes(x = Distance, y = dist_5_means), alpha = 0.1, color = "lightcoral") +
    geom_point(aes(x = Distance, y = distance_means), alpha = 0.1, color = "dodgerblue")

#############################
### ASSESSING PREDICTIONS ###
#############################

# in sample
fg_models %>%
    reframe(
        global_rmse = sqrt(mean((Success - global_mean)^2)),
        kicker_rmse = sqrt(mean((Success - kicker_mean)^2)),
        dist_5_rmse = sqrt(mean((Success - dist_5_means)^2)),
        distance_rmse = sqrt(mean((Success - distance_means)^2))
    )

# out of sample data
fg_test = read_csv("../data/nfl_fg_test.csv")
# add bins to fg test data
fg_test_full = fg_test %>%
    # add bins
    mutate(dist_5 = cut(Distance, breaks = seq(from = 15, to = 80, by = 5))) %>%
    # join in global mean
    mutate(global_mean = fg_models$global_mean[1]) %>%
    # join in kicker
    left_join(fg_models %>% select(Kicker, kicker_mean) %>% distinct(), by = "Kicker") %>%
    # join in distance bins
    left_join(fg_models %>% select(dist_5, dist_5_means) %>% distinct(), by = "dist_5") %>%
    # join in full distance
    left_join(fg_models %>% select(Distance, distance_means) %>% distinct(), by = "Distance")

# evaluate out of sample
fg_test_full %>%
    reframe(
        global_rmse = sqrt(mean((Success - global_mean)^2)),
        kicker_rmse = sqrt(mean((Success - kicker_mean)^2)),
        dist_5_rmse = sqrt(mean((Success - dist_5_means)^2)),
        distance_rmse = sqrt(mean((Success - distance_means)^2))
    )
