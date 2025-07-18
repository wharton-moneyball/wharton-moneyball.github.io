library(tidyverse)
library(ggplot2)

load('../data/batting_2014_2015.RData')
batting_2014_2015

batting_2014_2015_wide <- batting_2014_2015 |> 
    pivot_wider(
        names_from = yearID,
        values_from = BA, 
        names_prefix = "BA_"
    )
head(batting_2014_2015_wide)

batting_2014_2015_long <- batting_2014_2015_wide |> 
    pivot_longer(
        cols = starts_with("BA_"),
        names_to = "yearID",
        values_to = "BA", 
    ) |> 
    mutate(
        yearID = sub("BA_", "", yearID)
    )

head(batting_2014_2015_long)

batting_2014_2015_slice <- batting_2014_2015_long |> 
    slice_head(n = 20) #take the first 10 players

#plot 
ggplot(data = batting_2014_2015_slice, aes (x = playerID, y = BA, fill = yearID)) +
           geom_col(position = "dodge", color = 'black', alpha = 0.8) +
           theme (axis.text.x = element_text(angle = 45, hjust = 1))

# add global mean
batting_1 = batting_2014_2015_wide |> 
    mutate(global_mean = mean(BA_2015))

# calculate averages
avg_2014 = mean(batting_1$BA_2014)
avg_2015 = mean(batting_1$BA_2015)

# scatterplot between years
ggplot(data = batting_1) +
    geom_point(mapping = aes(x = BA_2014, y = BA_2015), size = 1) +
    geom_hline(yintercept = avg_2015, col = "red", lty = 2) +
    geom_vline(xintercept = avg_2014, col = "blue", lty = 2)

# add our binned averages
batting_2 = batting_1 |> 
    mutate(bins = cut(BA_2014, breaks = c(min(BA_2014), avg_2014, max(BA_2014)))) |> 
    # for each bin
    group_by(bins) |> 
    # add the average
    mutate(bin_2_avg = mean(BA_2015)) |> 
    ungroup() |> 
    select(-bins)

# visualize two predictions
ggplot(data = batting_2) +
    geom_point(mapping = aes(x = BA_2014, y = BA_2015), size = 1) +
    # global mean
    geom_point(mapping = aes(x = BA_2014, y = global_mean), col = "lightcoral", size = 1) +
    # binned average
    geom_point(mapping = aes(x = BA_2014, y = bin_2_avg), col = "lightblue", size = 1)

# add third prediction
batting_3 = batting_2 |> 
    mutate(bins = cut(BA_2014, breaks = seq(from = min(BA_2014), to = max(BA_2014), by = 0.01))) |> 
    group_by(bins) |> 
    # add the average
    mutate(bin_3_avg = mean(BA_2015)) |> 
    ungroup() |> 
    select(-bins)

# visualize three preds
ggplot(data = batting_3) +
    geom_point(mapping = aes(x = BA_2014, y = BA_2015), size = 1) +
    # global mean
    geom_point(mapping = aes(x = BA_2014, y = global_mean), col = "lightcoral", size = 1) +
    # 2 binned average
    geom_point(mapping = aes(x = BA_2014, y = bin_2_avg), col = "lightblue", size = 1) +
    # 3rd binned
    geom_point(mapping = aes(x = BA_2014, y = bin_3_avg), col = "darkseagreen3", size = 1)

# mean squared error
batting_3 |> 
    reframe(
        mse_global = mean((BA_2015 - global_mean)^2),
        mse_bin2 = mean((BA_2015 - bin_2_avg)^2),
        mse_bin3 = mean((BA_2015 - bin_3_avg)^2)
    )
