# Visualization 1: body size and daily sleep in mammals
# Data: ggplot2::msleep
# Source: https://ggplot2.tidyverse.org/reference/msleep.html
# Sleep and weight data: Savage & West (2007), doi:10.1073/pnas.0610080104

#install.packages(ggplot2)
#install.packages (dplyr)

library(ggplot2)
library(dplyr)

##PLOT1

#calling msleep function from package
sleep_data <- ggplot2::msleep

# Create the scatterplot and add a fitted regression line.
sleep_plot_1 <- ggplot(sleep_data, aes(x = bodywt, y = sleep_total)) +
  #scatterplot for each row of data
  geom_point(size = 2.5, alpha = 0.7, colour = "grey30") +
  #linear regression line
  geom_smooth(
    method = "lm",
    formula = y ~ x,
    se = TRUE,
    colour = "black",
    fill = "grey80"
  ) +
  #body weight on a log scale as too much variance in weight normally
  #regression will still plot this even though it comes after
  scale_x_log10(
    breaks = c(0.01, 0.1, 1, 10, 100, 1000, 10000),
    labels = c("0.01", "0.1", "1", "10", "100", "1,000", "10,000")
  ) +
  #y scale
  #set continuous with break point 0-24, 4
  scale_y_continuous(breaks = c(0, 4, 8, 12, 16, 20, 24)) +
  #range
  coord_cartesian(ylim = c(0, 24)) +
  #add labels to full plot
  labs(
    title = "Total Mammal Sleep (hrs) vs Body Weight (kgs)",
    x = "Body weight (kg; log scale)",
    y = "Total sleep (hours per day)",
    caption = paste(
      "Line: linear regression; shaded band: 95% confidence interval.",
      "Source: ggplot2::msleep (Savage & West, 2007).",
      sep = "\n"
    )
  ) +
  
  theme_classic(base_size = 12)

print(sleep_plot_1)

##PLOT2
#count the types of animal in the 'orders' (e.g. rodent, primate)
#then only take ones with greater than 6 animals
#then make new column with the n of the animal beside the order
sleep_by_order <- sleep_data |> 
  add_count(order, name = "n") |>
  filter(n >= 6) |>
  mutate(order_label = paste0(order, " (n = ", n, ")"))

#then repeat plotting process from step 1 but now broken down by the types of mammal
#using 

sleep_plot_2 <- ggplot(
  sleep_by_order,
  aes(x = bodywt, y = sleep_total)
) +
  geom_point(size = 2.5, alpha = 0.7, colour = "grey30") +
  geom_smooth(
    method = "lm",
    formula = y ~ x,
    se = TRUE,
    level = 0.95,
    colour = "black",
    fill = "grey80"
  ) +
  #making a unique panel for each order
  #scales 'fixed' to compare across panels
  facet_wrap(~ order_label, ncol = 2, scales = "fixed") +
  scale_x_log10(
    breaks = c(0.01, 0.1, 1, 10, 100, 1000, 10000),
    labels = c("0.01", "0.1", "1", "10", "100", "1,000", "10,000")
  ) +
  scale_y_continuous(breaks = seq(0, 24, 4)) +
  coord_cartesian(
    xlim = range(sleep_data$bodywt),
    ylim = c(0, 24)
  ) +
  labs(
    title = "The size-sleep relationship varies across groups",
    subtitle = paste(
      nrow(sleep_by_order), "of", nrow(sleep_data),
      "mammals; orders with at least 6 observations"
    ),
    x = "Body weight (kg; log scale)",
    y = "Total sleep (hours per day)",
    caption = paste(
      "Separate linear fits; shaded bands: 95% confidence intervals.",
      "Source: ggplot2::msleep (Savage & West, 2007).",
      sep = "\n"
    )
  ) +
  theme_classic(base_size = 12)

# Display the plot, including when this entire script is sourced.
print(sleep_plot_2)
