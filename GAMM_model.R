```{r}
# Load librarys
library(ggplot2)
library(dplyr)
library(mgcv)
library(tidymv)
library(gratia)

# load data
data <- read.csv('/Users/rorycave/distance_file.csv', header=TRUE)

# Filter data
data_filtered <- data %>%
  filter(time_divergent <=10)

# Calculate mean and standard error for the distances
mean_distance <- data_filtered %>%
  summarise(mean_dist = mean(Distance_km), se = sd(Distance_km) / sqrt(n()))

print(mean_distance)
```
```{r}
# set SampleID as factor
data_filtered$SampleID1 <- as.factor(data_filtered$SampleID1)
data_filtered$SampleID2 <- as.factor(data_filtered$SampleID2)

# Make GAMM model
gam_model <- gam(Distance_km ~ s(time_divergent) + s(SampleID1, bs="re") + s(SampleID2, bs="re"),
                 data = data_filtered, method = "REML")

# Check model                 
summary(gam_model)
```
```{r}
# Plot model
plot_smooths(
  model = gam_model,
  series = time_divergent,
) +
   geom_line(color="blue")+
  geom_hline(yintercept = mean_distance$mean_dist, linetype = "dashed", color = "red") +
  geom_vline(xintercept = saturation_point$time_divergent, linetype = "dashed", color = "purple") +
   labs(x = "Time divergent between isolate pairs (years)", y = "Distance between isolate pairs (km)") +
  scale_x_continuous(breaks = seq(0, 10, by = 1)) +
  theme(
    panel.background = element_rect(fill = "white", colour = NA), 
    text = element_text(size = 14),# Set panel background to white
    plot.background = element_rect(fill = "white", colour = NA),
    panel.grid.major = element_blank(),  # Remove major gridlines
    panel.grid.minor = element_blank(),  # Remove minor gridlines
    axis.line = element_line(colour = "black")  # Add axis lines
  )
```

  

