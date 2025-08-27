library(ggplot2)
library(dplyr)
library(tidyr)
processed_data <- read.csv("D:/DS/ERP/output.csv")
####################################################################
#	Drawing the boxplot to observe the overall distribution.
####################################################################
trips_count <- processed_data$count
boxplot(trips_count,
        main = "Boxplot of the number of trips per 15 mins",
        ylab = "Values",
        col = "lightblue",
        border = "darkblue")
summary(trips_count)
quantile(trips_count, probs = c(0.25, 0.5, 0.75))

nonzero_trips_count <- trips_count[trips_count != 0]
boxplot(nonzero_trips_count,
        main = "Boxplot of the non-zero-number of trips per 15 mins",
        ylab = "Values",
        col = "lightblue",
        border = "darkblue")
summary(nonzero_trips_count)
quantile(nonzero_trips_count, probs = c(0.25, 0.5, 0.75))

####################################################################
#	Encode the data with levels.
####################################################################
target_column <- processed_data$count  
q <- quantile(nonzero_trips_count, probs = c(0.25, 0.5, 0.75), na.rm = TRUE)

# add new column
processed_data$level_code <- with(processed_data, ifelse(processed_data$count <= q[1], "SL",
                                     ifelse(processed_data$count <= q[2], "RL",
                                            ifelse(processed_data$count <= q[3], "RH", "SH"))))
write.csv(processed_data, file = "D:/DS/ERP/encoded_output.csv", row.names = FALSE)


hist(processed_data$count,main = "Histogram of numbers",   
     xlab = "Numbers",                     
     ylab = "Frequency",                 
     col = "skyblue",                    
     border = "white",                    
     breaks = 50)
processed_data$Log_Number_of_Pickups <-ifelse(
  processed_data$count == 0,
  0,
  log(processed_data$count)+1
)
Log_Number_of_Pickups<- processed_data$Log_Number_of_Pickups
summary(Log_Number_of_Pickups) 

processed_data$level_code <- with(processed_data, 
                                  ifelse(Log_Number_of_Pickups == 0, "ND",
                                         ifelse(Log_Number_of_Pickups > 0 & Log_Number_of_Pickups <= 1, "SL",
                                                ifelse(Log_Number_of_Pickups > 1 & Log_Number_of_Pickups <= 2, "RL",
                                                       ifelse(Log_Number_of_Pickups > 2 & Log_Number_of_Pickups <= 3, "MD",
                                                              ifelse(Log_Number_of_Pickups > 3 & Log_Number_of_Pickups <= 4, "RH",
                                                                     ifelse(Log_Number_of_Pickups > 4 & Log_Number_of_Pickups <= 5, "SH",
                                                                            "EH"  # Otherwise
                                                                     )
                                                              )
                                                       )
                                                )
                                         )
                                  )
)
processed_data$class_code <- with(processed_data, 
                                  ifelse(Log_Number_of_Pickups == 0, 1,
                                         ifelse(Log_Number_of_Pickups > 0 & Log_Number_of_Pickups <= 1, 2,
                                                ifelse(Log_Number_of_Pickups > 1 & Log_Number_of_Pickups <= 2, 3,
                                                       ifelse(Log_Number_of_Pickups > 2 & Log_Number_of_Pickups <= 3, 4,
                                                              ifelse(Log_Number_of_Pickups > 3 & Log_Number_of_Pickups <= 4, 5,
                                                                     ifelse(Log_Number_of_Pickups > 4 & Log_Number_of_Pickups <= 5, 6,
                                                                            7  # Otherwise
                                                                     )
                                                              )
                                                       )
                                                )
                                         )
                                  )
)
hist(Log_Number_of_Pickups,main = "Histogram of processed log numbers of pick-ups",  
     xlab = "Numbers",                    
     ylab = "Frequency",                 
     col = "skyblue",                   
     border = "white",                   
     breaks = 50)

ggplot(processed_data, aes(x = level_code)) +
  geom_bar(fill = "blue", color = "white") +
  labs(title = "Distribution of Classes", 
       x = "Classes", 
       y = "Frequency") +
  theme_minimal()

desired_order <- c("ND", "SL", "RL", "MD", "RH", "SH","EH")
processed_data$level_code <- factor(processed_data$level_code, 
                                    levels = desired_order)
ggplot(processed_data, aes(x = level_code)) +
  geom_bar(fill = "pink", color = "white") +
  labs(title = "Distribution of Classes", 
       x = "Classes", 
       y = "Frequency") +
  theme_minimal()

####################################################################
#	Draw the plot of one day Trip Count.
####################################################################
day_data01 <- processed_data %>%
  filter(location.ID == 140, date == "2024-02-10")
time_breaks <- seq(1, 96, by = 12)  
time_labels <- sprintf("%02d:00", (time_breaks - 1) %/% 4)  

ggplot(day_data01, aes(x = time.slot, y = count)) +
  geom_col(fill = "steelblue", width = 0.9) +  
  labs(
    title = "Trip Count by 15-Minute Interval on 2024-02-10",
    x = "Time of Day (0:00 - 24:00)",
    y = "Count"
  ) +
  scale_x_continuous(
    breaks = time_breaks,  
    labels = time_labels,  
    limits = c(1, 96)      
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

##切分数据
grouped_counts <- split(processed_data$count, 
                        ceiling(seq_along(processed_data$count) / 96))
grouped_classes <- split(processed_data$class_code, 
                         ceiling(seq_along(processed_data$class_code) / 96))
str(grouped_counts)
str(grouped_classes)
####################################################################
#	Draw the small multiples of nine-day Trip Count.
####################################################################
# Filter for the date range and location
multi_day_data <- processed_data %>%
  filter(location.ID == 14, 
         date >= "2024-02-01" & date <= "2024-02-09")

# Create time breaks and labels (same as before)
time_breaks <- seq(1, 96, by = 12)  
time_labels <- sprintf("%02d:00", (time_breaks - 1) %/% 4)  

# Create the plot with facets
ggplot(multi_day_data, aes(x = time.slot, y = count)) +
  geom_col(fill = "steelblue", width = 0.9) +  
  facet_wrap(~ date, ncol = 3) +  # Creates 3x3 grid of plots
  labs(
    title = "Trip Count by 15-Minute Interval (Feb 1-9, 2024)",
    x = "Time of Day (0:00 - 24:00)",
    y = "Count"
  ) +
  scale_x_continuous(
    breaks = time_breaks,  
    labels = time_labels,  
    limits = c(1, 96)      
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        strip.text = element_text(size = 10))  # Style for the facet labels

## Model selection##
model_results <- data.frame(
  Model = paste0("Classes", 2:6),
  LogLikelihood = c(-320192.5, -313110, -309729.3, -307409.5, -306393.8),
  AIC = c(640535.1, 626476, 619800.6, 615247.1, 613301.5),
  BIC = c(60120815, 90196780, 120280129, 150365599, 180453678)
)
# Convert to long format

model_results_long <- model_results_long %>%
  group_by(Metric) %>%
  mutate(Value_scaled = scale(Value))  # Z-score normalization

ggplot(model_results_long, aes(x = Model, y = Value_scaled, color = Metric, group = Metric)) +
  geom_line(linewidth = 1) +
  geom_point(size = 3) +
  labs(y = "Normalized Value")

####################################################################
##mean-number-of-pickups-per-day for all locations##
####################################################################
matrix_form <- do.call(cbind, grouped_counts)
average_vector <- rowMeans(matrix_form)
print(average_vector)
time_slots <- 1:96
plot_data <- data.frame(
  time.slot = time_slots,
  count = average_vector
)
ggplot(plot_data, aes(x = time.slot, y = count)) +
  geom_col(fill = "steelblue", width = 0.9) +  
  labs(
    title = "The mean-number-of-pickups-per-day for All locations",
    x = "Time of Day (0:00 - 24:00)",
    y = "Count"
  ) +
  scale_x_continuous(
    breaks = time_breaks,  
    labels = time_labels,  
    limits = c(1, 96)      
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

####################################################################
## the average value of Top 5% popular locations##
####################################################################
data <- read.csv("D:/DS/ERP/24-02data.csv")

length(unique(data$PULocationID))

# Count the occurrences of each PULocationID
location_counts <- table(data$PULocationID)

# Sort by frequency in descending order
location_counts_sorted <- sort(location_counts, decreasing = TRUE)

# View the top10%  most frequent locations
head(location_counts_sorted, 13)

locations <- c(161, 237, 236, 132, 162, 230, 186, 142, 239, 163, 
               138, 170, 234)

location_ranges <- data.frame(
  Location = locations,
  Start_Row = (locations - 1) * 29 + 1,
  End_Row = locations * 29
)
invalid_locations <- locations[locations > 254]
print(invalid_locations)

ranges <- list(
  4641:4669,
  6845:6873,
  6816:6844,
  3800:3828,
  4670:4698,
  6642:6670,
  5366:5394,
  4090:4118,
  6903:3931,
  4699:4727,
  3974:4002,
  4902:4930,
  6758:6786
)

matrix_list <- lapply(ranges, function(r) do.call(cbind, grouped_counts[r]))

matrix_combined <- Reduce(cbind, matrix_list)

average_vector_top <- rowMeans(matrix_combined)
print(average_vector)
time_slots <- 1:96
plot_data_top <- data.frame(
  time.slot = time_slots,
  count = average_vector_top
)
ggplot(plot_data_top, aes(x = time.slot, y = count)) +
  geom_col(fill = "steelblue", width = 0.9) +  
  labs(
    title = "The mean-number-of-pickups-per-day for Top5% locations",
    x = "Time of Day (0:00 - 24:00)",
    y = "Count"
  ) +
  scale_x_continuous(
    breaks = time_breaks,  
    labels = time_labels,  
    limits = c(1, 96)      
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
