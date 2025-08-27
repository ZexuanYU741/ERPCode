####################################################################
#	Drawing the plot to show the classes distribution.
####################################################################
data <- read.csv("D:/DS/ERP/assignment_matrix.csv")
summary(data)

class_totals <- colSums(data)
class_proportions <- class_totals / sum(class_totals)

dominant_class <- apply(data, 1, function(x) which.max(x))
dominant_class_freq <- table(dominant_class)

shannon_diversity <- apply(data, 1, function(x) {
  p <- x/sum(x)
  -sum(p * log(p + 1e-10))  # 加一个小值避免log(0)
})

class_df <- data.frame(
  Class = colnames(data),
  Count = class_totals,
  Proportion = class_proportions
)

ggplot(class_df, aes(x = Class, y = Count, fill = Class)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = Count), vjust = -0.5) +
  labs(title = "The distribution of Classes", x = "Class", y = "Number of occurrences") +
  theme_minimal()