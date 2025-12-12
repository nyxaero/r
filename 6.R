library(ggplot2)
library(reshape2)
library(dplyr)

data("mpg")

ggplot(mpg, aes(x = displ, y = hwy, color = class)) +
  geom_point(size = 3, alpha = 0.7) +
  geom_smooth(method = "lm", se = TRUE,
              linetype = "dashed", color = "black", size = 1) +
  labs(title = "Scatter Plot of Engine Displacement vs Highway MPG with Regression Line",
       x = "Engine Displacement (L)",
       y = "Highway Miles per Gallon",
       color = "Vehicle Class") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
        axis.title = element_text(size = 14),
        legend.position = "bottom")

ggplot(mpg, aes(x = displ, y = hwy)) +
  geom_point(color = "darkgreen", size = 2) +
  facet_wrap(~class, ncol = 3) +
  labs(title = "Faceted Scatter Plot by Vehicle Class",
       x = "Engine Displacement (L)",
       y = "Highway Miles per Gallon") +
  theme_minimal() +
  theme(strip.text = element_text(size = 12, face = "italic"),
        plot.title = element_text(hjust = 0.5, size = 16))


data("diamonds")

cor_matrix <- cor(diamonds[, sapply(diamonds, is.numeric)], use = "complete.obs")

cor_data <- melt(cor_matrix)

ggplot(cor_data, aes(Var1, Var2, fill = value)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white",
                       midpoint = 0, limit = c(-1, 1), space = "Lab",
                       name = "Correlation") +
  labs(title = "Heatmap of Correlation Matrix for Diamonds Dataset",
       x = "Variables", y = "Variables") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
        axis.text.y = element_text(size = 12),
        plot.title = element_text(hjust = 0.5, size = 16))

ggplot(mpg, aes(x = displ, y = hwy, color = class)) +
  geom_point(size = 3, shape = 21, fill = "lightblue", alpha = 0.8) +
  theme_light() +
  scale_color_brewer(palette = "Set2") +
  labs(title = "Customized Scatter Plot with Aesthetic Enhancements",
       x = "Engine Displacement (L)",
       y = "Highway Miles per Gallon",
       color = "Class") +
  theme(plot.title = element_text(face = "bold", size = 14),
        axis.title.x = element_text(size = 13),
        axis.title.y = element_text(size = 13),
        legend.background = element_rect(fill = "gray90"))

annotated_plot <- ggplot(mpg, aes(x = displ, y = hwy)) +
  geom_point(color = "purple", size = 3) +
  annotate("text", x = 4, y = 40, label = "High Efficiency Zone",
           color = "red", size = 5, fontface = "bold", angle = 15) +
  annotate("rect", xmin = 2, xmax = 4, ymin = 30, ymax = 45,
           alpha = 0.2, fill = "yellow", color = "orange") +
  labs(title = "Annotated Scatter Plot with Highlighted Zone",
       x = "Engine Displacement (L)",
       y = "Highway Miles per Gallon") +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5))
annotated_plot

ggsave("annotated_scatter_plot.png", annotated_plot,
       width = 10, height = 8, dpi = 300)
