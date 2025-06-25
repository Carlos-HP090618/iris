#load libraries
library(tidyverse)
library(rstatix)

#load the dataset
iris <- iris

# Preparation for visualization and analysis
normality_irir <- iris %>% 
  group_by(Species) %>% 
  shapiro_test(Sepal.Length, Sepal.Width, Petal.Length, Petal.Width)

iris_long <- iris %>%
  pivot_longer(-Species)

iris_summary_long <- iris_long %>%
  group_by(Species, name) %>% 
  summarise(mean_value = mean(value),sd_value = sd(value), .groups = 'drop')

# Data visualization
ggplot(iris_long, aes(x = Species, y = value, color = Species, fill = Species)) + 
  geom_jitter(width = 0.15, alpha = 0.3, shape = 21, size = 3) + 
  facet_wrap(~name, scales = "free") + 
  geom_pointrange(data = iris_summary_long, aes(x = Species, 
                                                y = mean_value, 
                                                ymin = mean_value - sd_value,
                                                ymax = mean_value + sd_value), color = "black") + 
  labs(title = "Iris dataset summary", y = "Centimeters") + 
  theme_bw() + 
  theme(legend.position = "none")
