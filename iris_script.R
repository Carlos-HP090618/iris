#load libraries
library(tidyverse)
library(rstatix)
library(tidymodels)
library(vip)

#load the dataset
iris <- iris
iris$Species <- as.factor(iris$Species)
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


# Machine learning
#split the data set
iris_split <- initial_split(iris, strata = Species)
iris_training <- training(iris_split)
iris_testing <- testing(iris_split)

# multinomial  regression
multi_spec <- multinom_reg() %>%
  set_engine("nnet") %>%
  set_mode("classification")
multi_model <- multi_spec %>%
  fit(Species ~., data = iris_training)
predictions <- predict(multi_model, new_data = iris_testing, type = "class") %>%
  bind_cols(Species = iris_testing$Species)

#performance
custom_metrics <- metric_set(accuracy, sens, spec)
multi_results <- custom_metrics(predictions, estimate = .pred_class, truth = Species)
multi_results

#Decision tree
tree_spec <- decision_tree() %>%
  set_engine("rpart") %>%
  set_mode("classification")
tree_model <- tree_spec %>%
  fit(Species ~., data = iris_training)
predictions_tree <- predict(tree_model, new_data = iris_testing, type = "class") %>%
  bind_cols(Species = iris_testing$Species)

#tree performance
tree_results <- custom_metrics(predictions_tree, estimate = .pred_class, truth = Species)
tree_results
