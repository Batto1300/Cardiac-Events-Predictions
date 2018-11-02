library(corrplot)
library(Hmisc)
library(magrittr)
library(tidyr)
library(ggplot2)
library(purrr)

data = read.csv("datasets/CardiacDataAdj.csv")
data.categorical = keep(data, function(x) {all(x %in% (c(0,1,1.5)))})

data.categorical %>%
  gather() %>%                             # Convert to key-value pairs
  ggplot(aes(value)) +                     # Plot the values
  facet_wrap(~ key, scales = "free_x") +   # In separate panels
  geom_bar() +                             # Bar Plot
  scale_x_discrete()                       # Discrete x values