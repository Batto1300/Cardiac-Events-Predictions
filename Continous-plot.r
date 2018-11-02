library(magrittr)
library(tidyr)
library(ggplot2)
library(purrr)

data = read.csv("datasets/CardiacDataAdj.csv")
data.continous = keep(data, function(x) {!(all(x %in% c(0,1,0.5)))})
data.categorical = keep(data, function(x) {all(x %in% (c(0,1,1.5)))})

data.continous %>%
  gather() %>%                             # Convert to key-value pairs
  ggplot(aes(value)) +                     # Plot the values
  facet_wrap(~ key, scales = "free") +     # In separate panels
  geom_density() +                         # Density Plot 
  theme(axis.text.x = element_text(angle = 90))
