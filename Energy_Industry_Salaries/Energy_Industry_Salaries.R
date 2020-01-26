# Load packages
library("tidyverse")
library("ggthemes")
devtools::install_github("tidyverse/ggplot2")
library(ggplot2)

# Load data
d <- read.csv("Energy_Industry_Salaries.csv") %>% 
  as_tibble()

# Plot all charts
for (i in seq_along(unique(d$Segment))) {
  
  xmin <- d %>% 
    filter(Segment == unique(d$Segment)[i]) %>% 
    select(Lower) %>% 
    min() - 5
  
  xmax <- d %>% 
    filter(Segment == unique(d$Segment)[i]) %>% 
    select(Upper) %>% 
    max() + 5
    
  p <- d %>% 
    filter(Segment == unique(d$Segment)[i]) %>% 
    gather(Metric, Value, -Segment, -Title) %>% 
    mutate(Title = fct_reorder(Title, Value, max)) %>% 
    ggplot(aes(x = Value, y = Title)) +
    geom_path(size = 1.5, col = "#2C8C22") +
    geom_point(size = 3.5, col = "#2C8C22") +
    geom_text(aes(label = paste0("£", Value, "K")), nudge_y = 0.23, size = 4.7, col = "black") +
    theme_wsj(color = "brown", title_family = "sans") +
    theme(
      axis.text.y = element_text(size = 14, face = "plain"),
      plot.title.position = "plot",
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank(),
      axis.line.x = element_blank(),
      plot.caption = element_text(size = 9)
    ) +
    xlim(xmin, xmax) +
    labs(
      title = unique(d$Segment)[i],
      caption = "Chart by Dmitrii Ishutin  |  Source: Green Recruitment Company salary survey 2020"
    )
  
  print(p)
}
