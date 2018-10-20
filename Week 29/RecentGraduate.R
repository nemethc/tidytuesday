setwd('/Users/corban_nemeth/documents/github/tidytuesday/Week 29')

library(tidyverse)
library(car)
library(scales)
library(plotly)
library(ggthemes)
library(ggalt)

options(scipen = 999)

input.file <- "recent-grads.csv"
grad.data <- read.csv(input.file)


gg <- ggplot(grad.data, aes(x=ShareWomen, y=Median, major = Major)) + 
  geom_point(aes(col=Major_category, size=Employed)) + 
  labs(y="Median Earnings", 
       x="Percent Women", 
       title="College Major by Earnings and Percent Women", 
       caption = "Source: TidyTuesday") +
  scale_y_continuous(labels=dollar_format(prefix="$")) +
  geom_smooth(aes(group = 1), method="loess", se=TRUE, fullrange=FALSE, level=0.95) +
  scale_color_viridis_d()

  
  
gg <- ggplotly(gg, tooltip = c("major", "Median", "ShareWomen"))
gg














