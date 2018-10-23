setwd('/Users/corban_nemeth/documents/github/tidytuesday/Week 30')

library(tidyverse)
library(scales)
library(plotly)
library(lubridate)

options(scipen = 999)

input.file <- "movie_profit.csv"
movie.profit <- read.csv(input.file)

movie.profit$release_date <- mdy(movie.profit$release_date)
head(movie.profit)

movie.profit <- movie.profit %>%
  mutate(year = year(release_date),
         total_gross = domestic_gross + worldwide_gross,
         ROI = total_gross/production_budget) %>%
  group_by(genre, year, add = TRU) %>%
  summarise(mean(ROI)) %>%
  ungroup() %>%
  arrange(desc(`mean(ROI)`))


head(movie.profit)



















