setwd('/Users/corban_nemeth/documents/github/tidytuesday')

library(tidyverse)
library(reshape2)
library(lubridate)
library(tis)
library(scales)
library(ggthemes)

options(scipen = 999)

input.file <- "voter_turnout.csv"
turnoutData <- read_csv(input.file)

president_years <- seq(1980, 2012, 4)
midterm_years <- seq(1982, 2014, 4)
election.type <- data.frame(president_years , midterm_years)

electionType <- melt(election.type, variable.name = "type", value.name = "year")

head(turnout.data)

turnoutData <- left_join(turnoutData, electionType, by = "year") %>%
  mutate(turnout_percent = turnout.data$votes / turnout.data$eligible_voters) %>%
  group_by(turnout.data$state, turnout.data$type) %>%
  summarize(avg_turnout = mean(turnout.data$turnout_percent)) %>%
  ungroup()


head(turnoutData)
























