setwd('/Users/corban_nemeth/documents/github/tidytuesday/Week 27')

library(tidyverse)
library(lubridate)
library(tis)
library(scales)
library(ggthemes)

options(scipen = 999)

input.file <- "us_births_2000-2014.csv"
births.data <- read_csv(input.file)

Holidays <- data.frame(holiday = names(holidays(2000:2014, businessOnly = F)), 
                       date = ymd(holidays(2000:2014, businessOnly = F)))

Births <- births.data %>%
  mutate(date = ymd(paste(year, month, date_of_month, sep="-")),
         dow = wday(date, label=T, abbr=F, week_start = getOption("lubridate.week.start", 1)))
  
Births <- left_join(Births, Holidays, by="date") %>%
  mutate(daytype = case_when(
    !is.na(holiday) & day_of_week < 6 ~ "Weekday Holiday",
    !is.na(holiday) & day_of_week > 5 ~ "Weekend Holiday",
    day_of_week < 6 ~ "Weekday",
    day_of_week > 5 ~ "Weekend")) %>%
  select(births, date, holiday, daytype)
  
(BirthPlot <- ggplot(Births, aes(births)) +
              geom_density(aes(fill=factor(daytype)), alpha = 0.6) + 
              labs(title="Births by Day Type",
                   caption="Source: 538.com",
                   fill="Day Type"))


  
