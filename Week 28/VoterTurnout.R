setwd('/Users/corban_nemeth/documents/github/tidytuesday/Week 28')

library(tidyverse)
library(reshape2)
library(scales)
library(ggthemes)
library(ggalt)

input.file <- "voter_turnout.csv" #from R4DS repo
input.file2 <- "partisanlean.csv" #from gallup report
turnout.data <- read_csv(input.file)
partisan.lean <- read_csv(input.file2)

president_year <- seq(1980, 2012, 4) #set presidential years
midterm_year <- seq(1982, 2014, 4)  #set midterm years
election.type <- data.frame(president_year , midterm_year)

electionType <- melt(election.type, variable.name = "type", value.name = "year")

turnoutData <- left_join(turnout.data, electionType, by = "year")
turnoutData <- left_join(turnoutData, partisan.lean, by = "state")

turnoutData <- turnoutData %>%
  mutate(turnout_percent = (votes / eligible_voters)) %>% #calculate % turnout
  group_by(state) %>%
  filter(sum(is.na(turnout_percent))<9)%>% #filters states with missing observations
  group_by(state, type, partisan) %>%
  summarize(avg_turnout = mean(turnout_percent, na.rm = TRUE)) %>% #avg turnout by state
  ungroup() %>%
  spread(type, avg_turnout) %>%
  filter(state != "United States (Excl. Louisiana)" & state != "United States")

#Define as factor so the values can be used for ranking
turnoutData$state <- factor(turnoutData$state, levels = turnoutData$state[order(desc(turnoutData$president_year))])

#graph building
gg <- ggplot(turnoutData, aes(x=midterm_year, xend=president_year, y=state, group=state, colour = partisan )) + 
  geom_dumbbell(size=0.75) + 
  scale_x_continuous(label=percent, limits = c(.3, .8)) + 
  scale_color_manual(values = c("purple", "blue", "red"),
                     name = "Partisan Lean") +
  labs(x=NULL, 
       y=NULL, 
       title="Average Midterm and Presidential Voter Turnout", 
       subtitle="1980 - 2014", 
       caption="@corbannemeth", size = 15) +
  theme_tufte(base_size = 11, base_family = "serif", ticks = TRUE) +
  theme(plot.caption = element_text(size = 9, face = "italic")) + 
  theme(plot.title = element_text(face = "bold"))
  
plot(gg)