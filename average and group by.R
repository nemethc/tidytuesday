average_births <- births %>%
  mutate(date = ymd(paste(year, month, date_of_month))) %>%
  group_by(month(date, label = T, abbr = F), day(date)) %>%
  summarize(avg_births = mean(births)) %>%
  ungroup() %>%
  set_names(c("month", "day", "avg_births"))

ggplot(average_births,
       aes(month, day)) +
  geom_tile(aes(fill = avg_births),colour = "white") + 
  scale_fill_gradient(low = "white",high = "steelblue") +
  theme_fivethirtyeight()