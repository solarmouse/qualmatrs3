'df %>%
  group_by(Lake_Name, Year) %>%
  mutate(n_obs = n()) %>%
  filter(n_obs > 2, Lake_Name != "Tree") %>%
  ggplot()+
  geom_line(aes(x = Julian.day, y = freq_metsch, color = as.factor(Year)))+
  geom_point(aes(x = Julian.day, y = freq_metsch, color = as.factor(Year)))+
  labs (x = "Julian Day", y = "Infection Frequency") +
  theme_bw()+
  theme(axis.text = element_text(size = 16, color = "black"),
        axis.title = element_text(size = 24, color = "black"),
        legend.title = element_blank()) +
  facet_wrap(~Lake_Name)'
