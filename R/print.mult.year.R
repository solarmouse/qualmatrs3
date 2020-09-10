#' Prints graphs including multiple years
#' @param df Is a dataframe
#' @param yaxis Is a column name
#' @param axistitle Is a string
#' @return Graphs
#' @export


print.mult.year <- function(df, yaxis, axistitle){
df <- as.data.frame(df)

library(tidyverse)
df$Year <- format(as.Date(df$date), "%Y")
df %>%
  group_by(Lake_Name, Year) %>%
  mutate(n_obs = n()) %>%
  filter(n_obs > 2) %>%
  ggplot()+
  geom_line(aes(x = JD, y = yaxis, color = as.factor(Year)))+
  geom_point(aes(x = JD, y = yaxis, color = as.factor(Year)))+
  labs (x = "Julian Day", y = paste(axistitle)) +
  theme_bw()+
  theme(axis.text = element_text(size = 16, color = "black"),
        axis.title = element_text(size = 24, color = "black"),
        legend.title = element_blank(),
        axis.text.x = element_text(angle=315, vjust=0.5, hjust=0)) +
  facet_wrap(~Lake_Name)

}
