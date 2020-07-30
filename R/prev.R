#'Returns data frame that onlu includes the prevelance indicated
#'@param x Dataframe
#'@param y A numeric value, desired level of prevelance
#'@return A data frame
#'@export

library(tidyverse)
prev.mod <- function(x,y){
  df <- as.data.frame(x)
  prev <- y
  for (i in 1:length(levels(df$Lake_Name))) {
    df$max_prev[i]<- mean(df$overal_m_prev[which(df$Lake_Name==levels(df$Lake_Name)[i])], na.rm = TRUE) >0
    'selected_prev <- df$max_prev[which(df$Lake_Name==levels(df$Lake_Name)[i])] > prev'}
  'new_df <- subset(df, df$selected_prev == TRUE)'
  return(df)
}

'Create for loop that cycles through lakes use which command and calc max value
then make another column that is TRUE of FALSE, then subset data if ==TRUE, return
new dataframe'
