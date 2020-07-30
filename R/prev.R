#'Returns data frame that onlu includes the prevelance indicated
#'@param x Dataframe
#'@param y A numeric value, desired level of prevelance
#'@return A data frame
#'@export

library(tidyverse)
prev.mod <- function(x,y){
  df <- as.data.frame(x)
  prev <- y

  new_df <- df %>%
    arrange(Lake_Name, Round)%>%
    group_by(Lake_Name, Round)%>%
    mutate(overall_m_prev = overall_m_prev > y)%>%
    distinct() %>% as.data.frame()

  return(new_df)
}
