#'Returns data frame that excludes Lake-Years with maximum prevalence below the prevelance indicated
#'@param x Dataframe
#'@param y A numeric value, desired level of prevelance
#'@return A data frame
#'@export

if(require("tidyverse")){
  print("tidyverse is loaded correctly")
} else {
  print("trying to install tidyverse")
  install.packages("tidyverse")
  if(require(tidyverse)){
    print("tidyverse installed and loaded")
  } else {
    stop("could not install tidyverse, and I lazily made this function using it.  Sorry, but you'll need to install before use.")
  }
}

prev <- function(x,y){
  df <- as.data.frame(x)
  prev <- as.numeric(y)
  for (i in 1:length(df$date)){
    df$year[i] <- as.numeric(paste(format(as.Date(df$date, format="%d/%m/%Y"),"%Y")[i]))
  }
  tdf <- df %>%
    arrange(Lake_Name, date) %>% # organize rows in this nested order
    group_by(Lake_Name, year) %>%
    filter(!is.na(overall_m_prev)) %>% # drop rows with invalid frequencies
    mutate(MPREV = max(overall_m_prev, na.rm=T))%>%
    filter(MPREV > prev)%>%
    select(everything()) %>% # keep only the columns we want
    distinct() %>% as.data.frame() # drop duplicates.

  return(tdf)
}

# 'Create for loop that cycles through all levels, if level does nothas one value
# that is over the limit, then it is deleted from that data frame'
