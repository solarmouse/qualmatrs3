#'Returns data frame that onlu includes the prevelance indicated
#'@param x Dataframe
#'@param y A numeric value, desired level of prevelance
#'@return A data frame
#'@export

#Just realized that -Inf stands for negative infinity

'prev <- function(x,y){
  df <- as.data.frame(x)
  prev <- as.numeric(y)
  df[, "max_prev"] <- NA
  for (i in 1:length(levels(df$Lake_Name))) {
    if df$overall_m_prev[which(df$Lake_name==levels(df$Lake_Name)[i])] > prev{}
    else}

  return(df)
}'

'Create for loop that cycles through all levels, if level does nothas one value
that is over the limit, then it is deleted from that data frame'
