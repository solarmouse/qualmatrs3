#'Reordering Data Function
#'You may need to change Lake_Name to Lake or Round to week depending on the data
#'@param x A data frame with Lake and Round in the order you want
#'@param y is the Dataframe you are trying to order
#'@return An ordered dataframe
#'@export
combiner <- function(x, y){
  x <- as.data.frame(x)
  y <- as.data.frame(y)
  x$id <- factor(paste(x$Lake_Name, c(x$Round)))
  y$id <- factor(paste(y$Lake, c(y$Week)))
  df2 <- merge(x, y, by = "id", all = TRUE, incomparables = NULL)
  sort(df2$id, decreasing = FALSE, na.last = TRUE)
  return(df2)
}
#Need some way of assining place holders a Lake name and Week.
#Probably can look at the last character of the id and
#read it as numeric. Can figure out lake name by comparing
#the first 3 characters to the lake name above and below
#and assigning the appropriate one
