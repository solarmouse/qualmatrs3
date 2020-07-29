#'Prints 3 graphs
#'
#'Prints 3 graphs with quality indices on the x axis and prevelance on the y axis
#'@param x A dataframe
#'@param y A numeric Year
#'@return 3 graphs
#'@export
print.quality.disease <- function(x, y) {
  x <- as.data.frame(x)
  xdata <- subset(x, format(as.Date(x$date), "%Y")== y)
  xdata$Lake_Name <- factor(xdata$Lake_Name)
  par(mfrow = c(2,3))
  for (i in 1:length(levels(xdata$Lake_Name))) {
  plot(xdata$overall_m_prev[which(xdata$Lake_Name==levels(xdata$Lake_Name)[i])] ~ xdata$Edible_Chl[which(xdata$Lake_Name==levels(xdata$Lake_Name)[i])], xdata,
       main=paste(levels(xdata$Lake_Name)[i]),
       xlim = c(range(xdata$Edible_Chl, na.rm=T)), ylim = c(range(xdata$overall_m_prev, na.rm=T)),
       ylab = "Overall Metch Prev", xlab = "Edible Chl", type = "p")
  plot(xdata$overall_m_prev[which(xdata$Lake_Name==levels(xdata$Lake_Name)[i])] ~ xdata$E_Ratio[which(xdata$Lake_Name==levels(xdata$Lake_Name)[i])], xdata,
       main=paste(levels(xdata$Lake_Name)[i]),
       xlim = c(range(xdata$E_Ratio, na.rm=T)), ylim = c(range(xdata$overall_m_prev, na.rm=T)),
       ylab = "Overall Metch Prev", xlab = "Egg ratio", type = "p")
  plot(xdata$overall_m_prev[which(xdata$Lake_Name==levels(xdata$Lake_Name)[i])] ~ xdata$E_Chla_Ratio[which(xdata$Lake_Name==levels(xdata$Lake_Name)[i])], xdata,
       main=paste(levels(xdata$Lake_Name)[i]),
       xlim = c(range(xdata$E_Chla_Ratio, na.rm=T)), ylim = c(range(xdata$overall_m_prev, na.rm=T)),
       ylab = "Overall Metch Prev", xlab = "Egg Chl Ratio", type = "p")
}}
