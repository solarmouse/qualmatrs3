#' Prints sixdata plots with the same axdatais
#'
#' For blank blank blank
#' @param xdata A dataframe
#' @param y A numeric year
#' @return sixdata graphs with the same axdatais
#' @exdataport

print.same.axis <- function(x, y){
  x <- as.data.frame(x)
  xdata <- subset(x, format(as.Date(x$date), "%Y")== y)
  xdata$Lake_Name <- factor(xdata$Lake_Name)

  par(mfrow = c(2,3))
  for (i in 1:length(levels(xdata$Lake_Name))) {
  plot(xdata$overall_m_prev[which(xdata$Lake_Name==levels(xdata$Lake_Name)[i])] ~ xdata$date[which(xdata$Lake_Name==levels(xdata$Lake_Name)[i])], xdata,
       main=paste(levels(xdata$Lake_Name)[i]),
       xlim = c(range(xdata$date, na.rm=T)), ylim = c(range(xdata$overall_m_prev, na.rm=T)),
       ylab = "Overall Metch Prev", xlab = "date", type = "l")

  plot(xdata$Total_infected[which(xdata$Lake_Name==levels(xdata$Lake_Name)[i])] ~ xdata$date[which(xdata$Lake_Name==levels(xdata$Lake_Name)[i])], xdata,
       main= paste(levels(xdata$Lake_Name)[i]),
       xlim = c(range(xdata$date, na.rm=T)), ylim = c(range(xdata$Total_infected, na.rm=T)),
       ylab = "Total infected", xlab = "date", type = "l")

  plot(xdata$dent[which(xdata$Lake_Name==levels(xdata$Lake_Name)[i])] ~ xdata$date[which(xdata$Lake_Name==levels(xdata$Lake_Name)[i])], xdata,
       main=paste(levels(xdata$Lake_Name)[i]),
       xlim = c(range(xdata$date, na.rm=T)), ylim = c(range(xdata$dent, na.rm=T)),
       ylab = "Daphnia Density", xlab = "date", type = "l")

  plot(xdata$E_Ratio[which(xdata$Lake_Name==levels(xdata$Lake_Name)[i])]~ xdata$date[which(xdata$Lake_Name==levels(xdata$Lake_Name)[i])], xdata,
       main=paste(levels(xdata$Lake_Name)[i]),
       xlim = c(range(xdata$date, na.rm=T)), ylim = c(range(xdata$E_Ratio, na.rm=T)),
       ylab = "Egg Ratio", xlab = "date", type = "l")
  abline(h=1, lty=2)

  plot(xdata$E_Chla_Ratio[which(xdata$Lake_Name==levels(xdata$Lake_Name)[i])] ~ xdata$date[which(xdata$Lake_Name==levels(xdata$Lake_Name)[i])], xdata,
       main=paste(levels(xdata$Lake_Name)[i]),
       xlim = c(range(xdata$date, na.rm=T)), ylim = c(range(xdata$E_Chla_Ratio, na.rm=T)),
       ylab = "Egg:Chl", xlab = "date", type = "l")
  abline(h=1, lty =2)

  plot(xdata$Chl_Ratio[which(xdata$Lake_Name==levels(xdata$Lake_Name)[i])] ~ xdata$date[which(xdata$Lake_Name==levels(xdata$Lake_Name)[i])], xdata,
       main= paste(levels(xdata$Lake_Name)[i]),
       xlim = c(range(xdata$date, na.rm=T)), ylim = c(range(xdata$Chl_Ratio, na.rm=T)),
       ylab = "Inedible Chl Ratio", xlab = "date", type = "l")
  abline(h=1, lty=2)
}}
