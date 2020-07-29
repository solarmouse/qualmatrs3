#' Preliminary exploration od data
#' @param x Dataframe
#' @param y numeric Year
#' @return A whole lot of graphs
#' @export
alpha.exploration <- function(x, y) {
  x <- as.data.frame(x)
  x <- subset(x, format(as.Date(x$date), "%Y")== y)
  x$Lake_Name <- factor(x$Lake_Name)
#Daphnia Density
par(mfrow = c(3,6))
for (i in 1:length(levels(x$Lake_Name))) {
  plot(x$dent[which(x$Lake_Name==levels(x$Lake_Name)[i])] ~ x$date[which(x$Lake_Name==levels(x$Lake_Name)[i])], x,
       main=paste(levels(x$Lake_Name)[i]),
       xlim = c(range(x$date, na.rm=T)), ylim = c(range(x$dent, na.rm=T)),
       ylab = "daphnia dens", xlab = "date", type = "l")
}

#Amount of Edible Chla
par(mfrow = c(3,6))
for (i in 1:length(levels(x$Lake_Name))) {
  plot(x$Edible_Chl[which(x$Lake_Name==levels(x$Lake_Name)[i])] ~ x$date[which(x$Lake_Name==levels(x$Lake_Name)[i])], x,
       main=paste(levels(x$Lake_Name)[i]),
       xlim = c(range(x$date, na.rm=T)), ylim = c(range(x$Edible_Chl, na.rm=T)),
       ylab = "Edible Chla", xlab = "date", type = "l")
}

#Overall prevelance of metch infection
par(mfrow = c(3,6))
for (i in 1:length(levels(x$Lake_Name))) {
  plot(x$overall_m_prev[which(x$Lake_Name==levels(x$Lake_Name)[i])] ~ x$date[which(x$Lake_Name==levels(x$Lake_Name)[i])], x,
       main=paste(levels(x$Lake_Name)[i]),
       xlim = c(range(x$date, na.rm=T)), ylim = c(range(x$overall_m_prev, na.rm=T)),
       ylab = "Overall Metch Prev", xlab = "date", type = "l")
}

#Ration of edible to inedible Chla

par(mfrow = c(3,6))
for (i in 1:length(levels(x$Lake_Name))) {
  plot(x$Chl_Ratio[which(x$Lake_Name==levels(x$Lake_Name)[i])] ~ x$date[which(x$Lake_Name==levels(x$Lake_Name)[i])], x,
       main=paste(levels(x$Lake_Name)[i]),
       xlim = c(range(x$date, na.rm=T)), ylim = c(range(x$Chl_Ratio, na.rm=T)),
       ylab = "Chl Ratio", xlab = "date", type = "l")
}

#Estimated total daphnia infected with metch


par(mfrow = c(3,6))
for (i in 1:length(levels(x$Lake_Name))) {
  plot(x$Total_infected[which(x$Lake_Name==levels(x$Lake_Name)[i])] ~ x$date[which(x$Lake_Name==levels(x$Lake_Name)[i])], x,
       main=paste(levels(x$Lake_Name)[i]),
       xlim = c(range(x$date, na.rm=T)), ylim = c(range(x$Total_infected, na.rm=T)),
       ylab = "Total infected", xlab = "date", type = "l")
}

#Spore yield
par(mfrow = c(3,6))
for (i in 1:length(levels(x$Lake_Name))) {
  plot(x$Spore_yield[which(x$Lake_Name==levels(x$Lake_Name)[i])] ~ x$date[which(x$Lake_Name==levels(x$Lake_Name)[i])], x,
       main=paste(levels(x$Lake_Name)[i]),
       xlim = c(range(x$date, na.rm=T)), ylim = c(range(x$Spore_yield, na.rm=T)),
       ylab = "Total infected", xlab = "date", type = "l")
}

#Eggs per daphnia
par(mfrow = c(3,6))
for (i in 1:length(levels(x$Lake_Name))) {
  plot(x$E_Ratio[which(x$Lake_Name==levels(x$Lake_Name)[i])] ~ x$date[which(x$Lake_Name==levels(x$Lake_Name)[i])], x,
       main=paste(levels(x$Lake_Name)[i]),
       xlim = c(range(x$date, na.rm=T)), ylim = c(range(x$E_Ratio, na.rm=T)),
       ylab = "Egg Ratio", xlab = "date", type = "l")
}

#Eggs per daphnia per chlorophyll


par(mfrow = c(3,6))
for (i in 1:length(levels(x$Lake_Name))) {
  plot(x$E_Chla_Ratio[which(x$Lake_Name==levels(x$Lake_Name)[i])] ~ x$date[which(x$Lake_Name==levels(x$Lake_Name)[i])], x,
       main=paste(levels(x$Lake_Name)[i]),
       xlim = c(range(x$date, na.rm=T)), ylim = c(range(x$E_Chla_Ratio, na.rm=T)),
       ylab = "Egg:Chla Ratio", xlab = "date", type = "l")
}
#Nitrogen
par(mfrow = c(3,6))
for (i in 1:length(levels(x$Lake_Name))) {
  plot(x$N_ug[which(x$Lake_Name==levels(x$Lake_Name)[i])] ~ x$date[which(x$Lake_Name==levels(x$Lake_Name)[i])], x,
       main=paste(levels(x$Lake_Name)[i]),
       xlim = c(range(x$date, na.rm=T)), ylim = c(range(x$N_ug, na.rm=T)),
       ylab = "Nitrogen", xlab = "date", type = "l")
}

#Quality indices
#Nitrogen to Edible Chla ratio


par(mfrow = c(3,6))
for (i in 1:length(levels(x$Lake_Name))) {
  plot(x$N_Chla[which(x$Lake_Name==levels(x$Lake_Name)[i])] ~ x$date[which(x$Lake_Name==levels(x$Lake_Name)[i])], x,
       main=paste(levels(x$Lake_Name)[i]),
       xlim = c(range(x$date, na.rm=T)), ylim = c(range(x$N_Chla, na.rm=T)),
       ylab = "Nitrogen to Edible Chla ratio", xlab = "date", type = "l")
}

#P to Edible Chla ratio


par(mfrow = c(3,6))
for (i in 1:length(levels(x$Lake_Name))) {
  plot(x$P_Chla[which(x$Lake_Name==levels(x$Lake_Name)[i])] ~ x$date[which(x$Lake_Name==levels(x$Lake_Name)[i])], x,
       main=paste(levels(x$Lake_Name)[i]),
       xlim = c(range(x$date, na.rm=T)), ylim = c(range(x$P_Chla, na.rm=T)),
       ylab = "P to Edible Chla ratio", xlab = "date", type = "l")
}
}
