#' Organizes original data
#' This function doesn't actually work
#' @param x Dataframe
#' @return A new edited dataframe
#' @export
organize <- function(x) {
  x <- as.data.frame(x)
  x$Lake_Name <- factor(x$Lake_Name)
  levels(x$Lake_Name)

  x$M_m_e[which(x$M_m_e == "#DIV/0!")]<-NA
  x$M_s_e[which(x$M_s_e == "#DIV/0!")]<-NA

  x$M_m_e <- as.numeric(paste(x$M_m_e))
  x$M_s_e <- as.numeric(paste(x$M_s_e))

  x$date <- as.Date(x$date, "%m/%d/%Y")
  x$Lake_Name[which(x$Lake_Name=="university")]<- "University"
  MostxData <- subset(x, x$Lake_Name != "University")
  MostxData$Lake_Name[which(MostxData$Lake_Name==" Todd")]<- "Todd"
  MostxData$Lake_Name <- factor(MostxData$Lake_Name)
  MostxData$Ined_Chla <- MostxData$Total_Chl - MostxData$Edible_Chl
  for (i in 1:length(MostxData$Ined_Chla)){
    if (is.na(MostxData$Ined_Chla[i])==TRUE){} else
      if(MostxData$Ined_Chla[i] < 0) {MostxData$Ined_Chla[i] <- 0}
  }
  MostxData$Chl_Ratio <- MostxData$Ined_Chl/MostxData$Total_Chl
  MostxData$Total_infected <- MostxData$dent*MostxData$overall_m_prev
  MostxData$E_Chla_Ratio <- MostxData$E_Ratio/MostxData$Edible_Chl
  MostxData$N_Chla <- MostxData$N_ug/MostxData$Edible_Chl
  MostxData$P_Chla <- MostxData$P_ug/MostxData$Edible_Chl

  return(MostxData)
}
