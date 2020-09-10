#'Function allows user to select the year, prevelance, number of graphs printed, and types of graphs
#'@param df A dataframe
#'@param year Year, a numeric
#'@param prev Minimum prevlance proportion, a numeric
#'@param name A string containing the types of graphes you want to print
#'@return Several graphes for the desired year
#'@export

seplot <- function(df, year, prev, name){
  df <- as.data.frame(df)
  year <- as.numeric(year)
  prev <- as.numeric(prev)
  name <- as.character(name)
  if (is.na(year) == TRUE){
    stop("ERROR:You have not selected a year, if you wish to print multiple years, please use the print.mult.year function")
  }else{
    df <- subset(df, format(as.Date(df$date), "%Y")== year)
    df <- prev(df, prev)
    if (name == "quality"){
    print.quality.disease(df)
      }else if (name == "Time Series"){
      print.same.axis(df)
          }else if (name == "CP Ratio"){
          print.CP.ratio(df)
        }else{
          stop("ERROR:You have not inputed a valid graphtype Current types are quality and Time Series")}
  }
}
}
