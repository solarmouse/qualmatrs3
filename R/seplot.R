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
  df <- subset(df, format(as.Date(df$date), "%Y")== year)
  df <- prev(df, prev)
  if (name == "quality"){
    print.quality.disease(df)
  }else
    if (name == "prevalence"){
      print.same.axis(df)} else
        if (name == "Types"){
          print("quality, prevalence")
        }else{
        print("Function failed: Please list graph types or
              write Types if you would like a list of graph options")
      }

}
