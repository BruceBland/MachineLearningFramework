# Plumber Example
# See: https://www.rplumber.io/docs/index.html for more details
#
# This function provides three end points
#
# /echo which tests the system is working
#
# and 
#
# /plot which returns a plot as a png
#
# and 
#
# /table which will return a json table
#
# Use MakePlumberAPI to run this file

#' Echo the parameter that was sent in
#' @param msg The message to echo back.
#' @get /echo
function(msg=""){
  list(msg = paste0("The test message is: '", msg, "'"))
}

#' Plot out data from the iris dataset
#' @param auto If provided, filter the data to only this species (e.g. 'setosa')
#' @get /plot
#' @png (width = 800, height = 500)
function(auto){
  
  myData <- mtcars
  ChartTitle <- "All Cars"
  
  # Filter if the species was specified
  if (!missing(auto)){
    ChartTitle <- paste("MTCARS - Showing where auto filed = ",auto)
    myData <- subset(myData, am == auto)
  }
  
  # Load ggplot2 library
  library(ggplot2)
  
  # Now print the chart
  print(ggplot(myData) + 
    geom_point(aes(x=mpg,y=wt,colour=cyl),alpha=0.8) + 
    labs(title=ChartTitle) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) )
  
}

#' Echo the parameter that was sent in
#' @param dataset The data set to return
#' @get /table
#' @json
function(dataset=""){
  
  # Filter if the species was specified
  if (!missing(dataset)){
    if (dataset=="mtcars")
    {
      list(mtcars)
    } else {
      list(msg = paste0("Cannot handle dataset of: '", dataset, "'"))
    }
  } else {
    msg = paste0("Cannot process if no datset is given")
  }
}

#' This funtion will return mtcars as a csv text formatted list
#' @get /csvdata
function(res) {
  
  # Convert to text
  con <- textConnection("val","w")
  write.csv(mtcars, con)
  close(con)
  
  # Add to body
  res$body <- paste(val, collapse="\n")
  res
  
}

#* @serializer contentType list(type="application/pdf")
#* @get /pdf
function(){
  tmp <- tempfile()
  pdf(tmp)
  plot(1:10, type="b")
  text(4, 8, "PDF from plumber!")
  text(6, 2, paste("The time is", Sys.time()))
  dev.off()
  
  readBin(tmp, "raw", n=file.info(tmp)$size)
}
