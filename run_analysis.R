require(reshape2)

doIt <- function(directory, outputFileName)
{
  ## the file is of fixed width format. Each column is 16 character wide.
  ## There are 561 columns (features)
  width <- rep(16, 561)
  
  ## we are going to read from test and train directories
  setNames <- c("test", "train")
  ## this is a temporary variable to the first data frame
  dtPtr = NULL
  
  featureFile <- sprintf("%s/features.txt", directory)
  features  <- read.table(featureFile, sep=" ")
  colNames <- as.vector(features$V2)
  
  for(i in seq_along(setNames))
  {
    dataFile <- sprintf("%s/%s/X_%s.txt", directory, setNames[i], setNames[i])
    labelFile <- sprintf("%s/%s/y_%s.txt", directory, setNames[i], setNames[i])
    subjectFile <- sprintf("%s/%s/subject_%s.txt", directory, setNames[i], setNames[i])
    
    data <- read.fwf(dataFile, widths=width)
    activities <- read.table(labelFile)
    subjects <- read.table(subjectFile)
    
    ## combine the activity with data frame
    data$Activity <- activities$V1
    ## combine the subject with data frame
    data$Subject <- subjects$V1
    
    if (is.null(dtPtr))
    {
      dtPtr <- data
    }
    else
    {
      ## when we have two data frames (dtPtr and data)
      ## we will merge them
      dt <- merge(dtPtr, data, all=TRUE)
    }
  }
  ## set the column names
  names(dt)[1:561] <- as.vector(colNames)
  
  actFile <- sprintf("%s/activity_labels.txt", directory)
  activities <- read.table(actFile, sep=" ")
  ## set the Activity column to be descriptive
  dt$Activity <- factor(dt$Activity, levels=1:6, labels=activities$V2)
  
  ## create the regular expression to filter the column names
  selectedFeatures <- c("mean", "std")
  selectedFeatures <- sapply(selectedFeatures, function(x) x[1] <- paste(x[1], "\\(\\)", sep=""))
  selectedFeatures <- c(selectedFeatures, "Activity", "Subject")
  re <- paste(selectedFeatures, collapse="|")
  ## filter the features that we want
  dt2 <- dt[,grep(re, n)]
  
  ## write to a file
  write.csv(dt2, file=outputFileName, row.names = F)
  
  mdt <- melt(dt, id=c("Subject", "Activity"))  
  df <- dcast(mdt, Subject + Activity ~ variable, mean)
  
  write.csv(df, file="featureAverage.txt", row.names=F)
  return(dt)
}