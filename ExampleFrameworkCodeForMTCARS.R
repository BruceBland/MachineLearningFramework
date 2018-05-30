# Example Framework code
rm(list=ls())

# Split function
SplitData <- function(DataFrame,PercentageToSplit)
{
  # Will add index column
  DataFrame$x <- seq(1,nrow(DataFrame))
  
  ## 70% of the sample size
  smp_size <- floor(0.70 * nrow(DataFrame))
  
  # Create sampling index
  train_ind <- sample(DataFrame$x, size = smp_size)
  
  # Now split te data into training and test
  train <- DataFrame[train_ind, ]
  test <- DataFrame[-train_ind, ]
  
  # Return list of data frames
  ReturnList <- list(train,test)
  
  return(ReturnList)
}

ImportancePlot <- function(Model,Title="",SubTitle="",Caption="")
{
  
  library(ggplot2)
  
  MeanDecreaseGini <- importance(Model)
  MeanDecreaseGini <- as.data.frame(MeanDecreaseGini)
  MeanDecreaseGini$Variable <- rownames(MeanDecreaseGini)
  
  ImpPlot <- ggplot(data = MeanDecreaseGini, aes(Variable, IncNodePurity)) +
    geom_bar(stat = "identity", position = "dodge",colour="red",alpha=0.8,fill="red") +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    labs(caption = Caption) +
    ggtitle(Title,
            subtitle = paste(SubTitle)) 
  print(ImpPlot)
}

# Function to load a check data 
DataLoadAndFormat <- function(Backtest=TRUE,Debug=TRUE)
{
  if (Debug==TRUE) {print("Loading data")}
  
  # Use example iris data set
  DataFrame <- mtcars
  

  
  return(DataFrame)
}

# Function to pre-process the data
PreProcess <- function(DataFrame,Columns,ColumnNames,Backtest=TRUE,Debug=TRUE)
{
  if (Debug==TRUE) {print("PreProcessing data")}
  
  # Generic code
  DataFrame <- DataFrame[,Columns]
  colnames(DataFrame) <- ColumnNames
  
  # Split data
  ListOfDataFrames <- SplitData(DataFrame,PercentageToSplit=80)
  
  return(ListOfDataFrames)
}

TrainingModel <- function(DataFrame,ColumnNames,PredictVariable="S",NTrees=5,
                          Backtest=TRUE,Debug=TRUE,SaveModel=FALSE,PlotImportance=TRUE)
{
  if (Debug==TRUE) {print("Training on data")}
  
  library(randomForest)
  
  PredictVariable <- DataFrame[,PredictVariable]
  
  VariableColumns <- DataFrame[,ColumnNames]
  
  DataFrame <- cbind(VariableColumns,PredictVariable)
  
  # Train
  Model = randomForest(PredictVariable ~ . ,
                       data = DataFrame,
                       keep.forest=TRUE,
                       importance=TRUE,
                       ntrees = NTrees)
  
  # Save model if required
  if (SaveModel == TRUE) {saveRDS(Model, "RandomForest.rds")}
  
  # Plot importance
  if (PlotImportance == TRUE) {ImportancePlot(Model,"Example Importance Plot","Random Forest Model")}
  
  return(Model)
  
}

Predict <- function(DataFrame,Model,
                    Backtest=TRUE,Debug=TRUE)
{
  if (Debug==TRUE) {print("Predicting from model and data")}
  
  DataFrame$Prediction <- predict(Model,DataFrame)
  
  return(DataFrame)
}

PostProcess <- function(DataFrame,PredictVariable="",
                        Backtest=TRUE,Debug=TRUE)
{
  
  if (Debug==TRUE) {print("Post processing data")}
  
  PredictVariable <- DataFrame[,PredictVariable]
  
  # reformat data
  DataFrame$Error <- DataFrame$Prediction - PredictVariable
  print(DataFrame)
  
  RMSE <- sqrt(sum(DataFrame$Error^2))
  DataFrame <- data.frame(Desc="RMS Error",RMSE=RMSE)
  return(DataFrame)
}

# Main process code here
#[, 1]	mpg	Miles/(US) gallon
#[, 2]	cyl	Number of cylinders
#[, 3]	disp	Displacement (cu.in.)
#[, 4]	hp	Gross horsepower
#[, 5]	drat	Rear axle ratio
#[, 6]	wt	Weight (1000 lbs)
#[, 7]	qsec	1/4 mile time
#[, 8]	vs	Engine (0 = V-shaped, 1 = straight)
#[, 9]	am	Transmission (0 = automatic, 1 = manual)
#[,10]	gear	Number of forward gears
#[,11]	carb	Number of carburetors

# Load data
DataFrame <- DataLoadAndFormat(Backtest=TRUE,Debug=TRUE)
if (nrow(DataFrame)>0) 
{
  # Pre-Process
  ListOfDataFrames <- PreProcess(DataFrame,c("carb","cyl","disp","hp","gear"),
                                 c("carb","Cyl","Displace","HP","Gears"),
                                 Backtest=TRUE,
                                 Debug=TRUE)
  # Training
  Model <-     TrainingModel(ListOfDataFrames[[1]],
                             c("Cyl","Displace","HP","Gears"),
                             PredictVariable="carb",
                             NTrees=5,
                             SaveModel=FALSE,
                             PlotImportance=TRUE)
  
  # Predict from training
  TrainingPredictions <- Predict(ListOfDataFrames[[1]],Model,
                                 Backtest=TRUE,Debug=TRUE)
  # Predict testing set
  TestingPredictions <- Predict(ListOfDataFrames[[2]],Model,
                                Backtest=TRUE,Debug=TRUE)
  # Do results processing
  ResultsDataFrame <- PostProcess(TestingPredictions,PredictVariable="carb",
                                  Backtest=TRUE,Debug=TRUE)
  
  print(ResultsDataFrame)
  
} else {
  print("No data found")
}
