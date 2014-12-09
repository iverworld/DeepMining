## Unit Test

source("GetData.R");

GetDataTest = function(){
  data = GetData("train.csv", "test.csv");
  if(nrow(data$train) == 0 || nrow(data$test) == 0){
    warning("Empty training or testing data");
  }
}
GetDataTest();