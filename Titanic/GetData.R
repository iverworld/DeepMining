GetData = function(trainFile, testFile){
  # Get train and test data from csv format to table.
  #
  # Args:
  #   trainFile  : train file path.
  #   testFile   : test file path.
  # Returns:
  #   $train : train data table.
  #   $test  : test data table.
  source("extern.R");
  column.types <- c('integer',   # PassengerId
                    'factor',    # Survived 
                    'factor',    # Pclass
                    'character', # Name
                    'factor',    # Sex
                    'numeric',   # Age
                    'integer',   # SibSp
                    'integer',   # Parch
                    'character', # Ticket
                    'numeric',   # Fare
                    'character', # Cabin
                    'factor'     # Embarked
                    );
  tryCatch(
      {
      train = read.csv(trainFile, colClasses=column.types, na.strings=c("NA", ""));                    
      test = read.csv(testFile, colClasses=column.types[-2], na.strings=c("NA", ""));
      train$Forcast = 0;
      test$Forcast = 0;
      return (list("train"=train, "test"=test));
    }, 
    error = function(){
      print("Error in reading files");
    }
  );  
};