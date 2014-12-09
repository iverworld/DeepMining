## Getdata

source("extern.R");

GetData = function(trainFile, testFile){
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