source("extern.R");

changeTitles <- function(data, old.titles, new.title) {
  # Change the title for grouoping use.
  # Args:
  # data        : data.
  # old.titles  : list of titles to be converted.
  # new.title   : converted title.
  # Return:
  # data$titles.
  print(sprintf("changeTitles to %s...", new.title));
  for (honorific in old.titles) {
    data$Title[ which( data$Title == honorific)] <- new.title
  }
  return (data);
}

getTitle <- function(names) {
  # Get the title from name.
  # Args:
  # names.
  # Return:
  # title.
  return (unlist(sapply(names, function(x){
    title.dot.start <- regexpr("\\,[A-Z ]{1,20}\\.", x, TRUE);
    title.comma.end <- title.dot.start + attr(title.dot.start, "match.length")-1;
    return(substr(x, title.dot.start+2, title.comma.end-1));
  })));
}   

factTitle = function(data){
  # Factor the title into a few groups.
  # Args:
  # data.
  # Return:
  # data$Title.
  data = changeTitles(data, c("Capt", "Col", "Don", "Dr", "Jonkheer", "Lady", "Major", "Rev", "Sir"), "Sir");
  data = changeTitles(data, c("the Countess", "Ms"), "Mrs");
  data = changeTitles(data, c("Mlle", "Mme"), "Miss");
  data$Title <- as.factor(data$Title);  
}

imputeMedian <- function(impute.var, filter.var) {
  # Fill NA with mean group by filter.var.
  # Args:
  # impute.var  : Vector with NA values.
  # filter.var  : Vector for grouping.
  # var.levels  : NA value field.
  # Return:
  # impute.var with filled values.
  var.levels = unique(filter.var[which(is.na(impute.var))]);
  for (v in var.levels) {
    impute.var[ which( filter.var == v)] <- impute(impute.var[ 
      which( filter.var == v)])
  }
  return (impute.var)
};

fillGap = function(data){
  # Fill missing data gap.
  # Args: 
  # data.
  # Return:
  # data with filled gaps.
  print("getTitle is called...");
  data$Title = getTitle(data$Name);
  print("factTitle is called...");
  data$Title = factTitle(data);
  print("imputeMedian is called...");
  data$Age = imputeMedian(data$Age, data$Title);
  data$Embarked[which(is.na(data$Embarked))] <- 'S'
  return (data);
}

GetData = function(trainFile, testFile){
  # Get train and test data from csv format to table.
  #
  # Args:
  #   trainFile  : train file path.
  #   testFile   : test file path.
  # Returns:
  #   $train : train data table.
  #   $test  : test data table.
  source("FeatureEngrg.R")
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
  # tryCatch(
  #     {
      train = read.csv(trainFile, colClasses=column.types, na.strings=c("NA", ""));                    
      test = read.csv(testFile, colClasses=column.types[-2], na.strings=c("NA", ""));
      print("train and test data are loaded...");
      ## Using Fate ILO Survived because term is shorter and just sounds good
      train$Fate <- train$Survived
      ## Revaluing Fate factor to ease assessment of confusion matrices later
      train$Fate <- revalue(train$Fate, c("1" = "Survived", "0" = "Perished"))      
      train$Forcast = 0;
      test$Forcast = 0;
      train = fillGap(train);
      test = fillGap(test);
      train = FeatureEngrg(train);
      test = FeatureEngrg(test);
      return (list("train"=train, "test"=test));
    # }, 
    # error = function(){
    #   print("Error in reading files");
    # }
  # );  
};
