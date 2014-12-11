source("GetData.R");
data = GetData("train.csv", "test.csv");

set.seed(23);
train.batch.rows = createDataPartition(data$train$Survived, p = 0.8, list = FALSE);
train.batch = data$train[train.batch.rows,];  

Titanic.logit.1 <- glm(Fate ~ Sex + Class + Age + Family + Embarked + Fare, 
                       data = train.batch, family=binomial("logit"));

cv.ctrl <- trainControl(method = "repeatedcv", repeats = 3,
                        summaryFunction = twoClassSummary,
                        classProbs = TRUE);
glm.tune.1 <- train(Fate ~ Sex + Class + Age + Family + Embarked,
                    data = data$train,
                    method = "glm",
                    metric = "ROC",
                    trControl = cv.ctrl);

summary(glm.tune.1)

ada.grid = expand.grid(.iter = c(50, 100), .maxdepth = c(4, 8), .nu = c(0.1, 1))
set.seed(35)
ada.tune <- train(Fate ~ Sex + Class + Age + Family + Embarked, 
                  data = train.batch,
                  method = "ada",
                  metric = "ROC",
                  tuneGrid = ada.grid,
                  trControl = cv.ctrl)