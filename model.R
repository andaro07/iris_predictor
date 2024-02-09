library(RCurl)
library(randomForest)
library(caret)


iris_df = read.csv(text = getURL("https://raw.githubusercontent.com/dataprofessor/data/master/iris.csv"))
# str(iris_df)

iris_df$Species = as.factor(iris_df$Species)

train_ids = createDataPartition(iris_df$Species, p=0.8, list = FALSE)
training_set = iris_df[train_ids,]
testing_set = iris_df[-train_ids,]

write.csv(training_set, "training_set.csv")
write.csv2(testing_set, "testing_set.csv")

train_set = read.csv("training_set.csv", header = TRUE)
train_set$Species = as.factor(train_set$Species)
train_set = train_set[,-1]


model = randomForest(Species~., data = train_set, ntree=500, mtry=4, importance=TRUE)

saveRDS(model, "model.rds")
