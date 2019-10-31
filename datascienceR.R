# This 
rm(list = setdiff(ls(), lsf.str()))
library(caret)
install.packages("ellipse")
# 
# Load data.  We look at the Iris dataset.  This can be found here \cite{}

iris_data <- read.csv(file='C:\\Users\\NB310068\\Downloads\\iris.data', header=FALSE, sep=",")

# set the column names in the dataset to "Sepal.Length","Sepal.Width","Petal.Length","Petal.Width","Species"
colnames(iris_data) <- c("Sepal.Length","Sepal.Width","Petal.Length","Petal.Width","Species")

# create a list of 80% of the rows in the original dataset we can use for training
train_index <- createDataPartition(iris_data$Species, p=0.80, list=FALSE)
# select 20% of the data for validation
test_data <- iris_data[-train_index,]
# use the remaining 80% of data to training and testing the models
train_data <- iris_data[train_index,]

# 
# Dimensions of the iris dataframe:
dim(iris_data)

# Types of the attributes:
sapply(iris_data, class)

# show the top 20 rows of the dataset:
head(iris_data,20)

# list the levels for the Species column:
levels(iris_data$Species)

# Breakdown of the instances in each class.

percentage <- prop.table(table(iris_data$Species)) * 100
cbind(freq=table(iris_data$Species), percentage=percentage)

# Statistical summary of all attributes.
summary(iris_data)


# Plot Iris variables
# 1. Univariate plots
# Split the data.  x into an nx4 dataframe:
x <- train_data[,1:4]
y <- train_data[,5]


# Create a boxplot for the value of each attribute and place the all boxplots on one image:
par(mfrow=c(1,4))
for(i in 1:4) {
  boxplot(x[,i], main=names(iris)[i])
}


# Create a bar plot for the species column:
plot(y)


# lattice graphs fir the features in the Iris dataset:
featurePlot(x=x, y=y, plot="ellipse")

# box and whisker plots for each attribute
featurePlot(x=x, y=y, plot="box")

# density plots for each attribute by class value
scales <- list(x=list(relation="free"), y=list(relation="free"))
featurePlot(x=x, y=y, plot="density", scales=scales)

# 
# 5. Evaluate Some Algorithms
# Now it is time to create some models of the data and estimate their accuracy on unseen data.
# 
# Here is what we are going to cover in this step:
#   
#   Set-up the test harness to use 10-fold cross validation.
# Build 5 different models to predict species from flower measurements
# Select the best model.
# 5.1 Test Harness
# We will 10-fold crossvalidation to estimate accuracy.

# This will split our dataset into 10 parts, train in 9 and test on 1 and release for all combinations of train-test splits. We will also repeat the process 3 times for each algorithm with different splits of the data into 10 groups, in an effort to get a more accurate estimate.

# Run algorithms using 10-fold cross validation, use the trainControl function and create the control variable:
control <- trainControl(method="cv", number=10)
# Create a metric variable set it equal to 'Accuracy':
metric <- "Accuracy"


# Train 5 models and use the caret train() function.  Set the method, metric and trcontrol values 

# a) linear algorithms
set.seed(7)
fit.lda <- train(Species~., data=train_data, method="lda", metric=metric, trControl=control)
# b) nonlinear algorithms
# CART
set.seed(7)
fit.cart <- train(Species~., data=train_data, method="rpart", metric=metric, trControl=control)
# kNN
set.seed(7)
fit.knn <- train(Species~., data=train_data, method="knn", metric=metric, trControl=control)
# c) advanced algorithms
# SVM
set.seed(7)
fit.svm <- train(Species~., data=train_data, method="svmRadial", metric=metric, trControl=control)
# d) Random Forest
set.seed(7)
fit.rf <- train(Species~., data=train_data, method="rf", metric=metric, trControl=control)


# Summarize all the models using the resamples function and the summary function to print results.
results <- resamples(list(lda=fit.lda, cart=fit.cart, knn=fit.knn, svm=fit.svm, rf=fit.rf))
summary(results)

# compare the accuracy from the results variable above of the 5 models using dotplot:
dotplot(results)

# summarize Best Model
# show the lda predictions against the test_data
predictions <- predict(fit.lda, test_data)

# Draw a confusion matrix using confusionMatrix() from the caret package.
confusionMatrix(predictions, test_data$Species)
