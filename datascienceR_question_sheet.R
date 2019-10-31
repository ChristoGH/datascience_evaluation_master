# Created by Christo Strydom.
# 
# You do not need to be an R champion to do any of the tasks below.
# Please ask if youre stuck or if anything is unclear.
# It is done to evaluate your current level of skill as an aspiring data scientist.
# The answers to the 'questions' can easily be found in a google search.
# You have internet access.  Please use it!
# 
# 0. Infrastructure preparation
# 1. Data acquisition
# 2. Data exploration
# 3. Data visualization
# 4. Model construction
# 5. Model Evaluation
# 6. Interpretation


# Firstly, remove all variables from the R environment with the following line of code:
rm(list = setdiff(ls(), lsf.str()))

# load the caret package:
'***YOUR CODE HERE***'

# Install the ellipse package
install.packages('***YOUR CODE HERE***')

# Change candidate.name to your own name in lower case and no spaces as per the directory under candidates:
candidate.name <- 'candidate.name'

# construct the save.dir variable for graphs using paste0:
save.dir <- paste0('C:\\candidates\\',candidate.name,'\\graphs\\')

# Set the working directory to yours:
setwd(paste0('***YOUR CODE HERE***'))

# =============================================================================================
# 1. Data acquisition
# --------------------------------------------------------------------------------------------
# Download and save the Iris dataset (it can be found here()):
# Load the Iris dataset (comma delimited file and NO HEADER), call the data set iris_data: 
  iris_data <- read.csv('***YOUR CODE HERE***')

  # Set the column names in the dataset of iris_data to "Sepal.Length","Sepal.Width","Petal.Length","Petal.Width","Species".  
  # Use the colnames function:
'***YOUR CODE HERE***'


  # use the caret createDataPartition() function and create an index list of 80% 
  # of the rows into the original dataset, call the result train_index:
  train_index <- '***YOUR CODE HERE***'
  
  # Create the test dataset, call it test_data:
  # select 20% of the data for testing or validation. Use train_index:
  test_data <- '***YOUR CODE HERE***'
  
  # use the remaining 80% of data for training models, call the data set train_data:
  train_data <- '***YOUR CODE HERE***'

# ==========================================================================================
# 2. Data exploration
# ------------------------------------------------------------------------------------------------------------------------------------------------  

    # Reveal the dimensions of the train data:
  '***YOUR CODE HERE***'

  # List the class types of each of the 5 attributes. 
  # Use the sapply function and the train dataset:
  '***YOUR CODE HERE***'

  # print to screen the top 20 rows of the train dataset:
  '***YOUR CODE HERE***'

  # The Species column contains factors.
  # List the levels for the Species column.  
  # This reveals the number of categories.
  '***YOUR CODE HERE***'

  # Count the number of members in each class of the species column,
  # assign it to species.count.table:
  '***YOUR CODE HERE***'
    
  # Use the the prop.table function to create a table of proportions:
  # assign it to species.percentage.table
  '***YOUR CODE HERE***'
  
  # Use the summary() function to reveal a statistical summary of all attributes
  # in train_data:
  '***YOUR CODE HERE***'

    
# ================================================================================================================================================
# 3. Data visualization
# ------------------------------------------------------------------------------------------------------------------------------------------------
    
  # Assign the Iris variables "Sepal.Length","Sepal.Width","Petal.Length","Petal.Width" to x:
    x <- '***YOUR CODE HERE***'
    
  # Assign the Iris variables "Species" to y:
    y <- '***YOUR CODE HERE***'

  # -Plot 1---------------------------------------------------------------------------------------
  # Create a boxplot for the values of each of the 4 attributes in x.
  # Place all boxplots on one image. Use the par function to divide the plotting area up
  # into 4 plotting areas:
    '***YOUR CODE HERE***'
    
  # Loop through each of the attributes of x, use boxplot and assign the name of 
  # of the attribute to its plot:
    '***YOUR CODE HERE***'

  # Create a variable fname which is the concatenation of iris_plot1_, your name and *.png
    fname <- '***YOUR CODE HERE***'
  # use dev.copy to save the figure:
    '***YOUR CODE HERE***'
  # Turn dev off:
    '***YOUR CODE HERE***'

  # -Plot 2---------------------------------------------------------------------------------------
  # Create a bar plot for the species column, use the plot() function:
    '***YOUR CODE HERE***'
    # Create a variable fname which is the concatenation of iris_plot2_, your name and *.png
    '***YOUR CODE HERE***'
    # use dev.copy to save the figure:
    '***YOUR CODE HERE***'
    # Turn dev off:
    '***YOUR CODE HERE***'

  # -Plot 3---------------------------------------------------------------------------------------
  # Produce lattice graphs for the features.  USe featurePlot.  Use y=y and x=x and set plot = ellipse.
    '***YOUR CODE HERE***'
    # Create a variable fname which is the concatenation of iris_plot3_, your name and *.png
    '***YOUR CODE HERE***'
    # use dev.copy to save the figure:    
    '***YOUR CODE HERE***'
    # Turn dev off:    
    '***YOUR CODE HERE***'
    
  # -Plot 4---------------------------------------------------------------------------------------
  # Do the same as Plot 3 but create a box plot instead.  That is, set plot=box and use featurePlot:
    '***YOUR CODE HERE***'
    # Create a variable fname which is the concatenation of iris_plot3_, your name and *.png    
    '***YOUR CODE HERE***'
    # use dev.copy to save the figure:        
    '***YOUR CODE HERE***'
    # Turn dev off:        
    '***YOUR CODE HERE***'
    
  # -Plot 5---------------------------------------------------------------------------------------
  # Create density plots for each attribute by class value.
    # Define scales as follows:
    scales <- list(x=list(relation="free"), y=list(relation="free"))
    # Use featurePlot with scales=scales:
    '***YOUR CODE HERE***'
    # Create a variable fname which is the concatenation of iris_plot3_, your name and *.png    
    '***YOUR CODE HERE***'
    # use dev.copy to save the figure:        
    '***YOUR CODE HERE***'
    # Turn dev off:        
    '***YOUR CODE HERE***'
    
# =========================================================================================
# 4. Model construction
# -----------------------------------------------------------------------------------------
  # In the following section we create some standard (basic) machine learning models to 
  # estimate the most likely species given 
  # "Sepal.Length","Sepal.Width","Petal.Length","Petal.Width" (the flower measurement)
  # of unseen data.
  # 
  # Set-up a framework to perform 10-fold cross validation.
  # Build 5 different models to predict species from flower measurements.
  # Select the best model.
  
  # create the control variable with trainControl set the number to 10 and the method to 'cv':
    '***YOUR CODE HERE***'
    
  # Create a metric (string) variable and set it equal to 'Accuracy':
  '***YOUR CODE HERE***'
  
  # Train 5 models using the caret train() function.  
  # Set the method, metric and trcontrol values, data = train_data.
  
  # a) linear discriminant analysis (lda), method = 'lda'
    # set seed to 7:
  '***YOUR CODE HERE***'
    # create the model andd call it fit.lda:
  '***YOUR CODE HERE***' <- train('***YOUR CODE HERE***')
    
  # b) Decision tree
    # The decision tree method is a powerful predictive machine learning technique 
    # used for both classification and regression. It is also known as 
    # Classification and Regression Trees (CART).
    # Set method = 'rpart'
    # set seed to 7:
    set.seed('***YOUR CODE HERE***')
    # create the model and assign it fit.cart:
    fit.cart <- train('***YOUR CODE HERE***')
    
  # c) kNN
    # K Nearest Neighbor is a Supervised Machine Learning algorithm. 
    # It classifies a new data point into the target class, 
    # depending on the features of its neighboring data points. 
    # Set the seed to 7 and method to 'knn'.  Assign the model to a variable called fit.knn.
    set.seed('***YOUR CODE HERE***')
    fit.knn <- train('***YOUR CODE HERE***')
    
  # d) The Support vector machine
    # A support vector machine is a supervised learning technique and is  
    # mostly used in classification problems. 
    # In this algorithm, each data item is plotted as a point in the feature space
    # with the value of each feature being the value of a particular coordinate. 
    # the model ultimately finds that hyper-plane that best separates all the classes.
    # Set the seed to 7 and method to 'svmRadial'.  
    # Assign the model to a variable called fit.svm
    set.seed('***YOUR CODE HERE***')
    '***YOUR CODE HERE***' <- train('***YOUR CODE HERE***')
    
  # e) Random Forest
    # Decision trees make binary predictions. 
    # The Random Forest technique is an improvement on Decision Tree Classifiers in that it trains not only one 
    # classifier but a group, each trained on a different random subset
    # of the train set. The predicted class is the one that gets the most votes from 
    # all the individual trees.
    # 
    # We will proceed as follow to train the Random Forest:
    # Set the seed to 7 and method to 'rf'.  
    # Assign the model to a variable called fit.rf  
    set.seed('***YOUR CODE HERE***')
    fit.rf <- train('***YOUR CODE HERE***')
  

# =========================================================================================
# 5. Evaluation
# -----------------------------------------------------------------------------------------
      
  # Summarize all the models using the resamples function and the summary function to print (to screen) results:
    results <- resamples(list('***YOUR CODE HERE***'))
    summary('***YOUR CODE HERE***')
  
  # compare the accuracy from the results variable above of the 5 models using dotplot:
    dotplot('***YOUR CODE HERE***')
  
  # summarize Best Model
  # show the lda predictions against the test_data
    predictions <- predict('***YOUR CODE HERE***')
  
  # Draw a confusion matrix using confusionMatrix() from the caret package.
    confusionMatrix('***YOUR CODE HERE***')


# =========================================================================================
# 5. Interpretation
# -----------------------------------------------------------------------------------------
# 
# a)  Which model do you prefer?
    
# b)  What will happen with species not seen before?
    
# c)  What makes these models so good?
#     
# Save this script here: ()


# This is the end of this R candidate evaluation.
# Thank you and good luck!
