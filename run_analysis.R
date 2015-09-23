#==============================================================================
# run_analysis.R
# Getting and Cleaning Data Course Project
#==============================================================================

# Approach --------------------------------------------------------------------
# This is a long script. It is broken up into a number of steps, each separated 
# by comments extended out to 80 column width by dashes ('-').
#
# The steps are as follows:
#   1 - save environment values so the script can reset them befor exiting
#   2 - read in X values for both the training and test data sets
#   3 - rename the columns of X using text from the features.txt file
#   4 - read in Y (activity) values for the training and test data sets
#   5 - append the Y data sets as added columns on X, with appropriate names
#   6 - combine the test and training data sets
#   7 - cut any columns on x that are not a mean or std dev value
#   8 - 
#------------------------------------------------------------------------------

library(dplyr)
library(data.table)
library(reshape2)


# 1 - initialise, check the working directory and save the environment --------
  print("---> Step 1 -------------------------------------------")

  print("---> checking working directory contains data...")
  # working directory must contain "UCI HAR Dataset" directory
  if(!file.exists("UCI HAR Dataset")) {
    print("Error: this script needs a UCI HAR Dataset directory.")
    stop("No data directory found.")
  }
  data_dir <- "./UCI HAR Dataset"
  test_dir <- "./UCI HAR Dataset/test"
  train_dir <- "./UCI HAR Dataset/train"
  
  print("---> remembering environment...")
  # Remember environment variables so they can be reset at end
  old.wd <- getwd()


# 2 - read in the X_test and X_training data sets -----------------------------

  # The x_train/x_test files contain 561 measurements made over time, they are
  # located in subdirectories (test and train). Read these in to separate data
  # frames.
  print("---> Step 2 -------------------------------------------")
  print("---> loading x data sets...")
  
  print("---> loading x_training data set...")
  # load the training X data set, from the train directory
  setwd(old.wd)
  setwd(train_dir)
  x_train <- read.table("X_train.txt")

  print("---> loading x_test data set...")
  # load the test X data set, from the test directory
  setwd(old.wd)
  setwd(test_dir)
  x_test <- read.table("X_test.txt")

  
# 3 - update the column names of X data sets from features.txt file -----------
  
  # The X data sets have 561 columns named V1, V2, .. V561. These are not 
  # very informative. The features file has 561 entries, describing the 561
  # columns in X. To make the X data sets more informative, replace the names
  # V1, V2, ... V561 with the names held in features file.
  print("---> Step 3 -------------------------------------------")
  print("---> renaming x data set columns...")
  
  # read in the features file.
  setwd(old.wd)
  setwd(data_dir)
  features <- read.table("features.txt", stringsAsFactors = FALSE)
  
  # convert features into a vector and strip unsuitable characters:"()",
  # replace characters "," and "-" with underscore "_".
  features <- gsub('([)])','', features[,2])  # renders features as a vector
  features <- gsub('([(])','', features)
  features <- gsub('([,])','_', features)
  features <- gsub('([-])','_', features)
  # set a vector of the current names used in the X data sets, as the test and
  # trial data sets have the same column names use one of the sets for both
  old_x_names <- names(x_test)
  
  # rename the x data set columns to the more user-friendly ones in feature
  setnames(x_train, old=old_x_names, new=features)
  setnames(x_test,  old=old_x_names, new=features)

  
# 4 - append the activity data from the y_test and y_training data sets -------

  print("---> Step 4 -------------------------------------------")
  print("---> adding y column to x data sets...")
  
  # set up a vector of activity text values
  activity_code <- c("WALKING", "WALKING-UPSTAIRS", "WALKING_DOWNSTAIRS",
                     "SITTING", "STANDING", "LAYING")
  

  print("---> processing y_train...")
  # load the training y data set, from the train directory
  setwd(old.wd)
  setwd(train_dir)
  y_train <- read.table("y_train.txt")

  # make the activity codes more readable by looking them up in
  # the activity_code vector, build a character vector to hold the lookup
  # values
  y_train_activities <- vector()
  for(i in 1:nrow(y_train)){
    y_train_activities[i] <- activity_code[match(y_train[i,1], 1:6)]
  }
  
  # bind the training data set to x_train
  x_train  <- cbind(y_train_activities,  x_train,  stringsAsFactors = FALSE)
  setnames(x_train, old=c("y_train_activities"), new=c("Activity"))
  
  print("---> processing y_test...")
  # load the training y data set, from the train directory
  setwd(old.wd)
  setwd(test_dir)
  y_test <- read.table("y_test.txt")
  
  # make the activity codes more readable by looking them up in
  # the activity_code vector, build a character vector to hold the lookup
  # values
  y_test_activities <- vector()
  for(i in 1:nrow(y_test)){
    y_test_activities[i] <- activity_code[match(y_test[i,1], 1:6)]
  }
  
  # bind the training data set to x_test
  x_test  <- cbind(y_test_activities,  x_test,  stringsAsFactors = FALSE)
  setnames(x_test, old=c("y_test_activities"), new=c("Activity"))
  
  
# 5 - append the subject data from the subject data files ---------------------
  
  print("---> Step 5 -------------------------------------------")
  print("---> adding subject data to x data sets...")
  
  print("---> adding training subjects...")
  # load the training subject data set, from the train directory
  setwd(old.wd)
  setwd(train_dir)
  subject_train <- read.table("subject_train.txt")

  print("---> adding test subjects...")
  # make the subject train column name readable
  setnames(subject_train, old=c("V1"), new=c("Subject"))
  
  # load the test subject data set, from the test directory
  setwd(old.wd)
  setwd(test_dir)
  subject_test <- read.table("subject_test.txt")

  # make the subject test column name readable
  setnames(subject_test, old=c("V1"), new=c("Subject"))
  subjects <- unique(rbind(subject_train, subject_test))
  subjects <- sort(subjects$Subject)
  
  # append the test and training subject data sets to their corresponding 
  # x data set
  x_test  <- cbind(subject_test,  x_test,  stringsAsFactors = FALSE)
  x_train <- cbind(subject_train, x_train, stringsAsFactors = FALSE)
  

# 6 - merge the test and training data sets -----------------------------------
  
  # x_test and X_train are constructed data sets with identical columns
  # they are the original x_test and x_train file contents with 2 prepended
  # columns - subject and activity, their original column names have been
  # changed from V1, V2...V561 to the names stored in the features file.
  
  print("---> Step 6 -------------------------------------------")
  print("---> merging the test and train data sets...")
  
  # add the data sets together
  x_data <- base::rbind(x_test, x_train)

# 7 - extract only measurements of mean and std dev for each measurement ------

  print("---> Step 7 -------------------------------------------")
  print("---> subset to retain just the mean and std columns...")
  # the column names in x_data include the text fragments 'mean' and 'std' for 
  # each measurement group, for example: 'tBodyAcc_mean_X' and 
  # 'tBodyAcc_std_Y', we want to retain only those columns for mean and std 
  # measures, as well as the subject and activity columns.
  
  print("---> finding mean and std columns to keep...")
  # build a logical vector against the x_data column names, set TRUE
  # if it contains mean or std, FALSE if otherwise; note the first two columns
  # contain subject and activity data and should be kept.
  cols <- names(x_data)
  cols_upp <- toupper(cols)
  keep <- vector()
  for(i in 1:length(cols)){
    if(i==1 | i==2){
      keep[i] <- TRUE
    } else {
        keep[i] <- grepl("MEAN", cols_upp[i]) || grepl("STD", cols_upp[i])
    }
  }
  
  print("---> subsetting columns...")
  x_data <- x_data[keep]

# 8 - deconstruct the measurement columns into one measure per row ------------

  print("---> Step 8 -------------------------------------------")
  print("---> creating a deconstructed data set...")
  
  # The x_data set contains multiple columns of measures per observation. There
  # is one row per observation, but no observation time is given. We need to
  # reduce the data set to five columns: observation_number, subject, activity, 
  # measure, value. There is no observation number so we first need to add
  # an index column to the data set, then reduce the measurement columns down
  # to populate the measure and value columns.
  
  print("---> adding an index column...")
  # Add an index column to the data set.
  observation_number <- c(1:nrow(x_data))
  x_data <- cbind(observation_number, x_data)
    
  print("---> melt measures into a narrow table...")
  # reshape x_data_meanstd, replace multiple measure columns with one
  x_data_melt <- reshape2::melt(x_data,c(1:3), c(4:89), variable.factor = TRUE)

# 9 - create a data set with average of each variable, activity and subject ---

  print("---> Step 9 -------------------------------------------")
  print("---> creating an independent tidy data set...")
  
  # create a second, independent tidy data set with the average of 
  # each variable for each activity and each subject.
  x_cast <- dcast(x_data_melt, Subject + Activity  ~ variable, mean)
  
# 10 - print the data set -----------------------------------------------------
  
  print("---> Step 10 ------------------------------------------")
  print("---> print the results...")
  setwd(old.wd)
  write.table(x_cast, file = "tidy_data.txt", row.name = FALSE) 
              
              
# 11 - reset the environment and exit
  print("---> Step 11 ------------------------------------------")
  print("---> resetting the environment...")
  # Reset environment to tidy up
  setwd(old.wd)

  print("---> Successfully completed ---------------------------")
  