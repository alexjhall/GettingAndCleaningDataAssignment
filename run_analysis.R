
## Set up
  # Set working directory
  getwd()
  setwd("~/R/Coursera/3. Getting and cleaning data/Assignment")
  
  # Load packages
  library(tidyverse)
  library(readr)
  library(kableExtra)
  



## Initial data download
  # Download file
  download.file("https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip",
                           "Dataset.zip")
  
  # Unzip file
  unzip("Dataset.zip")
  
  ## Set directories, to be used when reading files.
    # Set 'General' directory
    GenDir <- paste0(getwd(), "/UCI HAR Dataset")
    
    # Set 'test' directory
    TestDir <- paste0(GenDir, "/test")
    
    # Set 'train' directory
    TrainDir <- paste0(GenDir, "/train")
  
## Activity label load and clean
  ## Loads the activity data so that it can be used to assign the correct 
  ## activity labels to activity classes later on
    # Read activity labels into data frame
    ActivityLabels <- read_table(file = paste0(GenDir, "/activity_labels.txt"),
                                 col_names = FALSE)
    # Label Activity Labels columns
    names(ActivityLabels) <- c("ActivityClass", "Activity")


## Features load and clean
  ## Loads the features data so that it can be used to assign the correct 
  ## feature variable names to the Test and Train datasets later on
    # Read features into data frame
    Features <- read_table(file = paste0(GenDir, "/features.txt"),
                           col_names = FALSE)
    
    # Separate Feature label
    # This didn't turn out to be essential, but was useful in identifying
    # the mean and std measures.
    FeaturesDelimited <- separate(Features,
                                  col = X2,
                                  into = c("Feature", "Measure", "Spec"),
                                  sep = '-')
    
    # Remove parentheses from 'Measure'
    # Does so by removing all punctuation, instead of removing each 
    # bracket separately.
    FeaturesDelimited$Measure <- 
      gsub("[[:punct:]]", "", FeaturesDelimited$Measure)
    
    # Rename first column (X1) as FeatureID
    # Didn't end up using FeatureID to match to the Feature labels
    FeaturesDelimited <- 
      FeaturesDelimited %>%
        rename(FeatureID = X1)
    
    # Create T/F vector indicating which measures are mean or std
    FeatureMeanStd <- FeaturesDelimited$Measure == 'mean' |
                      FeaturesDelimited$Measure == 'std'
    
    # Add three 'TRUE's to the start, which have categorical variables
    # Therefore creates vector covering all of TestTrain (see later)
    FeatureMeanStdPrefix <- c(TRUE, TRUE, TRUE, FeatureMeanStd)
    
    # Turns FeatureMeanStdPrefix logical into numeric vector according to TRUEs
    # This will be used to select variables from TestTrain using select()
    FeatureMeanStdPrefixNumeric <- seq_along(FeatureMeanStdPrefix)[FeatureMeanStdPrefix]
    FeatureMeanStdSelect <- FeatureMeanStdPrefixNumeric[!is.na(FeatureMeanStdPrefixNumeric)]
  
  

## Test dataset load and clean
  # Read subject_test
  SubjectTest <- read_table(file = paste0(TestDir, "/subject_test.txt"),
                         col_names = FALSE)
  
  # Change SubjectTest colname
  names(SubjectTest) <- "Subject"
  
  # Read X test
  XTest <- read_table(file = paste0(TestDir, "/X_test.txt"),
                            col_names = FALSE)
  
  # Read Y test
  YTest <- read_table(file = paste0(TestDir, "/y_test.txt"),
                      col_names = FALSE)

  # Change YTest colname
  names(YTest) <- "ActivityClass"

  # Combine test files
  Test <- cbind(SubjectTest, YTest, XTest)
  
  # Add dataset group identifier - Test
  Test <-
    Test %>%
      mutate(DatasetGroup = "Test",
             .after = ActivityClass)
  
  
  
## Train load and clean
  # Read subject_train
  SubjectTrain <- read_table(file = paste0(TrainDir, "/subject_train.txt"),
                            col_names = FALSE)
  
  # Change SubjectTrain colname
  names(SubjectTrain) <- "Subject"
  
  # Read X train
  XTrain <- read_table(file = paste0(TrainDir, "/X_train.txt"),
                      col_names = FALSE)
  
  # Read Y train
  YTrain <- read_table(file = paste0(TrainDir, "/y_train.txt"),
                      col_names = FALSE)
  
  # Change YTrain colname
  names(YTrain) <- "ActivityClass"
  
  #Combine train files
  Train <- cbind(SubjectTrain, YTrain, XTrain)
  
  # Add dataset group identifier - Train
  Train <-
    Train %>%
    mutate(DatasetGroup = "Train",
           .after = ActivityClass)

  
## *********************************
## This completes part 1 of the assignment
## *********************************   
## Merge datasets
  TestTrain <- rbind(Test, Train)
  
  
## *********************************
## This completes part 4 of the assignment
## *********************************    
## Rename variables in TestTrain with labels from feature list
  names(TestTrain) <- c(names(TestTrain)[1:3], #Put in names of prefix columns first
                        as.character(Features$X2)) #Then use names taken from Features dataframe loaded earlier
  
  # Replace duplicate "BodyBody" with single "Body
  names(TestTrain) <- 
    gsub("BodyBody", "Body", names(TestTrain))
    
  
## *********************************
## This completes part 2 of the assignment
## *********************************      
## Create new dataframe which only contains mean and std measurements
  TestTrainMeanStd <- select(TestTrain,
                             FeatureMeanStdSelect) # Uses numerical vector created earlier

## *********************************
## This completes part 3 of the assignment
## *********************************      
## Create new dataframe with correct activity labels
  
  # Merge previously created file (TestTrainMeanStd) with ActivityLabels lookup
  x <- left_join(TestTrainMeanStd, ActivityLabels)
  # Change position of newly added variable (Activty) to be next to the ActivityClass code
  x <- relocate(x,
                Activity,
                .after = ActivityClass)
  # Drop the ActivityClass code
  TestTrainMeanStdLabels <- select(x, -ActivityClass)
  # Drop temporary object x
  rm(x)
  
  # Activity variable: Replace underscore with space
  TestTrainMeanStdLabels$Activity <- 
    gsub("_", " ", TestTrainMeanStdLabels$Activity)
  
  # Convert activity labels to Sentance case
  TestTrainMeanStdLabels$Activity <- 
    str_to_sentence(TestTrainMeanStdLabels$Activity)
  
  
## *********************************
## This completes part 5 of the assignment
## *********************************  
  
## Create new summary dataframe (organised according to tidy data principles)
## Summarised by mean on activity and subject
  
  FinalTidyData <- # Final output dataset
    TestTrainMeanStdLabels %>% # Within previously created dataframe
      group_by(Subject, Activity, DatasetGroup) %>% # Group by (and keep) these three variables
        # summarised by mean, for all vars except first three    
        summarise(across(`tBodyAcc-mean()-X`:`fBodyGyroJerkMag-std()`, mean)) 
      
## Output final tidy data to .txt
  write.table(FinalTidyData,
              "FinalTidyData.txt",
              row.names = FALSE)
  
  
