######################################
# Step 1 Download and unzip the datset
######################################


zipUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
zipFile <- "UCI HAR Dataset.zip"
if (!file.exists(zipFile)) {
  download.file(zipUrl, zipFile, mode = "wb")
}

# unzip zip file containing data if data directory doesn't already exist

dataPath <- "UCI HAR Dataset"
if (!file.exists(dataPath)) {
  unzip(zipFile)
}

###########################################
# Read data
###########################################

## Activity files
ActivityTest  <- read.table(file.path(dataPath, "test", "Y_test.txt"),header = FALSE)
ActivityTrain <- read.table(file.path(dataPath, "train", "Y_train.txt"),header = FALSE)

## Subject files
SubjectTest  <- read.table(file.path(dataPath, "test", "subject_test.txt"),header = FALSE)
SubjectTrain <- read.table(file.path(dataPath, "train", "subject_train.txt"),header = FALSE)

## Features files
FeaturesTest  <- read.table(file.path(dataPath, "test", "X_test.txt" ),header = FALSE)
FeaturesTrain <- read.table(file.path(dataPath, "train", "X_train.txt"),header = FALSE)

## Closer look at the data
str(ActivityTest)
str(ActivityTrain)
str(SubjectTrain)
str(SubjectTest)
str(FeaturesTest)
str(FeaturesTrain)

########################################################################
# Step 2 - Merge the training and the test sets to create one data set
#######################################################################

SubjectData <- rbind(SubjectTrain, SubjectTest)
ActivityData <- rbind(ActivityTrain, ActivityTest)
FeaturesData <- rbind(FeaturesTrain, FeaturesTest)

names(SubjectData) <-c("subject")
names(ActivityData) <- c("activity")
FeaturesDataNames <- read.table(file.path(dataPath, "features.txt"), head = FALSE)
names(FeaturesData) <- FeaturesDataNames$V2

CombineData <- cbind(SubjectData, ActivityData)
AllData <- cbind(FeaturesData, CombineData)


##############################################################################
# Step 3 - Extract only the measurements on the mean and standard deviation
#          for each measurement
##############################################################################

## Extract using grep
subFeaturesDataNames <- FeaturesDataNames$V2[grep("mean\\(\\)|std\\(\\)", FeaturesDataNames$V2)]
## Subset the data frame Data by selected names of Features
selectedNames <- c(as.character(subFeaturesDataNames), "subject", "activity" )
Data2 <- subset(AllData,select=selectedNames)
##test
str(Data2)

## Read descriptive activity names from "activity_labels.txt"
ActivityLabels <- read.table(file.path(dataPath, "activity_labels.txt"), header = FALSE)
Data2$activity <- factor(Data2$activity,labels=ActivityLabels[,2])

##############################################################################
# Step 4 - Appropriately label the data set with descriptive variable names
##############################################################################

names(Data2) <- gsub("^t", "time", names(Data2))
names(Data2) <- gsub("^f", "frequency", names(Data2))
names(Data2) <- gsub("Acc", "Accelerometer", names(Data2))
names(Data2) <- gsub("Gyro", "Gyroscope", names(Data2))
names(Data2) <- gsub("Mag", "Magnitude", names(Data2))
names(Data2) <- gsub("BodyBody", "Body", names(Data2))
##test
names(Data2)

##############################################################################
# Step 5 - Create a second, independent tidy set with the average of each
#          variable for each activity and each subject
##############################################################################
FinalData <- aggregate(. ~subject + activity, Data2, mean)
FinalData <- FinalData[order(FinalData$subject,FinalData$activity),]
write.table(FinalData, file = "tidydata.txt",row.name = FALSE,quote = FALSE, sep = '\t')
