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


# Reading testing tables
subject_test <- read.table(file.path(dataPath, "test", "subject_test.txt"))
x_test <- read.table(file.path(dataPath, "test", "X_test.txt"))
y_test <- read.table(file.path(dataPath, "test", "y_test.txt"))


# Reading trainings tables
subject_train <- read.table(file.path(dataPath, "train", "subject_train.txt"))
x_train <- read.table(file.path(dataPath, "train", "X_train.txt"))
y_train <- read.table(file.path(dataPath, "train", "y_train.txt"))


# Read features
features <- read.table(file.path(dataPath, "features.txt"), as.is = TRUE)


# Read activity labels
activityLabels <- read.table(file.path(dataPath, "activity_labels.txt"))

# assign column names
colnames(x_test) <- features[,2] 
colnames(y_test) <- "activityId"
colnames(subject_test) <- "subjectId"

colnames(activityLabels) <- c("activityId", "activityType")
colnames(x_train) <- features[,2] 
colnames(y_train) <-"activityId"
colnames(subject_train) <- "subjectId"


########################################################################
# Step 2 - Merge the training and the test sets to create one data set
#######################################################################


merge_train <- cbind(y_train, subject_train, x_train)
merge_test <- cbind(y_test, subject_test, x_test)
allData <- rbind(merge_train, merge_test)
colNames <- colnames(allData)

##############################################################################
# Step 3 - Extract only the measurements on the mean and standard deviation
#          for each measurement
##############################################################################

mean_and_std <- (grepl("activityId" , colNames) | 
                   grepl("subjectId" , colNames) | 
                   grepl("mean.." , colNames) | 
                   grepl("std.." , colNames) 
)

setForMeanAndStd <- allData[ , mean_and_std == TRUE]


setWithActivityNames <- merge(setForMeanAndStd, activityLabels,
                              by='activityId',
                              all.x=TRUE)

##############################################################################
# Step 4 - Appropriately label the data set with descriptive variable names
##############################################################################

setWithActivityNames <- merge(setForMeanAndStd, activityLabels,
                              by='activityId',
                              all.x=TRUE)

##############################################################################
# Step 5 - Create a second, independent tidy set with the average of each
#          variable for each activity and each subject
##############################################################################

Data2 <- aggregate(. ~subjectId + activityId, setWithActivityNames, mean)
Data2 <- Data2[order(Data2$subjectId, Data2$activityId),]

# output to file "tidy_data.txt"
write.table(Data2, "tidy_data.txt", row.name=FALSE)