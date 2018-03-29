Data
tidy_data.txt
Each row contains, for a given subject and activity, 79 averaged signal measurements.
Measurements
Measurements are classified into two domains Time & Frequency
Variables
Identifiers:
Subject - The ID of the test subject, Integer, ranges from 1 to 30.
Activity -  The type of activity performed when the corresponding measurements were taken. 

Activity Labels: (6 possible values)
originally coded as integers from 1 to 6 were replaced with “descriptive names”
WALKING (value 1): subject was walking during the test
WALKING_UPSTAIRS(Value 2): subject was walking up a staircase during the test
WALKING_DOWNSTAIRS(Value 3): subject was walking down a staircase during the test
SITTING (value 4): subject was sitting during the test
STANDING (value 5): subject was standing during the test
LAYING (value 6): subject was laying down during the test

Activity, Subject and Features will be used as part of descriptive Variable names for data in dataframe.
Values of Variable Activity consist of data from “y_train.txt” and “Y_test.txt”
Values of variable Subject consist of data from “subject_train.txt” and subject_test.txt”
Values of variables Features consist of data from “X_train.txt” and “X_test.txt”
Names of variables Features comes from “features.txt”
Levels of variable Activity come from “activity_labels.txt”
Mean & Standard Deviation 
Calculated from data contained when test and training sets merged 
Extract “mean” & “std” measurements and discard everything else as instructed.

Transformations
The variable names were replaced with descriptive variable names
Acc- Converted to “Accelerometer”
Gyro- Converted to “Gyroscope”
Mag- Converted to “Magnitude”
Std - “Standard Deviation”
T- Prefix replaced by “time”
F- Prefix replaced by “frequency”
BodyBody- replaced incorrect label with “Body”
