# You should create one R script called run_analysis.R that does the following.

# Merges the training and the test sets to create one data set.
# Extracts only the measurements on the mean and standard deviation for each measurement.
# Uses descriptive activity names to name the activities in the data set
# Appropriately labels the data set with descriptive variable names.
# From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

require("data.table")

#read data and set column names
features <- read.table('./features.txt',header=FALSE); 
activityType <- read.table('./activity_labels.txt',header=FALSE,col.names=c('activityId', 'activityType'));
subjectTrain <- read.table('./train/subject_train.txt',header=FALSE,col.names="subjectId"); 
xTrain <- read.table('./train/x_train.txt',header=FALSE,col.names=features[,2]);
yTrain <- read.table('./train/y_train.txt',header=FALSE,col.names="activityId"); 

subjectTest = read.table('./test/subject_test.txt',header=FALSE,col.names="subjectId"); 
xTest       = read.table('./test/x_test.txt',header=FALSE,col.names=features[,2]); 
yTest       = read.table('./test/y_test.txt',header=FALSE,col.names="activityId"); 

#combine both data sets
combinedData <- rbind(cbind(xTrain,yTrain,subjectTrain),cbind(xTest,yTest,subjectTest));
column_names <- colnames(combinedData);
#mean and standard dev for each measurement
mean_std <- combinedData[,grepl("mean|std|subjectId|activityId", names(combinedData))];

# Uses descriptive activity names to name the activities in the data set

mean_std <- join(mean_std, activityType, by = "activityId", match = "first");
mean_std <- subset(mean_std,select = -activityId);

# Appropriately labels the data set with descriptive variable names.
names(combinedData)<-gsub("^t", "Time", names(combinedData));
names(combinedData)<-gsub("^f", "Frequency", names(combinedData));
names(combinedData)<-gsub("Acc", "Accelerometer", names(combinedData));
names(combinedData)<-gsub("Gyro", "Gyroscope", names(combinedData));
names(combinedData)<-gsub("Mag", "Magnitude", names(combinedData));
names(combinedData)<-gsub("BodyBody", "Body", names(combinedData));

# From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
tidy_data <- aggregate(mean_std[, 3:ncol(mean_std)],
                       by=list(subjectId = mean_std$subjectId,
                               activityType = mean_std$activityType),mean);

write.table(format(tidy_data, scientific=T), "tidyData.txt",row.names=F);
