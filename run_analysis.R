# These comments are re-written in the README.md of the github repository 

# Set the working directory
setwd("C:/Users/Pelin/Documents/R class/UCI HAR Dataset/");

# Read the features into R
features <- read.table("./features.txt",header=FALSE)

# Read the activity labels into R, rename columns for simplicity later
activityLabels <- read.table("./activity_labels.txt",header=FALSE)
colnames(activityLabels) <- c("activityId","activityType")

# Load the training files, rename columns for simplicity later
subjectTrain <- read.table("./train/subject_train.txt",header=FALSE)
colnames(subjectTrain) <- "subjectId"
xTrain <- read.table("./train/X_train.txt",header=FALSE)
colnames(xTrain) <- features[,2]
yTrain <- read.table("./train/y_train.txt",header=FALSE)
colnames(yTrain) <- "activityId"

# make a merged training dataset
training <- cbind(yTrain,subjectTrain,xTrain)

# Load the test files, rename columns for simplicity later
subjectTest <- read.table("./test/subject_test.txt",header=FALSE)
colnames(subjectTest) <- "subjectId"
xTest <- read.table("./test/X_test.txt",header=FALSE)
colnames(xTest) <- features[,2]
yTest <- read.table("./test/y_test.txt",header=FALSE)
colnames(yTest) <- "activityId"

# make a merged test dataset
testing <- cbind(yTest,subjectTest,xTest)

# make a merged dataset out of both training and testing datasets
dataSet<-rbind(training,testing)

# grab the activity id, subject id columns and only the columns that contain mean or standard deviation
colNames <- colnames(dataSet)
logical <- (grepl("activity..",colNames) | grepl("subject..",colNames) | grepl("-mean..",colNames) & !grepl("-meanFreq..",colNames) & !grepl("mean..-",colNames) | grepl("-std..",colNames) & !grepl("-std()..-",colNames))
finalData <- dataSet[logical==TRUE]

# merge the shortened dataset with descriptive activity labels (added to the end)
finalData <- merge(finalData,activityLabels,by='activityId',all.x=TRUE)

# simplify variable (column) names
colNames <- colnames(finalData)
for (i in 1:length(colNames)) 
{
        colNames[i] <- gsub("\\()","",colNames[i])
        colNames[i] <- gsub("-std","StdDev",colNames[i])
        colNames[i] <- gsub("-mean","Mean",colNames[i])
        colNames[i] <- gsub("^(t)","time ",colNames[i])
        colNames[i] <- gsub("^(f)","freq ",colNames[i])
        colNames[i] <- gsub("AccMag","Acc magnitude ",colNames[i])
        colNames[i] <- gsub("Bodyaccjerkmag","BodyAccJerk magnitude ",colNames[i])
        colNames[i] <- gsub("JerkMag","Jerk magnitude ",colNames[i])
        colNames[i] <- gsub("GyroMag","Gyro magnitude ",colNames[i])
};
# apply the new variable (column) names
colnames(finalData) <- colNames

# to tidy up, first remove the descriptive activity labels
finalData1 <- finalData[,names(finalData) !="activityType"]
# create a tidy dataset independent from but based on the previous dataset with the average of each variable for each activity and each subject
tidyData <- aggregate(finalData1[,names(finalData1) != c('activityId','subjectId')],by=list(activityId=finalData1$activityId,subjectId = finalData1$subjectId),mean)
# put back the descriptive activity labels (added to the end) & print the tidy dataset
tidyData <- merge(tidyData,activityLabels,by='activityId',all.x=TRUE)
write.table(tidyData,"./tidyData.txt",row.names=FALSE,sep='\t')
