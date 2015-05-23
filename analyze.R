#required library for aggregate function
####################################################

library(plyr);

#check the folder, assign the url to variable, download the zip file from this url, then unzip.
####################################################

if (!file.exists("data")) 
    {
        dir.create("data")
    }
fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(fileUrl, destfile = "./data/ziped.zip")
dateDownloaded <- date()
unzip ("./data/ziped.zip", exdir="./data")

#declare the path to the files.
####################################################

path_files <- file.path("./data" , "UCI HAR Dataset")

#load the requerided files from zip to data tables
####################################################

dataActivityTest  <- read.table(file.path(path_files, "test" , "Y_test.txt" ),header = FALSE)
dataActivityTrain  <- read.table(file.path(path_files, "train" , "Y_train.txt" ),header = FALSE)

dataSubjectTrain <- read.table(file.path(path_files, "train", "subject_train.txt"),header = FALSE)
dataSubjectTest  <- read.table(file.path(path_files, "test" , "subject_test.txt"),header = FALSE)

dataFeaturesTest  <- read.table(file.path(path_files, "test" , "X_test.txt" ),header = FALSE)
dataFeaturesTrain <- read.table(file.path(path_files, "train", "X_train.txt"),header = FALSE)

dataFeaturesNames <- read.table(file.path(path_files, "features.txt"),head=FALSE)
activityLabels <- read.table(file.path(path_files, "activity_labels.txt"),header = FALSE)


#merge the data tables to one full data table with its names described in features.txt file
##################################################################

dataSubject <- rbind(dataSubjectTrain, dataSubjectTest)
dataActivity<- rbind(dataActivityTrain, dataActivityTest)
dataFeatures<- rbind(dataFeaturesTrain, dataFeaturesTest)

#asign the names to its columns
names(dataSubject)<-c("subject")
names(dataActivity)<- c("activity")
names(dataFeatures)<- dataFeaturesNames$V2

dataCombine <- cbind(dataSubject, dataActivity)
Data <- cbind(dataFeatures, dataCombine)

#Extracts only the measurements on the mean and standard deviation for each measurement.
#####################################################################

subdataFeaturesNames<-dataFeaturesNames$V2[grep("-(mean|std)\\(\\)", dataFeaturesNames$V2)]

selectedNames<-c(as.character(subdataFeaturesNames), "subject", "activity" )
Data<-subset(Data,select=selectedNames)

#type of activity rewrited to its definition from activity_labels.txt 
#######################################################################

Data[, 68] <- activityLabels[Data[, 68], 2]
#sensor_data_mean_std <- join(Data, activityLabels, by = "Subject", match = "first")

#make names of columns more specific
######################################################################

names(Data)<-gsub("^t", "time", names(Data))
names(Data)<-gsub("^f", "frequency", names(Data))
names(Data)<-gsub("Acc", "Accelerometer", names(Data))
names(Data)<-gsub("Gyro", "Gyroscope", names(Data))
names(Data)<-gsub("Mag", "Magnitude", names(Data))
names(Data)<-gsub("BodyBody", "Body", names(Data))

#calculate mean of each variable for each activity and each subject using aggregate function
######################################################################

Data2<-aggregate(. ~subject + activity, Data, mean)

#order the table firs by subject, then by activity
########################################################################

Data2<-Data2[order(Data2$subject,Data2$activity),]

#export the datatable to tidydata.txt file
#######################################################################

write.table(Data2, file = "tidydata.txt",row.name=FALSE)