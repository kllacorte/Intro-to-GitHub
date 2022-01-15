## Merges the training and the test sets to create one data set.
unzip("UCI HAR Dataset.zip")
setwd("~/UCI HAR Dataset")
train_data <- cbind(read.table('./train/subject_train.txt'), 
                   read.table('./train/y_train.txt'), 
                   read.table('./train/X_train.txt'))

test_data <- cbind(read.table('./test/subject_test.txt'), 
                  read.table('./test/y_test.txt'),
                  read.table('./test/X_test.txt'))
data <- rbind(train_data, test_data)

## Appropriately labels the data set with descriptive variable names
features <- (read.table('features.txt')$V2)
colnames(data) <- c("Subject_ID", "Activity_Category", features)

## Extracts only the measurements on the mean and standard deviation for each measurement
mean_sd <- cbind(data[,1:2], data[, grepl("mean\\(\\)|std\\(\\)", names(data))])

## Uses descriptive activity names to name the activities in the dataset
activity_names <- read.table("activity_labels.txt", col.names = c("Activity Number", "Activity Description") )
for (i in 1:10299){
  for (j in 1:6){
    if(data$Activity_Category[i] == activity_names[j,1]){
      data$Activity_Category[i] = activity_names[j,2]
    }else{
      j=j+1
    } 
  }
  i=i+1
}
data

## Create a second, independent tidy data set with the average of each 
##        variable for each activity and each subject.
library(dplyr)
ind_tidy_data <- 
  data %>% 
  setNames(make.names(names(.), unique = TRUE)) %>%  
  group_by(Subject_ID, Activity_Category) %>%       
  summarise_all(list(mean = mean)) 
