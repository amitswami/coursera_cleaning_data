## Script run_analysis.r  ##
## Usage : r run_analysis.r
## Expects : A folder "UCI HAR Dataset" containing the data set for analysis


## Read labels for X data sets
x_labels = read.table("UCI HAR Dataset/features.txt" , colClasses = "character", header=FALSE)

## Merge the train data set Subject_train , X_train and Y_train
x_train = read.table("UCI HAR Dataset/train/X_train.txt" ,  header=FALSE)
names(x_train) <- x_labels[, "V2"]
y_train = read.table("UCI HAR Dataset/train/y_train.txt" ,  header=FALSE, col.names = c("TrainingLabel"))
xy_train = cbind(x_train , y_train)
subject_train = read.table("UCI HAR Dataset/train/subject_train.txt" ,  header=FALSE , col.names = c("Subject"))
train_dataset = cbind(xy_train , subject_train)



## Merge the test data set Subject_test , X_test and Y_test

x_test = read.table("UCI HAR Dataset/test/X_test.txt" ,  header=FALSE)
names(x_test) <- x_labels[, "V2"]
y_test = read.table("UCI HAR Dataset/test/y_test.txt" ,  header=FALSE , col.names = c("TrainingLabel"))
xy_test = cbind(x_test , y_test)
 

subject_test = read.table("UCI HAR Dataset/test/subject_test.txt" ,  header=FALSE , , col.names = c("Subject"))
test_dataset = cbind(xy_test , subject_test)

## Apend test data set to train data set 
dataset = rbind(train_dataset , test_dataset)

head(dataset , n=1)

## Step 2 : Extracts only the measurements on the mean and standard deviation for each measurement. 
## For this we are selecting the fields whose header contain the word mean / std
## 

mean_cols = x_labels[, "V2"][grep("mean()" , x_labels[, "V2"])]
std_cols = x_labels[, "V2"][grep("std()" , x_labels[, "V2"])]
all_cols = c(mean_cols, std_cols )
all_cols = c(c("Subject" , "TrainingLabel") , all_cols)

## Filtered dataset with subject , Training label , mean and std deviation columns
filtered_dataset = dataset[, all_cols]





### functions 

to_activity_label <- function(x){
  if(x == 1)
    return("WALKING")
  if(x == 2)
    return("WALKING_UPSTAIRS")
  if(x == 3)
    return("WALKING_DOWNSTAIRS")
  if(x == 2)
    return("SITTING")
  if(x == 2)
    return("STANDING")
  else
    return("LAYING")
}

# Create descripttive activity names
# activity_labels = read.table("UCI HAR Dataset/activity_labels.txt" , header=FALSE, colClasses = "character")
filtered_dataset$Activity <- sapply(filtered_dataset$TrainingLabel , to_activity_label)

## Create second dataset with average of each variable for each activity for each subject

library(reshape2)
second_ds = melt(filtered_dataset , id.vars=c("Subject" , "Activity") )
second_ds = dcast(second_ds , Subject+Activity ~ variable , fun.aggregate=mean)
write.csv(second_ds , "second_ds.csv" , row.names=FALSE)


