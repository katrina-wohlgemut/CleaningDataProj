#load library
library(dplyr)

#read test data
x_test <- read.table("./X_test.txt")
y_test <- read.table("./y_test.txt")
sub_test <- read.table("./subject_test.txt")

#read train data
x_train <- read.table("./X_train.txt")
y_train <- read.table("./y_train.txt")
sub_train <- read.table("./subject_train.txt")

#read variable labels
variable_labels <- read.table("./features.txt")

#read activity labels
activity_labels <- read.table("./activity_labels.txt")

#bind test and train sets together
x <- rbind(x_test, x_train)
y <- rbind(y_test, y_train)
sub <- rbind(sub_test, sub_train)

#label variables
colnames(x) <- variable_labels$V2

#extract mean and std info
vl_subset <- variable_labels[(grepl("mean", variable_labels$V2))|(grepl("std", variable_labels$V2)),]
wrong <- grepl("meanFreq", vl_subset$V2)
vl_vector <- as.vector(vl_subset$V2)
vl_vector <- vl_vector[!wrong]
x_subset <- subset(x, select = vl_vector)

#set names
sub <- setNames(sub, "subject")
y <- setNames(y, "activityNum")
activity_labels <- setNames(activity_labels, c("activityNum", "activityName"))

#finish merging tables
ysub <- cbind(y, sub)
dt <- cbind(ysub, x_subset)

#label activities
labelled <- merge(activity_labels, dt, by = "activityNum", all.x = TRUE)

#sort data by activity and subject
sorted <- arrange(labelled, activityNum, subject)
sortGroup <- group_by(sorted, activityNum, subject)

#find mean for each variable by activity and subject
meanGroup <- sortGroup %>% summarise(across(2:67, mean))

#print out table
write.table(meanGroup, file = file("./tidyDataSet.txt", "wb"), row.names = FALSE)


