
library(plyr)

# 0 - Load Data from text files

# intiliaze the file names
base_directory <- "UCI HAR Dataset"

feature_file <- paste(base_directory, "features.txt", sep = "/")

activity_labels_file <- paste(base_directory, "activity_labels.txt", sep = "/")
activity_labels_file <- paste(base_directory, "activity_labels.txt", sep = "/")

x_train_file <- paste(base_directory, "train", "X_train.txt", sep = "/")
y_train_file <- paste(base_directory, "train", "y_train.txt", sep = "/")
subject_train_file <- paste(base_directory, "train", "subject_train.txt", sep = "/")

x_test_file  <- paste(base_directory, "test", "X_test.txt", sep = "/")
y_test_file  <- paste(base_directory, "test", "y_test.txt", sep = "/")
subject_test_file <- paste(base_directory, "test", "subject_test.txt", sep = "/")

# Load each file
features <- read.table(feature_file, header = FALSE)

activity_labels <- read.table(activity_labels_file, header = FALSE,  col.names = c("ActivityId", "Activity"))

x_train <- read.table(x_train_file, header = FALSE, col.names = features[,2], check.names = FALSE )
y_train <- read.table(y_train_file, header = FALSE, col.names = c("ActivityId") )
subject_train <- read.table(subject_train_file, header = FALSE, col.names = c("Subject") )

x_test <- read.table(x_test_file, header = FALSE, col.names = features[,2], check.names = FALSE )
y_test <- read.table(y_test_file, header = FALSE, col.names = c("ActivityId") )
subject_test <- read.table(subject_test_file, header = FALSE, col.names = c("Subject") )




##################################################################
# 1. Merges the training and the test sets to create one data set.
##################################################################
# combine all data sets into one data set  with rbind()


# Binding sensor data

# track source = train
DataSource <- "train"
training_sensor_data <- cbind(DataSource, subject_train, y_train, x_train)


# track source = test
DataSource <- "test"
test_sensor_data <- cbind(DataSource, subject_test, y_test, x_test)

sensor_data <- rbind(training_sensor_data, test_sensor_data)

# 2. Extracts only the measurements on the mean and standard deviation for each measurement.

sensor_data_mean_std0 <- sensor_data[,grepl("mean|std|DataSource|Subject|ActivityId", names(sensor_data))]

# 3. Uses descriptive activity names to name the activities in the data set

sensor_data_mean_std <- join(activity_labels, sensor_data_mean_std0, by = "ActivityId", match="all")

## Remove ActvityId
sensor_data_mean_std <- sensor_data_mean_std[,-1]


# 4. Appropriately labels the data set with descriptive names.

# Remove parentheses
names(sensor_data_mean_std) <- gsub('\\(|\\)',"",names(sensor_data_mean_std), perl = TRUE)

# Make syntactically valid names
names(sensor_data_mean_std) <- make.names(names(sensor_data_mean_std))

# Make clearer names
names(sensor_data_mean_std) <- gsub('Acc',"Acceleration",names(sensor_data_mean_std))
names(sensor_data_mean_std) <- gsub('GyroJerk',"AngularAcceleration",names(sensor_data_mean_std))
names(sensor_data_mean_std) <- gsub('Gyro',"AngularSpeed",names(sensor_data_mean_std))
names(sensor_data_mean_std) <- gsub('Mag',"Magnitude",names(sensor_data_mean_std))

names(sensor_data_mean_std) <- gsub('^t',"TimeDomain.",names(sensor_data_mean_std))
names(sensor_data_mean_std) <- gsub('^f',"FrequencyDomain.",names(sensor_data_mean_std))

names(sensor_data_mean_std) <- gsub('\\.mean',".Mean",names(sensor_data_mean_std))
names(sensor_data_mean_std) <- gsub('\\.std',".StandardDeviation",names(sensor_data_mean_std))

names(sensor_data_mean_std) <- gsub('Freq\\.',"Frequency.",names(sensor_data_mean_std))
names(sensor_data_mean_std) <- gsub('Freq$',"Frequency",names(sensor_data_mean_std))


# 5. Creates a second, independent tidy data set with the average of each variable for each activity and each subject.

sensor_avg_by_act_sub = ddply(sensor_data_mean_std, c("Subject","Activity"), numcolwise(mean))
write.table(sensor_avg_by_act_sub, file = "tidy_dataset_avg_each_var_each_activity_each_subject.txt")
