train_url <- 'http://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv'
test_url  <- 'http://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv'

train_file <- './pml-training.csv'
test_file  <- './pml-testing.csv'

force <- 0 # helps control reruns of file reads
if (force | ! file.exists(train_file)) {
    download.file(train_url, destfile = train_file)
}

if (force | ! file.exists(test_file)) {
    download.file(test_url, destfile = test_file)
}

raw_train <- read.csv(train_file,na.strings=c("", "NA","#DIV/0!")) ## YAW
raw_test  <- read.csv(test_file,na.strings=c("", "NA","#DIV/0!"))

# only use columns which are not missing data - see how well this works?

use <- colSums(is.na(raw_train))==0
proc_data <- raw_train[,use]
proc_test <- raw_test[,use]

# remove also columns corresponding to id and summary info. Incorporate timestamps diffs later

dontuse <- grep('X|user|timestamp|window|total',names(proc_data))
proc_data <- proc_data[,-dontuse]
proc_test <- proc_test[,-dontuse]

dontuse <- grep('_x|_y|_z',names(proc_data))
proc_data <- proc_data[,-dontuse]
proc_test <- proc_test[,-dontuse]

set.seed(121)

library(caret); library(lattice); library(ggplot2)

# create test and validation sets
# note that test file above is not the same as the testing file below which is used for cross-validation

use <- createDataPartition(y=train$classe, p=0.8, list=FALSE)
training <- proc_data[ use,]
testing  <- proc_data[-use,]

ctrl <- trainControl(method = "cv", number = 4, allowParallel = TRUE)
fit <- train(classe ~., method="rf", data=training, trControl=ctrl)

confusionMatrix(predict(fit,training),training$classe)

confusionMatrix(predict(fit,testing), testing$classe)
