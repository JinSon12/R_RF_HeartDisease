library(randomForest)
require(caTools)

# DataSet Originated from the below webpage : 
# https://archive.ics.uci.edu/ml/datasets/Heart+Disease 
# Guide for the model creation
# https://towardsdatascience.com/random-forest-in-r-f66adf80ec9


data <- read.csv(
  "processed.cleveland.data", 
  header=FALSE
)

# getting the row and the number of columns of the data
dim(data)

# specifying the column names 
names(data) <- c("age", "sex", "cp", "trestbps", "choi", "fbs", 
                 "restecg", "thalach", "exang", "oldpeak", 
                 "slope", "ca", "thai", "num") 

head(data)

#replacing the values with 1 
data$num[data$num > 1] <- 1

summary(data)

sapply(data, class)

# transforming 
data <- transform(
  data,
  age=as.integer(age),
  sex=as.factor(sex),
  cp=as.factor(cp),
  trestbps=as.integer(trestbps),
  choi=as.integer(choi),
  fbs=as.factor(fbs),
  restecg=as.factor(restecg),
  thalach=as.integer(thalach),
  exang=as.factor(exang),
  oldpeak=as.numeric(oldpeak),
  slope=as.factor(slope),
  ca=as.factor(ca),
  thai=as.factor(thai),
  num=as.factor(num)
)
sapply(data, class)

summary(data)

# ===============================================================
#                   Data Cleaning 
# ===============================================================
# replacing the data points having "?" with NA 
data[ data == "?"] <- NA

# View the missing value counts of each column 
colSums(is.na(data))

# replace NA values for column thai with "3.0" = normal 
data$thai[which(is.na(data$thai))] <- as.factor("3.0")

# dropping the rows where ca = NA 
data <- data[!(data$ca %in% c(NA)),]

# the total number of NA values. 
# result = none for all the columns 
colSums(is.na(data))

# returns the summary of the data
# however, still views "?" as a potential class (although it has 0 instances of it)
summary(data)

# casting the columns to factors 
data$ca <- factor(data$ca)
data$thai <- factor(data$thai)
summary(data)

# ===============================================================
#                   Data Splitting 
# ===============================================================
sample = sample.split(data$num, SplitRatio = .75)
train = subset(data, sample == TRUE)
test  = subset(data, sample == FALSE)
dim(train)
dim(test)

# ===============================================================
#       Initializing an instance of the randomForest Class 
#       -- this is different from the scikit-learn -- 
#       -- no need to explicitly call "fit" method to train the model -- 
# 
# 1. by default, # of decision trees in the forest = 500 
# 2. by default, # of features used as potential candidates for each split = 3 
# 3. the model will auto attempt to classify each of the samples in the Out of Bag dataset 
# 4. and display a confusion matrix with the results. 
# *OOB error rate: 
# ===============================================================
rf <- randomForest(
  num ~ .,
  data=train
)

pred = predict(rf, newdata=test[-14])

# Classification problem => confusion matrix to evaluate the performance of the model 
# Recall that values diagonal = true pos, true neg (correct predictions)
# Others = fP, fN 
cm = table(test[,14],pred)
print(cm)
