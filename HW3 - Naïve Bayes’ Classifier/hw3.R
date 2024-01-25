setwd("/Users/tolga/Desktop")
data_set_images <- read.csv("hw03_data_set_images.csv", header = FALSE)
data_set_labels <- read.csv("hw03_data_set_labels.csv", header = FALSE)

a_image_train <- as.matrix(data_set_images[1:25,])
a_image_test <- as.matrix(data_set_images[26:39,])
b_image_train <- as.matrix(data_set_images[40:64,])
b_image_test <- as.matrix(data_set_images[65:78,])
c_image_train <- as.matrix(data_set_images[79:103,])
c_image_test <- as.matrix(data_set_images[104:117,])
d_image_train <- as.matrix(data_set_images[118:142,])
d_image_test <- as.matrix(data_set_images[143:156,])
e_image_train <- as.matrix(data_set_images[157:181,])
e_image_test <- as.matrix(data_set_images[182:195,])

image_train <- rbind(a_image_train, b_image_train, c_image_train, d_image_train, e_image_train)
image_test <- rbind(a_image_test, b_image_test, c_image_test, d_image_test, e_image_test)

a_label_train <- as.matrix(data_set_labels[1:25,])
a_label_test <- as.matrix(data_set_labels[26:39,])
b_label_train <- as.matrix(data_set_labels[40:64,])
b_label_test <- as.matrix(data_set_labels[65:78,])
c_label_train <- as.matrix(data_set_labels[79:103,])
c_label_test <- as.matrix(data_set_labels[104:117,])
d_label_train <- as.matrix(data_set_labels[118:142,])
d_label_test <- as.matrix(data_set_labels[143:156,])
e_label_train <- as.matrix(data_set_labels[157:181,])
e_label_test <- as.matrix(data_set_labels[182:195,])

label_train <- rbind(a_label_train, b_label_train, c_label_train, d_label_train, e_label_train)
label_test <- rbind(a_label_test, b_label_test, c_label_test, d_label_test, e_label_test)

for(i in 1:length(label_train)){
  if(label_train[i] == "A"){
    label_train[i] = 1
  }
  if(label_train[i] == "B"){
    label_train[i] = 2
  }
  if(label_train[i] == "C"){
    label_train[i] = 3
  }
  if(label_train[i] == "D"){
    label_train[i] = 4
  }
  if(label_train[i] == "E"){
    label_train[i] = 5
  }
}

for(i in 1:length(label_test)){
  if(label_test[i] == "A"){
    label_test[i] = 1
  }
  if(label_test[i] == "B"){
    label_test[i] = 2
  }
  if(label_test[i] == "C"){
    label_test[i] = 3
  }
  if(label_test[i] == "D"){
    label_test[i] = 4
  }
  if(label_test[i] == "E"){
    label_test[i] = 5
  }
}

x_truth <- image_train
x_test <- image_test
y_truth <- as.numeric(label_train)
y_test <- as.numeric(label_test)

p1 <- colMeans(a_image_train)
p2 <- colMeans(b_image_train)
p3 <- colMeans(c_image_train)
p4 <- colMeans(d_image_train)
p5 <- colMeans(e_image_train)

class_priors <- sapply(X = 1:5, FUN = function(c) {mean(y_truth == c)})
print(class_priors)

pcd <- cbind(p1,p2,p3,p4,p5)
print(pcd[,1])
print(pcd[,2])
print(pcd[,3])
print(pcd[,4])
print(pcd[,5])

tableA<- ifelse(p1 >class_priors[1], 1, 0)
matrixA <- matrix(tableA, nrow = 20)
print(matrixA)

tableB<- ifelse(p2 >class_priors[2], 1, 0)
matrixB <- matrix(tableB, nrow = 20)
print(matrixB)

tableC<- ifelse(p3 >class_priors[3], 1, 0)
matrixC <- matrix(tableC, nrow = 20)
print(matrixC)

tableD<- ifelse(p4 >class_priors[4], 1, 0)
matrixD <- matrix(tableD, nrow = 20)
print(matrixD)

tableE<- ifelse(p5 >class_priors[5], 1, 0)
matrixE <- matrix(tableE, nrow = 20)
print(matrixE)

rotate <- function(x) t(apply(x, 2, rev))

image(rotate(matrixA), axes=FALSE, col = gray.colors(12, rev = TRUE))
image(rotate(matrixB), axes=FALSE, col = gray.colors(12, rev = TRUE))
image(rotate(matrixC), axes=FALSE, col = gray.colors(12, rev = TRUE))
image(rotate(matrixD), axes=FALSE, col = gray.colors(12, rev = TRUE))
image(rotate(matrixE), axes=FALSE, col = gray.colors(12, rev = TRUE))

abc <- image_train %*% pcd
y_predicted_train <- apply(abc, 1, which.max)
y_train <- as.numeric(label_train)
confusionMatrix1 <- table(y_predicted_train, y_train)
print(confusionMatrix1)

pcd2 <- cbind(colMeans(a_image_test), colMeans(b_image_test), colMeans(c_image_test), colMeans(d_image_test), colMeans(e_image_test))
def <- image_test %*% pcd2
y_predicted_test <- apply(def, 1, which.max)
y_test <- as.numeric(label_test)
confusionMatrix2 <- table(y_predicted_test, y_test)
print(confusionMatrix2)
