setwd("/Users/tolga/Desktop")
data_set_images <- read.csv("hw02_data_set_images.csv", header = FALSE)
data_set_labels <- read.csv("hw02_data_set_labels.csv", header = FALSE)

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
y_truth <- matrix(0, 125, 5)
y_truth[cbind(1:125, as.numeric(label_train))] <- 1

eta <- 0.01 
epsilon <- 1e-3 

sigmoid <- function(X, w, w0) {
  return (1 / (1 + exp(-sweep(X%*%w, 2, w0))))
}

gradient_w <- function(X, y_truth, y_predicted) {
  return(-t(X)%*%(((y_truth * y_predicted) - y_predicted^2) - (((y_truth * y_predicted) - y_predicted^2) * y_predicted)))
}

gradient_w0 <- function(y_truth, y_predicted) {
  return (-colSums(((y_truth * y_predicted) - y_predicted^2) - (((y_truth * y_predicted) - y_predicted^2) * y_predicted)))
}

set.seed(100)
a <- runif(ncol(image_train), min = -0.01, max = 0.01)
b <- runif(ncol(image_train), min = -0.01, max = 0.01)
c <- runif(ncol(image_train), min = -0.01, max = 0.01)
d <- runif(ncol(image_train), min = -0.01, max = 0.01)
e <- runif(ncol(image_train), min = -0.01, max = 0.01)
w<-cbind(a,b,c,d,e)
f<-runif(1, min = -0.01, max = 0.01)
g<-runif(1, min = -0.01, max = 0.01)
h<-runif(1, min = -0.01, max = 0.01)
i<-runif(1, min = -0.01, max = 0.01)
j<-runif(1, min = -0.01, max = 0.01)
w0 <- rbind(f,g,h,i,j)

iteration <- 1
objective_values <- c()
while(1) {
  y_predicted_train <- sigmoid(x_truth, w, w0)
  
  objective_values <- c(objective_values, sum(0.5 * (y_truth - y_predicted_train)^2))
  w_old <- w
  w0_old <- w0
  
  w <- w - eta * gradient_w(x_truth, y_truth, y_predicted_train)
  w0 <- w0 - eta * gradient_w0(y_truth, y_predicted_train)
  
  if(sqrt((w0 - w0_old)^2 + sum((w - w_old)^2)) < epsilon) {
    break
  }
  
  iteration <- iteration + 1
}

plot(1:iteration, objective_values, xlab = "Iteration", ylab = "Error", type = "l", lwd = 2, las = 1)

y_predicted_train <- apply(y_predicted_train, 1, which.max)
confusionMatrix1 <- table(y_predicted_train, as.numeric(label_train))
print(confusionMatrix1)

y_predicted_test <- sigmoid(x_test, w, w0)
y_predicted_test <- apply(y_predicted_test, 1, which.max)
confusionMatrix2 <- table(y_predicted_test, as.numeric(label_test))
print(confusionMatrix2)
