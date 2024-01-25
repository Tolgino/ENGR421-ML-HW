class_means <- matrix(c(0.0,2.5,-2.5,-2.0,2.5,-2.0), nrow = 2)
sd1 <- matrix(c(3.2,0.0,0.0,1.2), nrow = 2)
sd2 <- matrix(c(1.2,-0.8,-0.8,1.2), nrow = 2)
sd3 <- matrix(c(1.2,0.8,0.8,1.2), nrow = 2)
class_deviations <- array(c(sd1,sd2,sd3),dim = c(2,2,3))
class_sizes <- c(120, 90, 90)

library(MASS)
points1 <- mvrnorm(n = class_sizes[1], class_means[,1], class_deviations[,,1])
points2 <- mvrnorm(n = class_sizes[2],  class_means[,2], class_deviations[,,2])
points3 <- mvrnorm(n = class_sizes[3],  class_means[,3],  class_deviations[,,3])

plot(points1, col="red", pch=20, xlim = c(-6,6), ylim= c(-6,6), xlab="x1", ylab = "x2")
points(points2, col="green", pch=20)
points(points3, col="blue", pch=20)

data_setX <- rbind(points1, points2, points3)
class <- c(rep(1, class_sizes[1]), rep(2, class_sizes[2]), rep(3, class_sizes[3]))
data<-cbind(data_setX,class)

x <- data[,1]
y <- data[,2]

sample_meansX <- sapply(X = 1:3, FUN = function(c) {mean(x[class == c])})
sample_meansY <- sapply(X = 1:3, FUN = function(c) {mean(y[class == c])})
sample_means <- rbind(sample_meansX,sample_meansY)
print(sample_means)

sample_covariances <- array(c(cov(points1),cov(points2),cov(points3)),dim = c(2,2,3))
print(sample_covariances)

class_priors <- sapply(X = 1:3, FUN = function(c) {mean(class == c)})
print(class_priors)

sample_deviationsX <- sapply(X = 1:3, FUN = function(c) {sqrt(mean((x[class == c] - sample_means[c])^2))})
sample_deviationsY <- sapply(X = 1:3, FUN = function(c) {sqrt(mean((y[class == c] - sample_means[c])^2))})
sample_deviations <- rbind(sample_deviationsX,sample_deviationsY)

data_interval <- seq(from = 0, to = +6, by = 0.01)
score_values <- sapply(X = 1:3, FUN = function(c) {- 0.5 * log(2 * pi * sample_deviations[c]^2) - 0.5 * (data_interval - sample_means[c])^2 / sample_deviations[c] + log(class_priors[c])})
log_posteriors <- score_values - sapply(X = 1:nrow(score_values), FUN = function(r) {max(score_values[r,]) + log(sum(exp(score_values[r,] - max(score_values[r,]))))})
points(data_interval, exp(log_posteriors[,2]), type = "l", lwd = 2)

data_interval <- seq(from = -6, to = 0, by = 0.01)
score_values <- sapply(X = 1:3, FUN = function(c) {- 0.5 * log(2 * pi * sample_deviations[c]^2) - 0.5 * (data_interval - sample_means[c])^2 / sample_deviations[c] + log(class_priors[c])})
log_posteriors <- score_values - sapply(X = 1:nrow(score_values), FUN = function(r) {max(score_values[r,]) + log(sum(exp(score_values[r,] - max(score_values[r,]))))})
points(data_interval, exp(log_posteriors[,3]), type = "l", lwd = 2)
points(exp(log_posteriors[,2]), data_interval, type = "l", lwd = 2)

greenpointsX <- ifelse(points2[,1]<mean(exp(log_posteriors[,2])), "yes", "no")
table(greenpointsX)

greenpointsY <- ifelse(points2[,2]<mean(exp(log_posteriors[,3]))+sd(exp(log_posteriors[,3])), "yes", "no")
table(greenpointsY)

bluepointsX <- ifelse(points3[,1]>mean(exp(log_posteriors[,2])), "yes", "no")
table(bluepointsX)

bluepointsY <- ifelse(points3[,2]<mean(exp(log_posteriors[,3]))+sd(exp(log_posteriors[,3])), "yes", "no")
table(bluepointsY)

redpointsplit <- split(points1,points1[,1]>mean(exp(log_posteriors[,2])))
redpointLow<- matrix(redpointsplit$"FALSE", nrow = length(redpointsplit$"FALSE")/2, ncol=2) 
redpointHigh<- matrix(redpointsplit$"TRUE", nrow = length(redpointsplit$"TRUE")/2, ncol=2) 

redpointLowY <- ifelse(redpointLow[,2]>mean(exp(log_posteriors[,3]))-sd(exp(log_posteriors[,3])), "yes", "no")
table(redpointLowY)

redpointHighY <- ifelse(redpointHigh[,2]>mean(exp(log_posteriors[,3]))-sd(exp(log_posteriors[,3])), "yes", "no")
table(redpointHighY)

if(is.na(table(greenpointsY)[2])){
  greenY<-append(table(greenpointsY), 0, after = 0)
}else{
  greenY<-table(greenpointsY)
}

if(is.na(table(greenpointsX)[2])){
  greenX<-append(table(greenpointsX), 0, after = 0)
}else{
  greenX<-table(greenpointsX)
}

if(is.na(table(bluepointsY)[2])){
  blueY<-append(table(bluepointsY), 0, after = 0)
}else{
  blueY<-table(bluepointsY)
}

if(is.na(table(bluepointsX)[2])){
  blueX<-append(table(bluepointsX), 0, after = 0)
}else{
  blueX<-table(bluepointsX)
}

if(is.na(table(redpointLowY)[2])){
  redLowhY<-append(table(redpointLowY), 0, after = 0)
}else{
  redLowhY<-table(redpointLowY)
}

if(is.na(table(redpointHighY)[2])){
  redHighY<-append(table(redpointHighY), 0, after = 0)
}else{
  redHighY<-table(redpointHighY)
}

c1r1 <- redHighY[2] + redLowhY[2]
c1r2 <- redLowhY[1]
c1r3 <- redHighY[1]
c2r1 <- greenY[1]
c2r2 <- (greenY[2] + greenX[2] - greenY[1] - greenX[1])/2
c2r3 <- greenX[1]
c3r1 <- blueY[1]
c3r2 <- blueX[1]
c3r3 <- (blueY[2] + blueX[2] - blueY[1] - blueX[1])/2

confusionMatrix <- matrix(c(c1r1,c1r2,c1r3,c2r1,c2r2,c2r3,c3r1,c3r2,c3r3), nrow = 3)
print(confusionMatrix)
