#setwd("/Users/tolga/Desktop")
set.seed(720)
class_means <- matrix(c(2.5, 2.5, -2.5, 2.5, -2.5, -2.5, 2.5, -2.5, 0.0, 0.0), nrow = 2)
cov1 <- matrix(c(0.8, -0.6, -0.6, 0.8), nrow = 2)
cov2 <- matrix(c(0.8, 0.6, 0.6, 0.8), nrow = 2)
cov3 <- matrix(c(0.8, -0.6, -0.6, 0.8), nrow = 2)
cov4 <- matrix(c(0.8, 0.6, 0.6, 0.8), nrow = 2)
cov5 <- matrix(c(1.6, 0.0, 0.0, 1.6), nrow = 2)
class_covariances <- array(c(cov1, cov2, cov3, cov4, cov5),dim = c(2,2,5))
class_sizes <- c(50, 50, 50, 50, 100)

library(MASS)
points1 <- mvrnorm(n = class_sizes[1], class_means[,1], class_covariances[,,1])
points2 <- mvrnorm(n = class_sizes[2],  class_means[,2], class_covariances[,,2])
points3 <- mvrnorm(n = class_sizes[3],  class_means[,3],  class_covariances[,,3])
points4 <- mvrnorm(n = class_sizes[4],  class_means[,4],  class_covariances[,,4])
points5 <- mvrnorm(n = class_sizes[5],  class_means[,5],  class_covariances[,,5])
data_points <- rbind(points1, points2, points3, points4, points5)
plot(data_points[,1], data_points[,2], type = "p", pch = 20, col = "black", las = 1, xlim = c(-6, 6), ylim = c(-6, 6), xlab = "x1", ylab = "x2")

K <- 5
N <- sum(class_sizes)
update_centroids <- function(memberships, data_points){
  if(is.null(centroids) == TRUE){
    # initialize centroids
    centroids <- data_points[sample(1:N, K),]
  } else{
    # update centroids
    for (k in 1:K) {
      centroids[k,] <- colMeans(data_points[memberships == k,])
    }
  }
  return(centroids)
}

update_memberships <- function(centroids, data_points){
  D <- as.matrix(dist(rbind(centroids, data_points), method = "euclidean"))
  D <- D[1:nrow(centroids), (nrow(centroids) + 1):(nrow(centroids) + nrow(data_points))]
  memberships <- sapply(1:ncol(D), function(i) {which.min(D[,i])})
  return(memberships)
}

memberships <- NULL
centroids <- NULL
centroids <- update_centroids(memberships, data_points)
memberships <- update_memberships(centroids, data_points)
centroids <- update_centroids(memberships, data_points)
memberships <- update_memberships(centroids, data_points)

means <- t(centroids)
prior1 <- 50/N
prior2 <- 50/N
prior3 <- 50/N
prior4 <- 50/N
prior5 <- 100/N
cov1 <- cov(points1)
cov2 <- cov(points2)
cov3 <- cov(points3)
cov4 <- cov(points4)
cov5 <- cov(points5)
covariances <- array(c(cov1, cov2, cov3, cov4, cov5),dim = c(2,2,5))

centroids <- NULL
memberships <- NULL
iteration <- 1
while(1) {
  if(iteration == 101){
    break
  }
  old_centroids <- centroids
  centroids <- update_centroids(memberships, data_points)
  old_memberships <- memberships
  memberships <- update_memberships(centroids, data_points)
  iteration = iteration + 1
  means <- t(centroids)
}

print(t(means))

plot(data_points[memberships == 1, 1], data_points[memberships == 1, 2], type = "p", pch = 20, col = "red", las = 1, xlim = c(-6, 6), ylim = c(-6, 6), xlab = "x1", ylab = "x2")
points(data_points[memberships == 2, 1], data_points[memberships == 2, 2], type = "p", pch = 20, col = "blue")
points(data_points[memberships == 3, 1], data_points[memberships == 3, 2], type = "p", pch = 20, col = "purple")
points(data_points[memberships == 4, 1], data_points[memberships == 4, 2], type = "p", pch = 20, col = "green")
points(data_points[memberships == 5, 1], data_points[memberships == 5, 2], type = "p", pch = 20, col = "orange")

library(mixtools)
ellipse(class_means[,1], class_covariances[,,1], .05, npoints = class_sizes[1], lty=2, lwd=2)
ellipse(means[,1], covariances[,,1], .05, npoints = class_sizes[1], lwd=2)

ellipse(class_means[,2], class_covariances[,,2], .05, npoints = class_sizes[2], lty=2, lwd=2)
ellipse(means[,5], covariances[,,2], .05, npoints = class_sizes[2], lwd=2)

ellipse(class_means[,3], class_covariances[,,3], .05, npoints = class_sizes[3], lty=2, lwd=2)
ellipse(means[,2], covariances[,,3],.05, npoints = class_sizes[2], lwd=2)

ellipse(class_means[,4], class_covariances[,,4], .05, npoints = class_sizes[4], lty=2, lwd=2)
ellipse(means[,4], covariances[,,4], .05, npoints = class_sizes[2], lwd=2)

ellipse(class_means[,5], class_covariances[,,5], .05, npoints = class_sizes[5], lty=2, lwd=2)
ellipse(means[,3], covariances[,,5], .05, npoints = class_sizes[2], lwd=2)


