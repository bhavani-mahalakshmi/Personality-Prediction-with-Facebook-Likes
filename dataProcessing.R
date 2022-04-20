#install needed packages
install.packages("Matrix")
install.packages("irlba")
install.packages("corrplot")
install.packages("ggplot2", "reshape2")
install.packages("reshape2")

#matrix library required for construction of sparse matrix
require(Matrix)

#load data set files into table/dataframe
usersTable <- read.csv(file = "C:/Users/sangr/Documents/573Project/dataset/users.csv")
likesTable <- read.csv(file = "C:/Users/sangr/Documents/573Project/dataset/likes.csv")
ulTable <- read.csv(file = "C:/Users/sangr/Documents/573Project/dataset/users-likes.csv")

#dropping political column from users table due to many null data points and renaming column
usersTable$political <- NULL
names(likesTable)[2] <- 'text'

#verifying correct table formats
head(usersTable, n=5)
head(likesTable, n=5)
head(ulTable, n=5)

#joining with reference to row number in likes and users table
ulTable$user_index<-match(ulTable$userid,usersTable$userid)
ulTable$like_index<-match(ulTable$likeid,likesTable$likeid)
head(ulTable, n=5)

#creating sparse matrix between user and like
ulMatrix <- sparseMatrix(i = ulTable$user_index, j = ulTable$like_index, x = 1)

#change row and col names from row index to userid and text
rownames(ulMatrix) <- usersTable$userid
colnames(ulMatrix) <- likesTable$text

# Remove the values that are under threshold
repeat {                                       
  i <- sum(dim(ulMatrix))                             
  ulMatrix <- ulMatrix[rowSums(ulMatrix) >= 50, colSums(ulMatrix) >= 150] 
  if (sum(dim(ulMatrix)) == i) break                  
}

usersTable <- usersTable[match(rownames(ulMatrix), usersTable$userid), ]

library(irlba)

# Get 5 SVD dimensions for the user, like matrix
svdUL <- irlba(ulMatrix, nv = 5)

# Get SVD scores
u <- svdUL$u
v <- svdUL$v

# Get the rotated V matrix:
rotVmatrix <- unclass(varimax(svdUL$v)$loadings)

# The cross-product of user like Matrix and rotated V Matrix
rotUmatrix <- as.matrix(ulMatrix %*% rotVmatrix)

# Correlate user traits and their SVD scores
cor(rotUmatrix, usersTable[,-1], use = "pairwise")

#libraries to be loaded
library(corrplot)
library(ggplot2)
library(reshape2)

# SVD
top <- list()
bottom <-list()
for (i in 1:5) {
  f <- order(rotVmatrix[ ,i])
  temp <- tail(f, n = 10)
  top[[i]]<-colnames(ulMatrix)[temp]  
  temp <- head(f, n = 10)
  bottom[[i]]<-colnames(ulMatrix)[temp]  
}