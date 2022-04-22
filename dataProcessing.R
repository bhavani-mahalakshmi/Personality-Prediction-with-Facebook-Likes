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

# calculating correlations
correlation<-round(cor(rotUmatrix, usersTable[,-1], use="p"),2)

# Using ggplot2 to redesigning it to make it uncomplicated
Melt<-melt(correlation)
colnames(Melt)<-c("SVD", "Trait", "r")

# Generating the heatmap for SVD dimensions and understanding personality traits 
qplot(p=SVD, q=Trait, data=q, fill=r, geom="tile") +
  scale_fill_gradient2(limits=range(p), breaks=c(min(p), 0, max(p)))+
  theme(axis.text=element_text(size=12), 
        axis.title=element_text(size=14,face="bold"),
        panel.background = element_rect(fill='white', colour='white'))+
  labs(p=expression('SVD'[rot]), q=NULL)

# Performing k-fold cross validations to divide users in 10 groups
splits <- sample(1:10, size = nrow(usersTable), replace = T)

#Creating a subset of users from group 1 and allocating them in trail set
trial <- splits == 1
library(irlba)

# Extracting SVD dimensions from the trial subset
svdM <- irlba(ufp[!trial, ], nv = 50)

# Rotating Like SVD scores for trial subset
rotVmatrix <- unclass(varimax(svdM$v)$loadings)

# Rotating user SVD scores for the total set
rotUmatrix <- as.data.frame(as.matrix(ufp %*% rotVmatrix))
