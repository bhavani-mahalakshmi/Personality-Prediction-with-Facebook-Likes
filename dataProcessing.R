#install needed packages
install.packages("Matrix")

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
