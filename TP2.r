
#creates a vector
x <- 1:12
x

#creates a matrix from vector
m1 <-matrix(x,4,3, byrow = TRUE)
m1

#check if it is a matrix
is.matrix(m1)

#checks class of an object
class(m1)

#gives the diminutions of the matrix
dim(m1)

#gives the number of colons and rows
ncol(m1)
nrow(m1)

#creat a matrix with a sample note in sample
#second is size of sample and must give the right size to fit rows
m2<-matrix(sample(20,16),4)
m2

#gives the determinanet of a squared matrix
det(m2)

#finds the transpose matrix
t(m2)

#finds the inverse matrix
solve(m2)

#To create identitiy matrix of any size example sier = 3
diag(3)


#To give a matrix row names or colon names use this to remove use <-NULL
rownames(m2) <- c('K','G','L','F')
colnames(m2) <- c('ro','bi','cm','xv')
m2

#matrix multiplication
m3<-matrix(sample(50,16),4)
m4<-matrix(sample(20,16),4)
m3
m4
m5<-m3%*%m4
m5


#To calculate sum of a row or colon apply(matrix name,  1 for rows or 2 for colons, function)

apply(m5,1,sum)
apply(m5,2,sum)


#To calculate product of rows or colons we use prod function

apply(m5,1,prod)
apply(m5,2,prod)

#To calculate the mean and standard deviation in a matrix

apply(m5, 1, mean)
apply(m5,1,sd)


#An idempotent matrix is one which, when multiplied by itself, doesn't change.
#power of a matrix
dd<-c(2,-1,1,-2,3,-2,-4,4,-3)
z1<-matrix(dd,3,3)
library(expm)
z1%^%3
z1%*%z1





































Mat2 = rbind(c(0.50, 0.50, 0.00),
              c(0.00, 0.25, 0.75),
              c(0.75, 0.00, 0.25))

rownames(Mat1) <- c('A','B','C')
colnames(Mat1) <- c('A','B','C')
Mat1






Mat1%^%5
Mat1%^%20
Mat1%^%40



newM = Mat1






library(markovchain)
Mat1%^%30

m <- nrow(Mat1)
m




Mat1 = rbind(c(0.50, 0.50, 0.00 , 0.00 , 0.00),
             c(0.00, 0.25, 0.75 , 0.00 , 0.00),
             c(0.00, 0.00, 1.00 , 0.00 , 0.00),
             c(0.00, 0.00, 0.75 , 0.00 , 0.25),
             c(0.75, 0.00, 0.00 , 0.25 , 0.00))
Mat1


equiv <- function(P){
  
  m <- nrow(P)
  T = matrix(0 ,m, m)
  i = 1
  while(i<=m){
    a <- c(i)
    b <- matrix(0, ncol = 1, nrow = m)
    b[1:i] <- 1
    prev <- 1
    curr <- 0
    
      while (prev != curr) {
        prev <- sum(which(b>0))
        n <- nrow(a)
        c <- cbind(sum(P[a,],1))
        d <- which(c>0)
        f <- nrow(d)
        o <- matrix(rep(1,f), ncol = 1, nrow = f, byrow = FALSE)
        b[1,d] <- o[1,f]
        newM <- sum(which(b>0))
        a <- b
      }
    T(i,) = b
    i=i+1
  }
  
  F <- t(T)
  C <- T&F
  V <- (sum(t(C) == t(T)) == m)
  
  return(C)
}



equiv(Mat2)












