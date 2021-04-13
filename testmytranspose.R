require(testthat)
source("mytranspose.R")

#test function
test <- function(object) {
mattest <- function(x) {
 test_that("mytranspose_test",{
  for(i in 1:nrow(x)){
   for(j in 1:ncol(x)){
     result <- mytranspose(x)
     expect_equal(as.character(x[i,j]), as.character(result[j,i]))
   }
  }
 })
}
  if (is.null(object))
    stop('transpose_test: input is null.',
         call.=FALSE)
  if (!is.matrix(object)){
    if (is.atomic(object)){
     x <- matrix(object,byrow=FALSE)      
    } else if (is.data.frame(object)){
      x <- as.matrix(object, byrow =FALSE)
    } else {
      stop('transpose_test: input is not a matrix or a vector or a dataframe.',
           call.=FALSE)
    }
  } else if((nrow(object)== 0) & (ncol(object)== 0)){
    stop('transpose_test: input is 0 x 0.',
         call.=FALSE)
  } else {
    x <- object
  }
  return(mattest(x))
}


#case1
myvar1 <-  matrix(1:10, nrow=5, ncol=2)
myvar1
mytranspose(myvar1)
test(myvar1)

#case2
myvar1 <-  matrix(NA, nrow=0, ncol=0)
myvar1
mytranspose(myvar1)
test(myvar1)

#case3
myvar1 <-  matrix(c(1,2), nrow=1, ncol=2)
myvar1
mytranspose(myvar1)
test(myvar1)

#case4
myvar1 <-  matrix(c(1,2), nrow=2, ncol=1)
myvar1
mytranspose(myvar1)
test(myvar1)

#case5
myvar2 <- c(1,2,NA,3)
myvar2
mytranspose(myvar2)
test(myvar2)

#case6
myvar2 <- c(NA)
myvar2
mytranspose(myvar2)
test(myvar2)

#case7
myvar2 <- c()
myvar2
mytranspose(myvar2)
test(myvar2)

#case8
d <- c(1,2,3,4)
e <- c("red", "white", "red", NA)
f <- c(TRUE,TRUE,TRUE,FALSE)
mydata3 <- data.frame(d,e,f)
mydata3
mytranspose(mydata3)
test(mydata3)