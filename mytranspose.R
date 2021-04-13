mytranspose <- function(object=NULL){
 mattranspose <- function(x) {
 y <- matrix(1, nrow=ncol(x), ncol = nrow(x))
  for(i in 1:nrow(x)) {
   for(j in 1:ncol(x)) {
    y[j,i] <- x[i,j]
   }
  }
  return(y)
 }
  if (is.null(object))
    stop('transpose: input is null.',
         call.=FALSE)
  if (!is.matrix(object)){
    if (is.atomic(object)){
     x <- matrix(object,byrow=FALSE)      
     y <- mattranspose(x)
     result <- as.vector(y)
    } else if (is.data.frame(object)){
      x <- as.matrix(object, byrow =FALSE)
      y <- mattranspose(x)
      result <- as.data.frame(y)
      rownames(result)<- colnames(object)
      colnames(result)<- rownames(object)   
    } else {
      stop('transpose: input is not a matrix or a vector or a dataframe.',
           call.=FALSE)
    }
  } else if((nrow(object)== 0) & (ncol(object)== 0)){
    stop('transpose: input is 0 x 0.',
         call.=FALSE)
  } else {
    result <- mattranspose(object)
  }
  return(result)
}