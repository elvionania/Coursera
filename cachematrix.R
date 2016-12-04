
cachesolve<-function(x,...){
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}

makeCacheMatrix<-function(x){
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(matrice) i <<- matrice
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

isRegularMatrice<-function(x){
  if(!is.matrix(x)){
    return(0)
  }else if(ncol(x)!=nrow(x)){
    return(0)
  }
  return(1)
}
