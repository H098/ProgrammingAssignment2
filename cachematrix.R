## This project is to cache the inverse of a matrix
## functions do

##makeCache matrix is to cache it's inverse

makeCacheMatrix <- function(x = matrix()) {
  ##initialize the inverse
  I <- NULL
  ##to set the matrix
  set <- function( matrix ){
    x <<- matrix
    I <<- NULL
  }
  
  ##to get the matrix
  get <- function(){
    ## return the matrix
    x
  }
  
  ##to set the inverse of the function 
  
  setinverse <- function(inverse){
    I <<- inverse
  }
  getinverse <- function(){
    I
  }
  
  #return a list
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)

}


## if from the above function inverse has been calculated and the matrix has not changed
## then cashSolve should retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  I <- x$getinverse()
  
  #return the inverse if its already set
  if ( !is.null(I) )
  {
    message("getting cached data")
    return(I)
  }
  
  ## get the matrix from the object
  data <- x$get()
  
  I <- solve(data) %*% data
  x$setinverse(I)
  
  
  I
}
