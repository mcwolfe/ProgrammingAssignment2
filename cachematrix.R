## Put comments here that give an overall description of what your
## functions do

## This function takes a matrix as a parameter. The function then defines 
## Getters and setters for both the matrix and it's inverse. 
## Finally, a list is created with the getters and setters as items in the list.

makeCacheMatrix <- function(x = matrix()) {
  im <- NULL
  set <- function(y){
    x<<-y
    im <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) im <<- inverse
  getInverse <- function() im
  
  list(set=set, get=get,setInverse = setInverse, getInverse = getInverse)

}


## CacheSolve takes the matrix X and tries to get the pre-calculated inverse matrix.
## If that is possible, it returns that inverse matrix. If there is no inverse matrix, one is 
## calculated and stored in the matrix, and also returned. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  im <- x$getInverse()
  if(!is.null(im)){
    message("getting cached data")
    return im
  }
  data <- x$get()
  im <- solve(data)
  x$setInverse(im)
  im
}
