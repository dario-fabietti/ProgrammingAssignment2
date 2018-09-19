## Creates a function with getters/setters for base matrix (get/set) and inverse (getinverse/setinverse)
makeCacheMatrix <- function(x = matrix()) {
  
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) i <<- solve
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)     
  
}


## declare a function that takes a matrix as argument and: 
## - if the inverse matrix has NOT been previously calculated, calculates the inverse of a matrix (supposed to be invertible by requirement) 
## - if the inverse matrix has already been calculated, takes its value from the cache via $getinverse()

cacheSolve <- function(x, ...) {
  
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i     
  
  ## Return a matrix that is the inverse of 'x'
}

# EXAMPLE:
# (code to run to test the function; please run it again in the very unlucky case of a not invertible matrix)
# [the probability of a random matrix to be non invertible is quite small (zero if calculated via integration on pdf)]

# CODE:
# B <- matrix(rnorm(4,0,1),2,2) 
# show(B)
# B1 <- makeCacheMatrix(B)
# show(B1$get())
# cacheSolve(B1)
# B1$get()
# cacheSolve(B1)
