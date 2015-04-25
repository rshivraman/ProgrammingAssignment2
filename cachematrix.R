## makeCacheMatrix creates a special matrix object, and 
## then cacheSolve calculates the inverse of the matrix
## If the matrix inverse has already been calculated, 
## it will instead return it from cache and not calculate it again

## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  inv_x <- NULL
  ## Define function to set the value of the matrix
  set <- function(y) {
    x <<- y  ## Set the Value
    inv_x <<- NULL  ## Clear the cache
  }
  
  ## Define function to get the value of the matrix
  get <- function() x
  
  ## Define function to set the inverse
  setinverse<- function(inverse) inv_x <<-inverse
  
  ## Define function to get the inverse
  getinverse <- function() inv_x
  
  ## Return a list with the above four functions
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned by
## makeCacheMatrix above
## If the cached inverse is available, cacheSolve retrieves it 
## and if not available it computes, caches, and returns it

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  inv_x <- x$getinverse() ## fetches the cached value for the inverse
  
  if (!is.null(inv_x)) { ## If the cache exists then just return it
    message("getting cached inverse matrix")
    return(inv_x)
  } 
  ## when cache is empty then calculate, cache, and return it
  else {
    inv_x <- solve(x$get())  ## Calculate inverse
    x$setinverse(inv_x)  ## Cache the result
    return(inv_x)  ## Return the inverse
  }
}

