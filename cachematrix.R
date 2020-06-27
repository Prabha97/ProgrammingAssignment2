## This overall functions are for cacheing the inverse of a matrix.
## Computing the inverse of a square matrix can be done with the solve function

##  This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
    
      inver <- NULL
      
      ## Set value of matrix
      set <- function(y) {
        x <<- y
        inver <<- NULL
      }
      
      ## Get the matrix value
      get <- function() x
      
      ## set inverse of matrix value
      setInverse <- function(inverse) inver <<- inverse
      ## get the inverse matrix
      getInverse <- function() inver
      
      ## returning the list
      list(set = set, get = get,
           setInverse = setInverse,
           getInverse = getInverse)  
}


##  ## This function computes the inverse of the special "matrix" returned by makeCacheMatrix function.
cacheSolve <- function(x, ...) {
      ## Return a matrix that is the inverse of 'x'
      ## first checks to see if the inverse has already been calculated. 
 
      inver <- x$getInverse()
      
      if(!is.null(inver)) {
        ## gets the inverse from the cache and skips the computation.
        message("getting cached data")
        return(inver)
      }
      data <- x$get()
      ## calculates the inverse of the data.
      inver <- inverse(data, ...)
      ## sets the value of the inverse in the cache via the setmean function.
      x$setInverse(inver)
      inver
}
