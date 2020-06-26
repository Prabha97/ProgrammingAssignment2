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


## this function calculates the mean of the special "vector" created with the makeCacheMatrix function.
cacheSolve <- function(x, ...) { ## This function computes the inverse of the special "matrix" returned by makeCacheMatrix function.
      ## Return a matrix that is the inverse of 'x'
      ## first checks to see if the mean has already been calculated. 
 
      inver <- x$getInverse()
      
      if(!is.null(inver)) {
        ## gets the mean from the cache and skips the computation.
        message("getting cached data")
        return(inver)
      }
      data <- x$get()
      ## calculates the mean of the data.
      inver <- inverse(data, ...)
      ## sets the value of the mean in the cache via the setmean function.
      x$setInverse(inver)
      inver
}
