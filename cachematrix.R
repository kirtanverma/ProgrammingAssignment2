
## Setting and getting value of the matrix and inverse of matrix

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function (y) {
              x <<- y
              m <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) m <<- solve
        getinverse <- function() m
        list(set=set, get=get, setinverse = setinverse, getinverse = getinverse)
        
}


## Computes the inverse of the special matrix created above. 
## Retrieve the inverse from the cache if the inverse has already been calculated

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        
        ## checking id inverse has already been calculated
        if(!is.null(m)) {
              message("getting cached data")
              return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}
