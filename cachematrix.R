## Functions to quickly access inversed matrix 
## after inversing it for the first time

## function caches inversed matrix

makeCacheMatrix <- function(x = matrix()) {
   rev <- NULL
   set <- function(y) {
                x <<- y
                rev <<- NULL
        }
   get <- function() x
   setinv <- function(inversed) rev <<- inversed
   getinv <- function() rev
   list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
   
}


## function takes as argument result of function makeCacheMatrix 
## checkes whether inversed matrix is placed in cache 
## and if not inverses it

cacheSolve <- function(x, ...) {

        ## Return a matrix that is the inverse of 'x'

	  i <- x$getinv()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinv(i)
        i
}
