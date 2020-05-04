## A pair of functions that cache the inverse of a matrix: makeCacheMatrix and cacheSolve

## makeCacheMatrix: this function creates list that can store the inverse of a matrix object
makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        
        ##function set: sets value of x and clears value of m in parent env
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        
        ##function get: return input matrix
        get <- function() x
        
        ##function setinverse: assign 'inverse' to value i in parent environment
        setinverse <- function(inverse) i <<- inverse      #assign input argument 'inverse' to value i in parent environment
        
        ##function getinverse: returns inverse matrix (NULL if not yet calculated)
        getinverse <- function () i
        
        #output named list to enable use of $ operator in cacheSolve function
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## cacheSovle: this function looks through the list created by makeCacheMatrix to determine whether inverse
## matrix has been previously calculated. If so, then cacheSolve will retrieve the inverse from the cache.
## If not, cacheSolve will compute the inverse of the matrix, cache this result, and return the inverse. 
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        #if inverse matrix was previously calculated, retrieve from cache
        i <- x$getinverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        
        #if inverse matrix has not been calculated before, solve it, store it, and return it
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
}