## Functions makeCacheMatrix to store matrix and make variable for caching, cacheSolve to solve the inverse and cache


##creates list of functions with a variable m within the function to store inverse, get() to return original matrix, set()
##to change original matrix and remove m variable, setinverse to save the inverse, getinverse to display m

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) m <<- solve
        getinverse <- function() m
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## tests if getinverse is NULL, if so calculates inverse of x saves it to variable m and displays it,
## otherwise displays m from cache

cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}
