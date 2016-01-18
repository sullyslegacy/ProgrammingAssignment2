##The logic in the functions below is very similar to the logic
#from the examples provided
#mat = matrix(c(1, 0, 1, 2, 4, 0, 3, 5, 6), nrow = 3, ncol = 3)
#temp = makeCacheMatrix(mat)
#cacheSolve(temp)

## This function contains basic getters and setters for matrix values
#As well as getting and setting the inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) m <<- inverse
    getInverse <- function() m
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


##Check if the created cached matrix has inverse computed already
#if it does return the cached matrix
#else compute inverse and cache it
cacheSolve <- function(x, ...) {
    m <- x$getInverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setInverse(m)
    m
}
