## These functions parallel the example used for means, 
## but using matrices and the solve() function.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(a) {
                x <<- a
                m <<- NULL
        }
        get <- function() x
        setinv <- function(solve) inv <- solve
        getinv <- function() inv
        list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## If there is no value of the inverse already cached, 
## then the function computes and returns that inverse.

cacheSolve <- function(x, ...) {
        i <- x$getinv()
        if(!is.null(i)) {
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinv(i)
        i
}
