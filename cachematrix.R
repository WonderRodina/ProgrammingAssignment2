## Matrix inversion is usually a costly computation and there may be some 
## benefit to caching the inverse of a matrix rather than compute it repeatedly. 
## The  following two functions are used to cache the inverse of a matrix.


## `makeCacheMatrix`: This function creates a special "matrix" object that can 
## cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
        s <- NULL
        set <- function(mtrx) {
                x <<- mtrx
                s <<- NULL
                }
        get <- function() x
        setSolve <- function(solve) s <<- solve
        getSolve <- function() s
        list(set = set, get = get,
             setSolve = setSolve,
             getSolve = getSolve)

}


## `cacheSolve`: This function computes the inverse of the special "matrix" 
## returned by `makeCacheMatrix` above. If the inverse has already been 
## calculated (and the matrix has not changed), then `cacheSolve` should retrieve 
## the inverse from the cache.
cacheSolve <- function(x, ...) {
        s <- x$getSolve()
        if(!is.null(s)) {
                message("getting cached data")
                return(s)
        }
        mtrx <- x$get()
        s <- solve(mtrx)
        x$setSolve(s)
        s
}
