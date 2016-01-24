
makeCacheMatrix <- function(x = matrix()) {
##makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }                                                                                                                                                  
        get <- function() x
        setmatrix <- function(inverse) inv <<- inverse
        getmatrix <- function() inv
        list(set = set, get = get, setmatrix = setmatrix, getmatrix = getmatrix)
}
cacheSolve<- function(x, ...) {
## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.
        inv <- x$getmatrix()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
       mat.data <- x$get()
        inv <- solve(mat.data, ...)
        x$setmatrix(inv)
        inv
}
## Example
m<-matrix(1:4,2,2)
x<-makeCacheMatrix(m)
x$get()
     [,1] [,2]
[1,]    1    3
[2,]    2    4
cacheSolve(x)
     [,1] [,2]
[1,]   -2  1.5
[2,]    1 -0.5
cacheSolve(x)
getting cached data
     [,1] [,2]
[1,]   -2  1.5
[2,]    1 -0.5
