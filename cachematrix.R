## Caching the Inverse of a Matrix

## This function creates a special "matrix", which is really a list containing a function to
## 1- set the value of the matrix
## 2- get the value of the matrix
## 3- set the value of the inverse matrix
## 4- get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
    x <<- y
    i <<- NULL
  }
    get <- function() x
    set_inverse <- function(inverse) i <<- inverse
    get_inverse <- function() i
    list(set = set, get = get,
        set_inverse = set_inverse,
        get_inverse = get_inverse
  )

}


## This function computes the inverse of the special "matrix" returned by the function makeCacheMatrix

cacheSolve <- function(x, ...) {
    i <- x$get_inverse()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    m <- x$get()
    i <- solve(m, ...)
    x$set_inverse(i)
    i   ## Return a matrix that is the inverse of 'x'
}
