## This function creates a list of function that works with the environment.
##
## m holds the inverse of the matrix 
## x holds the original matrix
##
makeCacheMatrix <- function(x = matrix()) 
{
    m <- NULL
    set <- function(y) 
    {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    set_inverse <- function(inv) m <<- inv
    get_inverse <- function() m
    list(set = set, get = get,
         set_inverse = set_inverse,
         get_inverse = get_inverse)
}


## Computes the inverse of a matrix and stores it in
## a cache so that it can be returned without being recomputed
##
## Requires use of makeCacheMatrix()
##
## Sample usage:
##
##   t2<-makeCacheMatrix(matrix(5:8,nrow=2,ncol=2))
##   cacheSolve(t2)
##
cacheSolve <- function(x, ...) 
{
    m <- x$get_inverse()
    if (!is.null(m)) 
    {
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$set_inverse(m)
    m
}
