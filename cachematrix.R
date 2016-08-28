## This is a R function that stores the inverse of a matrix in cache to avoid 
## being calculated repeatedly when the inverse is needed in other functions. 
## This is especially useful when we need the inverse of a large matirx. 

## makeCacheMatrix creates a list that has a function to 1. set the value of a
## matrix, 2. get the value of the matrix, 3. set the inverse of the matrix and
## 4. get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinv <- function(inv) i <<- inv
        getinv <- function() i
        list(set=set, get=get, setinv=setinv, getinv=getinv)
}



## cacheSolve returns the inverse of the matrix. If the inverse is already 
## calculated, it will retrive the result from the cache and display the result.
## If there is nothing in the cache, it will compute the inverse using the solve
## function in R.

cacheSolve <- function(x, ...) {
        i <- x$getinv()
        if(!is.null(i)) {
                message("Getting cached data...")
                return(i)
        }
        data <- x$get()
        i <- solve(data)
        x$setinv(i)
        i
}
## Sample run;
## > x <- rbind(c(1,2,3), c(0,1,4), c(5,6,0))
## > m = makeCacheMatrix(x)
## > m$get(x)
##      [,1] [,2] [,3]
##[1,]    1    2    3
##[2,]    0    1    4
##[3,]    5    6    0
##> cacheSolve(m)
##      [,1] [,2] [,3]
##[1,]  -24   18    5
##[2,]   20  -15   -4
##[3,]   -5    4    1
##> cacheSolve(m)
##Getting cached data...
##      [,1] [,2] [,3]
##[1,]  -24   18    5
##[2,]   20  -15   -4
##[3,]   -5    4    1