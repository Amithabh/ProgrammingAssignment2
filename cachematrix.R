## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
makeCacheMatrix <- function(x = matrix()) {
    matrix_inv <- NULL
    set <- function(y) {
        x <<- y
        matrix_inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) matrix_inv <<- inverse
    getinverse <- function() matrix_inv
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}
## The makeCacheMatrix creates a special matrix which itself is a function to:
## get matrix 
## set matrix 
## get matrix inverse
## set matrix inverse



## Write a short comment describing this function

cacheSolve <- function(x, ...) {
    matrix_inv <- x$getinverse()
    if(!is.null(matrix_inv)) {
        message("getting cached data")
        return(matrix_inv)
    }
    data <- x$get()
    matrix_inv <- solve(data, ...)
    x$setinverse(matrix_inv)
    matrix_inv
}
## The cacheSolve is the actual function which computes the inverse of the matrix 
## created by the makeCacheMatrix function and stores in cache which can be retrieved later on.

# Usage:
#     > mat <- matrix(rnorm(9), nrow = 3, ncol = 3)
#     > cache_matrix <- makeCacheMatrix(mat)
#     > cacheSolve(cache_matrix)
#     [,1]       [,2]       [,3]
#     [1,] -0.7871344 -0.1143764  1.9300932
#     [2,]  0.2068301 -0.8926886 -1.2797428
#     [3,] -0.7697908  1.1061997  0.3248012
#     > cache_matrix$get()
#     [,1]       [,2]       [,3]
#     [1,] -0.6001182 -1.1580191 -0.9965559
#     [2,] -0.4893662 -0.6557749  0.3241958
#     [3,]  0.2443687 -0.5111265 -0.3872057
#     > cache_matrix$getinverse()
#     [,1]       [,2]       [,3]
#     [1,] -0.7871344 -0.1143764  1.9300932
#     [2,]  0.2068301 -0.8926886 -1.2797428
#     [3,] -0.7697908  1.1061997  0.3248012
    
    