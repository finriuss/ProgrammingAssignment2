## R Programming Assignment 2: Lexical Scoping--caching the inverse of a matrix

## The makeCacheMatrix function creates a list that sets and gets the value of the matrix
makeCacheMatrix <- function(x = matrix())
{
    I <- NULL
    set <- function(y)
    {
        x <<- y        
        I <<- NULL
    }
    get <- function() x    
    setInverse <- function(solve) I <<- solve
    getInverse <- function() I 
    list(set = set, get = get,
         setInverse = setInverse, getInverse = getInverse)
}

## The cacheSolve function calculates the inverse of the matrix
cacheSolve <- function(x, ...)
{
    I <- x$getInverse()
    if(!is.null(I))
    {
        message("getting cached data")
        return(I)
    }
    data <- x$get()
    I <- solve(data, ...)
    x$setInverse(I)
    I
}
