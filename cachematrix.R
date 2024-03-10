### Coursera Data Specialisation
### R programming
### Assignment 2
### Sandjinn

## This code creates two functions in order to save the a solved matrix in the cache
## And to retrieve it from the cache.

## makeCacheMatrix
## This function creates a special "matrix" object that can cache its inverse.
## The matrix is contained inside a list as one of the object

makeCacheMatrix <- function(m_og = matrix()) {
    m_sol <- NULL
    set <- function(y) {
        m_og <<- y
        m_sol <<- NULL
    }
    get <- function() m_og
    setsolve <- function(solve) m_sol <<- solve
    getsolve <- function() m_sol
    list(set = set, get = get,
         setsolve = setsolve,
         getsolve = getsolve)
}


## cacheSolve
## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
    m_sol <- x$getsolve()
    if(!is.null(m_sol)) {
        message("getting cached data")
        return(m_sol)
    }
    data <- x$get()
    m_sol <- solve(data, ...)
    x$setsolve(m_sol)
    m_sol
}

## Testing the functions

# create a matrix
mat <- matrix(rnorm(1:9), 3, 3)

# create the cachelist using the above matrix
mat_cache <- makeCacheMatrix(mat)

# retrieve the solve value from the cache, or calculate if not existing
cacheSolve(mat_cache)

# double check result by calculating the solve seperatly
mat_inv <- solve(mat)
mat_inv
