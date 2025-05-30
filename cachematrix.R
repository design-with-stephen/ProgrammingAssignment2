makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL  # Cached inverse
    
    set <- function(y) {
        x <<- y

        # Reset the cache when the matrix is changed
        inv <<- NULL  
    }
    
    get <- function() x
    
    setInverse <- function(inverse) inv <<- inverse
    
    getInverse <- function() inv
    
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


cacheSolve <- function(x, ...) {
    inv <- x$getInverse()
    
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    
    data <- x$get()

    # Compute the inverse
    inv <- solve(data, ...) 

     # Cache the result 
    x$setInverse(inv)
    inv
}


# Create a sample invertible matrix
inverseMatrix <- matrix(c(2, 1, 1, 2), 2, 2)

# Create a cache-aware matrix object
cacheMatrix <- makeCacheMatrix(inverseMatrix)

# Compute and cache the inverse
inverse1 <- cacheSolve(cacheMatrix)

# Retrieve cached inverse 
inverse2 <- cacheSolve(cacheMatrix)

inverse1
inverse2