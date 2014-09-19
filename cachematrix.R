# Matrix inversion is usually a costly computation and there is some benefit to 
# caching the inverse of a matrix rather than compute it repeatedly. 
# The following pair of functions provide a means to create a special "matrix" object and to
# compute the inverse or access the cached inverse of the special "matrix" object.

# Computing the inverse of a square matrix is done with the solve function in R. 
# The special "matrix" object is assumed for a matrix that is always invertible, and therefore
# has an inverse determinable with the solve function in R.

#
# makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
#
makeCacheMatrix <- function(m = matrix()) {
        inverse <- NULL
        set <- function(y) { # function to 
            m <<- y          #   set associated matrix, m, to the specified matrix y, and
            inverse <<- NULL #   clear any cached calculated inverse by setting cache to NULL
        }
        get <- function() m  # function to get the associated matrix of the "matrix" object
        setInverse <- function(inv) inverse <<- inv # set cached inverse
        getInverse <- function() inverse            # get cached inverse
        
        # returns a list of 4 functions
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse) 
}

#
# cacheSolve: This function computes the inverse of the special "matrix" returned by 
# makeCacheMatrix above. If the inverse has already been calculated (and the matrix has 
# not changed), then cachesolve retrieves the inverse from the cache. If it has not been
# calculated, it would be calculated with the solve function in R and the calculated 
# inverse cached in the "matrix" object.
#
# The special "matrix" is assumed for an invertible square matrix with its inverse
# determinable with solve function in R.
# 
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getInverse() # get the cached inverse
        
        # if there is a cached calculated inverse, return it.
        if(!is.null(inv)) { 
            message("getting cached data")
            return(inv)
        }
        
        # else use solve function to find the inverse
        m <- x$get()
        inv <- solve(m, ...)
        # then cache the calculated inverse
        x$setInverse(inv)
        # lastly return the calculated inverse
        inv   
}
