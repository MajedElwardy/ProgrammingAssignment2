## Matrix inversion is usually a costly computation and there may be some benefit
## to caching the inverse of a matrix rather than compute it repeatedly

# This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
        # Stores the inverse cache
        cached_inverse <- NULL
        
        # Sets a new matrix and nullifies the old cached matrix
        set <- function(new_matrix) {
                x <<- new_matrix
                cached_inverse <<- NULL
        }
        
        # Returns the matrix
        get <- function() {
                x
        }
        # Sets the cached inverse
        set_inverse <- function(inverse) {
                cached_inverse <<- inverse
        }
        
        # Gets the cached inverse
        get_inverse <- function() {
                cached_inverse
        }
        
        # Returns the defined matrix
        # Returns the functions
        list(set = set, get = get,
             set_inverse = set_inverse,
             get_inverse = get_inverse)
}

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inverse <- x$get_inverse()
        
        # If cached_inverse exists, return it
        if(!is.null(inverse)) {
                message("getting cached data")
                return(inverse)
        }
        
        # Gets the inverse and calculates it with solve()
        data <- x$get()
        inverse <- solve(data, ...)
        
        # Cache the matrix again
        x$set_inverse(inverse)
        
        # Returns the calculated matrix
        inverse
}
