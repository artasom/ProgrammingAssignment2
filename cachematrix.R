## Put comments here that give an overall description of what your
## functions do

## The makeCacheMatrix takes as parameter a matrix and its ouput is a list of functions
## to work with the matrix. The idea behind the function is to be able to cache the
## inverse of a matrix so it does not need to be calculated once and again 
## if ithas been alredady calculated.

makeCacheMatrix <- function(x = matrix()) {
        # The inverse of the matrix is initially NULL
        mI <- NULL
        # The set function, that sets the value
        # of the matrix x and assigns NULL to the matrix inversion
        set <- function(y) {
                x <<- y
                mI <<- NULL
        }
        # The get functions returns the value of the matrix x
        get <- function() x
        # The setMatrixInversion stores the value of the Matrix Inversion
        setMatrixInversion <- function(matrixInversion) mI <<- matrixInversion
        # And the getMatrixInversion returns the value of the Matrix Inversion
        getMatrixInversion <- function() mI
        # The output of the function is a list with all the above functions
        list(set = set, get = get,
             setMatrixInversion = setMatrixInversion,
             getMatrixInversion = getMatrixInversion)
}


## The cacheSolve function will calculate the Matrix Inversion but not of a "common" matrix
## but rather of the "Matrix" returned by the above function, makeCacheMatrix
## Should there be cached information, this is, the information was already calculated,
## this is the data returned. 
## Should there be no cached information, the Matrix Inversion had not been calculated,
## the information is processed and stored in the cache through the makeCacheMatrix function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        # First of all, the possible cached data of the Matrix Inversion is taken
        mI <- x$getMatrixInversion()
        # Should it be not null, this is, there is cached data
        if(!is.null(mI)) {
                # A message is displayed
                message("getting cached data of the Matrix Inversion")
                # And the cached data is returned, so the function ends
                return(mI)
        }
        # If this point of the funtion has been reached,
        # there is no cached datam so we get the value of the Matrix 
        data <- x$get()
        # The Matrix Inversion is calculated
        mI <- solve(data, ...)
        # The result is stored in the cache
        x$setMatrixInversion(mI)
        # And the Matrix Inversion is returned
        mI
}

# In order to test the functions, the following lines can be uncommented
# A invertible matrix called A is created
# A <- matrix(seq(1:4),nrow=2,ncol=2)
# The special caché matrix is created
# cacheMatrix <- makeCacheMatrix(A)
# Solving it, it is calculated for the first time and the result is displayed
# cacheSolve(cacheMatrix)
# Solving it for a second time along with the result the message 
# 'getting cached data of the Matrix Inversion' is displayed
# cacheSolve(cacheMatrix)