## makeCacheMatrix creates list, that contains 3 texts of functions

makeCacheMatrix <- function(x = matrix()) {
    SM = NULL
    
    getM <- function() x                                            ## get original matrix
    setSM <- function(inverse_matrix) SM <<- inverse_matrix         ## set cache the inverse of a matrix 
    getSM <- function() SM                                          ## get cache the inverse of a matrix

    list(getM = getM,
         setSM = setSM,
         getSM = getSM)
}


## cacheSolve computes inverse of matrix; 
## if inverse has already been computed, then cacheSolve return cache the inverse of a matrix

cacheSolve <- function(x, ...) {
    InvM <- x$getSM()
    if(!is.null(InvM)) {                                        ## if cache is not empty - return cache
        message("getting cached data")
        return(InvM)
    }
    
    InvM <- solve(x$getM())                                     ## Matrix inversion
    x$setSM(InvM)                                               ## set inverse of matrix in "special list"
    InvM
}

##a = matrix(sample(1:16, 16),4)
##A = makeCacheMatrix(a)
##cacheSolve(A)
