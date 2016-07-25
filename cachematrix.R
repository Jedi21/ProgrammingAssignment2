##----------------------------------------##
##
#  input   : a square invertible matrix
#  returns : a list containing functions to
#			1. set the matrix
#			2. get the matrix
#			3. set the inverse of the matrix
#			4. get the inverse of the matrix
#  comment : The output list is used as input to cacheSolve()
##
##----------------------------------------##

makeCacheMatrix <- function(mat = matrix()) {

        inv <- NULL
        set <- function(mat.new) {
                mat <<- mat.new
                inv <<- NULL
        }
        get <- function() mat
        setinv <- function(mat.inv) inv <<- mat.inv
        getinv <- function() inv
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


##----------------------------------------##
##
#  input   : output of makeCacheMatrix()
#  returns : inverse of original matrix
##
##----------------------------------------##

cacheSolve <- function(mat, ...) {
        inv <- mat$getinv()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        mat.data <- mat$get()
        inv <- solve(mat.data, ...)
        mat$setinv(inv)
        return(inv)
}
