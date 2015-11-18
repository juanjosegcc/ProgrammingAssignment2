## Juan-José García-Castro Crespo
## 18th November, 2015

## This script is intended to make matrices and storing its attributes using
## the <<- operator, this allows to store values in a different environment
## than the current environment.

## It constructs a list given a matrix X as parameter, it also contains the
## function definitions needed to access the object's attributes.

makeCacheMatrix <- function(X = matrix()) {
        INV <- NULL
        set <- function(Y) {
                X <<- Y
                INV <<- NULL
	}
	get <- function() X
	setINV <- function(Y) INV <<- Y
	getINV <- function() INV
	list(set = set, get = get,
             setINV = setINV,
	     getINV = getINV)
}


## Tries to solve matrix X, if solution i.e. X^-1 is already calculated, 
## this function just returns that value, otherwise it calculates it using
## solve() and it stores the value for further accesses.

cacheSolve <- function(X, ...) {
        ## Return a matrix that is the inverse of 'x'
        INV <- X$getINV()
        if(!is.null(INV)) {
                message("getting cached data")
                return(INV)
        }
        M <- X$get()
        INV <- solve(M)
        X$setINV(INV)
        INV
}
