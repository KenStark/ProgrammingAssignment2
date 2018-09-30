## Caching the Inverse of a Matrix
## As matrix inversion can be computationally intensive, it could be helpful to cache the 
## inverse of the matrix rather than computing it repeatedly. Those 2 functions below and 
##"<<-" operators are used to create a special object that stores a matrix and caches its
##inverse.

## This function returns a list containing 4 functions, which set the matrix, get the matrix,
## set the inverted matrix and get the inverted matrix, respectively. it uses "<<-" to cache
## the inverted matrix.

makeCacheMatrix <- function(x = matrix()) {
    invert<-NULL
    set<-function(y){
        x<<-y
        invert<<-NULL
    }
    get<-function() x
    setinvert<-function(inverted_m)  invert<<-inverted_m
    getinvert<-function() invert
    list(set = set, get = get,
         setinvert = setinvert,
         getinvert = getinvert)
}


## The second function gets the matrix and checks whether the inverted matrix already exists.
## If yes, the cached inverted matrix is returned, companied by the message "getting cached 
## data". If no, matrix inversion calculation is performed.

cacheSolve <- function(x, ...) {
    invert<-x$getinvert()
    ## Return a matrix that is the inverse of 'x'
    if (!is.null(invert)){
        message("getting cached data")
        return(invert)
    }
    data<-x$get()
    invert<-solve(data,...)
    x$setinvert(invert)
    invert
}

## For testing purposes:
firstmatrix<-makeCacheMatrix(matrix(c(1,3,1,4),nrow=2))
firstmatrix$get()
cacheSolve(firstmatrix)
cacheSolve(firstmatrix)
firstmatrix$getinvert()

firstmatrix$set(matrix(c(5,2,3,3),nrow=2))
firstmatrix$get()
cacheSolve(firstmatrix)
cacheSolve(firstmatrix)
firstmatrix$getinvert()
