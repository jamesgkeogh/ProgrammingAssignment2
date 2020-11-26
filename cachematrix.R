#Below are two functions that are used to create a special object that stores a matrix and cache's its inverse.

#makeCacheMatrix function creates a special "matrix", which is really a list containing a function to
#1. set the value of the matrix
#2. get the value of the matrix
#3. set the value of the inverse of the matrix
#4. get the value of the inverse of the matrix


makeCacheMatrix <- function(x = matrix()) {
        inverseMat <- NULL
        set <- function(y) {
                x <<- y              #assigns the value on the right side of the operator to an object in the parent environment named by the object on the left side of the operator.
                inverseMat <<- NULL  #clears any value already cached
        }
        get <- function() x          #lexical scoping used as x is retrieved from parent environment of makeCacheMatrix
        setinverse <- function(inverse) inverseMat <<- inverse  # assigns value of arguement to inverseMat in the parent environment
        getinverse <- function() inverseMat
        list(set = set, get = get,    #each element in the list is named allowing us to access the functions by name in cachesolve function later
             setinverse = setinverse,
             getinverse = getinverse)
}


#The following function calculates the inverse of the special "matrix" returned by the makeCacheMatrix funcion. 
#However, it first checks to see if the inverse has already been calculated. 
#If so, it gets the inverse from the cache and skips the computation. 
#Otherwise, it calculates the inverse of the matrix and sets the value of the inverse in the cache via the setinverse function.


cacheSolve <- function(x, ...) {    #function requires input arguement of type makeCacheMatrix()
        inverseMat <- x$getinverse()
        if(!is.null(inverseMat)) {
                message("getting cached data")
                return(inverseMat)
        }
        data <- x$get()
        inverseMat <- solve(data) #Computing the inverse of a square matrix can be done with the solve function in R
        x$setinverse(inverseMat)
        inverseMat
}
