# makeCacheMatrix
# Creates a special "matrix" object that can cache its inverse.
# Saves the matrix to variable x and its inverse to variable s in scope.
makeCacheMatrix <- function (y = matrix()){
        s <- NULL
        # set: sets matrix and resets cached inverse
        set <- function(y){
                x <<- y
                s <<- NULL
        }
        # get: returns matrix
        get <- function(){
                x
        }
        # setInverse: saves solve value
        setInverse <- function (solve){
                s <<- solve
        }
        # getInverse: returns cached inverse value
        getInverse <- function (){
                s
        }
        # Returned list
        list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

# Function to get the inversed matrix from a special object created by makeCacheMatrix.
cacheInverse <-  function (x,...) {
        s <- x$getInverse ()# Takes the object of that type as an argument 'x'
        
        
        
        if(!is.null(s)) {
                # checks if the inverse value is already cached; if it is returns the cached value
                message("getting cached data")
                return(s)
        }
        # If not, this function calculates the inverse for the matrix saved in the 'x', saves it into 'x' cache using method 'setSolve'
        # and returns the result.
        data <- x$get()
        s <- solve(data, ...)
        x$setInverse(s)
        s
}