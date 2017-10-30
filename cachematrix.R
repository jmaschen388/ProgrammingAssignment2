##The functions below are based on the mean functions from the 
##example in the assignment. The makeCacheMatrix sets and gets 
##the value of the vector and sets and gets the value of the inverse.

makeCacheMatrix <- function(x = matrix){
        m <- NULL #initializes a blank matrix
        set <- function(y){
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) m <<- inverse
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
        
}

#The cacheSolve function checks to see if the inverse has already been
##calculated and if it has, it gets the inverse from the makeCacheMatrix
##and saves time rather than recalculating. Else, it calculates the inverse
##of the matrix using the solve function.

cacheSolve <- function(x, ...) {
        m <-x$getinverse()
        if(!is.null(m)){
                message("getting chached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...) #calculates the inverse of x
        x$setinverse(m)
        m #returns the inverse of the matrix x
}