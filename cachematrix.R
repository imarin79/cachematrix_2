## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

# This function creates an object that stores a matrix.
makeCacheMatrix <- function(x = matrix()) {
  m <- null                # Here we set the value of the matrix as NULL as a placeholder to be used later on.
  set <- function(y) {     # We create a function to set the vector x to a new vector y
    x <<- y                
    m <<- NULL
  }
  get <- function() x       #returns the vector x
  setinverse<- function(inverse) m <<- inverse # sets the inverse to m
  getinverse <- function() m              # returns the inverse m
  list(set = set, 
       get = get,           # it returns a list with all created functions
       setinverse = setinverse,
       getinverse = getinverse)
}

# This function calculates the inverse of the matrix returned by makeCacheMatrix function outlined above.

cacheSolve <- function(x, ...) { # Here the function starts with a single argument x and an ellipsis that allows to pass additional arguments.
  m <- x$getinverse()          # The function tries to access the inverse from the object passed in as the argument.
  if(!is.null(m)) {            # If the result is not NULL, the function will return the value to the parent environment.
    message("getting cached data")
    return(m)
  }
  data <- x$get()               # The function gets the vector from the input object, calculates the inverse of the matrix,
  m <- solve(data, ...)         # and uses the setinverse() function to set the inverse of the matrix. Finally, it returns the value of
  # the matrix inverse by printing it. 
  x$setinverse(m)
  m
}




