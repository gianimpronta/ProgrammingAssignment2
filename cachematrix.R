## This function defines the 'setters' and 'getters'. It will contain a list with behavior (the functions)
## and state (the variables in the enviroment). The trick is that this list retains the original 
## enviroment of the makeCacheMatrix()   

makeCacheMatrix <- function(x = matrix()) {
      s <- NULL # define s 
      set <- function(y){           # define set() function
            x <<- y
            s <<- NULL
      }
      get <- function() x           # define get() function
      setsolve <- function(solve) {
            s <<- solve
            }                       # define setsolve() function
      getsolve <- function() s      # define getsolve() function
      list(set = set,               # gives the name 'set' to the set() function defined above
           get = get,               # gives the name 'get' to the get() function defined above                 
           setsolve = setsolve,     # gives the name 'setsolve' to the setsolve() function defined above
           getsolve = getsolve)     # gives the name 'getsolve' to the getsolve() function defined above
}


## Return a matrix that is the inverse of 'x'
cacheSolve <- function(x, ...) {
      # retrieves 's' from 'x' defined in the makeCacheMatrix function
      s <- x$getsolve()                    
      # if 's' is not NULL, that means the 'solve' have already been calculated then the function 
      # prints the message 'getting cached data', returns 's' and ends the function.
            if (!is.null(s)){                    
            message("getting cached data") 
            return(s)                      
            }
      # retrieves the value from 'x' defined in the makeCacheMatrix function
      data <- x$get()
      # solve the matrix and passes the value to 's'
      s <- solve(data, ...)
      # passes the matrix inverse to 'setsolve' in 'x'
      x$setsolve(s)
      # print 's'
      s
}
