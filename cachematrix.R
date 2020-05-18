## Put comments here that give an overall description of what your
## functions do
## This function creates an "inverted matrix" and saves it into a global enviroment.  It can be used later like a constant.

## Write a short comment describing this function
##MakeCachematrix: ##Creates a inverse matrix” and put it in the
##global environment.

makeCacheMatrix <- function(x = matrix()) { ##creates Function for matrix                 
  
  i <- NULL          ## erase any“inverse matrix data” from the memory
  set <- function(y) {
    x <<- y         ## operator that assigns a value to X
    i <<- NULL      ## erasing any inverse value from memory for other data
  }
  get <- function()x ##get the inverted matriz
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get,   ##list values calculated by MakeCache
       setinverse = setinverse,
       getinverse = getinverse)
  
  
  
}



## Write a short comment describing this function
##cacheSolve: Retrieves the inverse of the matrix. This matrix is the one created by MakeCacheMatrix

cacheSolve <- function(x, ...) {  ##function to get the specific cache-matrix
  i <- x$getinverse ()            ## get the inverse matrix calculated above 
 
  if(!is.null(i)) {
    message ("getting cached data")
    return(i)                    ##return the inverse of matrix
  }
  data <- x$get()                ## if there is no inverse-matrix, then it calculates the inverse
                                  ##calculates the inverse
  
  i <- solve(data, ...)          ##calculate the inverse & retrieve
  x$setinverse(i)
  i                              ##inverse of the matrix
}
