## Functions which create, cache, and invert invertable matrices
##Test Matrix
mat <- matrix(c(1/2, -1/4, -1, 3/4), nrow = 2, ncol = 2)


## Function Inputs a matrix and cache's it

makeCacheMatrix <- function(x = matrix(m1)) { ##inputs the matrix
  
  message("Matrix Input:")
  print(x)
  
  invert <- NULL  #Initializes the invert value
  
  set <- function(y) { ## changes invert value if the matrix is different
    
    x <<- y
    invert <<- NULL
  }
  
get <- function() x  
setinvert <- function(inverse) invert <<- inverse
getinvert <- function() invert

list(set = set, get = get, setinverse = setinvert, getinverse = getinvert)


}


## this function applies the solve function to invert the matrix

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  invert = x$getinverse()
  

  if (!is.null(invert)){
    # if the inverse has already been calculated it gets the data from the cache 
    message("getting cached data of inverted matrix")
   
    return(invert)
  }
  
  # If this is the first calculation of a given matrix, it will proceed to invert
  data = x$get()
  invert = solve(data, ...)
 
  x$setinverse(invert)
  
  message("Inverted Matrix Output:")
       invert
}

