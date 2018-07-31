## These functions cache the inverse of a given matrix, and uses it when needed.

##The first function creates a special matrix which contains a function to 1) set the matrix,
#get the matrix, 3) set the inverse of the matrix, and 4) get the matrix inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinversem <- function(solve) m <<- solve
  getinversem <- function() m
  list(set = set, get = get,
       setinversem = setinversem,
       getinversem = getinversem)
}

#The following function calculates the inverse of the special matrix created with the above function. 
#However, it first checks to see if the inverse has already been calculated. 
#If so, it gets the mean from the cache and skips the computation. 
#Otherwise, it calculates the inverse of the matrix and sets the inverse in the cache.

cacheSolve <- function(x, ...) {
  m <- x$getinversem()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinversem(m)
  m
}