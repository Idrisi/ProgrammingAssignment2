## This function will calculate the inverse of a matrix,
## then store the inver matrix in a cache so it may be 
## retrieved again with needing to recalculate it.

## This function creates a special "matrix" object, which is really a list 
## containing a function to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse
## 4. get the value of the inverse

makeCacheMatrix <- function(x = matrix()) { # create a matrix object (x)
  m <- NULL  ## define the cache (m)
  set <- function(y) {
    x <<- y ## assign matrix y to x
    m <<- NULL ## set matrix to null
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse ## Set the cache (m) to the inverse of the matrix
  getinverse <- function() m ## Get the cached inverse of the matrix
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## This function will calculate the inverse of the matrix, but will check to see if 
## if it is already stored in cache. If the inverse matrix is in cache, it will get 
## the inverse matrix from the cache, saving computational time.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  m <- x$getinverse()
  if(!is.null(m)) { ## Check if inverse matrix (m) exists
    message("getting cached data")
    return(m) ## ...grab inverse matrix (m) from cache
  }
  data <- x$get() ## put matrix into data
  m <- solve(data, ...) ## create inverse of matrix (m)
  x$setinverse(m) ## assign inverse matrix to (m)
  m ## display the inverse matrix
  
}
