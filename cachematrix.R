makeCacheMatrix<-function(x = matrix())
  {
    m <- NULL
    set <- function(y) 
      {
        x <<- y
        m <<- NULL
      }
    get <-function()x
    
    set_inverse<-function(solve) m %*%<<-solve
    get_inverse<-function() m
    
    list(set = set, get = get,
         setinverse = set_inverse,
         getinverse = get_inverse)
  }

cacheSolve <- function(x=matrix(), ...) {
  m <- x$get_inverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...) %*%data
  x$set_inverse(m)
  m
}