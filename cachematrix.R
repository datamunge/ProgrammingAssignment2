## The makeCacheMatrix function creates a list containing the functions
## 1. set the value of the vector
## 2. get the value of the vector
## 3. set the inverse of the matrix
## 4. get the inverse of the matrix

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

## The function cacheSolve accepts the argument of a matrix and checks to
## see if if it has already been inversed. If so, it skips the rest of the
## steps and returns the inverse of the matrix i.e. m. 
## Else, it calls solve to inverse the matrix and places it in the cache via the
## setinverse function

cacheSolve <- function(x=matrix(), ...) 
{
  
	m <- x$get_inverse()
  	if(!is.null(m)) 
	{
    	    message("getting cached data")
    	    return(m)
  	}

  	data <- x$get()
  	m <- solve(data, ...) %*%data
  	x$set_inverse(m)
  	m
}
