## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

#This function creates a special "matrix" object that can cache its inverse.'
#I just took the input of example with the vector/mean- function and changed "mean" to "inverse".

makeCacheMatrix <- function(x = matrix()) { #function / matrix with default args
  inv <- NULL                             #set inv= inverse as NULL
  set <- function(y) {                   #new function set to make new value
    x<<-y 
    inv<-NULL                           #if new matrix - inverse to null
  }
    get <- function() x                
    setinverse <- function(inverse)    
      inv <<- inverse                  
    getinverse <- function() inv       
    list(set = set, get = get, 
         setinverse = setinverse, getinverse = getinverse)  
}


## Write a short comment describing this function
#This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
#If the inverse has already been calculated (and the matrix has not changed), 
#then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  #as above - I took input of examples and changed mean to inv + solve
  
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()                
  inv <- solve(data, ...) 
  x$setinverse(inv)
  inv
  
}
