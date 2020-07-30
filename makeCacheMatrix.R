makeCacheMatrix <- function(s = matrix()){
     inv <- NULL
     set <- function(m) {
             s <<- m  
             inv <<- NULL
     }
     get <- function() (s)
     setInverse <- function(inverse) {inv <<- inverse}
     getInverse <- function() {inv}
     list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

cacheSolve <- function(s, ...){
  inv <-s$getInverse()
  if(!is.null(inv)){
         message("getting cached data")
         return(inv)
  }
  mat <- s$get()
  inv <- solve(mat, ...)
  s$setInverse(inv)
  inv
  
} 

