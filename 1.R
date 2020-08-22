#MakeCacheMatrix creates a matrix object and stores its inverse
#cacheSolve is a function that accepts a MakeCacheMatrix return value and then checks for a solution, solves and stores the soution inside of the object
makeCacheMatrix<-function(x = matrix()){ 
  i<-NULL                     ## create a variable
  set<-function(y){           
  x<<-y                       # store a matrix from outside
  i<<-NULL                    
}
  get<-function()x
  setinverse<-function(inverse)i<<- inverse
  getinverse<- function()i
  list(
    set = set,
    get = get,
    setinverse = setinverse,
    getinverse = getinverse
  )
}



cachemean <- function(x, ...) {
  m <- x$inverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
