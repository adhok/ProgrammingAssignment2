## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## To create a special matrix
## With list containing function for
## set/get(ting) value of the matrix
## set/get(ting) value of the required inverse

makeCacheMatrix <- function(x = matrix()) {
  
  m<-NULL
  set<-function(y){
    x <<-y
    m <<-NULL
  }
  get<- function() x
  setinverse <- function(inv) m <<- inv
  getinverse <- function() m
  list(
    set = set,
    get = get,
    setinverse = setinverse,
    getinverse= getinverse
  )
  

}


## Write a short comment describing this function
## Calculates the inverse of the special "matrix"
## Checks if inverse has already been calculated
## If that's the case then, use inverse from cache
## Else, calculate inverse from data and change cache
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m<-x$getinverse()
  if(!is.null(m))
  {
    message("getting cached data")
    return(m)
  }
  data<-x$get()
  m<-solve(data,...)
  x$setinverse(m)
  m
}
