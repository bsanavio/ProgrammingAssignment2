## This pair of functions (makeCacheMatrix, and cacheSolve) 
## calculate and cache the inverse of a (square, invertible) matrix 
## in order to speed up computation

##makeCacheMatrix: This function creates a special "matrix" object 
##that can cache the inverse of the input matrix.
makeCacheMatrix <- function(x = matrix()) {
  ## given x as a square invertible matrix
  ## the function returns a list used as the input to cacheSolve()
  ## the list contains functions to
  ##              1. set the matrix
  ##              2. get the matrix
  ##              3. set the inverse
  ##              4. get the inverse
  
  ##Initialize the inverse matrix InvMat as null
  InvMat <- NULL
  ##declare function set; `<<-` assign a value to an object in an environment 
  ## different from the current environment. 
  set <- function(y) {
    x <<- y
    ## change the value of inverse of the matrix in case the matrix was changed.
    InvMat <<- NULL
  }
  ## gets the value of the inverse
  get<-function() x
  #calculates the inverse of non-singular matrix via the solve function
  setInv <- function(solve) InvMat <<- solve
  getInv <- function() InvMat
  list(set=set, get=get,
       setInv=setInv,
       getInv=getInv)
}
##end of makeCacheMatrix


##cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix function.
##If the inverse has already been calculated (and the matrix has not changed), 
##then the cacheSolve retrieve the inverse from the cache.

cacheSolve <- function(x=matrix(), ...) {
  ## return a matrix that is the inverse of x 
  
  InvMat <- x$getInv()
  ## check if the inverse has already been calculated
  if(!is.null(InvMat)){
    # then it gets it from the cache and skips the computation. 
    message("getting cached data")
    return(InvMat)
  }
  
  ## otherwise, it calculates the inverse of the matrix via the solve function
  matrix <- x$get()
  InvMat <- solve(matrix, ...)
  ## sets the value of the inverse in the cache via the setInv function.
  x$setInv(InvMat)
  return(InvMat)
}