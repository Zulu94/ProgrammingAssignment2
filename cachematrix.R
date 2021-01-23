##makeCacheMatrix function: this function creates a special "matrix" object that can cache its inverse.##
makeCacheMatrix <- function(mtx = matrix()) {
  #Inverse init
  invmtx <- NULL
  
  #Matrix setup
  set <- function(matr) {
    mtx <<- matr
    invmtx <<- NULL
  }
  
  #Matrix get
  get <- function() {
    mtx
  }
  
  #Set for the inverse
  set.inverse <- function(setinvmtx) {
    invmtx <<- setinvmtx
  }
  
  #Get for the inverse
  get.inverse <- function() {
    invmtx
  }
  
  #Returning sets and gets for both the basic matrix and its inverse
  list(set = set, get = get, set.inverse = set.inverse, get.inverse = get.inverse)
}

##cacheSolve function: this function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated 
#(and the matrix has not changed),then cacheSolve should retrieve the inverse from the cache.##
cacheSolve <- function(cached.mtx, ...) { 
  #Stored inverse mtx get
  invmtx <- cached.mtx$get.inverse()
  
  #Returning the inverse provided that it exists
  if(!is.null(invmtx)) {
    message("Getting the cached inverse matrix")
    return(invmtx)
  }
  
  #Else calculate, store, and return the inverse
  raw.mtx <- cached.mtx$get()
  invmtx <- solve(raw.mtx, ...)
  cached.mtx$set.inverse(invmtx)
  
  #Returning the inverse
  invmtx
}
