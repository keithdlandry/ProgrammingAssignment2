

## This funtion creates a structure of functions that allows you to store
## and return a matrix and its inverse.

makeCacheMatrix <- function(x = matrix()) {
  
  mInverse <- NULL
  
  inverse  <- function(){mInverse}
  get      <- function(){x}
  set      <- function(mtx){
    x <<- mtx
    mInverse <<- NULL
  }
  setInverse <- function(invrs) {mInverse <<- invrs}
  
  list(set = set,
       get = get,
       inverse = inverse,
       setInverse = setInverse)

}


## This function if the structure above has an inverse matrix
## stored. If it does, it returns that matrix. If not it calculates
## the inverse, stores it in the structure and returns the inverse.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  mInverse = x$inverse()
  
  if (is.null(mInverse)){
    message("solving...")
    mInverse = solve(x$get())
    x$setInverse(mInverse)
    mInverse
  }
  else{
    message("geting inverse from cache")
    return(x$inverse())
  }
}

## I was bored at work so I made a thrid function to improve on the same idea.
## This function creates a structure of functions simillar to the makeCacheMatrix 
## but with the functionality of cacheSolve built in. 

makeAutoCacheMatrix <- function(x = matrix()) {
  
  mInverse <- NULL
  
  get <- function(){x}
  set <- function(mtx){
    x <<- mtx
    mInverse <<- NULL
  }
  
  #the inverse function now checks if NULL and calculates it automatically
  #so cacheSolve function is no longer nessesarry. The return value of 
  #x$inverse() can not be NULL now. You also don't
  #need the setInverse() function becasue it is all set internally.
  #This avoids the problem of a user manually calling x$setInverse() 
  #and setting it to a matrix that is not the inverse of the matrix x.
  inverse  <- function(){
    if (is.null(mInverse)) {
      message("calculating inverse")
      mInverse <<- solve(x)
      return(mInverse)
    }
    else{
      message("getting inverse from cache")
      mInverse
    }
  }
  
  list(set = set,
       get = get,
       inverse = inverse)
  
}
