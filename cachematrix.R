## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## function creates a matrix cache object
## before computing the inverse of the matrix we assign an empty matrix
makeCacheMatrix <- function(x = matrix()) {
  inverse_matrix <- matrix()
  set <- function(y) {
    x <<- y
    inverse_matrix <<- matrix()
  }
  
  get <- function(){x}
  set_inverse <- function(inverted_matrix){inverse_matrix <<- inverted_matrix}
  get_inverse <- function(){inverse_matrix}
  list(set=set,get=get,
       set_inverse=set_inverse,
       get_inverse=get_inverse)
  
}



## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$get_inverse()
  # if inverse is not null and thus has been computed
  # return cached inverse
  if(!identical(m,matrix())) {
    message("getting cached data")
    return(m)
  }
  # else compute and cache inverse for future use
  data <- x$get()
  m <- solve(data)
  x$set_inverse(m)
  # return inverse
  m
}
