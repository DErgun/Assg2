# Assignment: Programming Assignment 2: Lexical Scoping

makeCacheMatrix <- function(x = matrix()) {
  
  xinv <- NULL # where the outcome of inversion is stored
 
  set <- function(y) {
    x <<- y
    xinv <<- NULL # it also start up xinv to null
  }
  
  get <- function() x # go back the input matrix
  setInv <- function(inv) xinv <<- inv # the inversed matrix is set
  getInv <- function() xinv # return the inversed matrix

  list(set = set, get = get,
       setInv = setInv,
       getInv = getInv)
}


cacheSolve <- function(x, ...) {
  k <- x$getInv() # the inversed matrix is gotten from object x
  
  if(!is.null(k)) { # if the inversion result is there
    message("getting cached data")
    return(k) # go back the calculated inversion
  }
  data <- x$get() # if not, x$get is done to get the matrix object
  k <- solve(data) # data is solved
  x$setInv(k) # set it to the object
  k # return the solved result
}



# For testing it, random square and non-singular matrices are created

test <- matrix(runif(9,5,100),3,3)
test
  #        [,1]     [,2]     [,3]
  # [1,] 13.91770 88.06261 39.92235
  # [2,] 41.55447 52.53828 10.04545
  # [3,] 73.69376 75.77440 32.93620

test <- matrix(runif(9,1,100),3,3)
test
  #         [,1]      [,2]     [,3]
  # [1,] 84.35678  2.177663 69.48629
  # [2,] 25.30606  2.276995 23.78439
  # [3,] 57.66557 56.440316 92.11196


# the makeCacheMatrix object has been comprised of this matrix

testCached <- makeCacheMatrix(test)


# To calculate cacheSolve function

testInv <- cacheSolve(testCached)