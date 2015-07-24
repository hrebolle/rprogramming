## R Programming Assignment 2
## Hugo Rebolledo

## makeCacheMatrix Creates a list of methods to cache a matrix (setMatrix), cache its inverse (setInverse),
##                 get the cached matrix (getMatrix) and get the cached inverse (getInverse).

makeCacheMatrix <- function(inputMatrix = matrix()) {
  cachedInverse <- NULL                   # Be sure we begin with an empty cached inverse
  setMatrix <- function(someMatrix) {     # this method will set (or change) the input matrix
    inputMatrix <<- someMatrix            # this super assignment changes the input matrix
    cachedInverse <<- NULL                # in consequence, the cached inverse is no longer valid
  }
  getMatrix <- function() inputMatrix       # returns the input matrix
  # next line sets the cached inverse
  setInverse <- function(someInverse) cachedInverse <<- someInverse
  getInverse <- function() cachedInverse    # returns the cached inverse
  list(setMatrix = setMatrix, 
       getMatrix = getMatrix, 
       setInverse = setInverse, 
       getInverse = getInverse)             # returns the list of methods
}

## cacheSolve  Returns the inverse of a previously cached matrix (it won't work with regular matrices)
##             Once the inverse is solved, it will be cached together with the cached matrix, so in 
##             subsequents calls, the inverse will be read from cache, avoiding its computation.

cacheSolve <- function(cachedMatrix, ...) {  # argument here is a list returned by makeCachedMatrix()
  cachedInverse <- cachedMatrix$getInverse() # test if there is already a cached inverse for this cached matrix
  if(!is.null(cachedInverse)) {
    message("getting cached data")
    return(cachedInverse)
  }
  # At this point, there is no cached inverse of the input cached matrix
  localMatrix <- cachedMatrix$getMatrix()   # This is the actual matrix 
  localInverse <- solve(localMatrix, ...)   # regular computation of the inverse
  cachedMatrix$setInverse(localInverse)     # store the inverse of this cached matrix
  localInverse                              # and return it
}

# Test 1 : call makeCacheMatrix with a matrix as argument
# 
# > mx <- matrix(c(1,2,3,4,5,6,1,8,9),3,3)
# > mcached <- makeCacheMatrix(mx)
# > mcached$getInverse()
# NULL
# > cacheSolve(mcached)
#            [,1]       [,2]       [,3]
# [1,] -0.1666667 -1.6666667  1.5000000
# [2,]  0.3333333  0.3333333 -0.3333333
# [3,] -0.1666667  0.3333333 -0.1666667
# > mcached$getInverse()
#            [,1]       [,2]       [,3]
# [1,] -0.1666667 -1.6666667  1.5000000
# [2,]  0.3333333  0.3333333 -0.3333333
# [3,] -0.1666667  0.3333333 -0.1666667
#
# Test 2 : same as before, starting with an empty matrix, setting it later
# > rm(mcached)
# > mcached <- makeCacheMatrix()    # empty call
# > mcached$getMatrix()
#      [,1]
# [1,]   NA
# > mcached$setMatrix(mx)           # setting the matrix
# > mcached$getMatrix()
#      [,1] [,2] [,3]
# [1,]    1    4    1
# [2,]    2    5    8
# [3,]    3    6    9
# > mcached$getInverse()
# NULL
# > cacheSolve(mcached)
#            [,1]       [,2]       [,3]
# [1,] -0.1666667 -1.6666667  1.5000000
# [2,]  0.3333333  0.3333333 -0.3333333
# [3,] -0.1666667  0.3333333 -0.1666667
# 
# end of test
