## makeCacheMatrix creates an object of a matrix and it's inverse.
## cacheSolve computes the inverse of a cached matrix, updating the
## matrix object with it's inverse.  ie. only computing the inverse
## if one does not already exist.


## makeCacheMatrix -Initialises a matrix-like object and it's inverse.

makeCacheMatrix <- function(x = matrix()) {
  xInverse <- NULL
  # First function changes the value of the matrix "x".
  # If the matrix "x" is changed, the cached inverse reverts to NULL.
  set <- function(y) {
    x <- y
    xInverse <- NULL
  }
  # Second function returns the matrix "x" when called.
  get <- function() {
    return(x)
  }
  # Cache the inverse of "x".
  setInverse <- function(newInverse) {
    xInverse <<- newInverse
  }
  # Output the cached inverse of "x".
  getInverse <- function() {
    return(xInverse)
  }
  # Allow a list of the contained functions to be output.
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## cacheSolver() -Updates a cached matrix with it's inverted matrix attribute.

cacheSolve <- function(x, ...) {
  print(x$get())
  # First load whatever inverse "x" might already have.
  xInverse <- x$getInverse()
  # If xInverse is cached, xInverse is returned, exiting the function.
  if( !is.null(xInverse) ) {
    return(xInverse)
  }
  # If NULL, xInverse is calculated, then reassigned in this function.
  # Find the dimension of the matrix.
  xSize <- unique( dim( x$get() ) )
  print("xSize")
  print(xSize)
  # If theres more than one unique dim, x is not a square matrix..
  #.. and therefore not invertible.
  if( length(xSize) > 1 ) {
    stop("Non-square matrix can't be inverted.")
  }
  # If x * xInverse = <identity_matrix>, "xInverse" is the inverse of "x".
  print(class( x$get() ))
  xInverse <- solve( x$get(), diag(xSize) )
  # Update the inverse of "x" within the "x" object.
  x$setInverse(xInverse)
}


m <- matrix(c(1,0,-1,1),2,2)


M <- makeCacheMatrix(m)

print("The matrix M")
print(M$get())

cacheSolve(M)

print("The inverse of matrix M")
print(M$getInverse())


