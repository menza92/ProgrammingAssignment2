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
  # Calculate the inverse of "x", and cache it.
  setInverse <- function() {
    # Find the dimension of the matrix.
    xSize <- unique( dim(x) )
    # If theres more than one unique dim, x is not a square matrix..
    #.. and therefore not invertible.
    if( length(xSize) > 1 ) {
      stop("Non-square matrix can't be inverted.")
    }
    # If x * xInverse = <identity_matrix>, "xInverse" is the inverse of "x".
    xInverse <<- solve( x, diag(xSize) )
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
  # First load whatever inverse "x" might already have.
  xInverse <- x$getInverse()
  # If xInverse is cached, xInverse is returned, exiting the function.
  if( !is.null(xInverse) ) {
    return(xInverse)
  }
  # If NULL, xInverse is calculated, then reassigned in this function.
  x$setInverse()
  xInverse <- x$getInverse()
}
