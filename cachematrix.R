## Funktionen um die Umkehrungen einer Matrix zwischenzuspeichern
##
## Beispiel:
##
## m <- matrix(rnorm(16),4,4)
## m1 <- makeCacheMatrix(m)
## cacheSolve(m1)
## [,1]       [,2]       [,3]       [,4]
## [1,]  0.3707419 -0.2616290  1.1413200 -0.3423992
## [2,] -2.3907032 -1.0683140 -2.3245967 -1.4020180
## [3,] -0.9687390 -0.3200147 -0.2147001 -0.0955060
## [4,] -0.5080576 -0.4147343 -0.9020221  0.1392848



## Create a special "matrix", which is a list containing
## a function to
##   - set the value of the matrix
##   - get the value of the matrix
##   - set the value of the inverse matrix
##   - get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inv) i <<- inv
  getinverse <- function() i
  list(
    set = set,
    get = get,
    setinverse = setinverse,
    getinverse = getinverse
  )
}



##  Berechnung der Umkehrung
##  Funktion, Wiederverwendung des zwischengespeicherten Ergebnisses, sofern verfügbar

cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  m <- x$get()
  i <- solve(m, ...)
  x$setinverse(i)
  i
}
