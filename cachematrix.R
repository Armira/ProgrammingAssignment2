## The functions cache the inverse of a matrix.

## This function
## 1. Sets the matrix
## 2. Gets the matrix
## 3. Sets the inverse (m)
## 4. Gets the inverse

makeCacheMatrix <- function(x = matrix()) {
+     m <- NULL
+     set <- function(y) {
    +         x <<- y
    +         m <<- NULL
    +     }
+     get <- function() x
+     setSolve <- function(solve) m <<- solve
+     getSolve <- function() m
+     list(set = set, get = get,
+          setSolve = setSolve,
+          getSolve = getSolve)
+ }

## This function calculates the inverse of the matrix. If the inverse has already been calculated it skips the
## calculation and return the inverse. Otherwise, it calculates the inverse, sets its value and returns it.

cacheSolve <- function(x, ...) {
+     m <- x$getSolve()
+     if(!is.null(m)) {
    +         message("getting cached data")
    +         return(m)
    +     }
+     data <- x$get()
+     m <- solve(data, ...)
+     x$setSolve(m)
+     m
+ }


