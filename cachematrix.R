makeCacheMatrix <- function(x=matrix()) {
## This creates a matrix as described by x in Cache and provides 4 functions needed to manipulate it
  
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  invset <- function (inverse) inv <<- inverse  
  invget <- function() inv
  list(set=set, get=get, invset=invset, invget=invget)
  
}

cacheSolve <- function(x, ...) {

## This returns a matrix that is the inverse of 'x'
  
  inv_m = x$invget()
  if(!is.null(inv_m)){
    message("retrieving from cache")
    return(inv_m)
    }
   else {
     m_data = x$get()
     if ( det(m_data) == 0 ) {
       message("non invertible matrix")
       ## I put this in here, because I spent a lot of time trying to figure why my initial test matrix failed
       ## It turned out to be "singular / non invertible" [1:9] 3 rows, 3 columns is an example!
       }
     else {
       inv_m=solve(m_data,...)
       x$invset(inv_m)
       return(inv_m)
       }
     }
}
