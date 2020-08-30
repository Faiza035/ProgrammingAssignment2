# Caching the inverse of a matrix.
# Matrix inversion is usually a costly computation and there maybe some benefit
# to caching the inverse of a matrix rather than repeatedly computing it.
# Below are the two functions one among which is used to create a special 
# object and stores a matrix in it and another one caches its inverse.


#The function below creates a special matrix object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) { ## defining the argument of the function with default mode of matrix.
      inv <- NULL ## inv will hold the value of matrix inverse, here we are initializing inv with NULL.
      set <- function(y){ ## defining the set function to assign new value of matrix in parent environment.
      x <<- y ## <<-this used to assign a value in the environment which is different from current environment.
      inv <<- NULL ## if there is a new matrix, resetting the inverse to be null.
    }
    get <- function() x ## defining the get function which returns the value of matrix argument.
    setInverse <- function(inverse) inv <<- inverse ## assigning value of inverse in parent environment.
    getInverse <- function() inv ## getting the values of inverse when called.
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse) ##this is to refer to functions with $ operator.
}


# This function computes the inverse of the special "matrix" created by 
# makeCacheMatrix function above. If the inverse has already been calculated (and the 
# matrix has not changed), then it should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) { ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setInverse(inv)
  inv
}


