# Part_1: Create a matrix function:

 makeCacheMatrix <- function(x=matrix()) {
              inv <- NULL
              
      # Set the value of the vector    
        set_mat <- function(y) {
        x <<- (y)
        inv <<- NULL
              }
      #Get the value of vector function
        get_mat <- function() x
      
      # Set the value of the inverse
        set_matrix <- function(inverse) inv <<- inverse
      
      #Get the value of the inverse
        get_matrix <- function() inv
        list(set_mat = set_mat, 
           get_mat = get_mat,
           set_matrix = set_matrix,
           get_matrix = get_matrix)
    }

# Part_2: Compute function for matrix inverse:

 cacheSolve <- function(x, ...) {
    
      #Find matrix x 
      inv <- x$get_matrix()
      
      #Check if the matrix x is has been inversed
        if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
        }
      
      #Get the matrix x into the workspace
        data <- x$get_mat()
      
      #Compute the inverse of the matrix x
        inv <- solve(data)
      
      #Set the value of the inversed matrix into the cache
       x$set_matrix(inv)
       inv
    }
