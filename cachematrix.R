## Programming Assignment 2 to calculte the matrix inverse

## Cache function to cache the matrix calculations and used to retrieve later

makeCacheMatrix <- function(x = matrix()) 
{
    #Initialize the inverse matrix
    inv <- NULL 
    set <- function(y) 
    {
        x <<- y
        inv <<- NULL
    }
    #Function to get the matrix
    get <- function() x
    #Function to set the inverse
    setinv <- function(inverse) inv <<- inverse
    #Function to get the inverse
    getinv <- function() inv
    #Put the objects created above in a list
    list(set = set,get = get,setinv = setinv,getinv = getinv) 
}

## Function to solve for the matrix. This function is used to solve for the
## matrix. This matrix checks if the matrix has already been calculated. If so,
## the cached matrix is shown with a message. 

cacheSolve <- function(x, ...) 
{
    inv <- x$getinv()
    if(!is.null(inv)) 
    {
        message("getting cached data")
        print(inv)
    }
    #To get the matrix
    data <- x$get()
    #Solve the matrix
    inv <- solve(data, ...)
    #Caching the result from above
    x$setinv(inv)
    inv
}

x = matrix(4:7,nrow = 2, ncol = 2)
w = makeCacheMatrix(x)
y = cacheSolve(w)
print(y)
z = cacheSolve(w)