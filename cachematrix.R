## The makeCacheMatrix and cacheSolve functions together take an  
## invertable square matrix, create the inverse of the matrix, and then
## cache the resulting inverse matrix. They allow the inverse matrix to be 
## stored a retrieved rather recalculated, thus retuding the run time of a
## programs that requie the inverse of a matrix to be calculated.  

## The makeCacheMatrix takes as its input an invertable square matrix and stores 
## in meory external to the function so it can be retrieved by other functions. 

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y         
                                ## take the input to the set functon and assign 
                                ##it to X in memory external to the function. 
                
                inv <<- NULL
                                ## assign the contents of inv to NULL in memeory 
                                ## external to the function. 
        }
        
        get <- function() x
                                ## get outputs x
        setInverse <- function(Inverse) inv <<- Inverse
        
                                ## Defines the contents of inv and assigns
                                ## it to memory external to the fxn.
                                ##
        
        getInverse <- function() inv
                                ## outputs what ever setInverse has assinged 
                                ## to inv using setInverse 
        
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
                                ## story the the listed functions as part of the
                                ## makeCacheMatrix function. 
        
}


## The cacheSolve function takes as an input an invertable square matrix.  It 
## then pulls the inverse of that matrix from memeory if it has already been
## calulated or otherwise calcuates the inveres and stores in in memory external
## to the function. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        inv <- x$getInverse()
                        ## take the contensts of inv from external memory and
                        ## asign it to inv in the function. 
        
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
                        ## if the contents of inv have been defined (are not 
                        ## NUL), then return the contents of inv.
        }
        
        data <- x$get()
        inv <- solve(data, ...)
        x$setInverse(inv)
        inv
                        ## if the contnets of inv are NULL, then use get to 
                        ## retrieve the stored matix, calculate the inverse with
                        ## solve and assign to contents to external memory 
                        ## using setInverse, and return that inverted matrix. 
}
