## The function makeCacheMatrix() creates an object of type list, which then can
## be passed on to the function cacheSolve(), which first checks if inverse
## is already stored in cache for the given object. If present, it returns that 
## value, and if not- it calculates, stores in cache, and returns the newly
## calculated value.

## The function makeCacheMatrix creates an object of type list with
# variable 'inv' initialized to NULL. The short functions inside this
# can be accessed from the next function cacheSolve.

makeCacheMatrix <- function(x = matrix()) {     #input x will be matrix             
        inv <- NULL             #inv will store the inverse and is reset to NULL
        #every time makeCacheMatrix is called
        set <- function(y){
                x <<- y
                inv <<- NULL
        }
        # The three functions below are not run when makeCacheMatrix is run
        # But, they will be used by cacheSolve() to get values for x or
        # Or, for inverse (inv) and for setting the inverse.
        # These are called object "methods"
        get <- function() {x}           #returns value of original matrix
        setinv <- function(solve) {inv <<- solve} #called by cacheSolve() during 
                                                  #first access and
        #it will store the value using superassignment
        getinv <- function() {inv}      #returns the cached value to cacheSolve
                                        #() on subsequent accesses
        
        list(set = set, get = get,#This list returned with newly created object.
             setinv = setinv,     #Lists all the functions that are part of object.
             getinv = getinv      #If a function is not on the list, cannot be 
                                  #accessed externally.
        )
}

## The function cacheSolve checks if the inverse ("inv") has been stored
# And if not, calculates it, stores and also returns it. If the inverse
# of the object was already calculated and stored, it will fetch the result
# and return it.

cacheSolve <- function(x, ...) {   # input is object created by makeCacheMatrix
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinv()                       #access the object 'x' and get 
                                                #the inverse value
        if(!is.null(inv)){                      #if inverse was already cached 
                                                #(not NULL)
                message("getting cached data")  #..send this message to console
                return(inv)                     #..and return the inverse.."return" 
                                                #ends cacheSolve() 
        }
        data <- x$get()     #we reach here only if x$getinverse() returns NULL            
        inv <- solve(data)  #if inverse was NULL then, calculate it here
        x$setinv(inv)       #store the calculated inverse in 
                            #x ( setinv() in makevector)     
        inv                 #return the inverse to the code that 
                            #called this function    
}