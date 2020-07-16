## Programming Assignment 2: Lexical Scoping.
## These functions create a cache of an inverse matrix and then calls that cache, if for the same original matrix, or calculates the inverse matrix if not cached yet. The two functions work together, the first sets the stage and stores the defined object in the object "a" (eventually the solved inverse matrix), and the associated functions. These are then sourced in cacheSource through lexical scoping.  

## This function initializes object "a", which is assigned NULL value so that it clears any previous runs of the function. It then defines "get",  the getter for x, to be used in cacheSource, the function "setInv" to solve for the inverse matrix, and "getInv" which is the getter for the inverse matrix. These function names are made into a list so that cacheSource can use $ to call them.

makeCacheMatrix <- function(x = matrix()) {
        a<-NULL                                      
        set<-function(y){                             
                x<<-y                                         
                a<<-NULL                                      
        }
        get<-function() x 
        setInv<-function(solve) a<<- solve  
        getInv<-function() a                        
        list(set=set, get=get,              
             setInv=setInv,
             getInv=getInv)                         
}
        
       

## cacheSolve is the completion to makeCacheMatrix. It first uses the getter of the inverse matrix for object x "getInv" which was defined in makeCacheMatrix. If the output value is NULL, meaning there is no inverse matrix stored, then the function moves on to calculate the inverse matrix of object x and sets the value of "a" to the parent environment If there is a value, then objet "a" is printed. Lexical scoping is used to find functions and objects that have not been defined in cacheSolve, but have been definied in makeCacheMatrix.

cacheSolve <- function(x=matrix(), ...) {
        a <- x$getInv()
        if(!is.null(a)) {
                message("getting cached data!")
                return(a)
        }
        inv <- x$get()
        a <- solve(inv, ...)
        x$setInv(a)
        a
}
