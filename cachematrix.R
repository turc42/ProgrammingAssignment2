#Author:Bill Turczyn
#Date:2014/07/24 00:30:31
#Desc: Programming Assignment 2


########################################################################################
# Descripton: This function creates a  special matrix with functions to support caching 
#             the inverse of the matrix provided to the set() function 
#             making use of the `<<-` assignment operator
#
# Precondition:
# Postcondition: 
########################################################################################
makeCacheMatrix <- function(my_matrix = matrix()) {

    # set the inverse variable to null; used to determine if the results are cached or not
    inverse_matrix <- NULL

    #####################################################################
    # Argument: Matrix
    # Return  : NA
    # Sets inverse_matrix to NULL
    #####################################################################
    set <- function(y){
        #Check to insure that a matrix was passed to the set function
        if (!is.matrix(y)){
            message("Error: This function requires a matrix as the argument. The set was not executed.")
        }

        #Test to see if we are being passed the same matrix and let user know the cache is being set back to NULL
        if (is.matrix(y) && dim(y) == dim(my_matrix) && all(y == my_matrix && !is.null(inverse_matrix))){
            message("The matrix argument is the same as existing variable. The inverse in cache is being set to NULL...")
        }
        my_matrix <<- y
        inverse_matrix <<- NULL
    }   

    #####################################################################
    #Argument:NA 
    #Return: returns the matrix the was initialized with the set function
    #####################################################################
    get <- function() {
        my_matrix
    }

    #####################################################################
    # Argument: Inverse of Matrix
    # Return  :
    # Sets inverse_matrix to arguemnt im
    #####################################################################
    set_inverse_matrix <- function(im) {
        inverse_matrix <<- im
    }

    #####################################################################
    # Argument: 
    # Return  :inverse_matrix
    #####################################################################
    get_inverse_matrix <- function() {
        inverse_matrix
    }

        #create a list with fuctions to be used  by the cacheSolve function
        list(set = set, get = get,
             set_inverse_matrix = set_inverse_matrix,
             get_inverse_matrix = get_inverse_matrix)


}

########################################################################################
# Descripton: This function takes a matrix as an makeCacheMatrix object as an argument
# Precondition:
# Postcondition:
########################################################################################

cacheSolve <- function(the_matrix, ...) {
    # use the get function on the "special" matrix 
    # if the inverse matrix variable is not null Nb. Cached
    # Return a matrix that is the inverse of 'the_matrix'
    m <- the_matrix$get_inverse_matrix()
    if(!is.null(m)){
        message("getting cached data...")
        return(m)
    }
    message("no cached data available; calculating and caching now..")
    data <- the_matrix$get()
    m <- solve(data) %*% data
    the_matrix$set_inverse_matrix(m)
    the_matrix$get_inverse_matrix()
}
