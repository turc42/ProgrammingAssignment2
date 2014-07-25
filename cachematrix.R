################################################################
#Author:Bill Turczyn
#Date:2014/07/24 00:30:31
#Desc:   Programming Assignment 2
#Course: R Programming


########################################################################################
# Descripton: This function creates a contains a list with the following functions 
#             set(), get(), set_inverse(), get_inverse() and a maxtrix with it's inverse 
#             to support caching 
#             making use of the `<<-` assignment operator
#
# Precondition:
# Postcondition: the maxtrix and inverse are created
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

    #Make sure the argument is a list created by the makeCacheMatrix function
    if (! "get_inverse_matrix" %in% names(the_matrix)){
        return(message("Error: This function requires a list created with makeCacheMatrix as the argument."))
    }

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
#########################################
#Below commands test the functions      #
#########################################
#message("testing program...")
#my_matrix<-matrix(c(1,-2,0,0,1,0,0,0,1),3,3)
#test_me<-makeCacheMatrix()
#test_me$set(my_matrix)
##this will produce an error
#cacheSolve(my_matrix) 
#cacheSolve(test_me)
####################################
