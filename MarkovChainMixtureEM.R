####################################################################
#	A circle of routines that implement the EM algorithm 
# for mixtures of Markov chains.
# 
# mrm: Man Uni, 16 July 2019
# mrm & Rajenki Das: Man Uni, 5 Feb. 2020
#
# N.B. The convention here is that, in a transition
# matrix P, P_{ij} is the probability of a 
# transition i -> j.
######################################################
library( gtools )  # for rdirichlet()

####################################################################
#	Given a trajectory - a sequence of states drawn from
# {1, .., n.states} - build a matrix C whose i,j entry counts
# the number of times that the trajectory includes a transition 
# from state i to state j.
####################################################################

count.transitions <- function( traj, n.states=NA )
{
	# Deal with the possibility that n.states wasn't given 
	if( is.na(n.states) ) { n.states <- max( traj ) }
	
	# Initialise the result as a matrix full of zeroes.
	count.mat <- matrix( rep(0, n.states*n.states), nrow=n.states )
	
	# Run through the trajectory counting transitions
	n.transitions <- length( traj ) - 1
	for( t in 1:n.transitions ) {
		from.state <- traj[t] ;
		to.state <- traj[t+1] ;
		count.mat[from.state, to.state] <- count.mat[from.state, to.state] + 1 ;
	}
		
	return( count.mat )
}

if( exists("do.tests") && do.tests ) {
	# Do a small test
	test.traj <- c( 1, 1, 1, 1, 1, 2, 2, 1, 2, 2, 2, 1, 2 ) ;
	count.transitions( test.traj, 2 )
	count.transitions( test.traj, 3 )
	count.transitions( test.traj )
}

####################################################################
#	Given a count matrix of the sort produced by count.transitions()
# and a matrix of transition probabilities, compute a log-likelihood.
####################################################################

# It'll prove helpful to have a separate test for the 
# presence of impossible transitions.
traj.impossibleQ <- function( count.mat, trans.mat ) {
	count.vec <- as.vector( count.mat )
	prob.vec <- as.vector( trans.mat ) ;

	return( any( (prob.vec == 0) & (count.vec != 0) ) ) ;
}

traj.loglike <- function( count.mat, trans.mat ) {
	count.vec <- as.vector( count.mat )
	prob.vec <- as.vector( trans.mat ) ;
	log.prob.vec <- ifelse( prob.vec==0, 0, log(prob.vec) ) ;
	loglike <- sum(count.vec * log.prob.vec)
	
	return( loglike  ) ;
}

if( exists("do.tests") && do.tests ) {
	# Another test
	small.trans.mat <- matrix( rep( 0.5, 4), nrow=2 ) ;
	test.loglike <- traj.loglike( count.transitions( test.traj, 2 ), small.trans.mat )
	test.loglike == log(0.5) * (length(test.traj) - 1)

	small.trans.mat <- matrix( c(1, 0, 0, 1), nrow=2 ) ;
	traj.impossibleQ( count.transitions( test.traj, 2 ), small.trans.mat )
}

####################################################################
#	Given a matrix gamma whose i,j entry is the probability
# that the i-th subject belongs to the j-th class, compute a 
# vector pi whose j-th entry is the probability that a 
# randomly-selected subject belongs to class j.
####################################################################

estimate.mixture.weights <- function( gamma.mat )
{
	n.subjects <- nrow( gamma.mat )
	pi.vec <- colSums( gamma.mat ) / n.subjects
	return( pi.vec )
}

if( exists("do.tests") && do.tests ) {
	# As ever, do a test
	test.gamma.entries <- c( 1.0, 0.0, 1.0, 0.0, 0.0, 1.0 )
	test.gamma.mat <- matrix( test.gamma.entries, byrow=TRUE, ncol=2 )
	test.gamma.mat
	estimate.mixture.weights( test.gamma.mat )
}

####################################################################
#	Given a matrix gamma as described above and a list
# of count matrices of the sort produced by count.transitions(),
# estimate the transition matrices for all the classes.
####################################################################

estimate.transition.matrices <- function( gamma.mat, count.mats )
{
	# Examine gamma.mat and count.mat.list to get 
	# the numbers of subjects, classes and states
	n.subjects <- nrow( gamma.mat )
	n.classes <- ncol( gamma.mat )
	n.states <- nrow( count.mats[[1]] )
	
	# Initialise a list of empty pseudocount matrices, one per class.
	empty.mat <- matrix( rep( 0.0, n.states*n.states), nrow=n.states )
	pseudocount.mats <- lapply( 1:n.classes, function(j){ empty.mat }) ;
		
	# Accumulate the pseudocount matrices, adding a fraction gamma_{kj} 
	# of subject k's counts to the pseudocount matrix for the j-th class.
	for( k in 1:n.subjects ) {
		for( j in 1:n.classes ) {
			pseudocount.mats[[j]] <- pseudocount.mats[[j]] + gamma.mat[k,j] * count.mats[[k]] ;
		}
	}
	
	# Normalize the pseudocount matrices to get transition matrices.
	# Given that the i,j entry counts transitions from i to j, we want
	# the *rows* of our transition matrices to sum to one. We arrange
	# this by multiplying from the left by a diagonal normalising matrix
	# D whose entry D_{jj} is 1/ (sum of entries in j-th row). 
	for( j in 1:n.classes ) {
		# Compute the row sums
		crnt.row.sums <- rowSums( pseudocount.mats[[j]] ) ;
		
		# Compute the entries that go on the diagonal of the normalising
		# matrix. One needs a little care in the case where some row sums to zero
		# (It would correspond to a state from which no transitions were seen).
		diag.entries <- ifelse( crnt.row.sums==0, 1, 1/crnt.row.sums ) ;
		pseudocount.mats[[j]] <- diag( diag.entries ) %*% pseudocount.mats[[j]] ;
	}
	
	return( pseudocount.mats )
}

if( exists("do.tests") && do.tests ) {
	# As ever, do a test
	test.count.mat <- count.transitions( test.traj, 2 )
	test.count.mat
	test.count.mats = list( test.count.mat, test.count.mat, t(test.count.mat))
	estimated.trans.mats <- estimate.transition.matrices( test.gamma.mat, test.count.mats )
	estimated.trans.mats
}

####################################################################
#	Given a Markov-process mixture model and some count 
# matrices of the sort produced by count.transitions(),
# compute gamma, the matrix of class membership probs.
####################################################################

compute.gamma.mat <- function( count.mats, pi.vec, trans.mats )
{
	# Examine pi.vec and count.mats to get 
	# the numbers of subjects and classes.
	n.subjects <- length( count.mats )
	n.classes <- length( pi.vec )
	
	# Initialise gamma.mat with NA's. That way, if we somehow fail 
	# to fill some entry in, the mistake will be evident. 
	gamma.mat <- matrix( rep(NA, n.subjects*n.classes), nrow=n.subjects )
	
	# Arrange for the k,j entry to be the likelihood of the k-th count matrix 
	# being generated from the j-th transition matrix
	for( k in 1:n.subjects ) {
		for( j in 1:n.classes ) {
			if( traj.impossibleQ( count.mats[[k]], trans.mats[[j]] ) ) {
				gamma.mat[k,j] = 0.0 ;
			} else {
				crnt.loglike <- traj.loglike( count.mats[[k]], trans.mats[[j]] ) ;
				gamma.mat[k,j] <- pi.vec[j] * exp( crnt.loglike ) ;
			}
		}
	}
	
	# Normalize the matrix we've just constructed to get probabilities of membership.
	gamma.row.sums <- rowSums( gamma.mat ) ;
	if( any(gamma.row.sums <= 0) ) {
		warning( "Row sum of zero?!?" ) ;
	}
	
	diag.entries <- 1/gamma.row.sums ;
	gamma.mat <- diag( diag.entries ) %*% gamma.mat ;
	return( gamma.mat ) ;
}

if( exists("do.tests") && do.tests ) {
	compute.gamma.mat( test.count.mats, c(2/3, 1/3), estimated.trans.mats )

	#### Do some speed tests ##########
	library( microbenchmark )

	microbenchmark(
		count.transitions( test.traj, 2 ),
		estimate.mixture.weights( test.gamma.mat ),
		estimate.transition.matrices( test.gamma.mat, test.count.mats ),
		compute.gamma.mat( test.count.mats, c(2/3, 1/3), estimated.trans.mats ) 
	)
}

#####################################################################
#	Generate a random gamma matrix to initialise the EM algorithm
#####################################################################

random.gamma.mat <- function( n.subjects, n.classes, alpha.val=0.5 ) {
	# Draw the values from a Dirichlet distribution
	alpha <- rep( alpha.val, n.classes )
	
	return( rdirichlet( n.subjects, alpha ) )
}

if( exists("do.tests") && do.tests ) {
	random.gamma.mat( 4, 3 )
}

#####################################################################
#	Given a list of state trajectories and a number of classes,
# estimate a mixture of Markov chains that could have generated them.
#####################################################################
MarkovChainMixtureEM <- function(
	traj.list, n.classes, n.states=NA, max.cycles=100, tol=0.000001 
) {
	# If need be, get the number of states.
	if( is.na(n.states) ) {
		max.per.traj <- lapply( traj.list, max ) ;
		n.states <- max( max.per.traj ) ;
	}
	
	# Reduce the trajectories to count matrices
	count.mats <- lapply( traj.list, 
		function( my.mat ) { count.transitions( my.mat, n.states ) } 
	) ;
	
	# Generate a random matrix of class membership probs. This
	# serves a starting guess for gamma.
	n.subjects <- length( traj.list ) ;
	gamma.mat <- random.gamma.mat( n.subjects, n.classes ) ;
	
	# Do the thing.
	n.cycles <- 0 ;
	rms.delta.gamma <- NaN ; # This should get a numerical value before we need it.
	while( 
		(n.cycles == 0) ||
		((rms.delta.gamma >= tol) && (n.cycles < max.cycles)) 
	) {
		# Compute maximum-likelihood mixture parameters based on gamma
		pi.vec <- estimate.mixture.weights( gamma.mat ) ;
		trans.mats <- estimate.transition.matrices( gamma.mat, count.mats ) ;
		
		# Estimate a new gamma matrix based on the new parameters
		prev.gamma <- gamma.mat ;
		gamma.mat <- compute.gamma.mat( count.mats, pi.vec, trans.mats ) ;
		
		# Note that we've completed another EM cycle.
		n.cycles <- n.cycles + 1 ;
		
		# Evaluate the convergence criterion
		delta.gamma <- gamma.mat - prev.gamma ;
		dg.squared <- delta.gamma * delta.gamma ; # elementwise multiplication
		rms.delta.gamma <- sqrt( sum(dg.squared)/ (n.subjects*n.classes) ) ;
	}
	
	# The result has a degeneracy induced by permuting the class labels and so,
	# following advice offered by Michael Betancourt at
	#
	#	https://betanalpha.github.io/assets/case_studies/identifying_mixture_models.html
	#
	# we lift this degeneracy by enforcing the condition that pi.vec is increasing.
	perm <- order( pi.vec ) ;
	
	# Assemble the result
	result <- list(
		# The input data
		n.subjects	= n.subjects,
		n.classes	= n.classes,
		n.states	= n.states,
		traj.list 	= traj.list,
		
		# The results
		gamma.mat	= gamma.mat[,perm],
		trans.mats	= trans.mats[perm],
		pi			= pi.vec[perm],
		
		# Details about the computation
		n.cycles		= n.cycles,
		converged		= (rms.delta.gamma < tol),
		rms.delta.gamma = rms.delta.gamma
	)
	
	return( result )
}

