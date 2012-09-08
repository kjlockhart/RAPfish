rtri <- function(n=10,min=0,max=1,ml=0.5) {

  # Return independent random deviates from a
  # triangular distribution.
  
  # n is number of deviates requested
  # min, max are lower and upper limits of r.v. range
  # ml is the most-likely value

  # if ml = (min+max)/2, pdf is symmetric
  # if ml=min, pdf is left triangular
  # if ml=max, pdf is right triangular

  # Method reference is Section 7.3.9 of Law and Keaton (1982):
  
  # @Book{law82a,
  # author = {Averill M. Law and W. David Keaton},
  # title = {Simulation modeling and analysis},
  # publisher = {Mc{G}raw-Hill},
  # year = 1982
  #} 
   # Written by Andy Jacobson (andyj@splash.princeton.edu), 6 Dec 2002
    # Credit is also due to Jeffrey S. Smith, (jsmith@eng.auburn.edu),
    # whose C++ implementation I looked at first.
  
    if((ml<min)||(ml>max)) {
      stop("ml outside of range [min max]")
    }
    
    u <- runif(n)
  
    mode <- (ml-min)/(max-min) # "mode" defined in range [0 1] (rescaling will be done last)
  
    s1 <- which(u<=mode)
    s2 <- which(u>mode)
  
    u[s1] <- sqrt(mode*u[s1])
    u[s2] <- 1-sqrt((1-mode)*(1-u[s2]))
  
    min+(max-min)*u
    
  }
