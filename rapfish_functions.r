# RAPFish - Rapid Assessment of Fishery Sustaintability   
#   # for local execution in R 
#
# 2012-08-24 Divya Varkey	Additional 25 anchors not plotted
# 2012-04-15 Divya Varkey 	Rapfish, MC and Leverage
# 2011-09-27 Divya Varkey 	rapfish code for 1 field at a time
# 2011-08-14 Ken Lockhart       fine tune rotation
# 2011-08-13 Ken Lockhart       radarplots (Nx3 array) functional
# 2011-08-12 Ken Lockhart       RAPplot functional
# 2011-08-11 Ken Lockhart		Integrate Divya's plots in mainline.
# 2011-08-07 Divya Varkey		Radar plot
# 2011-08-02 Divya Varkey		MDS Plot w Legend
# 2011-07-23 Ken Lockhart	  	Created.
#

# RAPFish expects an input matrix of RAPFish scorecards defining a set of 
# fisheries.  The fisheries data is preceded by a some of anchors (to control
# the orientation of results sets generated by the multidimensional scaling
# algorythm that is the heart of RAPFish analysis.
# 
#       attributes ...->
# Good	   0   0   0  0  ..... 0
# Bad	   0   0   0  0  ..... 0
# anchor1   0   0   0  0  ..... 0
#   ...
# anchorN
# fishery1
#   ...
# fisheryN

# ----PREREQUISITES-----------------------------------------
# Load required libraries
library(grid)


# ----INPUT--------------------------------------------------
loaddata <- function(file.name){
  # Load the data into a data.frame
  fisheries = read.csv(file.name,row.names=1,check.names=TRUE)
  print(colnames(fisheries))
  print(rownames(fisheries))
  
  return(fisheries)
}


# ----toPolar----------------------------------------------
toPolar <- function(x,y) {
  # Polar coordinates from Cartesian coordinates.
  radius= sqrt(x^2 + y^2);
  theta = atan(y/x);
  theta = theta*(180/pi)		# convert radians to degrees
  return( c(radius,theta) )  
}

# ----toCartesian------------------------------------------
toCartesian <- function (radius, theta) {
  # Cartesian coordinates from Polar coordinates.
  x= radius * cos(theta)
  y= radius * sin(theta)
  return( c(x,y) )
}

# ----rotate------------------------------------------------
rotate <- function (x,y,angle) {
  # 2D rotation of a cartesian coordinate
  theta= angle* (pi/180)		# convert degrees to radians
  x= (x*cos(theta)) - (y*sin(theta))
  y= (x*sin(theta)) + (y*cos(theta))
  return( c(x,y) )
}


# ----NORMALIZE----------------------------------------------
normalize <- function(dataset) {
  # normalize & scale the data
  good= dataset[1,]
  bad = dataset[2,]

  results= NULL 
  for (i in 1:nrow(dataset)) {
    drow = dataset[i,]
    distance = ifelse(good<bad, bad-drow, drow)
    distance = t(distance)
    score = ( distance / (abs(good-bad)) )*100
    rownames(score) = rownames(drow)
    results = rbind(results, score)
  }
  
  return(results)
}


# ----MULTIDIMENSIONAL SCALING-------------------------------
mdscale <- function(dataset) {
  # use multidimensional scaling to compare the fisheries
  #
  disttbl= dist(dataset,"euclidean",diag=TRUE,upper=TRUE)
  coords = cmdscale(disttbl,k=2)

  # Flip & Scale - needed?
  #dx = coords['GOOD',1] - coords['BAD',1]
  if (coords['GOOD',1] < coords['BAD',1]) {
    coords[,1]= -coords[,1]  # flip horizontal
  }
    
  #dy = coords['GOOD',2] - coords['BAD',2]
  if (coords['UP',2] < coords['DOWN',2]) {
    coords[,2]= -coords[,2]  # flip vertical
  }  
  
  # Rotate - needed?
  p1= toPolar( coords['GOOD',1], coords['GOOD',2] )
  p2= toPolar( coords['BAD',1], coords['BAD',2] )
  angle= -( mean(c(p1[2],p2[2])))
  coords1= matrix(rotate(coords[,1],coords[,2],angle),,2)
  rownames(coords1)= rownames(coords)
  
  x.min= min(coords1[,1])
  x.max= max(coords1[,1])
  y.min= min(coords1[,2])
  y.max= max(coords1[,2])
  
  sc_x=100/(x.max-x.min)
  X_scores=coords1[,1]*sc_x
  ad_x=abs(X_scores[2])
  X_scores=X_scores+ad_x
     
  sc_y=100/(y.max-y.min)
  Y_scores=coords1[,2]*sc_y
     	 
  coords2=cbind(X_scores,Y_scores)

  return(coords2)
}



# ----RAPPLOT--------------------------------------------------


RAPplot1 <- function(coords2,num_fish,n_an) {
  # plot the Fishery rankings

  cols=rainbow(num_fish,start=0, end=.7)
  compass = coords2[1:4,]
  anchors = coords2[5:n_an,]
  ap=n_an-25
  anchorsplot=coords2[5:ap,]
  fish  = coords2[(n_an+1):nrow(coords2),]
  ind=seq(1,num_fish)
  fish=cbind(fish,ind)
  sfish = fish[order(fish[,1]),]

  x.min1= compass[2,1]
  x.max1= compass[1,1]
  y.min1= compass[4,2]
  y.max1= compass[3,2]

  plot(coords2,type="n",xlab="",ylab="",main="",
    xlim=c(x.min1,x.max1+30),las=1)
  lines(c(x.min1,x.max1),c(0,0),col="dark grey",lwd=1.5)
  lines(c(50,50),c(y.min1,y.max1),col="dark grey",lwd=1.5)
  #grid
  lines(c(25,25),c(y.min1,y.max1),col = "lightgray", lty = "dotted")
  lines(c(75,75),c(y.min1,y.max1),col = "lightgray", lty = "dotted")
 
  lines(c(x.min1,x.max1),c(25,25),,col = "lightgray", lty = "dotted")
  lines(c(x.min1,x.max1),c(-25,-25),,col = "lightgray", lty = "dotted")
  
  points(compass,xlab="",ylab="",col="dark red",pch=rownames(compass))
  points(anchorsplot,xlab="",ylab="",col="dark red",pch=19,cex=0.6)
  points(sfish,xlab="",ylab="",col=cols,pch=19)

  legend(x='right',rownames(sfish),col=cols,pch=19,cex=0.7,bty="n")
  output=list()
  output$scores=sfish[,1:2]
  output$ind=sfish[,3]
  output$cols=cols
  return(output)
}

RAPplot2 <- function(coords2,num_fish,n_an) {
  # plot the Fishery rankings

  cols=rainbow(num_fish,start=0, end=.7)
  compass = coords2[1:4,]
  anchors = coords2[5:n_an,]
  ap=n_an-25
  anchorsplot=coords2[5:ap,]
  fish  = coords2[(n_an+1):nrow(coords2),]
  ind=seq(1,num_fish)
  fish=cbind(fish,ind)
  sfish = fish[order(fish[,1]),]

  x.min1= compass[2,1]
  x.max1= compass[1,1]
  y.min1= compass[4,2]
  y.max1= compass[3,2]

  plot(coords2,type="n",xlab="",ylab="",main="",
    xlim=c(x.min1,x.max1+50),las=1)
  lines(c(x.min1,x.max1),c(0,0),col="dark grey",lwd=1.5)
  lines(c(50,50),c(y.min1,y.max1),col="dark grey",lwd=1.5)
  #grid
  lines(c(25,25),c(y.min1,y.max1),col = "lightgray", lty = "dotted")
  lines(c(75,75),c(y.min1,y.max1),col = "lightgray", lty = "dotted")
 
  lines(c(x.min1,x.max1),c(25,25),,col = "lightgray", lty = "dotted")
  lines(c(x.min1,x.max1),c(-25,-25),,col = "lightgray", lty = "dotted")

  points(compass,xlab="",ylab="",col="dark red",pch=rownames(compass))
  points(anchorsplot,xlab="",ylab="",col="dark red",pch=19,cex=0.6)
  points(sfish,xlab="",ylab="",col=cols,pch=19)
  
  lp=round(num_fish/2,0)
  legend(108,40,rownames(sfish[1:lp,]),col=cols[1:lp],pch=19,cex=0.7,bty="n")
  legend(130,40,rownames(sfish[(lp+1):num_fish,]),col=cols[(lp+1):num_fish],pch=19,cex=0.7,bty="n")
  
  output=list()
  output$scores=sfish[,1:2]
  output$ind=sfish[,3]
  output$cols=cols
  return(output)
}


#---- RADAR PLOTS ---------------------------------------------------
radarplot <- function(dataset) {
  # Draw the plot background
  cols=rainbow(max(10,nrow(dataset)),start=0, end=.7)
  yp = c(-10,-5,5,10,5,-5,-10)
  xp = c(0,-5,-5,0,5,5,0)
  
  plot(xp,yp,type='n',xlim=c(-8,12),ylim=c(-11,11),axes=F,xlab="",ylab="")
  polygon(xp,yp,border="dark grey",col="darkseagreen1",lwd=1.5)
  polygon(xp*0.7,yp*0.7,border=NA,col="lightyellow")
  polygon(xp*0.4,yp*0.4,border=NA,col="mistyrose1")

  lines(c(xp[1],xp[4]),c(yp[1],yp[4]),col="dark grey",lwd=1.5)
  lines(c(xp[2],xp[5]),c(yp[2],yp[5]),col="dark grey",lwd=1.5)
  lines(c(xp[3],xp[6]),c(yp[3],yp[6]),col="dark grey",lwd=1.5)

  # Label the background
  y.text = c(-11,-5,5,11,5,-5)
  x.text = c(0,-7,-7,0,7,7)
  text(x.text,y.text,colnames(dataset),col="grey20",offset=0)
  legend("right",rownames(dataset),col=cols,lty=1,lwd=2,bty="n",text.col="grey20",cex=0.8)

  # Prepare the data
  y.scale = yp[1:6]/100
  x.scale = c(0,1,-1,0,1,-1)

  fy = y.scale * t(dataset)
  fx = x.scale * fy
  fx = t(fx)
  fy = t(fy)


  # plot the data on the background image
  for(i in 1:nrow(dataset)) {
    polygon(fx[i,],fy[i,],border=cols[i],lwd=2)
  }
}
