#Uniform Distribution Monte Carlo for Rapfish
# 2012-04-15 Divya Varkey  Created

#to be obtained from site
nsim=100
num_fish=53
anchor_files=c('anchors4.csv','anchors5.csv','anchors6.csv','anchors7.csv','anchors8.csv','anchors9.csv','anchors10.csv','anchors11.csv','anchors12.csv')
filenames=c('CCRF_Field1.csv','CCRF_Field2.csv','CCRF_Field3.csv','CCRF_Field4.csv','CCRF_Field5.csv','CCRF_Field6.csv')

###########generated here
nfield=length(filenames)
discipline.names =strsplit(filenames, ".csv") 
images_umc=paste(discipline.names,"_Uniform_MC",".jpg",sep="")
res_unif=paste("MC_Uniform_",discipline.names,".csv",sep="")

source("rapfish_functions.R")

L1=num_fish+1
L2=L1+num_fish-1

U1=L2+1
U2=U1+num_fish-1


###############################UNIFORM DIST MC

Unif_RapfishMC<-function(fisheries.all,num_fish,nsim)
{
  n_att=ncol(fisheries.all)
  fisheries.dat=fisheries.all[1:num_fish,]  
  lb=fisheries.all[L1:L2,]
  ub=fisheries.all[U1:U2,]

  anchors=loaddata(anchor_files[n_att-3])
  colnames(anchors)<-colnames(fisheries.all)

  mc_init=array(data=0,dim=c(num_fish,n_att,nsim))
 
  for(j in 1:num_fish) {
    for(k in 1:n_att) {
      mc_init[j,k,]=runif(nsim,lb[j,k],ub[j,k])
    }
  }
	  
  fish_mc_res=array(data=0,dim=c(num_fish+nrow(anchors),2,nsim))
  for(m in 1:nsim) {
    fish_mc.dat=mc_init[1:num_fish,1:n_att,m]
    colnames(fish_mc.dat)<-colnames(fisheries.all)
    fish_mc.raw=rbind(anchors,fish_mc.dat)
    fish_mc.scaled = mdscale(fish_mc.raw)
    fish_mc_res[,,m]=fish_mc.scaled
  }

  output<-list()
  output$mc_init=mc_init
  output$fish_mc_res=fish_mc_res
  return(output) 
}
  

###########PLOTS

for(i in 1:nfield) {

  fisheries.all = loaddata(filenames[i])
  ud=Unif_RapfishMC(fisheries.all,num_fish,nsim)
  n_att=ncol(fisheries.all)
  fisheries.dat=fisheries.all[1:num_fish,]  

  anchors=loaddata(anchor_files[n_att-3])
  colnames(anchors)<-colnames(fisheries.all)
 
  fisheries.raw=rbind(anchors,fisheries.dat)
  fisheries.scaled = mdscale(fisheries.raw)

  n_an=nrow(anchors)
  plot1=n_an+1
  plot2=n_an+num_fish
  cols=rainbow(num_fish,start=0, end=.7)

  jpeg(filename=images_umc[i],width=20,height=16,units="cm",res=500)
  Res=ifelse(nfield>30,RAPplot1(fisheries.scaled,num_fish,n_an),RAPplot2(fisheries.scaled,num_fish,n_an))
 
  mtext(side=3, line=1, "Uniform MC",adj=1) 
  mtext(side=3, line=1, discipline.names[i],adj=0) 

  for(m in 1:nsim) {
    mcplot=ud$fish_mc_res[plot1:plot2,,m]
    mcplot = mcplot[order(fisheries.scaled[plot1:plot2,1]),]
    points(mcplot,xlab="",ylab="",col=cols,pch='.')
  }
  dev.off()

  mc_summ=matrix(data=0,nrow=num_fish,ncol=12)
  s_mcres=ud$fish_mc_res[plot1:plot2,,]

  for(fs in 1:num_fish) {
    xx=round(quantile(s_mcres[fs,1,],probs=c(0.5,0.25,0.75,0.025,0.975)),4)
    yy=round(quantile(s_mcres[fs,2,],probs=c(0.5,0.25,0.75,0.025,0.975)),4)

    mc_summ[fs,2:6]=xx
    mc_summ[fs,8:12]=yy
  }

  mc_summ[,1]=round(fisheries.scaled[plot1:plot2,1],4)
  mc_summ[,5]=round(fisheries.scaled[plot1:plot2,2],4)
  colnames(mc_summ)<-c("X_Scores","Median","25%","75%","2.5%","97.5%","Y_Scores","Median","25%","75%","2.5%","97.5%")
  rownames(mc_summ)<-rownames(fisheries.dat)
  write.csv(mc_summ,res_unif[i])
}
