# RAPFish - Rapid Assessment of Fishery Sustaintability
#   # for local execution in R 
#produces Rapfish Scores plots, radar plots, and writes the results to csv files
# 2012-04-15 Divya Varkey 	Created from Ken's main function for final execution 
                                                ##of all subsidiary Rapfish functions in rapfish_function.r

source("rapfish_functions.R")


#to be obtained from site
num_fish=12
anchor_files=c('anchors4.csv','anchors5.csv','anchors6.csv','anchors7.csv','anchors8.csv','anchors9.csv','anchors10.csv','anchors11.csv','anchors12.csv')
filenames=c('HG_Ecology.csv','HG_Economics.csv','HG_Ethical.csv','HG_Institutional.csv','HG_Social.csv','HG_Technology.csv')
nfield=length(filenames)



print("Starting...")

discipline.names =strsplit(filenames, ".csv") 
images=paste(discipline.names,".jpg",sep="")
resfiles=paste("Results_",discipline.names,".csv",sep="")
sustainability=matrix(data=0,nrow=num_fish,ncol=nfield)
 
 n_radar=round(num_fish/10,0)
 p1_radar=seq(1,n_radar*10,by=10)
 p2_radar=p1_radar
 p2_radar[1:(n_radar-1)]=p1_radar[2:n_radar]-1
 p2_radar[n_radar]=num_fish
 rad_images=paste("Radar_",seq(1,n_radar),".jpg",sep="")

for(i in 1:nfield)
{
# ----MAIN--------------------------------------------------

  fisheries.all = loaddata(filenames[i])
  n_att=ncol(fisheries.all)
  anchors=loaddata(anchor_files[n_att-3])
  n_an=nrow(anchors)
  colnames(anchors)<-colnames(fisheries.all)
  fisheries.dat=fisheries.all[1:num_fish,]
  fisheries.raw=rbind(anchors,fisheries.dat)
  fisheries.scaled = mdscale(fisheries.raw)
 
 jpeg(filename=images[i],width=20,height=16,units="cm",res=500)
 
  Res=ifelse(nfield>30,RAPplot1(fisheries.scaled,num_fish,n_an),RAPplot2(fisheries.scaled,num_fish,n_an))
   mtext(side=3, line=1, discipline.names[i],adj=0) 
   dev.off()
  write.csv(fisheries.scaled[(n_an+1):nrow(fisheries.scaled),],resfiles[i])
  sustainability[,i]= fisheries.scaled[(n_an+1):nrow(fisheries.scaled),1]
  
}  
   rownames(sustainability)<-rownames(fisheries.dat)
  colnames(sustainability)<-discipline.names 
  

#Radar plots
for(i in 1:n_radar)
{
jpeg(filename=rad_images[i],width=20,height=16,units="cm",res=500)

 radarplot(sustainability[p1_radar[i]:p2_radar[i],])
 dev.off()
}
 
  

 print("Complete.")
