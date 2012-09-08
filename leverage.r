#Leverage for Rapfish
# 2012-04-15 Divya Varkey  Created


#to be obtained from site
num_fish=53
anchor_files=c('anchors4.csv','anchors5.csv','anchors6.csv','anchors7.csv','anchors8.csv','anchors9.csv','anchors10.csv','anchors11.csv','anchors12.csv')
filenames=c('CCRF_Field1.csv','CCRF_Field2.csv','CCRF_Field3.csv','CCRF_Field4.csv','CCRF_Field5.csv','CCRF_Field6.csv')
nfield=length(filenames)
discipline.names =strsplit(filenames, ".csv") 
levfiles=paste("Leverage_",discipline.names,".csv",sep="")

source("rapfish_functions.R")

for(i in 1:nfield) {
  fisheries.all = loaddata(filenames[i])
  n_att=ncol(fisheries.all)
  anchors=loaddata(anchor_files[n_att-3])
  colnames(anchors)<-colnames(fisheries.all)
  n_an=nrow(anchors)
  plot1=n_an+1
  plot2=n_an+num_fish

  fisheries.dat=fisheries.all[1:num_fish,]
  fisheries.raw=rbind(anchors,fisheries.dat)
  fisheries.scaled = mdscale(fisheries.raw)
  res_main_x=fisheries.scaled[plot1:plot2,1]
  res_main_y=fisheries.scaled[plot1:plot2,2]

  fish_lv_res_x=matrix(data=0,nrow=num_fish,ncol=n_att)
  fish_lv_res_y=matrix(data=0,nrow=num_fish,ncol=n_att)

  anchors=loaddata(anchor_files[n_att-3-1])

  for(lv in 1:n_att) {
    fish_lv=fisheries.dat[,-lv]  
    colnames(anchors)<-colnames(fish_lv)
    n_an=nrow(anchors)
    plot1=n_an+1
    plot2=n_an+num_fish
    fish_lv.raw=rbind(anchors,fish_lv)
    fish_lv.scaled = mdscale(fish_lv.raw)
    fish_lv_res_x[,lv]=fish_lv.scaled[plot1:plot2,1]
    fish_lv_res_y[,lv]=fish_lv.scaled[plot1:plot2,2]
  }

  colnames(fish_lv_res_x)<-colnames(fisheries.all)
  colnames(fish_lv_res_y)<-colnames(fisheries.all)

  sumsqx=colSums((fish_lv_res_x-res_main_x)^2)
  sumsqy=colSums((fish_lv_res_y-res_main_y)^2)

  lev=cbind(sqrt(sumsqx/num_fish),sqrt(sumsqy/num_fish))
  colnames(lev)<-c("X_scores","Y_scores")
  write.csv(lev,levfiles[i])
}
