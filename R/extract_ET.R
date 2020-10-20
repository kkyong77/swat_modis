library(RColorBrewer)

###################
datapath_ET<-"C:/Project/Columbia_SWAT/data/MODIS/R_MODIS/data/GapfilledET/Projected/Mosaicked/Yakima"
setwd(datapath_ET)



# reading yakima headwater ET files
list_gtiff<-list.files(pattern=glob2rx("*tif"))

# stacking files
yakima_stack<-stack(list_gtiff)

# 
# #extract raster cell count (sum) within each polygon area (poly)
# #https://rpubs.com/rural_gis/254726
# s<-yakima_stack
# poly<-Yakima_shape
# # creating a data frame for saving extracted ET values
# ET<-matrix(0,ncol=1,nrow=length(list_gtiff))
# 
# for (i in 1:length(list_gtiff)){
#   ex <- extract(s[[i]], poly, fun=mean, na.rm=TRUE, df=TRUE)
#   # saving extracted mean et values
#   ET[i]<-ex[,2]
# }
# 



datapath_ET_ry<-"C:/Project/Columbia_SWAT/data/MODIS/R_MODIS/data/GapfilledET/Projected/Mosaicked/Reynolds"
setwd(datapath_ET_ry)



# reading yakima headwater ET files
list_gtiff<-list.files(pattern=glob2rx("*_mask.tif"))

# stacking files
rey_stack<-stack(list_gtiff)


tiff("C:/Project/Columbia_SWAT/data/MODIS/Reynold_MODISET_V300.tif", width = 4, height = 6, units = 'in', res = 300)

par(cex.main=0.8,cex=1,cex.lab=1.2,cex.axis=1.2)  
cols <- colorRampPalette(brewer.pal(9,"YlGn"))

levelplot(rey_stack[[750]]/8,col.regions=cols,main="MODIS ET(mm/day) \n  2016-07-11 ",draw=FALSE,margin=FALSE,cex.main=0.8)

dev.off()




#https://stackoverflow.com/questions/52975370/8-day-modis-raster-to-monthly-average-in-r
#https://gis.stackexchange.com/questions/237272/mean-by-month-on-r-stacked-raster

library(gridExtra)
# color 


p1 <- levelplot(yakima_stack[[10]],col.regions=cols) +latticeExtra::layer(sp.polygons(YakimaW_shape, fill='white', alpha=0.5))

p2 <- levelplot(and_stack[[10]],col.regions=cols) +latticeExtra::layer(sp.polygons(AndrewW_shape, fill='white', alpha=0.5))






grid.arrange(p1, p2,p3, ncol=3)

print(p1, split=c(1,1,2,1), more=TRUE)
print(p2, split=c(2,1,2,1),more=TRUE)
print(p2, split=c(2,2,2,2))


# area weighted averaged values
# reading shapefiles
datapath<-"C:/Project/Columbia_SWAT/data/shapefiles"
# #C:\Project\Columbia_SWAT\data\shapefiles
# CR_shape<-readOGR(datapath,layer="CR84_1km")
# Yakima_shape<-readOGR(datapath,layer="Yakima84")
# Prya_shape<-readOGR(datapath,layer="priest_yakima_wgs84")
# YakimaW_shape<-readOGR(datapath,layer="YakimaW84")
# YakimaHRU_shape<-readOGR(datapath,layer="Yakima_HRU84")
require(sf)
require(exactextractr)

s_poly<-st_read(datapath,"YakimaW84")
s_poly<-s_poly[,"GRIDCODE"]

plot(s_poly,main="Subbasin-ID")

# creating matrix files
x<-matrix(0,ncol=5,nrow=length(list_gtiff))


#calculateing weighted mean----
for (i in 1:length(list_gtiff)){
  x[i,] <- exact_extract(yakima_stack[[i]],s_poly, weighted.mean, na.rm=TRUE)
  }

### Reynolds


r_poly<-st_read(datapath,"ReynoldsW84")
r_poly<-r_poly[,"GRIDCODE"]

plot(r_poly,main="Subbasin-ID")

# creating matrix files
y<-matrix(0,ncol=7,nrow=length(list_gtiff))


#calculateing weighted mean----
for (i in 1:length(list_gtiff)){
  y[i,] <- exact_extract(rey_stack[[i]],r_poly, weighted.mean, na.rm=TRUE)
}

### Andrew

a_poly<-st_read(datapath,"AndrewW84")
a_poly<-a_poly[,"GRIDCODE"]

plot(a_poly,main="Subbasin-ID")

# creating matrix files
z<-matrix(0,ncol=5,nrow=length(list_gtiff))


#calculateing weighted mean----
for (i in 1:length(list_gtiff)){
  z[i,] <- exact_extract(and_stack[[i]],a_poly, weighted.mean, na.rm=TRUE)
}




#write to a data frame
require(lubridate)

names<-names(yakima_stack)
yday<-substr(names,8,14)
year<-as.numeric(substr(yday,1,4))
doy<-as.numeric(substr(yday,5,7))# ploting lai values per subbasin
date<-strptime(paste0(year,doy),format="%Y %j")

x<-data.frame(x)
x$date<-date
x$month<-month(x$date)
x$year<-year(x$date)

df<-x

#write to a data frame

write.csv(df,file="C:/Project/Columbia_SWAT/Yakima_SWAT/obs/8daysET500m_yakima_subbasin.csv",row.names =FALSE)



names<-names(rey_stack)
yday<-substr(names,8,14)
year<-as.numeric(substr(yday,1,4))
doy<-as.numeric(substr(yday,5,7))# ploting lai values per subbasin
date<-strptime(paste0(year,doy),format="%Y %j")

y<-data.frame(y)
y$date<-date
y$month<-month(y$date)
y$year<-year(y$date)

df2<-y

#write to a data frame

write.csv(df2,file="C:/Project/Columbia_SWAT/Reynold_SWAT30m/obs/8daysET500m_reynolds_subbasin.csv",row.names =FALSE)


names<-names(and_stack)
yday<-substr(names,8,14)
year<-as.numeric(substr(yday,1,4))
doy<-as.numeric(substr(yday,5,7))# ploting lai values per subbasin
date<-strptime(paste0(year,doy),format="%Y %j")

z<-data.frame(z)
z$date<-date
z$month<-month(z$date)
z$year<-year(z$date)

df3<-z

#write to a data frame

write.csv(df3,file="C:/Project/Columbia_SWAT/Andrew_SWAT30m/obs/8daysET500m_andrew_subbasin.csv",row.names =FALSE)


### HRU ET model outputs
hru_poly<-st_read(datapath,"Yakima_HRU84")
hru_poly<-hru_poly[,"HRUGIS"]

s<-yakima_stack
poly<-YakimaHRU_shape

yy<-matrix(0,ncol=nrow(hru_poly),nrow=length(list_gtiff))

#calculateing weighted mean----
for (i in 1:length(list_gtiff)){
  yy[i,] <- exact_extract(yakima_stack[[i]],hru_poly, weighted.mean, na.rm=TRUE)
}


yy<-data.frame(yy)
yy$date<-date
yy$month<-month(yy$date)
yy$year<-year(yy$date)

df2<-yy
#

write.csv(df2,file="C:/Project/Columbia_SWAT/Yakima_SWAT/obs/8daysET500m_HRU_yakima.csv",row.names =FALSE,C)


#1)
# 8days to monthly values
#https://stackoverflow.com/questions/52975370/8-day-modis-raster-to-monthly-average-in-r

# this function calculate the 8days ET to monthly ET per each time series
# x format
# year,ET, and month
  monthly_MODIS_ET<-function(df){
  ET_month=matrix(0,ncol=12,nrow=length(unique(df$year)))
  ET_month_ts=matrix(0,ncol=1,nrow=length(unique(df$year))*12)
  
  m=1
  for (k in 1:length(unique(df$year))){
    
    year<-unique(df$year)[k]
    # number of days in that year (leap year or not?)
    ndays <- ifelse(((year %% 100 != 0) & (year %%4 ==0)) | (year %% 400==0), 366 , 365)
    
    # how many layers?
    n <- ceiling(ndays/8) 
    # day of year for each layer
    nn <- rep(1:n, each=8)[1:ndays] 
    
    # day of year for each month
    m <- as.integer(format(as.Date(1:ndays, origin=paste0(year-1, "-12-31")), "%m"))
    x <- cbind(layer=nn, month=m)
    weights <- table(x[,1], x[,2])
    
    xx<-df$ET[df$year==year]
    
    j=1
    for (i in 1:12) {
      w <- weights[,i]
      ww <- w[w > 0] / 8
      tmp<-weighted.mean(xx[j:(j+length(ww)-1)], ww)*length(ww)
      j=j+length(ww)-1
      ET_month[k,i]=tmp
      ET_month_ts[m]<-ET_month[k,i]
      m=m+1
    }
    
  }
  
  # arrange monthyl values
  m=1
  for (k in 1:length(unique(df$year))){
    for (i in 1:12) {
      ET_month_ts[m]<-ET_month[k,i]
      m=m+1
    }
  }
  
  return(ET_month_ts)
  
}

  
  
  
  ### Yakima
  dff1<-as.data.frame(df[,c(1,7,8)])
  colnames(dff1)<-c("ET","month","year")
  
  dff2<-as.data.frame(df[,c(2,7,8)])
  colnames(dff2)<-c("ET","month","year")
  
  dff3<-as.data.frame(df[,c(3,7,8)])
  colnames(dff3)<-c("ET","month","year")
  
  dff4<-as.data.frame(df[,c(4,7,8)])
  colnames(dff4)<-c("ET","month","year")
  
  dff5<-as.data.frame(df[,c(5,7,8)])
  colnames(dff5)<-c("ET","month","year")
  
  m1<-monthly_MODIS_ET(dff1)
  m2<-monthly_MODIS_ET(dff2)
  m3<-monthly_MODIS_ET(dff3)
  m4<-monthly_MODIS_ET(dff4)
  m5<-monthly_MODIS_ET(dff5)
  
  ET_month_ts<-as.data.frame(cbind(m1,m2,m3,m4,m5))
  
  date_month<-seq(as.Date("2000/1/1"), by="month", length=132)
  
  ET_month_ts$date<-date_month
  ET_month_ts$mean<-apply(ET_month_ts[,c(1:5)],1,mean)
  ET_month_ts$sd<-apply(ET_month_ts[,c(1:5)],1,sd)
  ET_month_ts$month<-month(ET_month_ts$date)
  ET_month_ts$year<-year(ET_month_ts$date)
  
  write.csv(ET_month_ts,file="C:/Project/Columbia_SWAT/Yakima_SWAT/obs/monthly_ET500m_Yakima.csv",row.names =FALSE)
  
  
### Reynolds
df2_1<-as.data.frame(df2[,c(1,9,10)])
colnames(df2_1)<-c("ET","month","year")

df2_2<-as.data.frame(df2[,c(2,9,10)])
colnames(df2_2)<-c("ET","month","year")

df2_3<-as.data.frame(df2[,c(3,9,10)])
colnames(df2_3)<-c("ET","month","year")

df2_4<-as.data.frame(df2[,c(4,9,10)])
colnames(df2_4)<-c("ET","month","year")

df2_5<-as.data.frame(df2[,c(5,9,10)])
colnames(df2_5)<-c("ET","month","year")

df2_6<-as.data.frame(df2[,c(6,9,10)])
colnames(df2_6)<-c("ET","month","year")

df2_7<-as.data.frame(df2[,c(7,9,10)])
colnames(df2_7)<-c("ET","month","year")

m1<-monthly_MODIS_ET(df2_1)
m2<-monthly_MODIS_ET(df2_2)
m3<-monthly_MODIS_ET(df2_3)
m4<-monthly_MODIS_ET(df2_4)
m5<-monthly_MODIS_ET(df2_5)
m6<-monthly_MODIS_ET(df2_6)
m7<-monthly_MODIS_ET(df2_7)

ET_month_ts_rey<-as.data.frame(cbind(m1,m2,m3,m4,m5,m6,m7))

date_month<-seq(as.Date("2000/1/1"), by="month", length=132)

ET_month_ts_rey$date<-date_month
ET_month_ts_rey$mean<-apply(ET_month_ts_rey[,c(1:7)],1,mean)
ET_month_ts_rey$sd<-apply(ET_month_ts_rey[,c(1:7)],1,sd)
ET_month_ts_rey$month<-month(ET_month_ts_rey$date)
ET_month_ts_rey$year<-year(ET_month_ts_rey$date)

write.csv(ET_month_ts_rey,file="C:/Project/Columbia_SWAT/Reynold_SWAT30m/obs/monthly_ET500m_reynolds.csv",row.names =FALSE)



tiff("C:/Project/Columbia_SWAT/data/MODIS/monthly_ET_Reynold_MODISET_v300.tif", width = 6, height = 4, units = 'in', res = 300)

par(cex.main=1,cex=1,cex.lab=1.2,cex.axis=1.2)  
plot(date_month,ET_month_ts_rey$mean,type="l",xlab="Months",ylab="ET(mm/month)",col="red")
low=ET_month_ts_rey$mean-ET_month_ts_rey$sd
high=ET_month_ts_rey$mean+ET_month_ts_rey$sd

x<-date_month
#polygon(c(x,rev(x)),c(low,rev(high)),col="gray",border=NA)
lines(date_month,ET_month_ts_rey$mean,col="red",lwd=2)

dev.off()



### Andrew
df3_1<-as.data.frame(df3[,c(1,7,8)])
colnames(df3_1)<-c("ET","month","year")

df3_2<-as.data.frame(df3[,c(2,7,8)])
colnames(df3_2)<-c("ET","month","year")

df3_3<-as.data.frame(df3[,c(3,7,8)])
colnames(df3_3)<-c("ET","month","year")

df3_4<-as.data.frame(df3[,c(4,7,8)])
colnames(df3_4)<-c("ET","month","year")

df3_5<-as.data.frame(df3[,c(5,7,8)])
colnames(df3_5)<-c("ET","month","year")

m1<-monthly_MODIS_ET(df3_1)
m2<-monthly_MODIS_ET(df3_2)
m3<-monthly_MODIS_ET(df3_3)
m4<-monthly_MODIS_ET(df3_4)
m5<-monthly_MODIS_ET(df3_5)

ET_month_ts_and<-as.data.frame(cbind(m1,m2,m3,m4,m5))

date_month<-seq(as.Date("2000/1/1"), by="month", length=132)

ET_month_ts_and$date<-date_month

ET_month_ts_and$mean<-apply(ET_month_ts_and[,c(1:5)],1,mean)
ET_month_ts_and$sd<-apply(ET_month_ts_and[,c(1:5)],1,sd)
ET_month_ts_and$month<-month(ET_month_ts_and$date)
ET_month_ts_and$year<-year(ET_month_ts_and$date)

write.csv(ET_month_ts_and,file="C:/Project/Columbia_SWAT/Andrew_SWAT30m/obs/monthly_ET500m_andrew.csv",row.names =FALSE)

## calculating annual value of ET
ET_year_ya<-aggregate(ET_month_ts[,c(1:5,7,10)],by=list(ET_month_ts$year),sum)
ET_year_and<-aggregate(ET_month_ts_and[,c(1:5,7,10)],by=list(ET_month_ts_and$year),sum)
ET_year_rey<-aggregate(ET_month_ts_rey[,c(1:7,9,12)],by=list(ET_month_ts_rey$year),sum)


# comparing monthly ET among the three sites
par(mfrow=c(1,1))
par(mar=c(4,4,2,2),bty="l",cex=1,cex.lab=1.2,cex.axis=1.1)

plot(ET_year_ya$Group.1,ET_year_ya$mean,type="l",ylim=c(0,1000),xlab="Years",ylab="Annual ET(mm)",col="gray",lwd=2)
lines(ET_year_rey$Group.1,ET_year_rey$mean,type="l",col="red",lwd=2)
lines(ET_year_rey$Group.1,ET_year_and$mean,type="l",col="blue",lwd=2)
legend("topright",legend=c("Yakima","Reynold","Andrew"),col=c("gray","red","blue"),lwd=2,bty="n",lty=1,cex=1.2)

par(mfrow=c(1,1))
par(mar=c(4,4.5,2,2),bty="l",cex=1.1,cex.lab=1.2,cex.axis=1.1)

boxplot(ET_month_ts$mean~ET_month_ts$month,col="gray",xlab="months",ylab="monthly ET(mm)",ylim=c(0,200))
boxplot(ET_month_ts_rey$mean~ET_month_ts_rey$month,col="red",add=T)
boxplot(ET_month_ts_and$mean~ET_month_ts_and$month,col="blue",add=T)
legend("topright",legend=c("Yakima","Reynold","Andrew"),col=c("gray","red","blue"),lwd=2,bty="n",lty=1)


par(mfrow=c(1,1))
par(mar=c(4,4,2,2),bty="l",cex=1,cex.lab=1.2,cex.axis=1.1)

plot(ET_year_ya$Group.1,ET_year_ya$mean,type="l",ylim=c(0,1000),xlab="Years",ylab="Annual ET(mm)",col="gray",lwd=2)
lines(ET_year_rey$Group.1,ET_year_rey$mean,type="l",col="red",lwd=2)
lines(ET_year_rey$Group.1,ET_year_and$mean,type="l",col="blue",lwd=2)
legend("topright",legend=c("Yakima","Reynold","Andrew"),col=c("gray","red","blue"),lwd=2,bty="n",lty=1,cex=1.2)


boxplot(ET_month_ts$mean~ET_month_ts$month,col="gray",xlab="months",ylab="monthly ET(mm)",ylim=c(0,200))
boxplot(ET_month_ts_rey$mean~ET_month_ts_rey$month,col="red",add=T)
boxplot(ET_month_ts_and$mean~ET_month_ts_and$month,col="blue",add=T)


boxplot(ET_month_ts$sd~ET_month_ts$month,col="gray",xlab="months",ylab="S.D of monthly ET(mm)",ylim=c(0,20),main="Spatial varaition of ET within watersheds")
boxplot(ET_month_ts_rey$sd~ET_month_ts_rey$month,col="red",add=T)
boxplot(ET_month_ts_and$sd~ET_month_ts_and$month,col="blue",add=T)
legend("topright",legend=c("Yakima","Reynold","Andrew"),col=c("gray","red","blue"),lwd=2,bty="n",lty=1,cex=1.2)


#NCG_TP_wf_mon$date <- as.Date(with(, paste(Group.2,Group.1,01,sep="-")), "%Y-%m-%d")

par(mar=c(4,4,1,1),bty="l",cex=1,cex.lab=1.2,cex.axis=1.2)
plot(date_month,ET_month_ts$V1,xlab="months",ylab="monthly ET(mm/month)",type="l")
lines(date_month,ET_month_ts$V2,col="gray")
lines(date_month,ET_month_ts$V3,col="gray")
lines(date_month,ET_month_ts$V4,col="gray")
lines(date_month,ET_month_ts$V5,col="gray")

ET_month_ts$month<-month(ET_month_ts$date)
ET_month_ts$year<-year(ET_month_ts$date)

tmp<-ET_month_ts
tmp$date<-NULL
ET_year_ts=aggregate(tmp,by=list(tmp$year),sum,na.rm=T)

plot(ET_year_ts$Group.1,ET_year_ts[,2],type="l",xlab="Year",ylab="Annual ET(mm/year)",ylim=c(500,600))
lines(ET_year_ts$Group.1,ET_year_ts[,3],col="lightblue",lwd=2)
lines(ET_year_ts$Group.1,ET_year_ts[,4],col="gray")
lines(ET_year_ts$Group.1,ET_year_ts[,5],col="gray")
lines(ET_year_ts$Group.1,ET_year_ts[,6],col="gold",lwd=2)

write.csv(ET_month_ts,file="C:/Project/Columbia_SWAT/Yakima_SWAT/obs/monthly_ET500m_yakima.csv",row.names =FALSE)




## 2) model daily ET estimates converting to 8days estimates

# reading daily ET values
df2<-read.csv("df2.csv")

dailyET_8daysET<-function(x){
  
  k=1
  year<-unique(x$year)[k]
  # number of days in that year (leap year or not?)
  ndays <- ifelse(((year %% 100 != 0) & (year %%4 ==0)) | (year %% 400==0), 366 , 365)
  
  # how many layers?
  n <- ceiling(ndays/8) 
  # day of year for each layer
  nn <- rep(1:n, each=8)[1:ndays] 
  
  tmp<-subset(x,x$year==year[1])
  
  tmp$nn<-nn
  
  tmp2<-aggregate(tmp[,c("ET","nn")],by=list(tmp$nn),sum)
  
  for (k in 2:length(unique(x$year))){
    
    year<-unique(x$year)[k]
    # number of days in that year (leap year or not?)
    ndays <- ifelse(((year %% 100 != 0) & (year %%4 ==0)) | (year %% 400==0), 366 , 365)
    
    # how many layers?
    n <- ceiling(ndays/8) 
    # day of year for each layer
    nn <- rep(1:n, each=8)[1:ndays] 
    
    tmp<-x[which(x$year==year),]
    
    
    tmp$nn<-nn
    
    tmp3<-aggregate(tmp[,c("ET","nn")],by=list(tmp$nn),sum)
    
    # combining the two time series
    tmp2<-rbind(tmp2,tmp3)
    
  }
  
  return(tmp2$ET)
  
}


## 3) using reference ET (or NLDAS ET estimates) to rescale 8days MODIS ET and convert daily ET estimates






# create time series as basin average/variance---------- 
library(lubridate)
year<-as.numeric(substr(yday,1,4))
dayofyear<-as.numeric(substr(yday,5,7))
DOY<-dayofyear
#date<-as.Date(dayofyear,orgin="2002-01-01")

date<-strptime(paste0(year,dayofyear),format="%Y %j")

month<-month(date)
day<-day(date)

pr_ET_mean<-0
pr_ET_sd<-0
pr_ET_min<-0
pr_ET_max<-0
pr_ET_range<-0
pr_ET_var<-0
pr_ET_pixel<-0
pr_ET_sum<-0
pr_ET_NA_pixel<-0
pr_ET_NA_per<-0
pr_ET<-priest_ET

for (i in 1:length(ET_list))
{
  
  pr_ET_mean[i]<-cellStats(pr_ET[[i]], mean,na.rm=TRUE)
  pr_ET_sd[i]<-cellStats(pr_ET[[i]], sd,na.rm=TRUE)
  pr_ET_min[i]<-cellStats(pr_ET[[i]], min,na.rm=TRUE)
  pr_ET_max[i]<-cellStats(pr_ET[[i]], max,na.rm=TRUE)
  pr_ET_sum[i]<-cellStats(pr_ET[[i]],sum,na.rm=TRUE)
  pr_ET_var[i]<-cellStats(pr_ET[[i]], var,na.rm=TRUE)
  pr_ET_pixel[i]<-length(pr_ET[[i]])
  pr_ET_NA_pixel[i]<-cellStats(is.na(pr_ET[[i]]),sum)
  pr_ET_NA_per[i]<-pr_ET_NA_pixel[i]/pr_ET_pixel[i]*100
  
}





levelplot(tmp2[[1]], col.regions=cols) +latticeExtra::layer(sp.polygons(priest_shape, fill='white', alpha=0.5))

levelplot(tmp_crop0.1, col.regions=cols) +latticeExtra::layer(sp.polygons(priest_shape, fill='white', alpha=0.5))

levelplot(tmp_crop, col.regions=cols) +latticeExtra::layer(sp.polygons(priest_shape, fill='white', alpha=0.5))

levelplot(tmp_crop[3], col.regions=cols) +latticeExtra::layer(sp.polygons(priest_shape, fill='white', alpha=0.5))

