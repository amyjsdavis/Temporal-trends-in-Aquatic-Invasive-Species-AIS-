##Temporal species accumulation curve for exotic plant species occurence data up to 2005.


##load data
library(iNEXT)
source('D:/Data/R_projects/new_plants/TemporalTrends/temporalSACplants.R')
comp<-Plants_DB_conus_hucs_dated_LL
comp$xHUC8<-paste("x",comp$HUC8,sep="")##rename HUC8s


#simple counting function
 countfunct<-function(x){
y<-length(x)
}

trunc<-comp[which(comp$year<=2005),]##subset data

####prepare data for iNEXT
trunccounts<-aggregate(trunc$sciName,by=list(trunc$year,trunc$sciName,trunc$xHUC8),FUN=countfunct)
head(trunccounts)
w<-c(1900,1925,1945,1965,1985)  ##create vector to define time intervals
trunccounts$time20int<-findInterval(trunccounts$Group.1,w) #add interval to dataframe

trunc.df<-split(trunccounts,trunccounts$time20int) ##split df by time interval

##count occurrence of each species in every HUC8 for each time period
trunc20.df.counts<-lapply(trunc.df, function (x) aggregate(x$Group.2,by=list(x$Group.3,x$Group.2),FUN=countfunct))

#rename dataframes from integer 
names(trunc20.df.counts)[names(trunc20.df.counts)=="0"]<-"t0"
names(trunc20.df.counts)[names(trunc20.df.counts)=="1"]<-"t1"
names(trunc20.df.counts)[names(trunc20.df.counts)=="2"]<-"t2"
names(trunc20.df.counts)[names(trunc20.df.counts)=="3"]<-"t3"
names(trunc20.df.counts)[names(trunc20.df.counts)=="4"]<-"t4"
names(trunc20.df.counts)[names(trunc20.df.counts)=="5"]<-"t5"
##run table function to get matrix of species occurence by HUC8 
trunctab_0<-table(trunc20.df.counts$t0$Group.2,trunc20.df.counts$t0$Group.1)
trunctab_1<-table(trunc20.df.counts$t1$Group.2,trunc20.df.counts$t1$Group.1)
trunctab_2<-table(trunc20.df.counts$t2$Group.2,trunc20.df.counts$t2$Group.1)
trunctab_3<-table(trunc20.df.counts$t3$Group.2,trunc20.df.counts$t3$Group.1)
trunctab_4<-table(trunc20.df.counts$t4$Group.2,trunc20.df.counts$t4$Group.1)
trunctab_5<-table(trunc20.df.counts$t5$Group.2,trunc20.df.counts$t5$Group.1)
##must remove the 'table" classclass from the data for it to work in iNEXT since first row must be sample size
trunctab0<-unclass(trunctab_0)
trunctab1<-unclass(trunctab_1)
trunctab2<-unclass(trunctab_2)
trunctab3<-unclass(trunctab_3)
trunctab4<-unclass(trunctab_4)
trunctab5<-unclass(trunctab_5)
trunctabs_list<-list(trunctab0,trunctab1,trunctab2,trunctab3,trunctab4,trunctab5)
str(trunctabs_list) ##obtain number of records from this in oto determine endpoint for iNext 
## determine endpoint
#sample size
# t0  214
# t1	311
# t2	535
# t3	608
# t4	939
# t5	1079

# r = 2 x the min. sample size (428)
# if the maximum sample size is larger than r, use the maximum sample size at the endpoint according to Chao et al., 2014


### then must name dfs or it will return an error when running iNEXT
names(trunctabs_list)<-c("t0","t1","t2","t3","t4","t5")

##run iNEXT
trunc.raw<-iNEXT(trunctabs_list,datatype="incidence_raw",endpoint=428)
##modify default label using ggplot2
library(ggplots2)
g<-ggiNEXT(trunc.raw)
g1<-g+labs(x="Number of sampling units",y="Species Richness")
plot(g1)

##compare temporal communities by coverageh<-ggiNEXT(trunc.raw,type=3,color.var="site")
h<-ggiNEXT(trunc.raw,type=3,color.var="site")
h1<-h+labs(x="Number of sampling units",y="Species Richness")
plot(h1)

##extract asymptotic SR information 
plant2<-trunc.raw$iAsyEst
write.csv(plant2,"newplant_Asymp.csv")





