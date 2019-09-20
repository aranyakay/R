#######################################################
##############systematic error check###################
#####################for all data######################
##all clinical test result-full
#1 read in site data
#2 calculate basic characteristics by var 
#3 add extra information vars
#4 combine result
#5 save the result table
#######################################################
#library
library(dplyr)
library(readr)

#today's date
today = format(Sys.Date(),"%Y%m%d")

#######################################################
#######################################################
#function
#to check if binary
#non-numeric variables are also supported and return F
is.binary = function(v, naVal="NA") {
  if (is.character(v)) "string"
  else{
    if(is.logical(v)) "log"
    else{
      vSet = unique(v)
      if (!missing(naVal)) vSet[vSet == naVal] = NA
      vSet = vSet[!is.na(vSet)]
      
      if (any(as.integer(vSet) != vSet) || length(vSet) > 2) "con"
      else "bin"
    }
  }
}


#######################################################
#path
codepath = "D:/cj/code/R/"
resultpath = "D:/cj/output/"
dataraw = "D:/cj/data/raw_data/"
dataclean = "D:/cj/data/clean_data/"

#today's date
today = format(Sys.Date(),"%Y%m%d")

#default utf-8
#help chinese input
#in terminal shift+alt+T
# defaults write org.R-project.R force.LANG en_US.UTF-8
#######################################################

# fileEncoding = 'UTF-8', encoding="UTF-8",



#build file list
fileList = list.files(pattern = "*.csv", path = paste0(dataraw,"ori", sep=""))
fileList = fileList[ordered(fileList)]
#load in codebook
codebook = read.csv(paste(dataclean, "codebook_full20190611.csv", sep=""),
                     stringsAsFactors = F)


#load in dup variables
dup = read.csv(paste(dataclean, "varList_dup7020190611.csv", sep=""),
                     stringsAsFactors = F)


#loop
sumData <- rep(NA, 11)
for(f in 1:length(fileList))
{
  data1 <- read.csv(paste(dataraw,"ori/", fileList[f], sep=""), 
                     stringsAsFactors = F)
  # colnames()
  #remove all character variables
  data <- data1[,sapply(data1, function(x)!is.character(x))]

  #get site information
  site = as.numeric(gsub("[^0-9]", "", fileList[f]))
  #get variable information
  
  ###########################################
  #to check if binary
  bin <- rep(NA, ncol(data))
  for(i in 1:ncol(data)){
    bin[i] <- is.binary(data[, i])
  }
  ##########################################
  
  #build table of var characteristics
  varChar <- cbind.data.frame(colnames(data), bin)  
  colnames(varChar) <- c("varNames", "bin")
  
  #convert all logical var to numeric
  data[,which(varChar$bin=="log")] <- data.frame(lapply(data[,which(varChar$bin=="log")], 
                                                        function(x) as.numeric(x)))
  
  sumData0 <- do.call(cbind, lapply(data, summary))
  description0 <- colnames(sumData0)
  sumDataT <- t(sumData0)
  sumDataT <- as.data.frame(sumDataT)
  sumDataT$site <- site
  sumDataT$varNames <- description0
  sumDataT$sd <- apply(data, 2, function(x)sd(as.numeric(x), na.rm = T))
  sumDataT$nrow <- nrow(data)
  sumData <- rbind.data.frame(sumData, sumDataT)
  
}
    

sumData <- sumData[!is.na(sumData$nrow), ]
sumData$dup <- ifelse(sumData$varNames %in% dup$description, 1, 0)

# write.csv(sumData, paste(resultpath,"varAll_summary_bySite",today,".csv",sep=""))



#only test duplicated vars
sumDataDup <- sumData[sumData$dup==1, ]

#calculate whole mean by var
test <- sumDataDup %>% 
  group_by(varNames) %>%
  summarise(sMean = sum(Mean*nrow, na.rm=T)/sum(nrow, na.rm=T))


varNames <- unique(sumDataDup$varNames)
# for(i in 1:length(varNames)){
#   varChar$mean[varChar$varNames==varNames[i]] <- 
#     sum(sumDataDup$Mean[sumDataDup$var==varNames[i]]*sumDataDup$nrow[sumDataDup$var==varNames[i]])
# }
#get dup information in varchar
varChar$dup <- ifelse(varChar$varNames %in% dup$description, 1, 0)
#calculate whole sample mean
for(i in 1:nrow(varChar)) varChar$mean[i] = ifelse(varChar$dup[i] ==1, 
                                                   test$sMean[test$varNames==varChar$varNames[i]], NA)

for(i in 1:nrow(sumDataDup)) sumDataDup$Smean[i] = ifelse(sumDataDup$dup[i] ==1, 
                                                   test$sMean[test$varNames==sumDataDup$varNames[i]], NA)
colnames(sumDataDup)[1:11] <- c("Min","Q1st","Median","Mean","Q3rd","Max","NAs","site","varNames","sd","nrow")
sumDataDup <- sumDataDup %>%
  mutate(diff5 = ifelse(abs(Mean-Smean)/Smean >= .05, 1, 0), diff10 = ifelse(abs(Mean-Smean)/Smean >= .1, 1, 0),
         diff15 = ifelse(abs(Mean-Smean)/Smean >= .15, 1, 0), diff20 = ifelse(abs(Mean-Smean)/Smean >= .2, 1, 0),
         diff20nExists = ifelse((abs(Mean-Smean)/Smean >= .2) & (nrow - NAs) > 1000, 1, 0))

# write.csv(sumDataDup, paste(resultpath,"varAllDup_summary_bySite",today,".csv",sep=""))


#might better try median?
##################################################################
##################################################################
#median test begin

for(i in 1:nrow(varChar)) varChar$varNO[i] <- 
  ifelse(varChar$varNames[i] %in% dup$description, dup$varNames[dup$description==varChar$varNames[i]], NA)

varNoList <- na.omit(unique(varChar$varNO))

#calculate whole mean by var
test1 <- read.csv(paste(dataclean, "combine50_all_20190612.csv", sep=""),
                  stringsAsFactors = F)

test1 <- test1[,varNoList[1:42]]
test1$szy <- as.numeric(test1$szy)

Smedian <- rep(NA, ncol(test1))
for(i in 1:ncol(test1))Smedian[i] = median(test1[,i], na.rm=T)
#Smedian <- apply(test1,2, function(x)median(x, na.rm=T))


cols <- colnames(test1)
test1 <- NA


test2 <- cbind.data.frame(cols, Smedian)


test2$varNames = NA
test2$cols <- as.character(test2$cols)
for(i in 1:nrow(test2))test2$varNames[i] = dup$description[dup$varNames==test2$cols[i]]


# test2$varNames[1] <- as.character(varChar$varNames[1])
# test2$varNames[2] <- "f_NO"

for(i in 1:nrow(varChar))varChar$median[i] <- 
  ifelse(varChar$dup[i]==1, test2$Smedian[test2$varNames==as.character(varChar$varNames[i])], NA)

# write.csv(varChar, paste(resultpath,"just_for_safe",today,".csv",sep=""))

varMiss<- as.character(varChar$varNames[is.na(varChar$median)&varChar$dup==1])
varMissNo <- dup$varNames[dup$description %in% varMiss]

#reload 70var full file and calculate the missing of this two


#test <- dataframe
siteM <- read.csv(text = "var1,var2,var3")
for(f in 1:10){
  test1 <- read.csv(paste(dataraw,"ori/", fileList[f], sep=""), 
                        stringsAsFactors = F)

  test1 <- test1[, varNames[2:44]]
  site <- rep(f, ncol(test1))
  siteMedian <- rep(NA, ncol(test1))  
  for(i in 1:ncol(test1))siteMedian[i] <- median(as.numeric(test1[, i]), na.rm=T)
  siteMedian <- cbind.data.frame(siteMedian, site, varNames[2:44])
  siteM <- rbind.data.frame(siteM,c(NA, f, NA), siteMedian)
  
  # test1 <- test1[,varMiss]
  # test <- rbind.data.frame(test, test1)
  if(f==1) test <- test1  else test <- rbind.data.frame(test, test1)
}

sumDataDup0 <- merge(sumDataDup, siteM, by.x=c("varNames", "site"), by.y=c("varNames[2:44]", "site"), all.x=T)


for(i in 2:length(varNames)){
  varChar$median[varChar$varNames==varNames[i]] <-  median(as.numeric(test[,varNames[i]]),na.rm = T)
}
# varChar$median[varChar$varNames==varMiss[1]] <- median(test[,varMiss[1]],na.rm = T)
# 
# varChar$median[varChar$varNames==varMiss[2]] <- median(test[,varMiss[2]],na.rm = T)

sumDataDup$Smedian <- NA
for(i in 1:nrow(sumDataDup))sumDataDup$Smedian[i] = ifelse(sumDataDup$dup[i] ==1, 
                                                  varChar$median[varChar$varNames==sumDataDup$varNames[i]], NA)
#colnames(sumDataDup)[1:11] <- c("Min","Q1st","Median","Mean","Q3rd","Max","NAs","site","varNames","sd","nrow")
sumDataDup <- sumDataDup %>%
  mutate(diff5MD = ifelse(abs(Median-Smedian)/Smedian >= .05, 1, 0), 
         diff10MD = ifelse(abs(Median-Smedian)/Smedian >= .1, 1, 0),
         diff15MD = ifelse(abs(Median-Smedian)/Smedian >= .15, 1, 0), 
         diff20MD = ifelse(abs(Median-Smedian)/Smedian >= .2, 1, 0),
         diff20MDnExists = ifelse((abs(Median-Smedian)/Smedian >= .2) & (nrow - NAs) > 1000, 1, 0))

# write.csv(sumDataDup, paste(resultpath,"varAllDup_summary_bySite_v2",today,".csv",sep=""))








######################
codepath = "code/tj_all/"
datapath = "data/raw/zf190606/tj_all/cat/"

#today's date
today = format(Sys.Date(),"%Y%m%d")



##########
#read in file
#fill 70var table
#all current data
######################

dup = read.csv("data/summary/varList_dup7020190611.csv", stringsAsFactors = F,fileEncoding = 'GBK')
codebook = read.csv("data/summary/codebook_full20190611.csv", stringsAsFactors = F,fileEncoding = 'GBK')
fileList = read.csv("data/summary/fileList20190611.csv", stringsAsFactors = F,fileEncoding = 'GBK')

#build empty matrix
data = matrix(,,71)
colnames(data) = dup$varNames
dup$description = as.character(dup$description)
#data = as.data.frame(data)


#from codebook get only TRUE dup var
codebook1 = codebook[codebook$dup_all==T,]
for(i in 1:nrow(fileList))codebook1$f_NO[codebook1$file==fileList$x[i]] = fileList$X[i]

f_NO = sort(as.numeric(unique(codebook1$f_NO)))

#section = sort(as.numeric(unique(codebook1$section[codebook1$f_NO==1])))


#loop begin
library(plyr)
# library(xlsx)
#read in file by f_no by section
#only get ID when f_no==1
#others match description in dup$description[2:71]

for(f in 1:f_NO){

  section = sort(as.numeric(unique(codebook1$section[codebook1$f_NO==f])))  #prepare section for each file

  for(s in 1:length(section)){  #inner loop(2) begin
    
    var=NA #pre set var to NA
    
    data1 = read.csv(paste(datapath,"fulldata_f",f,"_",section[s],"_20190611.csv",sep=""),
                     stringsAsFactors = F, fileEncoding = 'UTF-8',encoding="UTF-8",header=F,skip=1)
    #data1 = as.data.frame(data1)
    if(s==1)colnames(data1)[2]=dup$description[1]
    data1 = data1[,colnames(data1) %in% dup$description]
    for(i in 1:ncol(data1))var[i] = dup$varNames[dup$description==colnames(data1)[i]]
    colnames(data1) = var
    if(s==1){
      data = merge(data,data1, by=var, all=T)
    }else{
      for(v in 2:length(var)){
        data[data1$tjbh,var[v]] = data[,var[v]]
      }
    }
    
    
    data = data[!is.na(data$tjbh),]
    print(paste("f_NO",f,", section", section[s]))
  }
}    

#current = nrow(data)



# for(i in 1:nrow(data1)){
#   start = ifelse(s==1,1,2)
#   for(j in start:ncol(data1)){
#     var = dup$varNames[dup$description==colnames(data1)[j]]
#     
#    data[current+i,var]=data1[i,j] 
#     
#     
#   }
# }
#data = rbind.fill(data[colnames(data1)],data1)





combine0 = read.csv("data/clean/combine50_all_20190612.csv", stringsAsFactors = F,fileEncoding = 'GBK')
dup = read.csv("data/summary/varList_dup50_20190612.csv", stringsAsFactors = F,fileEncoding = 'GBK')

#calculate some basic characteristics

#missing pctn
missingPctn = function(x) sum(is.na(x))/length(x)

#get a missing table by var by section

missingT = matrix(,ncol(combine0),10 )

missingT = as.data.frame(missingT)

for(i in 3:ncol(combine0)){
  for(j in 1:10){
    missingT[i,j] = missingPctn(combine0[combine0$f_NO==j,i])
  }
}

missingT$varNames= colnames(combine0)
colnames(missingT)[1:10]=paste("site",1:10,sep="")
missingT = missingT[!is.na(missingT[,1]),]

for(i in 1:nrow(missingT)) missingT$description[i] = ifelse(missingT$varNames[i] %in% dup$varNames,
                                                    dup$description[dup$varNames==missingT$varNames[i]], "文件编号")


#modify var char
combine0$szy = as.numeric(combine0$szy)

#test for int var
library(plyr)
count(combine0,"nt")
count(combine0,"jtdb")

count(combine0,"nyxsy")

count(combine0,"ndy")

count(combine0,"ntt")

count(combine0,"ndb")




head(combine0$xqgbzam[combine0$f_NO==4])

#gender missing?

head(combine0[is.na(combine0$xb),c(13:20)])






##########################################
##this is a test for read in all data
##binary clinical test result-diagnostic
#1 read in
#2 compile and save by 10var
#3 calculate basic char by var
#4 save the result table
######################

codepath = "code/tj_ad/"
datapath = "data/raw/zf190606/tj_zd/"

#today's date
today = format(Sys.Date(),"%Y%m%d")

#default utf-8
#help chinese input
#in terminal shift+alt+T
# defaults write org.R-project.R force.LANG en_US.UTF-8
###############################

#file list
fileList = list.files(pattern = "*.txt", path = paste0(datapath, "ansi/",sep="") )
con=file(paste0(datapath,"utf8/", fileList[3],sep=""),open="r")

varList = readLines(con, 1, encoding = "UTF-8" )

close(con)

# > Encoding(varList) = "ANSI"
# > Encoding(varList) = "GB2312"
# > Encoding(varList) = "UTF-8"
# > Encoding(varList) = "utf-8"
# > Encoding(varList) = "GBK"


#build description var
#build name var
description = unlist(strsplit(varList,"\t"), use.names=F)
description =  gsub(".*\\|","",description) 
# varNames = paste("var",gsub("[^0-9]","",description), sep="")
varNames = paste("var",gsub("\\|.*","",description), sep="")
varNames[1] = "varNO"
codebook_bin = cbind.data.frame(varNames,description)


#read in file with certain format
data <- 
  do.call("rbind", 
          lapply(fileList, 
                 function(x) 
                   read.table(paste(datapath,"utf8/", x, sep=''), 
                              skip=1,encoding="UTF-8",sep="\t",
                              stringsAsFactors = FALSE)))

# write.csv(data,paste(datapath,"fulldata_notitle_cat_",today,".csv",sep=""))
# write.csv(codebook_bin,paste(datapath,"codebook_cat_",today,".csv",sep=""))

#test corr for gan bing
gan = codebook_bin[grepl("肝",codebook_bin$description),]
data0 = data[,colnames(data) %in% gan$varNames]


#delete all NA row
data2= data0[apply(data0,1,function(x)any(!is.na(x))),]

#delete all NA column
data2= data2[, colSums(is.na(data2)) != nrow(data2)]

data3 = data2[complete.cases(data2),]

corr = cor(data0,use="pairwise.complete.obs")
corr2 = cor(data2,use="pairwise.complete.obs")

#delete all NA row
corr2= corr2[apply(corr2,1,function(x)any(!is.na(x))),]

#delete all NA column
corr2= corr2[, colSums(is.na(corr2)) != nrow(corr2)]

row.names(corr2) 
for(i in 1:nrow(corr2)) row.names(corr2)[i] = codebook_bin$description[codebook_bin$varNames==row.names(corr2)[i]]







