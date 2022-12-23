dev.off()
rm(list=ls())
###########################################################################################################################
###### AIM: prepare data for both health status analyses and correction of attrition 
##########################################################################################################################
##marital status and education 1st mention
library(foreign)
#library(nnet)
library(geepack)
library(utils)
library(HMDHFDplus)
library(xtable)
library(tidyr)
library(dplyr)
library(purrr)
library(nnet)



i=1
setwd(paste("C:\\Users\\Magdalena\\demography\\attrition\\data\\SHARE\\SHARE_",i,sep=""))
demo <- read.dta(file=paste("sharew",i,"_rel7-1-0_dn.dta", sep=""), convert.factors = FALSE)
edu <- read.dta(file=paste("sharew",i,"_rel7-1-0_gv_isced.dta", sep=""), convert.factors = FALSE)
maredu1  <- edu %>%
  select(mergeid,isced1997_r) %>%
  left_join(demo %>% select(mergeid,dn014_)) %>%
  rename(merg=mergeid, edu1=isced1997_r, mar1=dn014_)

i=2
setwd(paste("C:\\Users\\Magdalena\\demography\\attrition\\data\\SHARE\\SHARE_",i,sep=""))
demo <- read.dta(file=paste("sharew",i,"_rel7-1-0_dn.dta", sep=""), convert.factors = FALSE)
edu <- read.dta(file=paste("sharew",i,"_rel7-1-0_gv_isced.dta", sep=""), convert.factors = FALSE)
maredu2  <- edu %>%
  select(mergeid,isced1997_r) %>%
  left_join(demo %>% select(mergeid,dn014_)) %>%
  rename(merg=mergeid, edu2=isced1997_r, mar2=dn014_)

i=4
setwd(paste("C:\\Users\\Magdalena\\demography\\attrition\\data\\SHARE\\SHARE_",i,sep=""))
demo <- read.dta(file=paste("sharew",i,"_rel7-1-0_dn.dta", sep=""), convert.factors = FALSE)
edu <- read.dta(file=paste("sharew",i,"_rel7-1-0_gv_isced.dta", sep=""), convert.factors = FALSE)
maredu4  <- edu %>%
  select(mergeid,isced1997_r) %>%
  left_join(demo %>% select(mergeid,dn014_)) %>%
  rename(merg=mergeid, edu4=isced1997_r, mar4=dn014_)

i=5
setwd(paste("C:\\Users\\Magdalena\\demography\\attrition\\data\\SHARE\\SHARE_",i,sep=""))
demo <- read.dta(file=paste("sharew",i,"_rel7-1-0_dn.dta", sep=""), convert.factors = FALSE)
edu <- read.dta(file=paste("sharew",i,"_rel7-1-0_gv_isced.dta", sep=""), convert.factors = FALSE)
maredu5  <- edu %>%
  select(mergeid,isced1997_r) %>%
  left_join(demo %>% select(mergeid,dn014_)) %>%
  rename(merg=mergeid, edu5=isced1997_r, mar5=dn014_)

i=6
setwd(paste("C:\\Users\\Magdalena\\demography\\attrition\\data\\SHARE\\SHARE_",i,sep=""))
demo <- read.dta(file=paste("sharew",i,"_rel7-1-0_dn.dta", sep=""), convert.factors = FALSE)
edu <- read.dta(file=paste("sharew",i,"_rel7-1-0_gv_isced.dta", sep=""), convert.factors = FALSE)
maredu6  <- edu %>%
  select(mergeid,isced1997_r) %>%
  left_join(demo %>% select(mergeid,dn014_)) %>%
  rename(merg=mergeid, edu6=isced1997_r, mar6=dn014_)

i=7
setwd(paste("C:\\Users\\Magdalena\\demography\\attrition\\data\\SHARE\\SHARE_",i,sep=""))
demo <- read.dta(file=paste("sharew",i,"_rel7-1-0_dn.dta", sep=""), convert.factors = FALSE)
edu <- read.dta(file=paste("sharew",i,"_rel7-1-0_gv_isced.dta", sep=""), convert.factors = FALSE)
maredu7  <- edu %>%
  select(mergeid,isced1997_r) %>%
  left_join(demo %>% select(mergeid,dn014_)) %>%
  rename(merg=mergeid, edu7=isced1997_r, mar7=dn014_)

maredu17 <- maredu1 %>%
  full_join(maredu2) %>%
  full_join(maredu4) %>%
  full_join(maredu5) %>%
  full_join(maredu6) %>%
  full_join(maredu7) %>%
  mutate(edu= coalesce(edu1,edu2,edu4,edu5,edu6,edu7),
         mar12= coalesce(mar1,mar2,mar4,mar5,mar6,mar7),
         mar22= coalesce(mar2,mar1),
         mar42= coalesce(mar4,mar2,mar1),
         mar52 = coalesce(mar5,mar4,mar2,mar1),
         mar62 = coalesce(mar6,mar5,mar4,mar2,mar1),
         mar72 = coalesce(mar7,mar6,mar5,mar4,mar2,mar1))

edu <- maredu17 %>%
  select(merg,edu)
marmar <- maredu17 %>%
  select(merg, mar12:mar72)

setwd("C:\\Users\\Magdalena\\demography\\exedagger\\data\\basic")
write.table(marmar, file="marmar.csv", sep=",")
write.table(edu, file="maredu.csv", sep=",")

#############################################################################################################################
### other variables than marital and edu

dev.off()
rm(list=ls())

i=1
setwd(paste("C:\\Users\\Magdalena\\demography\\attrition\\data\\SHARE\\SHARE_",i,sep=""))
wavesr <- read.dta(file=paste("sharew",i,"_rel7-1-0_ph.dta", sep=""), convert.factors = FALSE) 
weightsd <- read.dta(file=paste("sharew",i,"_rel7-1-0_gv_weights.dta", sep=""), convert.factors = FALSE)
demo <- read.dta(file=paste("sharew",i,"_rel7-1-0_dn.dta", sep=""), convert.factors = FALSE)
housing <- read.dta(file=paste("sharew",i,"_rel7-1-0_gv_housing.dta", sep=""), convert.factors = FALSE)

health <-wavesr %>%
              select(mergeid,starts_with("ph006d")) %>%
  mutate(wave1=i) %>%
  left_join(housing %>%
              select(mergeid,nuts1_2003)) %>%
  left_join(weightsd %>%
              select(mergeid,dw_w1)) %>%
  rename("merg"="mergeid","NUTS1"="nuts1_2003", "weight1"="dw_w1") %>%
  filter(!is.na(weight1)) 

setwd("C:\\Users\\Magdalena\\demography\\exedagger\\data\\basic")
write.table(health, file=paste("wave",i,".csv", sep=""), sep=",")

######################################
dev.off()
rm(list=ls())

i=1
setwd(paste("C:\\Users\\Magdalena\\demography\\attrition\\data\\SHARE\\SHARE_",i,sep=""))
wavesr <- read.dta(file=paste("sharew",i,"_rel7-1-0_ph.dta", sep=""), convert.factors = FALSE)
weightsd <- read.dta(file=paste("sharew",i,"_rel7-1-0_gv_weights.dta", sep=""), convert.factors = FALSE)
demo <- read.dta(file=paste("sharew",i,"_rel7-1-0_dn.dta", sep=""), convert.factors = FALSE)
housing <- read.dta(file=paste("sharew",i,"_rel7-1-0_gv_housing.dta", sep=""), convert.factors = FALSE)


health <-wavesr %>%
  select(mergeid,starts_with("ph006d")) %>%
  mutate(wave1=i) %>%
  left_join(housing %>%
              select(mergeid,nuts1_2003)) %>%
  left_join(weightsd %>%
              select(mergeid,dw_w1)) %>%
  rename("merg"="mergeid","NUTS1"="nuts1_2003", "weight1"="dw_w1") %>%
  filter(!is.na(weight1)) %>%
  mutate(chr1=ph006d1+ph006d2+ph006d3+ph006d4+ph006d5+ph006d6+ph006d8+ph006d10+ph006d12,
         chr1=ifelse(chr1>0,1,chr1)) %>%
  select(-starts_with("ph006d"))

setwd("C:\\Users\\Magdalena\\demography\\exedagger\\data\\basic")
write.table(health, file=paste("wave",i,".csv", sep=""), sep=",")
##########################################################################
######################################
dev.off()
rm(list=ls())

i=2
setwd(paste("C:\\Users\\Magdalena\\demography\\attrition\\data\\SHARE\\SHARE_",i,sep=""))
wavesr <- read.dta(file=paste("sharew",i,"_rel7-1-0_ph.dta", sep=""), convert.factors = FALSE)
weightsd <- read.dta(file=paste("sharew",i,"_rel7-1-0_gv_weights.dta", sep=""), convert.factors = FALSE)
demo <- read.dta(file=paste("sharew",i,"_rel7-1-0_dn.dta", sep=""), convert.factors = FALSE)
housing <- read.dta(file=paste("sharew",i,"_rel7-1-0_gv_housing.dta", sep=""), convert.factors = FALSE)


health <-wavesr %>%
  select(mergeid,starts_with("ph006d")) %>%
  mutate(wave2=i) %>%
  left_join(housing %>%
              select(mergeid,nuts1_2003)) %>%
  left_join(weightsd %>%
              select(mergeid,dw_w2)) %>%
  rename("merg"="mergeid","NUTS1"="nuts1_2003", "weight2"="dw_w2") %>%
  filter(!is.na(weight2))%>%
  mutate(chr2=ph006d1+ph006d2+ph006d3+ph006d4+ph006d5+ph006d6+ph006d8+ph006d10+ph006d12+ph006d16,
         chr2=ifelse(chr2>0,1,chr2)) %>%
  select(-starts_with("ph006d"))

setwd("C:\\Users\\Magdalena\\demography\\exedagger\\data\\basic")
write.table(health, file=paste("wave",i,".csv", sep=""), sep=",")
##########################################################################
dev.off()
rm(list=ls())

i=4
setwd(paste("C:\\Users\\Magdalena\\demography\\attrition\\data\\SHARE\\SHARE_",i,sep=""))
wavesr <- read.dta(file=paste("sharew",i,"_rel7-1-0_ph.dta", sep=""), convert.factors = FALSE)
weightsd <- read.dta(file=paste("sharew",i,"_rel7-1-0_gv_weights.dta", sep=""), convert.factors = FALSE)
demo <- read.dta(file=paste("sharew",i,"_rel7-1-0_dn.dta", sep=""), convert.factors = FALSE)
housing <- read.dta(file=paste("sharew",i,"_rel7-1-0_gv_housing.dta", sep=""), convert.factors = FALSE)


health <-wavesr %>%
  select(mergeid,starts_with("ph006d")) %>%
  mutate(wave4=i) %>%
  left_join(housing %>%
              select(mergeid,nuts1_2010)) %>%
  left_join(weightsd %>%
              select(mergeid,dw_w4)) %>%
  rename("merg"="mergeid","NUTS1"="nuts1_2010", "weight4"="dw_w4") %>%
  filter(!is.na(weight4)) %>%
  mutate(chr4=ph006d1+ph006d2+ph006d3+ph006d4+ph006d5+ph006d6+ph006d8+ph006d10+ph006d12+ph006d16,
         chr4=ifelse(chr4>0,1,chr4)) %>%
  select(-starts_with("ph006d"))

setwd("C:\\Users\\Magdalena\\demography\\exedagger\\data\\basic")
write.table(health, file=paste("wave",i,".csv", sep=""), sep=",")

##########################################################################
dev.off()
rm(list=ls())

i=5
setwd(paste("C:\\Users\\Magdalena\\demography\\attrition\\data\\SHARE\\SHARE_",i,sep=""))
wavesr <- read.dta(file=paste("sharew",i,"_rel7-1-0_ph.dta", sep=""), convert.factors = FALSE)
weightsd <- read.dta(file=paste("sharew",i,"_rel7-1-0_gv_weights.dta", sep=""), convert.factors = FALSE)
housing <- read.dta(file=paste("sharew",i,"_rel7-1-0_gv_housing.dta", sep=""), convert.factors = FALSE)

health <-wavesr %>%
  select(mergeid,starts_with("ph006d")) %>%
  mutate(wave5=i) %>%
  left_join(housing %>%
              select(mergeid,nuts1_2010)) %>%
  left_join(weightsd %>%
              select(mergeid,dw_w5)) %>%
  rename("merg"="mergeid","NUTS1"="nuts1_2010", "weight5"="dw_w5") %>%
  filter(!is.na(weight5))  %>%
  mutate(ph006d8=ph006d19+ph006d20) %>%
  mutate(ph006d8=ifelse(ph006d8==2,1,ph006d8)) %>%
  mutate(chr5=ph006d1+ph006d2+ph006d3+ph006d4+ph006d5+ph006d6+ph006d8+ph006d10+ph006d12+ph006d16,
         chr5=ifelse(chr5>0,1,chr5)) %>%
  select(-starts_with("ph006d"))

setwd("C:\\Users\\Magdalena\\demography\\exedagger\\data\\basic")
write.table(health, file=paste("wave",i,".csv", sep=""), sep=",")


#######################################################################################################################  
dev.off()
rm(list=ls())

i=6
setwd(paste("C:\\Users\\Magdalena\\demography\\attrition\\data\\SHARE\\SHARE_",i,sep=""))
wavesr <- read.dta(file=paste("sharew",i,"_rel7-1-0_ph.dta", sep=""), convert.factors = FALSE)
weightsd <- read.dta(file=paste("sharew",i,"_rel7-1-0_gv_weights.dta", sep=""), convert.factors = FALSE)
demo <- read.dta(file=paste("sharew",i,"_rel7-1-0_dn.dta", sep=""), convert.factors = FALSE)
housing <- read.dta(file=paste("sharew",i,"_rel7-1-0_gv_housing.dta", sep=""), convert.factors = FALSE)

health <-wavesr %>%
  select(mergeid,starts_with("ph006d")) %>%
  mutate(wave6=i) %>%
  left_join(housing %>%
              select(mergeid,nuts1_2015)) %>%
  left_join(weightsd %>%
              select(mergeid,dw_w6)) %>%
  rename("merg"="mergeid","NUTS1"="nuts1_2015", "weight6"="dw_w6") %>%
  filter(!is.na(weight6))  %>%
  mutate(ph006d8=ph006d19+ph006d20) %>%
  mutate(ph006d8=ifelse(ph006d8==2,1,ph006d8)) %>%
  mutate(chr6=ph006d1+ph006d2+ph006d3+ph006d4+ph006d5+ph006d6+ph006d8+ph006d10+ph006d12+ph006d16,
         chr6=ifelse(chr6>0,1,chr6)) %>%
  select(-starts_with("ph006d"))

setwd("C:\\Users\\Magdalena\\demography\\exedagger\\data\\basic")
write.table(health, file=paste("wave",i,".csv", sep=""), sep=",")

#############################
dev.off()
rm(list=ls())

i=7
setwd(paste("C:\\Users\\Magdalena\\demography\\attrition\\data\\SHARE\\SHARE_",i,sep=""))
wavesr <- read.dta(file=paste("sharew",i,"_rel7-1-0_ph.dta", sep=""), convert.factors = FALSE) 
weightsd <- read.dta(file=paste("sharew",i,"_rel7-1-0_gv_weights.dta", sep=""), convert.factors = FALSE)
demo <- read.dta(file=paste("sharew",i,"_rel7-1-0_dn.dta", sep=""), convert.factors = FALSE)
housing <- read.dta(file=paste("sharew",i,"_rel7-1-0_gv_housing.dta", sep=""), convert.factors = FALSE)


health <-wavesr %>%
  select(mergeid,starts_with("ph006d")) %>%
  mutate(wave7=i) %>%
  left_join(housing %>%
              select(mergeid,nuts1_2015)) %>%
  left_join(weightsd %>%
              select(mergeid,dw_w7)) %>%
  rename("merg"="mergeid","NUTS1"="nuts1_2015", "weight7"="dw_w7") %>%
  filter(!is.na(weight7)) %>%
  mutate(ph006d8=ph006d19+ph006d20) %>%
  mutate(ph006d8=ifelse(ph006d8==2,1,ph006d8)) %>%
  mutate(chr7=ph006d1+ph006d2+ph006d3+ph006d4+ph006d5+ph006d6+ph006d8+ph006d10+ph006d12+ph006d16,
         chr7=ifelse(chr7>0,1,chr7)) %>%
  select(-starts_with("ph006d"))

setwd("C:\\Users\\Magdalena\\demography\\exedagger\\data\\basic")
write.table(health, file=paste("wave",i,".csv", sep=""), sep=",")

################################################################################
####################### merge together
dev.off()
rm(list=ls())

#coverscreen
setwd("C:\\Users\\Magdalena\\demography\\attrition\\data\\SHARE\\ALL_Coverscreen")
cover<- read.dta(file="sharewX_rel7-1-0_gv_allwaves_cv_r.dta",convert.factors = FALSE)

setwd("C:\\Users\\Magdalena\\demography\\exedagger\\data\\basic")
mar<- read.table(file="marmar.csv", sep=",", header=TRUE)
edu <- read.table(file="maredu.csv", sep=",", header=TRUE)


#### merge all
setwd("C:\\Users\\Magdalena\\demography\\exedagger\\data\\basic")
wave1 <- read.table(file="wave1.csv", sep=",", header=TRUE)
wave2 <- read.table(file="wave2.csv", sep=",", header=TRUE)
wave4 <- read.table(file="wave4.csv", sep=",", header=TRUE)
wave5 <- read.table(file="wave5.csv", sep=",", header=TRUE)
wave6 <- read.table(file="wave6.csv", sep=",", header=TRUE)
wave7 <- read.table(file="wave7.csv", sep=",", header=TRUE)

wave17 <- wave1 %>%
  full_join(wave2, by="merg") %>%
  full_join(wave4,by="merg") %>%
  full_join(wave5,by="merg") %>%
  full_join(wave6,by="merg") %>%
  full_join(wave7,by="merg") %>%
  left_join(cover %>%
              select(mergeid, country, gender, yrbirth, mobirth, int_year_w1, int_year_w2,int_year_w4,int_year_w5,
                     int_year_w6, int_year_w7, int_month_w1, int_month_w2, int_month_w4, int_month_w5, int_month_w6, int_month_w7,
                     deceased_year, deceased_month) %>%
              rename("merg"="mergeid", "sex"="gender")) %>%
  mutate(monthbirth= as.numeric(mobirth)-2,
         agew1= (int_year_w1-yrbirth)*12 + as.numeric(int_month_w1)-13-monthbirth, agew1=replace(agew1,agew1<595,-1),
         agew2= (int_year_w2-yrbirth)*12 + as.numeric(int_month_w2)-13-monthbirth, agew2=replace(agew2,agew2<595,-1),
         agew4= (int_year_w4-yrbirth)*12 + as.numeric(int_month_w4)-13-monthbirth, agew4=replace(agew4,agew4<595,-1),
         agew5= (int_year_w5-yrbirth)*12 + as.numeric(int_month_w5)-13-monthbirth, agew5=replace(agew5,agew5<595,-1),
         agew6= (int_year_w6-yrbirth)*12 + as.numeric(int_month_w6)-13-monthbirth, agew6=replace(agew6,agew6<595,-1),
         agew7= (int_year_w7-yrbirth)*12 + as.numeric(int_month_w7)-13-monthbirth, agew6=replace(agew6,agew6<595,-1),
         monthdeath= as.numeric(deceased_month)-4, 
         monthdeath=replace(monthdeath,0,1), monthdeath=replace(monthdeath,-1,6),
         agedeath= (deceased_year-yrbirth)*12 + monthdeath-monthbirth,
         agedeath=replace(agedeath,NA,-99),
         agedeath=replace(agedeath,agedeath< -99,-99)) %>%
  filter(agedeath>600|agedeath<0, country!="Netherlands") %>% droplevels() %>%
  mutate(start=coalesce(wave1,wave2,wave4,wave5,wave6,wave7)) %>% #wave start observation
  left_join(mar, by="merg") %>%
  left_join(edu, by="merg") %>%
  mutate(agew2=replace(agew2,(agew2>=agedeath & agedeath>0),-91),
         agew4=replace(agew4,(agew4>=agedeath & agedeath>0),-91),
         agew5=replace(agew5,(agew5>=agedeath & agedeath>0),-91),
         agew6=replace(agew6,(agew6>=agedeath & agedeath>0),-91),
         agew7=replace(agew7,(agew7>=agedeath & agedeath>0),-91),
         weight2=replace(weight2,agew2==-91,NA),
         weight4=replace(weight4,agew4==-91,NA),
         weight5=replace(weight5,agew5==-91,NA),
         weight6=replace(weight6,agew6==-91,NA),
         weight7=replace(weight7,agew7==-91,NA), # remove interview that happened after death
         agew1=replace(agew1,is.na(weight1),-1),
         agew2=replace(agew2,is.na(weight2),-1),
         agew4=replace(agew4,is.na(weight4),-1),
         agew5=replace(agew5,is.na(weight5),-1),
         agew6=replace(agew6,is.na(weight6),-1),
         agew7=replace(agew7,is.na(weight7),-1)) %>%
  mutate(NUTS1=coalesce(NUTS1.x,NUTS1.y,NUTS1.x.x,NUTS1.y.y, NUTS1.x.x.x, NUTS1.y.y.y))



#started in wave 7 only
cross7 <- wave17 %>%
  filter(start==7)
setwd("C:\\Users\\Magdalena\\demography\\exedagger\\data\\basic")
write.table(cross7, file="share17.csv", sep=",")

#started in wave 4 only
cross4 <- wave17 %>%
  filter(start==4)
setwd("C:\\Users\\Magdalena\\demography\\exedagger\\data\\basic")
write.table(cross4, file="share14.csv", sep=",")


#new longitudinal only
long17 <- wave17 %>%
  filter(start<7)
write.table(long17, file="share17long.csv", sep=",")


########################################################################################################################
####### prepare tranistions between waves, including returns
##############################################################################################################################
# wave 1-2 + returns after wave 2
wave11 <- long17 %>%
  filter(start==1 & agew1!=-1) %>%
  rename("edu1"="edu", "yeardeath"="deceased_year") %>%
  mutate(year=int_year_w1,
         age=agew1,
         wave=1,
         nextnm=0,
         nextnm=replace(nextnm, (is.na(weight2) & !is.na(weight7)),7),
         nextnm=replace(nextnm, (is.na(weight2) & !is.na(weight6)),6),
         nextnm=replace(nextnm, (is.na(weight2) & !is.na(weight5)),5),
         nextnm=replace(nextnm, (is.na(weight2) & !is.na(weight4)),4),
         mar1=mar12,
         mar2=mar22,
         mar2=ifelse(nextnm==4,mar42,mar2),
         mar2=ifelse(nextnm==5,mar52,mar2),
         mar2=ifelse(nextnm==6,mar62,mar2),
         mar2=ifelse(nextnm==7,mar72,mar2),
         agew2=ifelse(nextnm==4,agew4,agew2),
         agew2=ifelse(nextnm==5,agew5,agew2),
         agew2=ifelse(nextnm==6,agew6,agew2),
         agew2=ifelse(nextnm==7,agew7,agew2),
         
         chr2=ifelse(nextnm==4,chr4,chr2),
         chr2=ifelse(nextnm==5,chr5,chr2),
         chr2=ifelse(nextnm==6,chr6,chr2),
         chr2=ifelse(nextnm==7,chr7,chr2), 
         
         weight2=ifelse(nextnm==4,weight4,weight2),
         weight2=ifelse(nextnm==5,weight5,weight2),
         weight2=ifelse(nextnm==6,weight6,weight2),
         weight2=ifelse(nextnm==7,weight7,weight2),
         
         status2=1, 
         status2=ifelse(agew2==-1,-1,status2), 
         status2=ifelse((agedeath>0 & agew2==-1),0,status2),
         status2=ifelse((agedeath>0 & agedeath<agew2),0,status2))


#start wave 1, follow wave 2
wave12 <- long17 %>%
  filter(start==1 & agew2!=-1) %>%
  rename("edu1"="edu", "yeardeath"="deceased_year") %>%
  mutate(year=int_year_w2,
         age=agew2,
         wave=2,
         nextnm=0,
         nextnm=replace(nextnm, (is.na(weight4) & !is.na(weight7)),7),
         nextnm=replace(nextnm, (is.na(weight4) & !is.na(weight6)),6),
         nextnm=replace(nextnm, (is.na(weight4) & !is.na(weight5)),5),
         mar1=mar22,
         mar2=mar42,
         mar2=ifelse(nextnm==5,mar52,mar2),
         mar2=ifelse(nextnm==6,mar62,mar2),
         mar2=ifelse(nextnm==7,mar72,mar2),
         agew1=agew2,
         agew2=agew4,
         agew2=ifelse(nextnm==5,agew5,agew2),
         agew2=ifelse(nextnm==6,agew6,agew2),
         agew2=ifelse(nextnm==7,agew7,agew2),
        
         chr1=chr2,
         chr2=chr4,
         chr2=ifelse(nextnm==5,chr5,chr2),
         chr2=ifelse(nextnm==6,chr6,chr2),
         chr2=ifelse(nextnm==7,chr7,chr2), 
        
         weight1=weight2,
         weight2=weight4,
         weight2=ifelse(nextnm==5,weight5,weight2),
         weight2=ifelse(nextnm==6,weight6,weight2),
         weight2=ifelse(nextnm==7,weight7,weight2),
         
         status2=1, 
         status2=ifelse(agew2==-1,-1,status2), 
         status2=ifelse((agedeath>0 & agew2==-1),0,status2),
         status2=ifelse((agedeath>0 & agedeath<agew2),0,status2))

#start wave 1, follow wave 4
wave14 <- long17 %>%
  filter(start==1 & agew4!=-1) %>%
  rename("edu1"="edu", "yeardeath"="deceased_year") %>%
  mutate(year=int_year_w4,
         age=agew4,
         wave=4,
         nextnm=0,
         nextnm=replace(nextnm, (is.na(weight5) & !is.na(weight7)),7),
         nextnm=replace(nextnm, (is.na(weight5) & !is.na(weight6)),6),
         mar1=mar42,
         mar2=mar52,
         mar2=ifelse(nextnm==6,mar62,mar2),
         mar2=ifelse(nextnm==7,mar72,mar2),
         agew1=agew4,
         agew2=agew5,
         agew2=ifelse(nextnm==6,agew6,agew2),
         agew2=ifelse(nextnm==7,agew7,agew2),
         chr1=chr4,
         chr2=chr5,
         chr2=ifelse(nextnm==6,chr6,chr2),
         chr2=ifelse(nextnm==7,chr7,chr2), 
         weight1=weight4,
         weight2=weight5,
         weight2=ifelse(nextnm==6,weight6,weight2),
         weight2=ifelse(nextnm==7,weight7,weight2),
         
         status2=1, 
         status2=ifelse(agew2==-1,-1,status2), 
         status2=ifelse((agedeath>0 & agew2==-1),0,status2),
         status2=ifelse((agedeath>0 & agedeath<agew2),0,status2))

#start wave 1, follow wave 5
wave15 <- long17 %>%
  filter(start==1 & agew5!=-1) %>%
  rename("edu1"="edu", "yeardeath"="deceased_year") %>%
  mutate(year=int_year_w5,
         age=agew5,
         wave=5,
         nextnm=0,
         nextnm=replace(nextnm, (is.na(weight6) & !is.na(weight7)),7),
         mar1=mar52,
         mar2=mar62,
         mar2=ifelse(nextnm==7,mar72,mar2),
         agew1=agew5,
         agew2=agew6,
         agew2=ifelse(nextnm==7,agew7,agew2),
         chr1=chr5,
         chr2=chr6,
         chr2=ifelse(nextnm==7,chr7,chr2),
         weight1=weight5,
         weight2=weight6,
         weight2=ifelse(nextnm==7,weight7,weight2),
         
         status2=1, 
         status2=ifelse(agew2==-1,-1,status2), 
         status2=ifelse((agedeath>0 & agew2==-1),0,status2),
         status2=ifelse((agedeath>0 & agedeath<agew2),0,status2))

#start wave 1, follow wave 6
wave16 <- long17 %>%
  filter(start==1 & agew6!=-1) %>%
  rename("edu1"="edu", "yeardeath"="deceased_year") %>%
  mutate(year=int_year_w6,
         age=agew6,
         wave=6,
         nextnm=0,
         mar1=mar62,
         mar2=mar72,
         agew1=agew6,
         agew2=agew7,
         chr1=chr6,
         chr2=chr7,
         weight1=weight6,
         weight2=weight7,
         
         status2=1, 
         status2=ifelse(agew2==-1,-1,status2), 
         status2=ifelse((agedeath>0 & agew2==-1),0,status2),
         status2=ifelse((agedeath>0 & agedeath<agew2),0,status2))

wave1 <- rbind(wave11,wave12,wave14,wave15,wave16)
##############################################################################################################################         
# wave 2-4 + returns after wave 4
wave22 <- long17 %>%
  filter(start==2 & agew2!=-1) %>%
  rename("edu1"="edu", "yeardeath"="deceased_year") %>%
  mutate(year=int_year_w2,
         age=agew2,
         wave=2,
         nextnm=0,
         nextnm=replace(nextnm, (is.na(weight4) & !is.na(weight7)),7),
         nextnm=replace(nextnm, (is.na(weight4) & !is.na(weight6)),6),
         nextnm=replace(nextnm, (is.na(weight4) & !is.na(weight5)),5),
         mar1=mar22,
         mar2=mar42,
         mar2=ifelse(nextnm==5,mar52,mar2),
         mar2=ifelse(nextnm==6,mar62,mar2),
         mar2=ifelse(nextnm==7,mar72,mar2),
         agew1=agew2,
         agew2=agew4,
         agew2=ifelse(nextnm==5,agew5,agew2),
         agew2=ifelse(nextnm==6,agew6,agew2),
         agew2=ifelse(nextnm==7,agew7,agew2),
         chr1=chr2,
         chr2=chr4,
         chr2=ifelse(nextnm==5,chr5,chr2),
         chr2=ifelse(nextnm==6,chr6,chr2),
         chr2=ifelse(nextnm==7,chr7,chr2), 
         weight1=weight2,
         weight2=weight4,
         weight2=ifelse(nextnm==5,weight5,weight2),
         weight2=ifelse(nextnm==6,weight6,weight2),
         weight2=ifelse(nextnm==7,weight7,weight2),
         
         status2=1, 
         status2=ifelse(agew2==-1,-1,status2), 
         status2=ifelse((agedeath>0 & agew2==-1),0,status2),
         status2=ifelse((agedeath>0 & agedeath<agew2),0,status2))

wave24 <- long17 %>%
  filter(start==2 & agew4!=-1) %>%
  rename("edu1"="edu", "yeardeath"="deceased_year") %>%
  mutate(year=int_year_w4,
         age=agew4,
         wave=4,
         nextnm=0,
         nextnm=replace(nextnm, (is.na(weight5) & !is.na(weight7)),7),
         nextnm=replace(nextnm, (is.na(weight5) & !is.na(weight6)),6),
         mar1=mar42,
         mar2=mar52,
         mar2=ifelse(nextnm==6,mar62,mar2),
         mar2=ifelse(nextnm==7,mar72,mar2),
         agew1=agew4,
         agew2=agew5,
         agew2=ifelse(nextnm==6,agew6,agew2),
         agew2=ifelse(nextnm==7,agew7,agew2),
         chr1=chr4,
         chr2=chr5,
         chr2=ifelse(nextnm==6,chr6,chr2),
         chr2=ifelse(nextnm==7,chr7,chr2), 
         weight1=weight4,
         weight2=weight5,
         weight2=ifelse(nextnm==6,weight6,weight2),
         weight2=ifelse(nextnm==7,weight7,weight2),
         
         status2=1, 
         status2=ifelse(agew2==-1,-1,status2), 
         status2=ifelse((agedeath>0 & agew2==-1),0,status2),
         status2=ifelse((agedeath>0 & agedeath<agew2),0,status2))

wave25 <- long17 %>%
  filter(start==2 & agew5!=-1) %>%
  rename("edu1"="edu", "yeardeath"="deceased_year") %>%
  mutate(year=int_year_w5,
         age=agew5,
         wave=5,
         nextnm=0,
         nextnm=replace(nextnm, (is.na(weight6) & !is.na(weight7)),7),
         mar1=mar52,
         mar2=mar62,
         mar2=ifelse(nextnm==7,mar72,mar2),
         agew1=agew5,
         agew2=agew6,
         agew2=ifelse(nextnm==7,agew7,agew2),
         chr1=chr5,
         chr2=chr6,
         chr2=ifelse(nextnm==7,chr7,chr2), 
         weight1=weight5,
         weight2=weight6,
         weight2=ifelse(nextnm==7,weight7,weight2),
         
         status2=1, 
         status2=ifelse(agew2==-1,-1,status2), 
         status2=ifelse((agedeath>0 & agew2==-1),0,status2),
         status2=ifelse((agedeath>0 & agedeath<agew2),0,status2))


wave26 <- long17 %>%
  filter(start==2 & agew6!=-1) %>%
  rename("edu1"="edu", "yeardeath"="deceased_year") %>%
  mutate(year=int_year_w6,
         age=agew6,
         wave=6,
         nextnm=0,
         mar1=mar62,
         mar2=mar72,
         agew1=agew6,
         agew2=agew7,
         chr1=chr6,
         chr2=chr7,
         weight1=weight6,
         weight2=weight7,
         
         status2=1, 
         status2=ifelse(agew2==-1,-1,status2), 
         status2=ifelse((agedeath>0 & agew2==-1),0,status2),
         status2=ifelse((agedeath>0 & agedeath<agew2),0,status2))


wave2 <- rbind(wave22,wave24,wave25,wave26)
##########################################################################################
# wave 4-5 + returns after wave 5
wave44 <- long17 %>%
  filter(start==4 & agew4!=-1) %>%
  rename("edu1"="edu", "yeardeath"="deceased_year") %>%
  mutate(year=int_year_w4,
         age=agew4,
         wave=4,
         nextnm=0,
         nextnm=replace(nextnm, (is.na(weight5) & !is.na(weight7)),7),
         nextnm=replace(nextnm, (is.na(weight5) & !is.na(weight6)),6),
         mar1=mar42,
         mar2=mar52,
         mar2=ifelse(nextnm==6,mar62,mar2),
         mar2=ifelse(nextnm==7,mar72,mar2),
         agew1=agew4,
         agew2=agew5,
         agew2=ifelse(nextnm==6,agew6,agew2),
         agew2=ifelse(nextnm==7,agew7,agew2),
         chr1=chr4,
         chr2=chr5,
         chr2=ifelse(nextnm==6,chr6,chr2),
         chr2=ifelse(nextnm==7,chr7,chr2), 
         weight1=weight4,
         weight2=weight5,
         weight2=ifelse(nextnm==6,weight6,weight2),
         weight2=ifelse(nextnm==7,weight7,weight2),
         
         status2=1, 
         status2=ifelse(agew2==-1,-1,status2), 
         status2=ifelse((agedeath>0 & agew2==-1),0,status2),
         status2=ifelse((agedeath>0 & agedeath<agew2),0,status2))


wave45 <- long17 %>%
  filter(start==4 & agew5!=-1) %>%
  rename("edu1"="edu", "yeardeath"="deceased_year") %>%
  mutate(year=int_year_w5,
         age=agew5,
         wave=5,
         nextnm=0,
         nextnm=replace(nextnm, (is.na(weight6) & !is.na(weight7)),7),
         mar1=mar52,
         mar2=mar62,
         mar2=ifelse(nextnm==7,mar72,mar2),
         agew1=agew5,
         agew2=agew6,
         agew2=ifelse(nextnm==7,agew7,agew2),
         chr1=chr5,
         chr2=chr6,
         chr2=ifelse(nextnm==7,chr7,chr2), 
         weight1=weight5,
         weight2=weight6,
         weight2=ifelse(nextnm==7,weight7,weight2),
         
         status2=1, 
         status2=ifelse(agew2==-1,-1,status2), 
         status2=ifelse((agedeath>0 & agew2==-1),0,status2),
         status2=ifelse((agedeath>0 & agedeath<agew2),0,status2))

wave46 <- long17 %>%
  filter(start==4 & agew6!=-1) %>%
  rename("edu1"="edu", "yeardeath"="deceased_year") %>%
  mutate(year=int_year_w6,
         age=agew6,
         wave=6,
         nextnm=0,
         mar1=mar62,
         mar2=mar72,
         agew1=agew6,
         agew2=agew7,
         chr1=chr6,
         chr2=chr7,
         weight1=weight6,
         weight2=weight7,
         
         status2=1, 
         status2=ifelse(agew2==-1,-1,status2), 
         status2=ifelse((agedeath>0 & agew2==-1),0,status2),
         status2=ifelse((agedeath>0 & agedeath<agew2),0,status2))


wave4 <- rbind(wave44,wave45,wave46)
################################################################################################################
# wave 5-6 + returns after wave 6

wave55 <- long17 %>%
  filter(start==5 & agew5!=-1) %>%
  rename("edu1"="edu", "yeardeath"="deceased_year") %>%
  mutate(year=int_year_w5,
         age=agew5,
         wave=5,
         nextnm=0,
         nextnm=replace(nextnm, (is.na(weight6) & !is.na(weight7)),7),
         mar1=mar52,
         mar2=mar62,
         mar2=ifelse(nextnm==7,mar72,mar2),
         agew1=agew5,
         agew2=agew6,
         agew2=ifelse(nextnm==7,agew7,agew2),
         chr1=chr5,
         chr2=chr6,
         chr2=ifelse(nextnm==7,chr7,chr2), 
         weight1=weight5,
         weight2=weight6,
         weight2=ifelse(nextnm==7,weight7,weight2),
         
         status2=1, 
         status2=ifelse(agew2==-1,-1,status2), 
         status2=ifelse((agedeath>0 & agew2==-1),0,status2),
         status2=ifelse((agedeath>0 & agedeath<agew2),0,status2))


wave56 <- long17 %>%
  filter(start==5 & agew6!=-1) %>%
  rename("edu1"="edu", "yeardeath"="deceased_year") %>%
  mutate(year=int_year_w6,
         age=agew6,
         wave=6,
         nextnm=0,
         mar1=mar62,
         mar2=mar72,
         agew1=agew6,
         agew2=agew7,
         chr1=chr6,
         chr2=chr7, 
         weight1=weight6,
         weight2=weight7,
         
         status2=1, 
         status2=ifelse(agew2==-1,-1,status2), 
         status2=ifelse((agedeath>0 & agew2==-1),0,status2),
         status2=ifelse((agedeath>0 & agedeath<agew2),0,status2))

wave5 <- rbind(wave55,wave56)

#######################################################################################################################################
# wave 6-7
wave6 <- long17 %>%
  filter(start==6) %>%
  rename("edu1"="edu", "yeardeath"="deceased_year") %>%
  mutate(year=int_year_w6,
         age=agew6,
         wave=6,
         nextnm=0,
         mar1=mar62, 
         mar2=mar72,
         agew1=agew6,
         agew2=agew7,
         chr1=chr6,
         chr2=chr7,
         weight1=weight6,
         weight2=weight7,
         
         status7=1, 
         status7=replace(status7,agew2==-1,-1), 
         status7=replace(status7,(agedeath>0 & agew2==-1),0),
         status7=replace(status7,(agedeath>0 & agedeath<agew2),0))

colnames(wave2) <- colnames(wave1)
colnames(wave4) <- colnames(wave1)
colnames(wave5) <- colnames(wave1)
colnames(wave6) <- colnames(wave1)



all <- wave1 %>%
  mutate(wlong=1) %>%
  bind_rows(wave2 %>%
              mutate(wlong=2),
            wave4 %>%
              mutate(wlong=4),
            wave5 %>%
              mutate(wlong=5),
            wave6 %>%
              mutate(wlong=6)) %>%
  select(merg,weight1,weight2, chr1,chr2,country,sex,age,agedeath, agew7,edu1,NUTS1,mar1, mar2, start,nextnm,wave,status2,year,agew1,agew2,wlong) %>%
  mutate(age=replace(age,(age>594 & age<600),600)) %>%
  filter(age>-1)

setwd("C:\\Users\\Magdalena\\demography\\exedagger\\data\\basic")
write.table(all, file="alllonglong.csv", sep=",", row.names=FALSE)
