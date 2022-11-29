#############################################################################################
## AIM: prepare transition rates and starting population for the microsimulation
#############################################################################################
dev.off()
rm(list=ls())

#library(nnet)
library(geepack)
library(utils)
library(HMDHFDplus)
library(xtable)
library(tidyr)
library(dplyr)
library(purrr)
library(nnet)

data.in <- "C:\\Users\\Magdalena\\demography\\attrition\\data\\SHARE\\ready"
data.out <- "C:\\Users\\Magdalena\\demography\\attrition\\data\\SHARE\\microsimulation"

setwd(data.in)
##### prepare data
mydatao <- read.table(file="alllonglong.csv", sep=",", header=TRUE)
mydataa <- mydatao %>%
  mutate(cname=substr(merg,1,2), cname=recode(cname,"Bf"="BE", "Bn"="BE", 
                                              "Cf"="CH", "Ci"="CH", "Cg"="CH", 
                                              "F1"="FR", "Eg"="ES", "Ia"="IL",
                                              "Ih"="IL", "Ir"="IL"), 
         country = cname) %>%
  filter(country!="NL", country!="IE") %>% #no wave 7 in NL, IE
  mutate(country=dplyr::recode(country, "AT"=1, "BE"=2, "CH"=3, "CZ"=4, "DE"=5,"DK"=6, "EE"=7,
                               "ES"=8, "FR"=9, "GR"=10, "HR"=11, "HU"=12, "IL"=13, "IT"=14,
                               "LU"=15, "PL"=16, "PT"=17, "SE"=18, "SI"=19),
         edu=0, edu=replace(edu, edu1>=3,1), edu=replace(edu, edu1>=5,2),
         married1=0, married1=replace(married1,mar1<5,1), married2=0, married2=replace(married2,mar2<5,1),
         sex=recode(sex,"1"="0","2"="1"), 
         GALI1=recode(GALI1, "-2"="-1","-1" ="-1", "1"="2", "2"="1","3"="0"), 
         GALI2=recode(GALI2, "-2"="-1","-1" ="-1", "1"="2", "2"="1","3"="0"), GALI2=replace(GALI2, status2==0,3)) %>%
         filter(GALI1>-1,  !is.na(weight1), age> -1) %>% ##remove: those for whom we do not know age at wave 1
  mutate(agew1b=age/12) %>% #age at wave 1 in years
  mutate_all(as.factor) %>%
  mutate(agew1b=floor(as.numeric(as.character(agew1b))), #age back to numeric
         agegr=5*floor(agew1b/5),
         agegr=ifelse(agegr>90,90,agegr),
         agegr= as.factor(agegr))


######################################################################################################################################################
############### models for GALI
######################################
###wave 1######
k=1 #wave
i=1 #country

country <- unique(mydataa$cname[mydataa$wave==k])#countries the wave is available
mycountry <- country[i]

mydata2 <- mydataa %>%
  filter(cname==mycountry, wave==k, status2!=-1,GALI2!=-1) %>%
  mutate(weight1=as.numeric(as.character(weight1))/1000) %>%
  droplevels()

fitGALI <- multinom(GALI2~ sex+GALI1*agegr + edu + married1, weights=weight1,data = mydata2, maxit=200)

#probabilities of transition between health states
aaa <- expand.grid(sex=sort(unique(mydata2$sex)), agegr=sort(unique(mydata2$agegr)), GALI1=sort(unique(mydata2$GALI1)),edu=sort(unique(mydata2$edu)), married1=sort(unique(mydata2$married1)))
aaa$agegr <- as.factor(aaa$agegr)
aaa$GALI1 <- as.factor(aaa$GALI1)
aaa$married1 <- as.factor(aaa$married1)
aaa$sex <- as.factor(aaa$sex)
p.fit  <- predict(fitGALI, aaa, type='probs')

outp <- cbind(aaa,p.fit)
outp$country <- mycountry
outp$wave <- k
colnames(outp) <- c("sex", "agegr", "GALI","edu","married1", "GALI0","GALI1","GALI2","dead","country", "wave")  
setwd(data.out)
write.table(outp, file="GALIprob.csv", sep=",", row.names=FALSE)

for (i in 2:length(country)){
    mycountry <- country[i]

    mydata <- mydataa %>%
      filter(cname==mycountry, wave==k, status2!=-1,GALI2!=-1)%>%
      mutate(weight1=as.numeric(as.character(weight1))/1000) %>%
      droplevels()
      

    fitGALI <- multinom(GALI2~ sex+ GALI1*agegr + edu + married1, weights=weight1, data = mydata, maxit=200)
    
    #probabilities of transition between health states
    aaa <- expand.grid(sex=sort(unique(mydata2$sex)), agegr=sort(unique(mydata2$agegr)), GALI1=sort(unique(mydata2$GALI1)),edu=sort(unique(mydata2$edu)), married1=sort(unique(mydata2$married1)))
    aaa$agegr <- as.factor(aaa$agegr)
    aaa$GALI1 <- as.factor(aaa$GALI1)
    aaa$married1 <- as.factor(aaa$married1)
    aaa$sex <- as.factor(aaa$sex)
    p.fit  <- predict(fitGALI, aaa, type='probs')
    
    outp <- cbind(aaa,p.fit)
    outp$country <- mycountry
    outp$wave <- k
    write.table(outp, file="GALIprob.csv", sep=",", row.names=FALSE, col.names=FALSE, append=TRUE)
  }

####other waves########################
waves <- sort(unique(mydataa$wave))[-1]

for (k in waves){
  country <- unique(mydataa$cname[mydataa$wave==k])#countries the wave is available
  for (i in 1:length(country)){
    mycountry <- country[i]
  
    mydata <- mydataa %>%
      filter(cname==mycountry, wave==k, status2!=-1,GALI2!=-1)%>%
      mutate(weight1=as.numeric(as.character(weight1))/1000) %>%
      droplevels()
  
  
    fitGALI <- multinom(GALI2~ sex+ GALI1*agegr + edu + married1, weights=weight1, data = mydata, maxit=200)
  
    #probabilities of transition between health states
    aaa <- expand.grid(sex=sort(unique(mydata2$sex)), agegr=sort(unique(mydata2$agegr)), GALI1=sort(unique(mydata2$GALI1)),edu=sort(unique(mydata2$edu)), married1=sort(unique(mydata2$married1)))
    aaa$agegr <- as.factor(aaa$agegr)
    aaa$GALI1 <- as.factor(aaa$GALI1)
    aaa$married1 <- as.factor(aaa$married1)
    aaa$sex <- as.factor(aaa$sex)
    p.fit  <- predict(fitGALI, aaa, type='probs')
  
    outp <- cbind(aaa,p.fit)
    outp$country <- mycountry
    outp$wave <- k
    write.table(outp, file="GALIprob.csv", sep=",", row.names=FALSE, col.names=FALSE, append=TRUE)
  }
}

######### together for Modgen
outpa <- read.table(file="GALIprob.csv", sep=",",header=TRUE) 

###expand tghe grid to all countries at each wave
allcountries <- sort(unique(mydataa$cname))
wave7zero  #add wave 7, only needed to model in modgen

outp2 <- outp %>%
  expand(wave,sex,agegr,country,GALI,edu,married1) %>%
  left_join(outp)

write.table(outp2, file="GALIprob17.csv", sep=",", row.names=FALSE)
outp3 <- cbind(outp2$GALI0, outp2$GALI1,outp2$GALI2, outp2$dead)
write.table(outp3, file="GALIproball.txt", sep=",", row.names=FALSE, eol=",", quote=FALSE)

#####################################################################################################################################
############# changes in marital status, independent of health
i=1
j=1
country <- sort(unique(mydataa$cname))
mycountry <- country[i]
mydata2 <- mydataa %>%
  filter(cname==mycountry, status2!=-1, GALI2!=-1, sex==j-1)

amarit <- expand.grid(agegr=sort(unique(mydata2$agegr)), edu=sort(unique(mydata2$edu)), married1=sort(unique(mydata2$married1)))
amarit <- amarit %>%
  mutate_all(as.factor)

fitmar <- multinom(married2~ agegr + edu + married1, data = mydata2, maxit=200)


fitmar  <- predict(fitmar, amarit, type='probs')
fitsing <- 1-fitmar
outmar <- cbind(amarit,fitmar,fitsing)
outmar$country <- mycountry
outmar$sex <- j-1

outmar <- outmar[order(outmar$sex, outmar$agegr, outmar$edu, outmar$married1),]
colnames(outmar) <- c("agegr", "edu","married1", "p.married","p.single", "country", "sex")  
write.table(outmar, file="marprob.csv", sep=",", row.names=FALSE)


j=2
mycountry <- country[i]
mydata2 <- mydataa %>%
  filter(cname==mycountry, status2!=-1, GALI2!=-1, sex==j-1)
amarit <- expand.grid(agegr=sort(unique(mydata2$agegr)), edu=sort(unique(mydata2$edu)), married1=sort(unique(mydata2$married1)))
amarit <- amarit %>%
  mutate_all(as.factor)

fitmar <- multinom(married2~ agegr + edu + married1, data = mydata2, maxit=200)


fitmar  <- predict(fitmar, amarit, type='probs')
fitsing <- 1-fitmar
outmar <- cbind(amarit,fitmar,fitsing)
outmar$country <- mycountry
outmar$sex <- j-1

outmar <- outmar[order(outmar$sex, outmar$agegr, outmar$edu, outmar$married1),]
write.table(outmar, file="marprob.csv", sep=",", row.names=FALSE, col.names=FALSE, append=TRUE)

for (i in 2:length(country)){
  for (j in 1:2){
    mycountry <- country[i]
    mydata2 <- mydataa %>%
      filter(cname==mycountry, status2!=-1, GALI2!=-1, sex==j-1)
    amarit <- expand.grid(agegr=sort(unique(mydata2$agegr)), edu=sort(unique(mydata2$edu)), married1=sort(unique(mydata2$married1)))
    amarit <- amarit %>%
      mutate_all(as.factor)
    
    fitmar <- multinom(married2~ agegr + edu + married1, data = mydata2, maxit=200)
    fitmar  <- predict(fitmar, amarit, type='probs')
    fitsing <- 1-fitmar
    outmar <- cbind(amarit,fitmar,fitsing)
    outmar$country <- mycountry
    outmar$sex <- j-1
    
    outmar <- outmar[order(outmar$sex, outmar$agegr, outmar$edu, outmar$married1),]
    write.table(outmar, file="marprob.csv", sep=",", row.names=FALSE, col.names=FALSE, append=TRUE)
  }
}

#remember! reorder to fit the order in Modgen
outmar <- read.table(file="marprob.csv", sep=",",header=TRUE)
outmar <- outmar[order(outmar$sex, outmar$agegr, outmar$country, outmar$edu, outmar$married1),]

outmar2 <- cbind(outmar$sex, outmar$agegr, outmar$country,  outmar$edu,outmar$married1, outmar$p.single, outmar$p.married)
colnames(outmar2) <- c("sex","agegr", "country", "edu","married1","single2", "married2")
write.table(outmar2, file="marprob.csv", sep=",", row.names=FALSE)

outmar2 <- cbind(outmar$p.single, outmar$p.married)
write.table(outmar2, file="marprob2.txt", sep=",", row.names=FALSE, eol=",")


###################################################################################################################################
########## starting population
startpop <- mydataa %>%
  filter(!is.na(weight1), status2==-1, #only those who attrited at the 2nd interview and never returned, 
         age> -1) %>% ##remove: Ireland, no wave 7; those for whom we do not know age at wave 1
  mutate_all(as.factor) %>%
  mutate(agew1b=floor(as.numeric(as.character(age))/12))#age at wave 1 in years



write.table(startpop, file="startpopall.csv", sep=",", row.names=FALSE)
####starting population for simulation
###still the order to be checked
startpopsim <- startpop %>%
  select(merg,country,sex,chr1,GALI1,sr1,edu,married1,wave,year, agew1b) %>%
  relocate(chr1,GALI1,sr1, .after=wave)%>%
  relocate(agew1b, .after=married1) %>%
  relocate(year, .after=agew1b) %>%
  mutate(merg2=1:nrow(startpop))

ids <- startpopsim %>%
  select(merg2,merg)

startpopsim <- startpopsim %>%
  select(-merg) %>%
  relocate(merg2, .before=country)

colnames(startpopsim) <- c("no","country","sex","edu","mar","age","year","wave","chr","GALI","sr")

write.table(startpopsim, file="startposim.csv", sep=",", row.names=FALSE)
write.table(ids, file="mergids.csv", sep=",", row.names=FALSE)
