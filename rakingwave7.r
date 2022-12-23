#############################################################################################
## AIM: combine wave 7 and microsimulation files and then rake to external life tables
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


data.in <- "C:\\Users\\Magdalena\\demography\\exedagger\\data\\basic"
data.out <- "C:\\Users\\Magdalena\\demography\\exedagger\\data\\microsimulation"

setwd(data.in)
cross7 <- read.table(file="share17.csv", sep=",", header=TRUE) %>%
  mutate(chr=chr7,
         weight=weight7,
         age=agew7) %>%
  select(merg,weight,chr,country,sex,age,NUTS1)

observed <- read.table(file="alllonglong.csv", sep=",", header=TRUE) %>%
  filter(wave==6, status2==1) %>%
  mutate(age=agew7,
         chr=chr2,
         weight=weight2) %>%
  select(merg,weight,chr,country,sex,age,NUTS1) %>%
  add_row(cross7) %>%
  mutate(cname=substr(merg,1,2), cname=recode(cname,"Bf"="BE", "Bn"="BE", 
                                              "Cf"="CH", "Ci"="CH", "Cg"="CH", 
                                              "F1"="FR", "Eg"="ES", "Ia"="IL",
                                              "Ih"="IL", "Ir"="IL"), 
         country = cname) %>%
  mutate(sex=recode(sex,"1"="0","2"="1")) %>%
         filter(chr>-1,  !is.na(weight), age> -1) %>% ##remove: those for whom we do not know age at wave 1
  mutate(agew1b=age/12) %>% #age at wave 1 in years
  mutate(agew1b=floor(as.numeric(as.character(agew1b))), #age back to numeric
         agegr=5*floor(agew1b/5),
         agegr=ifelse(agegr>90,90,agegr)) 

#state at wave 7 from microsiumulation 

setwd(data.out)

attrited <- read.table(file="attrition_output.csv",sep=",",header=TRUE) %>%
  filter(wave_last_chron==5, sim_sr<2) %>%  #select only state at wave 7 and alive
  mutate(sex=SEX, age=sim_age, chr=sim_sr, merg2=ID) %>%
  select(merg2,chr,sex,age) %>%
  left_join(read.table(file="mergids.csv", sep=",", header=TRUE)) %>%
  left_join(read.table(file="C:\\Users\\Magdalena\\demography\\exedagger\\data\\basic\\alllonglong.csv", sep=",", header=TRUE), 
            by="merg") %>%
    mutate(age=agew7,
           chr=chr2,
           weight=weight1) %>%
    select(merg,weight,chr,country,sex,age,NUTS1) %>%
    add_row(cross7) %>%
    mutate(cname=substr(merg,1,2), cname=recode(cname,"Bf"="BE", "Bn"="BE", 
                                                "Cf"="CH", "Ci"="CH", "Cg"="CH", 
                                                "F1"="FR", "Eg"="ES", "Ia"="IL",
                                                "Ih"="IL", "Ir"="IL"), 
          country = cname), by="merg")


