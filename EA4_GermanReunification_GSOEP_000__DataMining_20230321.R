####################################
#### SESxEA4 data mining 2.0    #### 
#### 15.01.2023 - 21.03.2023    #### 
#### main Author: Deniz Fraemke #### 
####                            #### 
####################################

rm(list=ls())

library(dplyr)
library(psych)
library(data.table)
library(haven)


#### load Functions ####
max2 <- function(x) {max(x, na.rm = TRUE)}

#### Load Data sets ####
    #read in soep-is data
    #Version by Yayouk
    soepis  <- fread("~/EA4xSES on Cognitive Development/002_raw Data/GSOEP_merge_20230314.csv")
    
    head(soepis)
    #take out Rows > 2945, as they have only ID and the rest is NA; But they have methyldata;
    soepis[c(2495:2507),]
    soepis <- soepis[-c(2495:2507),]
    dim(soepis)
    
    describe(soepis)
    
    
    #Yayouk already did this
    #soepis[duplicated(soepis$pid)] #one duplicate? 392 and 393 appear to be the same 
    #soepis[which(soepis$pid == 1360002)] #no duplicates
    #soepis_ted <- soepis_ted[-393,] #erase duplicate
    #dim(soepis)
    #soepis$V1 <- c(1:length(soepis$V1))
    
    #check whether it worked
    #soepis[soepis$V1 == 393,]
    
    #read in gsoep data
    gene <- read.table("~/GSOEP_PGIrepo_v1.1.txt")
    
    #read in bioparen data
    bioparen <- read_dta("~/soep-is.2020_stata_de/bioparen.dta")
    
    #read in kid data set
    kid <-  read_dta("~/soep-is.2020_stata_de/kid.dta")

    #read in p data set for one education variable
    p <- read_dta("~soep-is.2020_stata_de/p.dta")

    #read in BIP pupil data set 
    ibip_pupil <- read_dta("~/soep-is.2020_stata_de/ibip_pupil.dta")
    
    #read in bioag Data set for most of education variable
    bioage <- read_dta("~/soep-is.2020_stata_de/bioage.dta")
    
    #Recommendation for specific schooltype
    ibip_parent <-  read_dta("~soep-is.2020_stata_de/ibip_parent.dta")
    
    
    bio <- read_dta("~/soep-is.2020_stata_de/bio.dta")
  

    
#### Choose Variables ####

    
#parents SES and EA
parents <-            select(bioparen, pid, bioyear, alter,
                             
                             
                             #pid mother
                             mpid,
                             fpid,
                             
                             #age
                             valter,
                             malter,
                             #level of education
                             vsbil,
                             msbil,
                             #type of education
                             vbbil,
                             mbbil,
                            
                             #source of information
                             vsinfo,
                             msinfo,
                             
                             #ISEI (SES)
                             visei88,
                             misei88,
                             visei08,
                             misei08,
                             
                             #origin of 
                             vbsinfo,
                             mbsinfo,
                             
                             #living with biol. parents or single partner or foster
                             living1,
                             living2,
                             living3,
                             living4,
                             living5,
                             living6,
                             living7,
                             living8
                             )

grades2 <-            select(ibip_pupil, pid, syear,
                            snot1,	#School grade: German
                            snot2,	#School grade: Math 
                            snot4,	#School grade: Foreign Language
                            snotno #noGrade
)

#Which school degree
edu_adult <-            select(p, pid, iyear,
                               plg0013,
                               plg0078,
                               plc0014

)

#education variables within bioage
education <-            select(bioage, pid, syear, cid,
                               school,               #curently attending school
                               schooltype,           #type of school (harmonized)
                               schooltypeb,          #schooltype 2014
                               schooltypebis13ab15,  #Schooltype till 2013 and from 2015
                               schoolallday,         #whole day school
                               sclenroly,            #Year of enrolment
                               entsch,               #Who decided on schooltype
                               
                               #ideal school completion; graduation anticipation
                               probgra1, #Hauptschule
                               probgra2, #Realschule
                               probgra3, #Gymnasium
                               
                               #graduated
                               donegra1, #Hauptschule
                               donegra2, #Realschule
                               donegra3,  #Gymnasium
                               
                               #which grade / class /Jahrgangsstufe
                               ckla,  #BIP Grade
                               
                               #Grades
                               cnot1,	#School grade: German
                               cnot2,	#School grade: Math 
                               cnot4,	#School grade: Foreign Language
                               
                               #Educational Aspirations
                               idegrad1,
                               idegrad2,
                               idegrad3,
                               
                               probgra1,
                               probgra2,
                               probgra3
                               
)

#Check data availability of Educational Aspirations

aspir <-            select(bioage, pid, syear,age,
                               #ideal graduation degree
                               idegrad1,
                               idegrad2,
                               idegrad3,
                               
                               #probable graduation degree
                               probgra1,
                               probgra2,
                               probgra3,
                           
                               #education goal. child should: be a good student
                               edgoal1, #be a good student
                           
                              #which grade / class /Jahrgangsstufe
                              ckla,  #BIP Grade
                           
                           #school enrolmetn year
                           sclenroly)

#e. aspirations from IBIP, but NO individual has genetic DATA
aspir_ibip <- select(ibip_pupil, pid, syear,
                     sschabe,
                     sschabt,
                     smat9, #perf. in math is important to me
                     )




#Check data availability of school attendance 
recommendation <-            select(ibip_parent, pid, syear,
                               akempfs)

#Check data availability of school type 
kid <-            select(kid, pid, syear,
                               ks_gen)
    
#Choose from cognitive potential Dataset
cog <-            select(cog, pid, syear,
                         f025r,
                         f099r90,
                         f099f90)
#to check how many siblings have (siblings yes/no)
bio.int <- select(bio, pid, syear, l0061, bisei1, bise188)

cog.core <- select(cog.core, pid, syear, sumindex)

#### Parental Occupational SES ####
      dim(soepis)
      parent_genes  <- left_join(soepis,parents, by = "pid")
      dim(parent_genes)

#dupicate check
    table(duplicated(soepis$pid)) #no duplicates
    
#generated general ISEI scores for both parents
        parent_genes <- mutate(parent_genes,
                       misei = case_when(bioyear < 2016 ~ misei88,
                                         bioyear >= 2016 ~ misei08),
                       visei = case_when(bioyear < 2016 ~ visei88,
                                         bioyear >= 2016 ~ visei08))
#check sample sizes 
        parent_genes<- mutate(parent_genes,
                      vbbil_exist = ifelse(vbbil > 0, 1,0 ),
                      mbbil_exist = ifelse(mbbil > 0, 1,0 ),
                      vsbil_exist = ifelse(vsbil > 0, 1,0 ),
                      msbil_exist = ifelse(msbil > 0, 1,0 ),
                      misei88_exist = ifelse(misei88 > 0, 1,0),
                      visei88_exist = ifelse(visei88 > 0, 1,0 ),
                      misei08_exist = ifelse(misei08 > 0, 1,0),
                      visei08_exist = ifelse(visei08 > 0, 1,0 ),
                      
                      misei_exist = ifelse(misei > 0, 1,0),
                      visei_exist = ifelse(visei > 0, 1,0 )
                      )
        
        #parental SES
        parent_genes_isei_n_sum <- parent_genes[]%>%
                              
                                  group_by(bioyear) %>%
                                  
                                  summarise(
                                    misei88_n = sum(misei88_exist),
                                    visei88_n = sum(visei88_exist),
                                    misei08_n = sum(misei08_exist),
                                    visei08_n = sum(visei08_exist),
                                    
                                    misei_n = sum(misei_exist),
                                    visei_n = sum(visei_exist))
        
        #Get descriptives of misei and visei
        parent_genes$misei[parent_genes$misei < 0] <- NA
        parent_genes$visei[parent_genes$visei < 0] <- NA

        #explore variables 
        describe(parent_genes$misei) 
        describe(parent_genes$visei)
        #looks good
        
        
        #generate mean parental ISEI
        
        parent_genes <-
          parent_genes %>%
          rowwise() %>%
          mutate( iseiparents = mean(c(misei , visei), na.rm = T))
        parent_genes$iseiparents[is.na(parent_genes$iseiparents) ] <- NA #weird format of NA (NaN). This code makes it congurent with other variables
        
        #add to original data set
        dim(soepis)
        soepis  <- left_join(soepis,parent_genes[,c("pid","misei","visei","iseiparents")], by = "pid")
        dim(soepis)
        

        #See the origin of parental occupation
        table(parent_genes$vbsinfo)  #28/(1966   +28 ) *100 [1] 1.404213%
        table(parent_genes$mbsinfo)
        #>90% is based on the biographical proxy directly assesed from the parents
        
#### Parental Education ####
    #check sample sizes for parental education
        parent_genes_n_sum <- parent_genes[]%>%
          
                              group_by(bioyear) %>%
                              
                              summarise(
                                        
                                        vbbil_n = sum(vbbil_exist),
                                        mbbil_n = sum(mbbil_exist),
                                        vsbil_n = sum(vsbil_exist),
                                        msbil_n = sum(msbil_exist)
                              )
        
        parent_genes_person_sum <- parent_genes[]%>%
          
                              group_by(pid) %>%
                              
                              summarise(
                                
                                vbbil_n = sum(vbbil_exist),
                                mbbil_n = sum(mbbil_exist),
                                vsbil_n = sum(vsbil_exist),
                                msbil_n = sum(msbil_exist)
                              )
        #check duplicates 
        parent_genes_person_sum[duplicated(parent_genes_person_sum$pid),] #no duplicates
        colSums(parent_genes_person_sum,na.rm = T)
        
  #prepare generation of new parental education variable
    
        #erase unavailable data points and 5 = "no degree
       # parent_genes$vsbil[parent_genes$vsbil <= 0 | parent_genes$vsbil == 5] <- NA
       # parent_genes$msbil[parent_genes$msbil <= 0 | parent_genes$msbil == 5] <- NA
       # 
       # parent_genes$msbil[parent_genes$msbil == 6] <- 0
       # parent_genes$vsbil[parent_genes$vsbil == 6] <- 0
                           
                           


        #generated general education degree score for both parents
        parent_edu.degree <- mutate(parent_genes,
                               m.edu.degree = if_else(mbbil == 32, 6, 
                                                          case_when(msbil == 1 ~ 2,
                                                                    msbil == 2 ~ 3,
                                                                    msbil == 3 ~ 4,
                                                                    msbil == 4 ~ 5,
                                                                    msbil == 5 ~ 4,
                                                                    msbil == 6 ~ 1)),
                               v.edu.degree = if_else(vbbil == 32, 6, 
                                                      case_when(vsbil == 1 ~ 2,
                                                                vsbil == 2 ~ 3,
                                                                vsbil == 3 ~ 4,
                                                                vsbil == 4 ~ 5,
                                                                vsbil == 5 ~ 4,
                                                                vsbil == 6 ~ 1)))
        #looks very good
        table(parent_edu.degree$m.edu.degree)
        table(parent_edu.degree$v.edu.degree)
        
        #Check: which subjects have data on vocational degree but not for education
        #parent_edu.degree[is.na(parent_edu.degree$m.edu.degree) & !is.na(parent_edu.degree$mbbil)]
        
        
        #generate years of SCHOOL education score for both parents
        parent_edu <- mutate(parent_genes,
                                   m.edu.years =  case_when(msbil == 6 ~ 7,
                                                            msbil == 1 ~ 9,
                                                            msbil == 2 ~ 10,
                                                            msbil == 3 ~ 12,
                                                            msbil == 4 ~ 13,
                                                            msbil == 5 ~ 10,
                                                            
                                                            msbil == 0 ~ 0),
                                   v.edu.years =  case_when(vsbil == 6 ~ 7,
                                                            vsbil == 1 ~ 9,
                                                            vsbil == 2 ~ 10,
                                                            vsbil == 3 ~ 12,
                                                            vsbil == 4 ~ 13,
                                                            vsbil == 5 ~ 10,
                                                            
                                                            msbil == 0 ~ 0))
        
        #add years of VOCATIONAL or UNIVERSITAL education score for both parents
        
        #add 1.5 years for apprenticeship and civil servants
        parent_edu$m.edu.years <- ifelse(parent_edu$mbbil %in% c(20:25,28,40) & parent_edu$m.edu.years != 0, 
                                          parent_edu$m.edu.years +1.5, parent_edu$m.edu.years)
        parent_edu$v.edu.years <- ifelse(parent_edu$mbbil %in% c(20:25,28,40) & parent_edu$v.edu.years != 0, 
                                          parent_edu$v.edu.years +1.5, parent_edu$v.edu.years)
        
        #add 2 years for technical and health schools
        parent_edu$m.edu.years <- ifelse(parent_edu$mbbil %in% c(26:27) & parent_edu$m.edu.years != 0, 
                                          parent_edu$m.edu.years +2, parent_edu$m.edu.years)
        parent_edu$v.edu.years <- ifelse(parent_edu$mbbil %in% c(26:27) & parent_edu$v.edu.years != 0, 
                                          parent_edu$v.edu.years +2, parent_edu$v.edu.years)
        
        #add 3 years for technical and health schools
        parent_edu$m.edu.years <- ifelse(parent_edu$mbbil %in% c(28) & parent_edu$m.edu.years != 0, 
                                          parent_edu$m.edu.years +3, parent_edu$m.edu.years)
        parent_edu$v.edu.years <- ifelse(parent_edu$mbbil %in% c(28) & parent_edu$v.edu.years != 0, 
                                          parent_edu$v.edu.years +3, parent_edu$v.edu.years)
        
        #add 5 years for university and college
        parent_edu$m.edu.years <- ifelse(parent_edu$mbbil %in% c(31:32) & parent_edu$m.edu.years != 0, 
                                          parent_edu$m.edu.years +5, parent_edu$m.edu.years)
        parent_edu$v.edu.years <- ifelse(parent_edu$mbbil %in% c(31:32) & parent_edu$v.edu.years != 0,
                                          parent_edu$v.edu.years +5, parent_edu$v.edu.years)
        
        #explore the "do not know"  responses [0]
        m.notknow.dat <- subset(parent_edu, m.edu.years == 0)
        table(m.notknow.dat$mbbil)
        
        v.notknow.dat <- subset(parent_edu, v.edu.years == 0)
        table(v.notknow.dat$vbbil)
        
        #infer "do not know" responses from mbbil/ vbbil
        #add minimum years of education required for occupational training; 
        # eg. 13+5 for university or 9 + 1.5 for apprentice ship
        
        #university 18 years
        parent_edu$m.edu.years <- ifelse(parent_edu$mbbil == 32 & parent_edu$m.edu.years == 0, 18, parent_edu$m.edu.years)
        parent_edu$v.edu.years <- ifelse(parent_edu$vbbil == 32 & parent_edu$v.edu.years == 0, 18, parent_edu$v.edu.years)
        
        #apprenticeship / vocational degree
        parent_edu$m.edu.years <- ifelse(parent_edu$mbbil %in% c(20:25) & parent_edu$m.edu.years == 0, 10.5, parent_edu$m.edu.years)
        parent_edu$v.edu.years <- ifelse(parent_edu$vbbil %in% c(20:25) & parent_edu$v.edu.years == 0, 10.5, parent_edu$v.edu.years)
        
        #Replace all remaining [no vocational training or also do not know mbbil] with NA?
        parent_edu$m.edu.years <- ifelse(parent_edu$m.edu.years == 0, NA, parent_edu$m.edu.years)
        parent_edu$v.edu.years <- ifelse(parent_edu$v.edu.years == 0, NA, parent_edu$v.edu.years)
        table(parent_edu$m.edu.years)
        table(parent_edu$v.edu.years)
        
        # plot histogram of both
        #par(mfrow=c(1,2))
        #hist(parent_edu$m.edu.years)
        #hist(parent_edu$v.edu.years)
        
        describe(parent_edu[,c("m.edu.years",
                                 "v.edu.years")])
        describe(parent_edu.degree[,c("m.edu.degree",
                                 "v.edu.degree")])
        
        #explore congruence with household education and correlation with ownedu for indiviudlas above 25 years
        
        check.edu <- subset(parent_edu,ageJan12019 > 25)
        cor.test(check.edu$m.edu.years,check.edu$v.edu.years) #0.8067 - strong assortative mating in parents
        plot(jitter(check.edu$m.edu.years),jitter(check.edu$v.edu.years))
        
        #mother and child
        cor.test(check.edu$m.edu.years,check.edu$owneduyears) #0.329 - mother and child
        plot(jitter(check.edu$m.edu.years),jitter(check.edu$owneduyears))
        
        #father and child
        cor.test(check.edu$v.edu.years,check.edu$owneduyears) #0.377 - father and child
        plot(jitter(check.edu$v.edu.years) ,jitter(check.edu$owneduyears)) #0.377 - father and child
        
        #mother and household max        
        cor.test(check.edu$m.edu.years,check.edu$maxedu)   #0.367 - low correlation
        
        #own and household max; 
        cor.test(check.edu$owneduyears,check.edu$maxedu) #0.889 very strong correlation
        plot(jitter(check.edu$owneduyears),jitter(check.edu$hhmaxeduyears.y) ) #completely normal, as many are part of the hh and have max edu.
        
        
        #See the origin of parental occupation
        table(parent_edu$vsinfo)  #28/(1966   +28 ) *100 [1] 1.404213% are 
        table(parent_edu$msinfo)
        
        #add mean of both parents
        describe(parent_edu[,c("m.edu.years",
                               "v.edu.years")]) 
        parent_edu <-
        parent_edu %>%
          rowwise() %>%
        mutate( eduparents = mean(c(m.edu.years , v.edu.years), na.rm = T))
        parent_edu$eduparents[is.na(parent_edu$eduparents) ] <- NA 
        
        describe( parent_edu$eduparents) #sample size better 
        
        parent_edu$mpid[parent_edu$mpid < 0] <- NA
        parent_edu$fpid[parent_edu$fpid < 0] <- NA
        
        #Add parental education to main dataset
        dim(soepis)
        soepis  <- left_join(soepis,parent_edu[,c("pid","m.edu.years","v.edu.years", "eduparents", "mpid","fpid")], by = "pid")
        dim(soepis)
        

#### Exploring Education related Variables ####

rec_genes  <- left_join(soepis,recommendation, by = c("pid"))
table(rec_genes$akempfs) #no data for genetic sample

edu_genes  <- left_join(soepis,education, by = "pid")
kid_genes  <- left_join(soepis,kid, by = c("pid"))
edu_genes  <- full_join(edu_genes,kid_genes)

table(edu_genes$syear,edu_genes$ks_gen) #at least a little bit of data


edu_genes<- mutate(edu_genes,
                      school_exist                   = ifelse(!is.na(school) & school > 0 , 1,0 ),
                      schooltype_exist               = ifelse(!is.na(schooltype) & schooltype > 0, 1,0 ),
                      schooltypeb_exist              = ifelse(!is.na(schooltypeb) & schooltypeb > 0, 1,0 ),
                      schooltypebis13ab15_exist      = ifelse(!is.na(schooltypebis13ab15) & schooltypebis13ab15 > 0, 1,0 ),
                      schoolallday_exist             = ifelse(!is.na(schoolallday) & schoolallday > 0, 1,0 ),
                      sclenroly_exist                = ifelse(!is.na(sclenroly) & sclenroly > 0, 1,0),
                      entsch_exist                   = ifelse(!is.na(entsch) & entsch > 0, 1,0 ),
                      
                      probgra1_exist = ifelse(!is.na(probgra1) & probgra1  > 0, 1,0 ),
                      probgra2_exist = ifelse(!is.na(probgra2) & probgra2  > 0, 1,0 ),
                      probgra3_exist = ifelse(!is.na(probgra3) & probgra3  > 0, 1,0 ),
                      donegra1_exist = ifelse(!is.na(donegra1) & donegra1  > 0, 1,0 ),
                      donegra2_exist = ifelse(!is.na(donegra2) & donegra2  > 0, 1,0),
                      donegra3_exist = ifelse(!is.na(donegra3) & donegra3  > 0, 1,0 ),
                      
                      ckla_exist = ifelse(!is.na(ckla) & ckla > 0, 1,0 ),
                      
                      cnot1_exist = ifelse(!is.na(cnot1) & cnot1 > 0, 1,0 ),
                      cnot2_exist = ifelse(!is.na(cnot2) & cnot2 > 0, 1,0 ),
                      cnot4_exist = ifelse(!is.na(cnot4) & cnot4 > 0, 1,0 ),
                   
                      ks_gen_exist = ifelse(!is.na(ks_gen) & ks_gen > 0, 1,0 )
                   )


edu_genes_n_sum <- edu_genes[]%>%

                            group_by(syear) %>%

                            summarise(school_n = sum(school_exist),
                                      schooltype_n = sum(schooltype_exist),
                                      schooltypeb_n = sum(schooltypeb_exist),
                                      schooltypebis13ab15_n = sum(schooltypebis13ab15_exist),
                                      ks_gen_n = sum(ks_gen_exist), 
                                      schooltypeb_n = sum(schooltypeb_exist),
                                      sclenroly_n = sum(sclenroly_exist),
                                      entsch_n = sum(entsch_exist),
                                      
                                      probgra1_n = sum(probgra1_exist),
                                      probgra2_n = sum(probgra2_exist),
                                      probgra3_n = sum(probgra3_exist),
                                      donegra1_n = sum(donegra1_exist),
                                      donegra2_n = sum(donegra2_exist),
                                      donegra3_n = sum(donegra3_exist),
                                      
                                      ckla_n = sum(ckla_exist),
                                      cnot1_n = sum(cnot1_exist),
                                      cnot2_n = sum(cnot2_exist),
                                      cnot4_n = sum(cnot4_exist)
                            )



edu_genes_n_sum_person <- edu_genes[]%>%
  
            group_by(pid) %>%
            
            summarise(school_n = sum(school_exist),
                      schooltype_n = sum(schooltype_exist),
                      schooltypeb_n = sum(schooltypeb_exist),
                      schooltypebis13ab15_n = sum(schooltypebis13ab15_exist),
                      ks_gen_n = sum(ks_gen_exist), 
                      sclenroly_n = sum(sclenroly_exist),
                      entsch_n = sum(entsch_exist),
                      
                      probgra1_n = sum(probgra1_exist),
                      probgra2_n = sum(probgra2_exist),
                      probgra3_n = sum(probgra3_exist),
                      donegra1_n = sum(donegra1_exist),
                      donegra2_n = sum(donegra2_exist),
                      donegra3_n = sum(donegra3_exist),
                      
                      ckla_n = sum(ckla_exist),
                      cnot1_n = sum(cnot1_exist),
                      cnot2_n = sum(cnot2_exist),
                      cnot4_n = sum(cnot4_exist)
            )


table(edu_genes_n_sum_person$school_n)


onePoint <- data.frame(1,18)

for(i in 2:18)
{
  edu_genes_n_sum_person[which(is.na(edu_genes_n_sum_person[,i])),i] <- 0
  onePoint[,i] <- sum(with(edu_genes_n_sum_person, edu_genes_n_sum_person[,i] > 0 ))
}

onePoint[1,1] <- 19982020
colnames(onePoint) <- colnames(edu_genes_n_sum)

edu_genes_n_sum[24,] <- onePoint[1,]




#### Explore E. Aspirations ####

#generate estimated school grade
mutate(aspir, grade = syear - sclenroly) #not sufificient data


#how old are kids in classes?
        aspir %>%
          
          group_by(ckla) %>%
          
          summarise(
            age = mean(age/12))
        
        
        aspir <- filter( aspir, probgra3 > 0)
        
        dat.edu.asp <- aspir %>%
                    
                    group_by(pid) %>%
                    
                    summarise(
                      eduAspiration = mean(probgra3, na.rm = T))
        
        aspir_genes_mean  <- left_join(soepis,dat.edu.asp, by = c("pid"))
        describe(aspir_genes_mean$eduAspiration)


        soepis  <- left_join(soepis,aspir_genes_mean[,c("pid","eduAspiration")], by = "pid")
#This is the result and the best way to go in my opinion. The rest of the code is useless and has the purpose to explore the data
              
              
              #if only 12-14 are taken: 
              
              aspir_range1214 <- filter( aspir, age >= 144 & age <= 168 )
              dat.edu.asp1214 <- aspir_range1214 %>%
                
                group_by(pid) %>%
                
                summarise(
                  eduAspiration = mean(probgra3, na.rm = T))
              aspir_genes_mean1214  <- left_join(soepis,dat.edu.asp1214, by = c("pid"))
              
              describe(aspir_genes_mean1214$eduAspiration.y)
              
              describe(aspir_genes_mean1214$eduAspiration.x)
              
              
              #How many who have missing data on owneduyears have educational aspirations? 
              table(is.na(aspir_genes_mean$owneduyears),is.na(aspir_genes_mean$eduAsp)) #203
              
              
              #from Bioage
              aspir_genes  <- left_join(soepis,aspir, by = c("pid"))
              
                  #Unfortunately we do not know the school grade the individuals are in as 
                        table(aspir_genes$ckla) #no data
                        aspir_genes <- aspir_genes[,-c("ckla")]
                  # Thats why this approach led to nothing
                        #aspir_grade4  <- filter(aspir, ckla == 4)
                        #aspir_genes_grade4  <- left_join(soepis,aspir_grade4, by = c("pid"))
                        #
                        #aspir_grade7  <- filter(aspir, ckla == 7)
                        #aspir_genes_grade7  <- left_join(soepis,aspir_grade7, by = c("pid"))
                        #
                        #aspir_grade10 <- filter(aspir, ckla == 10)
              
              #In which years E. aspir. were collected?
              table(aspir_genes$syear) #2011 - 2020
              plot(aspir_genes$ageJan12019,aspir_genes$syear) #2011 - 2020
              
              
              
              #look into most recent values and figure out their school grade
                      aspir_probgra3 <- filter(aspir, !is.na(probgra3))
                      aspir_probgra3$maxyear <- ave(aspir_probgra3$syear, aspir_probgra3$pid, FUN = max2)
                      aspir_mostrecent <- filter(aspir_probgra3, maxyear == syear)
                      aspir_mostrecent_genes  <- left_join(soepis,aspir_mostrecent, by = c("pid"))
                      
                      describe(aspir_mostrecent_genes[  ,c("idegrad1",
                                                "idegrad2",
                                                "idegrad3",
                                                "probgra1",
                                                "probgra2",
                                                "probgra3")]) #at least info on 316 people
                      
                      
                      cor.test(aspir_mostrecent_genes$idegrad1,aspir_mostrecent_genes$idegrad3)
                      plot(jitter(aspir_mostrecent_genes$idegrad1),jitter(aspir_mostrecent_genes$idegrad3))
                      
                      
                      aspir_mostrecent_genes$idegrad3[aspir_mostrecent_genes$idegrad3 < 0 ] <- NA
                      aspir_mostrecent_genes$idegrad2[aspir_mostrecent_genes$idegrad2 < 0 ] <- NA
                      aspir_mostrecent_genes$idegrad1[aspir_mostrecent_genes$idegrad1 < 0 ] <- NA
                      
                      aspir_mostrecent_genes$probgra3[aspir_mostrecent_genes$probgra3 < 0 ] <- NA
                      cor.test(aspir_mostrecent_genes$probgra3,aspir_mostrecent_genes$idegrad3)
                      
                      table(aspir_mostrecent_genes$idegrad3)
                      
                      #Maybe add ideal and probable degree to one score?
                      
                      
                      
              #look into individuals that have data on educational aspirations
              aspir_mostrecent_genes_data <- filter(aspir_mostrecent_genes, !is.na(idegrad3))
                    hist(aspir_mostrecent_genes_data$ageJan12019)
                    hist(aspir_mostrecent_genes_data$syear)
                    
                    plot(aspir_mostrecent_genes_data$syear, aspir_mostrecent_genes_data$ageJan12019,
                         xlab = "most recent measurement of educational aspirations",  
                         ylab = "age in January 2019")
                    
                    table(aspir_mostrecent_genes_data$owneduyears)
                    
                    
                    aspir_mostrecent_genes_data$idegrad3[aspir_mostrecent_genes_data$idegrad3 < 0 ] <- NA
                    aspir_mostrecent_genes_data$probgrad3[aspir_mostrecent_genes_data$probgrad3 < 0 ] <- NA
                    




#### Explore Socio-economic Attainment ####

bio_genes <- left_join(soepis,bio.int, by = c("pid"))

table(bio_genes$bisei1) #not enough data (N <2165)
table(bio_genes$bise188) #same. not enough data (N <2165); same variables anyways



#Looking into income 

#edu_adult$maxyear <- ave(edu_adult$iyear, edu_adult$pid, FUN = max2)
#edu_adult <- filter(edu_adult, maxyear == iyear)
bio_genes <- left_join(bio_genes,edu_adult[c("pid","iyear", "plc0014")], by = c("pid"))
bio_genes$plc0014[bio_genes$plc0014 < 0] <- NA
describe(bio_genes$plc0014) 
#vars    n    mean      sd median trimmed     mad min   max range skew kurtosis    se
#X1    1 1207 1956.65 1333.04   1800 1956.65 1037.82   0 12000 12000 2.13    10.09 38.37

inc_genes  <- left_join(soepis,edu_adult[c("pid","iyear", "plc0014")], by = c("pid"))
inc_genes<- mutate(inc_genes,
                   plc0014_exist                   = ifelse(plc0014 >= 0 , 1,0 ),
                   )


inc_genes_n_sum <- inc_genes[]%>%
  
  group_by(iyear) %>%
  
  summarise(plc0014_n = sum(plc0014_exist)
  )

inc_genes_reshape <- inc_genes[,c("pid","iyear", "plc0014")]
names(inc_genes_reshape)[names(inc_genes_reshape) == 'plc0014'] <- 'income'

inc_genes_reshape[inc_genes_reshape < 0,] <- NA

w <- reshape(inc_genes_reshape, 
             timevar = "iyear",
             idvar = c("pid"),
             direction = "wide")
describe(w)
w <- w[,-c("income.NA")]
names(w)
dim(w)

soepis <- left_join(soepis,w, by = c("pid"))



## Wrap up whole data fram


dim(soepis)

soepis$eversmoke[is.na(soepis$eversmoke)] <- 0


describe(soepis)
dim(soepis)
         soepis_final <-
           soepis[,c( "pid","ID","hid","genedata","methyldata","pgpartnr","mpid","fpid", "reltohead", #Indices
                      "owneduyears", "eduAspiration", "eduparents","iseiparents", #focal
                      "income.2009","income.2010","income.2011" ,"income.2012", "income.2013" ,"income.2014", "income.2015" , "income.2016", "income.2017" ,"income.2018", "income.2019", "income.2020" , "income.2021",
                      "maxedu", "eversmoke" , "male", "ageJan12019" , "hgi1hinc", "cmheight","kgweight","bmi","m.edu.years","v.edu.years",  "misei","visei"#Rest
                      )]
         dim(soepis_final)
         
describe(soepis_final)

        
      #Change Smoking NAs to 0 
      soepis_final$eversmoke[is.na(soepis_final$eversmoke)] <- 0

      #Change v.edu.years to f.edu.years and visei to fisei; signsnumberscorrect -> gf; wordtestcorrect -> gc; 
      names(soepis_final)[names(soepis_final) == 'v.edu.years'] <- 'f.edu.years'
      names(soepis_final)[names(soepis_final) == 'visei'] <- 'fisei'
      

#### Descriptives for Codebook ####
      
    
  #  library (openxlsx)
   # write.xlsx(describe(soepis_final), file="/Users/fraemke/Documents/004_Projects/EA4xSES on Cognitive Development/004_processed Data/add descriptives for SOEP-G codebook_20230321.xlsx")

    #Save Final Data set
    save(soepis_final, file = "/Users/fraemke/Documents/004_Projects/EA4xSES on Cognitive Development/005_Final Data/EA4xSES_GSOEP_000_SOEPvariables_20230321.rda")
    