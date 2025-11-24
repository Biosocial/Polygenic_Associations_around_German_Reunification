######################################## 
#### SESxEA4 merge Gene x phenotype #### 
#### 13.10.2023 - 16.10.2023        #### 
#### main Author: Deniz Fraemke     #### 
####                                #### 
######################################## 

## This SCript merges the phenotype from the SOEP-G that I have mined earlier with genomic and methylomic data.
## Additionally further steps in data preperation like the computing of a composite for parental SES and the residualisation 
## of the PGIs is done in this Script
##

rm(list=ls())

library(tidytable)
library(tidyverse)
library(haven)
library(MplusAutomation)
library(data.table)

#### load data
dat_pheno <- load( file = "/~/EA4xSES_GSOEP_000_SOEPvariables_20230321.rda")
dat_pheno <- soepis_final
rm(soepis_final)

#Add methylation to phenotype 
dat_meth <- read.csv( file = "/~/Methyl_Scores_Batch120221020.csv")
dat_meth <- dat_meth[c("person_ID","dup","age", "epi_g","epi_g_unscaled", "epi_g_res")]

#removing 24 duplicates
dat_meth = subset(dat_meth, is.na(dat_meth$dup))
dat_meth = subset(dat_meth, select = -c(dup))

#table(is.na(dat_meth$person_ID))
dat_meth$person_ID <- substr(dat_meth$person_ID, 6, 10)
colnames(dat_meth)[1] <- "ID"
dat_pheno$ID <- as.character(dat_pheno$ID)

dat_pheno_meth <- left_join(dat_pheno,dat_meth, by = c("ID"))
table(dat_pheno_meth$methyldata,is.na(dat_pheno_meth$epi_g)) #seems correctly merged

#Add gene data to phenotype
dat_gene <- read.table( file = "~/GSOEP_PGIrepo_v1.1.txt", header = T)
str(dat_gene)
colnames(dat_gene)

dat_gene <- dat_gene[c("IID","PGI_EA.single","PGI_EA.multi","PGI_CP.single","PGI_CP.multi",
         "PC1","PC2","PC3","PC4", "PC5",
         "PC6","PC7","PC8", "PC9","PC10",
         "PC11","PC12", "PC13","PC14","PC15",
         "PC16", "PC17","PC18","PC19","PC20", "QCpass")]
dat_gene$IID  <-  substr(dat_gene$IID, 6, 10)
colnames(dat_gene)[1] <- "ID"
dat_pheno_meth_gene <- left_join(dat_pheno_meth,dat_gene, by = c("ID"))

colnames(dat_pheno_meth_gene) <- c("pid","ID","hid","genedata","methyldata","pgpartnr","mpid","fpid","reltohead",           
                                   "owneduyears",     "edu_Aspir",       "par_EA",          "par_occStatus",        
                                   "income.2009",         "income.2010",         "income.2011",         "income.2012",         "income.2013",        
                                   "income.2014",         "income.2015",         "income.2016",         "income.2017",         "income.2018",        
                                   "income.2019",         "income.2020",         "income.2021",         "max_EA",              "eversmoke",          
                                   "male",                "ageJan12019",         "hgi1hinc",            "cmheight",            "kgweight",           
                                   "bmi",                 "m.edu.years",         "v.edu.years",         "misei",               "visei",              
                                   "age_epiSample", "epi_g",               "epi_g_unscaled",      "epi_g_res",           "PGI_EA.single",       "PGI_EA.multi",       
                                   "PGI_CP.single",       "PGI_CP.multi",        "PC1",                 "PC2",                 "PC3",                
                                   "PC4",                 "PC5",                 "PC6",                 "PC7",                 "PC8",                
                                   "PC9",                 "PC10",                "PC11",                "PC12",                "PC13",               
                                   "PC14",                "PC15",                "PC16",                "PC17",                "PC18",               
                                   "PC19",                "PC20",                "QCpass")

#### Construct mean income variable #### 
         
         income_columns <- c(
           "income.2009", "income.2010", "income.2011", "income.2012",
           "income.2013", "income.2014", "income.2015", "income.2016",
           "income.2017", "income.2018", "income.2019", "income.2020", "income.2021")
         dat <- as.data.frame(dat)
         dat$mean_income <- rowMeans(dat[income_columns], na.rm = TRUE)
         dat$mean_income[is.nan(dat$mean_income)] <- NA
                 
         qqnorm(log(dat$mean_income + 1))

         ##Make log of mean income
         shapiro.test(log(dat$mean_income + 1))
         qqnorm(log(dat$mean_income + 1))
         dat$mean_income_log  <- log(dat$mean_income + 1)
         


         
#### Add composite score of par_SES ####
         dat <- dat %>%
           mutate(par_SES = (scale(par_occStatus) + scale(par_EA) ) / 2)
         
#### Necessary Data Transformations ####
         
         #### residualise PGI on PCs and Age ####
         # Create a list of PGI variables to residualize
         PGI_list <- c("PGI_EA.single", "PGI_EA.multi", "PGI_CP.single", "PGI_CP.multi")
         
         # Loop through each PGI variable and perform residualization
         for (PGI in PGI_list) {
           # Define the formula for the linear regression
           formula <- as.formula(paste0(PGI, " ~ PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10 + PC11 + PC12 + PC13 + PC14 + PC15 + PC16 +    PC17 + PC18 + PC19 + PC20"))
           
           # Fit the linear regression model
           reg <- lm(formula, data = dat)
           
           # Create a new column with the residuals
           dat <- dat %>% mutate(!!paste0(PGI, ".res") := residuals(reg))
         }
         
#### Add Age squared ####
        
         dat <- dat %>%
           mutate(age_sq = ageJan12019^2,
                  age = ageJan12019)
         
         

         
         
setwd("~/005_Final Data")
write.csv(dat,  file = paste0("EA4xSES_GSOEP_001_FullDataset_", format(Sys.Date(),"%Y%m%d" ),".csv")) 

#dat_pheno_meth_gene_mplus = as.data.frame(as.matrix(dat_pheno_meth_gene))
#prepareMplusData(dat_pheno_meth_gene_mplus, file = paste0("EA4xSES_GSOEP_001_FullDataset_", format(Sys.Date(),"%Y%m%d" ),".inp"))

 
