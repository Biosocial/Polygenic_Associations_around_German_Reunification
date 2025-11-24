##############################################################
#### Polygenic associations with Educational Attainment   #### 
#### in East and West-Germany in SOEP"                    #### 
#### "01-12-2023 to 11.10.2024                            #### 
#### main Author: Deniz Fraemke                           #### 
#### Code Review: Dr. Yayouk Willems                      #### 
##############################################################


rm(list=ls())

#devtools::install_github("ben-domingue/scalingGxE") 

# Load required packages

library(ggplot2)
library(dplyr)  
library(psych)
library(tidyr)
library(tidyverse)
library(flextable)
library(scalingGxE) 
library(papaja)
library(haven)
library(broom)
library(rempsyc)
library(boot)
library(emmeans)
library(np)
library(pwr)

df <- read.csv("~/EA4xSES_GSOEP_001_FullDataset_20231026.csv")

### Data Preprocessing ####

#exclude Years of Education of individuals younger than 25
df$owneduyears_old25 <- df$owneduyears
df$owneduyears_old25[df$age < 25] <- NA

#add birth year from age variable "age at 01.01.2019"
df <- df %>%
  mutate(birth =  2019 - age)

#Standardize variables
covariate_vars <-c("age", "age_sq", "bmi","birth", "par_SES","par_EA", "owneduyears_old25")

df <- df %>%
  mutate(across(all_of(covariate_vars), ~scale(.), .names = "std_{.col}"))

#make binary "male" variable o appropriate factor-variable "gender"
df <- df %>% 
  mutate(df,gender = ifelse(male == 1, "male","female"))


#Make std Columns - Input variables
input_variables <- c("PGI_CP.single.res", "PGI_EA.single.res" )
df <- df %>%
  mutate(across(all_of(input_variables), ~scale(.), .names = "std_{.col}"))



#### Add First  East vs. West Germany Variable ("where did you live in 1989?")####



#now have faster version of reading one variable.
ppfad_EW <- read_dta("/~/ppfad_EW.dta")
df <- left_join(df, ppfad_EW, by= "pid")
rm(ppfad_EW)



#Create "East" variable 
df <- df %>%
  mutate(east =  case_when(loc1989 == 1 ~ "DDR", #GDR
                           loc1989 == 2 ~ "BRD", #FRG
                           loc1989 == 3  ~ "Abroad",
                           loc1989 == 4 ~ NA)) 
# loc1989 == 4 are people who have lived abroad and immigrated to germany after 1989 
# and should not be interesting for us in this case 
# as they probably received parts of their education in a different country

#### Add Second East vs. West Germany Variable ("Current household location - region (East vs. West)")####

#now have faster version of reading one variable.
hbrutto <- read_dta("/~/hbrutto_EW.dta")

hbrutto$sampreg[hbrutto$sampreg == -2] <- NA #change missingness indicator

### East-West Migration Exploration for SOEP-IS
#Here I try to find out, whether people moved between East and West, which could bias data.
#Create a new column 'sampreg' that shows the previous year's sampreg value for each household.
hbrutto <- hbrutto %>%  
  arrange(hid, syear) %>%  
  group_by(hid) %>%  mutate(sampreg_lag = lag(sampreg)) 

# Group by hid and count distinct bula values. gives hint on which households 
# moved becuase the others stayed at the same location all the time. 

household_sampreg_counts <- hbrutto %>%  
  group_by(hid) %>%  
  summarise(n_distinct_sampreg = n_distinct(sampreg))


# Filter for households with more than one distinct bula value; moved household
households_with_diff_sampreg <- household_sampreg_counts %>%  
  filter(n_distinct_sampreg > 1) # 18 households

#In which year did they move? A little messy code to check this:
table(filter(hbrutto, hid %in% households_with_diff_sampreg$hid))

table(filter(hbrutto, hid %in% households_with_diff_sampreg$hid)$syear)
#not much moving in the years closest to  reunification only 18 household 
# overall which I would include as their first location, as this should be 
# closer to the formative school years in which we are interest in.

#Now I add sampreg directly from initial dataset skipping the additional analyses:
hbrutto_east_sampreg_first <- hbrutto %>%  
  dplyr::select(hid, sampreg, syear) %>%  # Select the relevant columns  without sampreg
  arrange(hid, syear) %>%  # Sort by 'hid' and 'syear'  
  group_by(hid) %>%  # Group by 'hid'
  dplyr::slice(1)# select first row, to get first observation per household

#now I add the new variable to the previous full data set
df <- left_join(df, hbrutto_east_sampreg_first[,c("hid","sampreg","syear")], by= "hid")
df <- rename(df, east_hh_after1989 = sampreg)
df <- df %>%  
  mutate(east_hh_after1989 = as.numeric(east_hh_after1989)) %>%  
  mutate(east_hh_after1989 = recode(east_hh_after1989, `1` = "west", `2` = "east")) %>%
  mutate(year_first_hhloc = as.numeric(syear))   

df <- df %>%  
  mutate(east_west_old = case_when(
    east == "BRD" ~ "west",  
    east == "DDR" ~ "east",  
    east == "Abroad" ~ NA_character_,  
    is.na(east) ~ as.character(east_hh_after1989),  
    TRUE ~ "MISTAKE"
  )) #finally the variable with the maximum available amount of data is computed I assume, and it worked quite well



#  Add Reunification grouping base on An age cut-off of 15 years in 1990 when East Germans ate bananas and drove bigger cars.
df <- df %>%
  mutate(age_1990 = age - 2019 + 1990)

df <- df %>%
  mutate(reunification = ifelse( age_1990 >= 15, "pre", "post" ))


table(df$east_west,df$east,useNA = "always") # In the generation of this variable I still have to differentiate between people who lived 
# (before or after reunification) via age. that people who went to school in eg. 1960 ar not 
# allocated based on the household variable which is collected after 1998

#spoiler alert, this affects 5 people from


# Here I make the new East variable, that chooses between the two variables "loc1989" and "sampreg", based on which has information closest to the year that person turned 15 in.
#first compute the year that is in the middle of 1989 and first year of household info from sampreg:
df <- df %>%  
  rowwise() %>%  
  mutate(mean_year = mean(c(1989, year_first_hhloc)))

# now compute the East-West variable based on which Item in SOEP was closer to the year the person turned 15. 
# meaning, that IF person is older than 15 in the "mean year", the person is allocated to east or West based on "loc1989", otherwise it uses "sampreg"
df <- df %>%  
  mutate(east_west = ifelse(age - 2019 + mean_year >= 15,
                            
                            case_when(
                              east == "BRD" ~ "west",  
                              east == "DDR" ~ "east",  
                              east == "Abroad" ~ NA_character_,  
                              is.na(east) ~ as.character(east_hh_after1989),  
                              TRUE ~ "MISTAKE"), as.character(east_hh_after1989) #we have data for all on this variable so it can be used here for 
                            # all people with NA on the previous or who are older 
  ))



# Make additonal more transitional variable on how long individuals 
# live in east vs West on a continuus scale: 
df <- df %>%  
  mutate(years_in_gdr = pmax(pmin(age_1990, 25), 0))  


# Exclude all NA on Education or other variables rows for plotting
df_plot <- df[complete.cases(df[, c("reunification", "east_west_old", "std_PGI_EA.single.res")]),]  
df_plot_edu <- df[complete.cases(df[, c("reunification", "east_west_old", "std_PGI_EA.single.res", "std_owneduyears_old25")]),]  


table(df_plot_edu$east_west_old,df_plot_edu$reunification) #However this doesnt affect the people from whom we know the education anyways.

# Exclude people born before 1934, as they already graduated before the separation or shortly after and save them in another comprehensive data set
df_allcohorts <- df
df <- df_allcohorts[df_allcohorts$birth >= 1934,]
#excluding 74 people # but 12 of them bc of no age or too young? 


#rename std_PGI_EA.single.res to more practical PGI_education
df <- df %>% 
  dplyr::mutate( PGI_education = std_PGI_EA.single.res)
df_allcohorts <- df_allcohorts %>% 
  dplyr::mutate( PGI_education = std_PGI_EA.single.res)

#### Sample size Check ####
# after exclusion of > 85 years olds (living pre-GDR) 

table(is.na(df$PGI_education)) # 2408
table(is.na(df$reunification)) # 2408
table(is.na(df$east_west)) # 2313
table(is.na(df$std_owneduyears_old25)) #n= 2025

table(is.na(df$reunification), is.na(df$std_owneduyears_old25)) #n= 2025 #luckily all NAs on age (12) are also NA in eduYears
table(is.na(df$east_west), is.na(df$std_owneduyears_old25)) #n = 1930

table(is.na(df$east_west),is.na(df$reunification), is.na(df$std_owneduyears_old25)) #1930 
table(is.na(df$east_west),is.na(df$reunification), is.na(df$std_owneduyears_old25), is.na(df$std_bmi) ) #1902 

table(df$east_west, df$reunification, is.na(df$std_owneduyears_old25), is.na(df$gender))  #This and the next one are key and reported in manuscript
table(df$east_west, is.na(df$reunification), is.na(df$std_owneduyears_old25), is.na(df$gender))  #and used for sample size reporting in Paper

## Final sample size calculation: 
table(df$east_west, df$reunification, is.na(df$std_owneduyears_old25),is.na(df$gender)) #in sum 1930

#Get sample sizes of exclusion of people above 25 years
table(df$age > 25, is.na(df$east_west)) #95
table(df$age > 25, is.na(df$owneduyears)) # 9
table(df$age > 25, is.na(df$gender)) # 0

#check sample sizes before exclsion > 85 years olds
table(is.na(df_allcohorts$reunification))
table(is.na(df_allcohorts$east_west)) 

table(is.na(df_allcohorts$reunification), is.na(df_allcohorts$std_owneduyears_old25)) 
table(df_allcohorts$east_west, is.na(df_allcohorts$std_owneduyears_old25))


# Relevel variables do that pre and west are reference and Regression results are more intuitive
df$reunification <- as.factor(df$reunification)  
df$reunification <- relevel(df$reunification, ref = "pre")

df$east_west <- as.factor(df$east_west)  
df$east_west <- relevel(df$east_west, ref = "west")

df_allcohorts$east_west <- as.factor(df_allcohorts$east_west)  
df_allcohorts$east_west <- relevel(df_allcohorts$east_west, ref = "west")


#add binary numerical variable for the case they will be needed
df$QCpass_bin <- ifelse(df$QCpass == "Y", 1, 0)  
df$female <- ifelse(df$male == 1, 0, 1)  #Also make change the numerical "male" variable to the "female" numerical variable


#### Descriptives tables ####

descriptives_cont <- df[!is.na(df$east_west) & !is.na(df$owneduyears_old25),] %>%  
  tidyr::pivot_longer(c(birth, owneduyears_old25, PGI_education), names_to = "variable", values_to = "value") %>%  
  group_by(east_west, variable) %>%  
  summarise(  
    mean = mean(value, na.rm = TRUE),  
    sd = sd(value, na.rm = TRUE),  
    range = paste0("[", round(min(value, na.rm = TRUE), 1), "; ", round(max(value, na.rm = TRUE), 1), "]") 
  )

descriptives_bin <- df[!is.na(df$east_west) & !is.na(df$owneduyears_old25) ,] %>%    
  tidyr::pivot_longer(c(female, QCpass_bin), names_to = "variable", values_to = "value") %>%    
  group_by(east_west, variable) %>%    
  summarise(    
    mean = mean(value, na.rm = TRUE)*100  
  ) %>%    
  mutate(    
    variable = c("QCPass" , "gender"),    
    sd = NA,    
    range = NA  
  )    


descriptives <- bind_rows(descriptives_cont, descriptives_bin)  

descriptives <- descriptives %>%  
  tidyr::pivot_wider(names_from = east_west, values_from = c(mean, sd, range))  

colnames(descriptives) <- c("Variable", "West.Mean", "East.Mean", "West.SD", "East.SD","West.Range", "East.Range")  

descriptives <- descriptives[c(5,2,1,3,4),c("Variable", "West.Mean","West.SD", "West.Range", "East.Mean", "East.SD", "East.Range")]

descriptives$Variable <- c("Female (in %)", "Birth Year", "PGI-Education", "Years of Education", "strict QC pass (in %)")

descriptives_nice <- rempsyc::nice_table(descriptives, separate.header = TRUE, italics = seq(descriptives))#,
#title = c("Descriptives Table"))
#print(descriptives_nice, preview = "docx")


##Get t-tests between groups
t_test_attainment <- t.test(std_owneduyears_old25 ~ east_west, data = df) #not significant p-value = 0.3033
t_test_attainment[["p.value"]]
t_test_pgi <- t.test(PGI_education ~ east_west, data = df)   #not significant p-value = 0.4218
t_test_pgi[["p.value"]]

t_test_birth <- t.test(birth ~ east_west, data = df[!is.na(df$std_owneduyears_old25),])   #not significant p-value = 0.3
t_test_birth[["p.value"]]

# Perform Chi-sq tests for different proportions between groups

# Subset the data for > 25 year olds
df_subset <- df[df$birth < 1994, ]

gender_eastwest_table <- table(df_subset$gender, df_subset$east_west)
chi_squared_gender <- chisq.test(gender_eastwest_table) # p = 1

QCpass_bin_eastwest_table <- table(df_subset$QCpass_bin, df_subset$east_west)
chi_squaredc_QCpass <- chisq.test(QCpass_bin_eastwest_table) #p = 0.6 

#Plotting requires no missingness
df_plot <- df %>% filter(!is.na(reunification) & !is.na(east_west))
df_plot_allcohorts <- df_allcohorts %>% filter(!is.na(reunification) & !is.na(east_west))


# Plot Years of Education over  #Supplemental Results
ggplot(data = df_plot_allcohorts , mapping = aes(birth,jitter(owneduyears_old25, 3),colour = factor(east_west)))+
  geom_point(shape=20, size=1,alpha = 0.3, color="lightgray")+
  geom_smooth(method = loess, se=TRUE, aes(color=factor(east_west))) + 
  scale_color_manual(values = c("blue", "red"), labels = c("West Germany","East Germany")) +  
  labs(x = "Birth Year", y = "Years of Education",
       colour = "Region")+
  geom_vline(xintercept = 1975, color = "black") +
  #annotate("text", x = 1973, y = 17, label = "15 years old 
  #             at Reunification", vjust = 0) +  
  scale_x_continuous(limits = c(1934, 1995),breaks = seq(1940, 1990, by = 10))+
  scale_y_continuous(limits = c(7, 19),breaks = seq(8, 18, by = 2))+
  theme_minimal()+
  theme(#legend.position = "none",
    axis.text.x = element_text(angle = 45, hjust = 1))+
  theme(  
    axis.text.x =  element_text(angle = 45, hjust = 1, size = 15, family = "Helvetica"),    
    axis.text.y =  element_text(size = 15, family = "Helvetica"),  
    axis.title.x = element_text(size = 20, family = "Helvetica"),  # Increase x-axis label size    
    axis.title.y = element_text(size = 15, vjust = 2.5,family = "Helvetica"),  # Increase y-axis label size  
    legend.text =  element_text(size = 12, family = "Helvetica"),  # Change legend font  
    legend.title = element_text(size = 12, family = "Helvetica")  # Change legend title font  
  ) 


####### 1. Set Reunification Analyses #######

#### Model 1: Run Pre - Post Reunification Model SOEP ####  

result_reunification <- lm(std_owneduyears_old25 ~ 
                             PGI_education*reunification  #interaction of interest
                           
                           + PGI_education*gender  #control for gender * G
                           + reunification*gender  #control for gender * E
                           
                           , data = df )

summary(result_reunification)

table_reunification <- tidy(summary(result_reunification), conf.int = TRUE)
table_reunification <- table_reunification %>%  dplyr::select(-statistic) %>%  rename("Term" = term, "b" = estimate)

#delete covariate checks
table_reunification <-  table_reunification[c(2,3,5),]      

#round values
table_reunification <- table_reunification %>% mutate(across(where(is.numeric), ~ round(., 3)))

#make apa table
table_reunification_apa <-  nice_table(table_reunification,
                                       title = c("Model with twoway-interaction: PGI-Education x pre- vs. post-reunification"),
                                       note = (paste("N = ", length(result_reunification[["model"]][,1]) , 
                                                     "Npre = ", length(subset(result_reunification$model, reunification == 'pre')[,1]), 
                                                     "Npost = ", length(subset(result_reunification$model, reunification == 'post')[,1]), 
                                                     "; We controlled for the covariate Gender, and GenexGender, PrePostXGender and EastWestXGender.")))
#
table_reunification_apa
#print(table_reunification_apa, preview = "docx")   




#### Model 2: Run East - West Model SOEP ####  

result_eastwest <- lm(std_owneduyears_old25 ~ 
                         
                         PGI_education*east_west #interaction of interest
                       
                         + PGI_education*gender #control for gender * G
                         + east_west*gender  #control for gender * E
                         
                         + east_west*std_birth      #control for birth  * E
                         + PGI_education*std_birth  #control for birth  * G


                       , data = df )

summary(result_eastwest)

table_eastwest <- tidy(summary(result_eastwest), conf.int = TRUE)
table_eastwest <- table_eastwest %>%  dplyr::select(-statistic) %>%  rename("Term" = term, "b" = estimate) 


#take out covariate checks for manuscript tables
table_eastwest <-  table_eastwest[-c(1,4,5,7:10),]      

#round values
table_eastwest <- table_eastwest %>% mutate(across(where(is.numeric), ~ round(., 3)))

#make apa table
table_eastwest_apa <-  nice_table(table_eastwest,
                                  title = c("Model with twoway-interaction: PGI-Education x East vs. West Germany"),
                                  note = (paste("N = ", length(result_eastwest[["model"]][,1]) , 
                                                "Neast = ", length(subset(result_eastwest$model, east_west == 'east')[,1]), 
                                                "Nwest = ", length(subset(result_eastwest$model, east_west == 'west')[,1]), 
                                                "; We controlled for the two covariates Gender, and GenexGender, PrePostXGender and EastWestXGender.")))

table_eastwest_apa
#print(table_eastwest_apa, preview = "docx")


#### Model 3: Run East - West & Pre - Post Reunification Model ####  

result_eastwest_prepost <- lm(std_owneduyears_old25 ~ 
                              
                              #it is needed to test for threewayy interactions with covariate
                              + PGI_education*east_west*gender  #control for gender * G
                              + reunification*east_west*gender  #control for gender * E1
                              + PGI_education*reunification*gender      #control for gender * E2
                            
                              + PGI_education*reunification*east_west #interaction of interest
                            
                              , data = df)

summary(result_eastwest_prepost)



result_eastwest_prepost_clean <- tidy(summary(result_eastwest_prepost), conf.int = TRUE)
result_eastwest_prepost_clean <- result_eastwest_prepost_clean %>%  dplyr::select(-statistic) %>%  rename("Term" = term, "b" = estimate)


#delete covariate checks
result_eastwest_prepost_clean <-  result_eastwest_prepost_clean[c(2,3,5,6,9,11, 15),]

#round values
result_eastwest_prepost_clean <- result_eastwest_prepost_clean %>% mutate(across(where(is.numeric), ~ round(., 3)))

#make apa table
table_eastwest_prepost_clean_apa <-  nice_table(result_eastwest_prepost_clean,
                                                title = c("Model with threeway-interaction: PGI-Education x pre- vs. post-reunification x East vs. West Germany"),
                                                note = (paste("N = ", length(result_eastwest_prepost[["model"]][,1]) , "; We controlled for the two covariates Gender, and GenexGender, PrePostXGender and EastWestXGender.")))
#
table_eastwest_prepost_clean_apa
#print(table_eastwest_prepost_clean_apa, preview = "docx")







#### Post-Hoc Tests Reunification #### 

df_east <- df %>% filter(east_west == "east")
df_west <- df %>% filter(east_west == "west")


# Fit separate regression models for each group  
model_east <- lm(std_owneduyears_old25 ~ 
                   PGI_education * reunification
                   
                   + PGI_education*gender 
                   +   reunification *gender 

                 , data = df_east) 
summary(model_east)

model_east_clean <- tidy(summary(model_east), conf.int = TRUE)
model_east_clean

# Fit separate regression models for each group  
model_west_reuni <- lm(std_owneduyears_old25 ~
                         
                       PGI_education * reunification+
                       
                       PGI_education*gender +
                       reunification*gender 

                 , data = df_west) 

summary(model_west_reuni)

model_west_reuni_clean <- tidy(summary(model_west_reuni), conf.int = TRUE)
model_west_reuni_clean


#### Get R-Square effect size estimates ####

# Function to calculate R-squared  
calc_rsq <- function(data, indices) {
  # Bootstrap sample
  d <- data[indices, ]
  
  # Check if PGI_education and gender have variance (skip if no variance)
  if(any(sapply(d[, c("PGI_education", "gender")], function(x) length(unique(x))) < 2)) {
    return(c(rsq_incremental = NA, simple_adj_rsq = NA))
  }
  
  # Fit models
  model_full <- lm(std_owneduyears_old25 ~ PGI_education + gender, data = d)
  model_without_PGI <- lm(std_owneduyears_old25 ~ gender, data = d)
  model_PGI_only <- lm(std_owneduyears_old25 ~ PGI_education, data = d)
  
  # Calculate incremental R-squared
  rsq_incremental <- summary(model_full)$adj.r.squared - summary(model_without_PGI)$adj.r.squared
  simple_adj_rsq <- summary(model_PGI_only)$adj.r.squared
  
  # Return a numeric vector
  return(c(rsq_incremental = rsq_incremental, simple_adj_rsq = simple_adj_rsq))
}

# Function to bootstrap R-squared with confidence intervals
rsq_boot <- function(data, R = 2000) {
  # Perform bootstrap with 2000 repetitions
  boot_results <- boot(data, calc_rsq, R = R)
  
  # Get bootstrapped confidence intervals for incremental R-squared
  boot_out_incremental <- boot.ci(boot_results, index = 1, type = "bca")
  
  # Get bootstrapped confidence intervals for simple adjusted R-squared
  boot_out_simple <- boot.ci(boot_results, index = 2, type = "bca")
  
  # Return a tidy dataframe for both R-squared metrics
  return(tibble(
    r.squared_incremental = boot_out_incremental$t0,
    lower_incremental = boot_out_incremental$bca[4],
    upper_incremental = boot_out_incremental$bca[5],
    r.squared_simple = boot_out_simple$t0,
    lower_simple = boot_out_simple$bca[4],
    upper_simple = boot_out_simple$bca[5]
  ))
}



# Functions are not sensitive to missing data 
df_excl <- df %>% filter(!is.na(owneduyears_old25) & !is.na(reunification) & !is.na(east_west) ) # n = 1930

# Calculate R-squared based on complete cases dataset
rsq_data_reunif <- df_excl %>%  
  group_by(reunification, east_west) %>%  
  do(rsq_boot(.))

rsq_data_reunif



### Plot the incremental R-Squared ####


# in Psych Science Format

gg_rsq_reunif <- ggplot(rsq_data_reunif, aes(x = reunification, y = r.squared_simple * 100, color = east_west, group = east_west)) +  

  
  geom_point(position = position_dodge(width=0.1)) +    
  geom_line(position = position_dodge(width=0.1)) +    
  geom_errorbar(aes(ymin = lower_simple * 100, ymax = upper_simple * 100), width = 0.2, position = position_dodge(width=0.1)) +  # Add error bars    
  
  # Axis labels
  xlab("Reunification") +    
  ylab("Educational attainment R² (%)") +    
  
  # Customize color and legend
  scale_color_manual(values = c("#000080", "#be1918"),     
                     name = "German Region",     
                     labels = c("West Germany", "East Germany")) +
  
  # Minimal theme
 theme_minimal() +  
  
  # Font size customizations based on PsySci guidelines
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 9, family = "Helvetica"),  # X-axis text in 9-point font
    axis.text.y = element_text(size = 9, family = "Helvetica"),  # Y-axis text in 9-point font
    axis.title.x = element_text(size = 10, family = "Helvetica"),  # X-axis label in 10-point font
    axis.title.y = element_text(size = 10, family = "Helvetica", vjust = 2.5),  # Y-axis label in 10-point font, with vertical adjustment
    
    legend.position = "top",  # Place legend at the top
    legend.text = element_text(size = 9, family = "Helvetica"),  # Legend text in 9-point font
    legend.title = element_text(size = 9, family = "Helvetica"),  # Legend title in 9-point font
    
    plot.title = element_text(hjust = 0.5, size = 12, family = "Helvetica"),  # Title in 12-point font
    strip.text = element_text(size = 18, family = "Helvetica")  # Panel labels in 18-point font
  )

# Render the plot
gg_rsq_reunif

 
#ggsave(paste0(dir_plot,"FraemkeFigS3_", format(Sys.Date(),"%Y%m%d" ),".pdf"),  gg_rsq_reunif, width = 150, height = 100, units = "mm")


### Plot the std. Beta ####

# get estimated (marginal) means
emm <- emmeans(result_eastwest_prepost, ~ PGI_education | east_west * reunification, at = list(PGI_education = 1))

# Extract the betas for PGI_education in each group
betas <- emtrends(result_eastwest_prepost, ~ east_west * reunification, var = "PGI_education")
betas <- as.data.frame(betas)


# Psych Science Format

gg_betas_reunif <- ggplot( betas, aes(x = reunification, y = PGI_education.trend, color = east_west, group = east_west)) +  
  
  geom_point(position = position_dodge(width=0.1)) +    
  geom_line(position = position_dodge(width=0.1)) +    
  geom_errorbar(aes(ymin = lower.CL , ymax = upper.CL), width = 0.2, position = position_dodge(width=0.1)) +  # Add error bars    
  
  # Axis labels
  xlab("Reunification") +    
 ylab("Std. beta coefficients of PGI-Education") +  
  

  # no labs for manuscript
   # labs(x = NULL, y = NULL) +

  
  # Customize color and legend
  scale_color_manual(values = c("#000080", "#be1918"),     
                     name = "German Region",     
                     labels = c("West Germany", "East Germany")) +
  
  # Minimal theme
  theme_minimal() +  
  
  # set y-scale
  scale_y_continuous(limits = c(-0.01, 0.9), breaks = seq(0, 0.9, 0.1)) +  # Set the y-axis limits and breaks
  
  # Font size and family customizations based on APA guidelines
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 9, family = "Helvetica"),  # X-axis text in 9-point font
    axis.text.y = element_text(size = 9, family = "Helvetica"),  # Y-axis text in 9-point font
    axis.title.x = element_text(size = 10, family = "Helvetica"),  # X-axis label in 10-point font
    axis.title.y = element_text(size = 10, family = "Helvetica", vjust = 2.5),  # Y-axis label in 10-point font, with vertical adjustment
    
    legend.position = "top",  # Place legend at the top
    legend.text = element_text(size = 9, family = "Helvetica"),  # Legend text in 9-point font
    legend.title = element_text(size = 9, family = "Helvetica"),  # Legend title in 9-point font
    
    plot.title = element_text(hjust = 0.5, size = 12, family = "Helvetica"),  # Title in 12-point font
    strip.text = element_text(size = 18, family = "Helvetica") # Panel labels in 18-point font
    
  )+
    geom_hline(yintercept = 0, color = "black",size =  0.1)
gg_betas_reunif


# ggsave(paste0(dir_plot,"FraemkeFig1A_", format(Sys.Date(),"%Y%m%d" ),".pdf"),  gg_betas_reunif, width = 150, height = 100, units = "mm")




#### Heteroscedasticity ####


# Load function 
scalinggxe_test <-  function(E, y, G, data){
  
  
  scaling_df_E <- data[c(E,y,G)]
  scaling_df_E <- scaling_df_E[complete.cases(scaling_df_E), ]
  
  #Test for Heteroscedasticity in Environment that might drive the GxE effect
  colnames(scaling_df_E) <- c("E", "y", "g")
  est.E <- mlest(scaling_df_E, hess = TRUE)
  xi.E <- xi.test(est.E, p.value = F)
  xi.test.E <- xi.test(est.E, p.value = T)
  
  #Test for Heteroscedasticity in PGI that might drive the GxE effect
  scaling_df_G <- scaling_df_E[c("g", "y", "E")]
  colnames(scaling_df_G) <- c("E", "y", "g")
  est.G <- mlest(scaling_df_G, hess = TRUE)
  xi.G <- xi.test(est.G, p.value = F)
  xi.test.G <- xi.test(est.G, p.value = T)
  
  
  print(c(est.E$est,
          standard_errors = sqrt(diag(est.E$var)),
          lambda2 =  est.G[["est"]][["lambda1"]],
          lambda2.se = sqrt(diag(est.E$var))["lambda1"],
          xi.E = xi.E[1], 
          xi.E.se = abs(xi.E[1,"value"] - xi.E[1,"97.5 %"]) / 1.96,
          #xi.E.CI.lower = xi.E[2], 
          #xi.E.CI.upper = xi.E[3], 
          xi.E.p.value = xi.test.E[["p.value"]][["Chisq"]],
          xi.G = xi.G[1],
          xi.G.se = abs(xi.G[1,"value"] - xi.G[1,"97.5 %"]) / 1.96,
          # xi.G.CI.lower =  xi.G[2],
          # xi.G.CI.upper =  xi.G[3],
          xi.G.p.value =   xi.test.G[["p.value"]][["Chisq"]] ))
}

# function can only handle numeric variable expression
df <- df %>%
  mutate(reunification_01 = recode(reunification, "pre" = 0, "post" = 1))  

df <- df %>%
  mutate(east_west_o1 = recode(east_west, "west" = 0, "east" = 1))  

# run heteroscedasticity model twoway interaction
scalinggxe_pre_post <- scalinggxe_test("reunification_01","std_owneduyears_old25","PGI_education", data = df)
# E and G significant


# run heteroscedasticity model for two-way interactions, underlying significant three-way interaction

# Reunification x PGI-Education | East Germany
df_east <- df %>% filter(east_west == "east")
scalinggxe_pre_post_EAST <- scalinggxe_test("reunification_01","std_owneduyears_old25","PGI_education", data = df_east)
# E and G significant

# Region x PGI-Education | Post Reunification
df_post <- df %>% filter(reunification == "post")
scalinggxe_east_west_POST <- scalinggxe_test("east_west_o1","std_owneduyears_old25","PGI_education", data = df_post)
# E and G significant


####### 2. Set Birth Year Analyses #######

#### Model 1b: Birth Year ####  

results_birth <- lm(std_owneduyears_old25 ~
                      
                             PGI_education*gender #control for gender * G
                            + std_birth*gender  #control for gender * E
                    
                            + PGI_education*std_birth #interaction of interest
                          
                          ,  data = df)

summary(results_birth)



table_birth <- tidy(summary(results_birth), conf.int = TRUE)
table_birth <- table_birth %>%  dplyr::select(-statistic) %>%  rename("Term" = term, "b" = estimate) 


#delete covariate checks
table_birth <-  table_birth[c(2,4,7 ),]      

#round values
table_birth <- table_birth %>% mutate(across(where(is.numeric), ~ round(., 3)))

#make apa table
table_birth_apa <-  nice_table(table_birth,
                                title = c("Model with twoway-interaction: PGI-Education x pre- vs. post-reunification"),
                                note = (paste("N = ", length(results_birth[["model"]][,1]) ,
                                              ";  We controlled for the two covariates Gender, and PGI x Gender, Birth Year x Gender and EastWest X Gender.")))
table_birth_apa
#print(table_birth_apa, preview = "docx")



#### Model 3b: East - West & Birth Year ####  

result_eastwest_cohort <- lm(std_owneduyears_old25 ~
                              
                             # Testing for threeway interactions with gender
                             + PGI_education*east_west*gender  #control for gender * G
                             + std_birth*east_west*gender  #control for gender * E1
                             + PGI_education*std_birth*gender      #control for gender * E2

                             + PGI_education*std_birth*east_west  #interaction of interest
                                 
                          ,   data = df)

summary(result_eastwest_cohort)

table_eastwest_cohort <- tidy(summary(result_eastwest_cohort), conf.int = TRUE)
table_eastwest_cohort <- table_eastwest_cohort %>%  dplyr::select(-statistic) %>%  rename("Term" = term, "b" = estimate)


#delete covariate checks
table_eastwest_cohort <-  table_eastwest_cohort[c(2,5,3,
                                                  11,6,9,
                                                  15),] # Put paramters in desired order

#round values
table_eastwest_cohort <- table_eastwest_cohort %>% mutate(across(where(is.numeric), ~ round(., 3)))

#make apa table
table_eastwest_cohort_apa <-  nice_table(table_eastwest_cohort,
                                         title = c("Model with threeway-interaction: PGI-Education x Birth Year x East vs. West Germany"),
                                         note = (paste("N = ", length(result_eastwest_cohort[["model"]][,1]) , 
                                                       "; We controlled for the two covariates Gender, and PGI xGender, Birth Year x Gender and EastWest x Gender, as well as a PGI x Birth Year x Gender, EastWest x Birth Year x Gender and PGI x EastWest x Gender")))

table_eastwest_cohort_apa
#print(table_eastwest_cohort_apa, preview = "docx")


#### Post-Hoc Tests Birth Year #### 

# Fit separate regression models for East Germany  
model_east_birth <- lm(std_owneduyears_old25 ~ 
                         PGI_education * std_birth+
                         
                         PGI_education*gender +
                         std_birth*gender 
                         
                       , data = df_east) 
summary(model_east_birth)

# Fit separate regression models for West Germany  
model_west_birth <- lm(std_owneduyears_old25 ~ 
                         PGI_education * std_birth+
                         
                         PGI_education*gender +
                         std_birth*gender 

                       , data = df_west) 
summary(model_west_birth)

#### non-parametric Plotting Birth Years ####

library(np)


results <- data.frame(
  birth_year = integer(),
  group = character(),
  beta = numeric(),
  lower_CI = numeric(),
  upper_CI = numeric(),
  bw = numeric()
)



# Loop over east vs. west
for (group in levels(df_allcohorts$east_west)) {
  
  df_group <- df_allcohorts[complete.cases( df_allcohorts[c('std_owneduyears_old25', "PGI_education", 'birth', "east_west")]),] 
  df_group <- subset(df_group, east_west == group)
  
  # Determine optimal bandwidth using the np package
  bw <- npregbw( xdat = cbind(df_group$birth, df_group$PGI_education ), ydat = as.vector(df_group$std_owneduyears_old25))$bw
  
  birth_years <- c(ceiling(min(df_group$birth)):floor(max(df_group$birth)))
  
  # Loop over each birth year
  for (year in birth_years) {
    
    
    # Compute kernel weights
    weights <- dnorm((df_group$birth - year) / bw[1])
    
    # Fit the weighted regression model
    model <- lm(std_owneduyears_old25 ~ PGI_education + gender, data = df_group, weights = weights)
    
  
    # Extract the beta coefficient and confidence intervals
    beta <- coef(model)["PGI_education"]
    
    # Calculate effective sample size
    n_eff <- sum(weights)^2 / sum(weights^2)
    
    # Get standard errors from model
    se_standard <- sqrt(diag(vcov(model)))["PGI_education"]
    
    # Adjust standard error
    n <- length(weights)
    se_adjusted <- se_standard * sqrt(n / n_eff)
    
    # Calculate confidence intervals
    lower_CI <- beta - 1.96 * se_adjusted
    upper_CI <- beta + 1.96 * se_adjusted
  
    # Same results when using a robust SEs from sandwich estimator
    # library(sandwich)
    # robust_se <- sqrt(diag(vcovHC(model, type = "HC1")))
    # se <- robust_se["PGI_education"]
    
    # Save the results
    results <- rbind(results, data.frame(
      birth_year = year,
      group = group,
      beta = beta,
      lower_CI = lower_CI,
      upper_CI = upper_CI,
      bw = bw[1]
    ))
  }
}


# Plot the beta coefficients over birth years
np_birth <- ggplot(results, aes(x = birth_year, y = beta, color = group)) +
  geom_line() +
  geom_ribbon(aes(ymin = lower_CI, ymax = upper_CI, fill = group), alpha = 0.2, linetype = 3) +
  labs(
    title = "",# "Association between Years of Education and PGI-education over Birth Years",
    x = "Birth Year",
     y = "Std. Beta Coefficient of PGI-Education"
  ) +
  scale_x_continuous(limits = c(1922, 1998), breaks = seq(1925, 1995, 10)) +  # Set the x-axis limits and breaks
  scale_y_continuous(limits = c(-0.25, 1), breaks = seq(-0.2, 0.9, 0.1)) +  # Set the x-axis limits and breaks
  scale_color_manual(values = c("#be1918","#000080"),         
                     name = "German Region",         
                     labels = c("East Germany", "West Germany")) +   
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1) )+
  theme( legend.position = "none") +
  
  #Add lines for historical events
  geom_vline(xintercept = 1975, color = "black", linetype = 2) + # Reunification
  geom_vline(xintercept = 1934, color = "black", linetype = 2) +  # Division

geom_hline(yintercept = 0, color = "black",size =  0.1)

np_birth

#ggsave(paste0(dir_plot, "FraemkeFig1B_",format(Sys.Date(),"%Y%m%d" ),".pdf"),np_birth, width = 15.92, height = 8.5, units = "cm",  dpi = 400)



##### Heteroscedasticity #####

# run heteroscedasticity model two-way interaction
scalinggxe_birth <- scalinggxe_test("std_birth","std_owneduyears_old25","PGI_education", data = df)
# E and G significant

# run heteroscedasticity model for two-way interactions, which are underlying significant three-way interaction
# Birth Year x PGI-Education | East Germany
df_east <- df %>% filter(east_west == "east")
scalinggxe_birth_EAST <- scalinggxe_test("std_birth","std_owneduyears_old25","PGI_education", data = df_east)
# E and G significant


##### Combine all Heteroscedasticity models to formatted APA-table ####


table_scaling_GxE <- as.data.frame(rbind(scalinggxe_pre_post,
                                                   scalinggxe_pre_post_EAST,
                                                   scalinggxe_east_west_POST,
                                                   scalinggxe_birth, 
                                                   scalinggxe_birth_EAST))

Interaction <-  c(
  "PGI_education x Reunification",
  "PGI_education x Reunification | East Germans ",
  "PGI_education x Region | Post Reunification ",
  "PGI_education x Birth Year",
  "PGI_education x Birth Year | East Germans")

table_scaling <- cbind(Interaction,table_scaling_GxE)

#put in right order
table_scaling <- table_scaling[ ,c(1,2,7,
                                   3,8,
                                   4,9,
                                   5,10,
                                   6,11,
                                   12,13,
                                   14:19) ]

colnames(table_scaling) <- c( "Interaction Term", "tau.value","tau.se",  
                              "pi_0.value","pi_0.se",
                              "pi_1.value","pi_1.se",
                              "lambda_0.value","lambda_0.se",
                              "lambda_1.value","lambda_1.se", 
                              "lambda_2.value","lambda_2.se", 
                               "E.value","E.se","E.p",
                              "G.value","G.se","G.p")

table_scaling <- table_scaling %>% mutate(across(where(is.numeric), ~ round(., 2)))

# identify Se and Value columns
se_columns <- grep("\\.se$", colnames(table_scaling), value = TRUE)
value_columns <- gsub("\\.se$", ".value", se_columns)

# Put SE's in bracket
table_scaling_bracketed <- table_scaling %>%
  mutate(across(all_of(value_columns), ~ paste0(., " (", table_scaling[[gsub(".value", ".se", cur_column())]], ")"))) %>%
  dplyr::select(-all_of(se_columns)) %>%
  rename_with(~ gsub("\\.value$", "", .), ends_with(".value"))

# Example of formatting output for your table
table_scaling_apa <- nice_table(table_scaling_bracketed, separate.header = TRUE,
                                title = c("Model Paramaters of full heteroscedasticity models in GxE Analyses"),
                                note = ("Significant Chi²-tests of Xi indicate, that the Gene-by-environment interaction is not driven by heteroscedasticity in the environment or the PGI correspondingly. H0: Xi = 0; Gene-by-environment interaction is driven by dispersion in outcome related to G or E. H1: Xi ≠ 0; Gene-by-environment interaction is not driven by dispersion in outcome related to G or E.  There is no established procedure for testing for heteroscedasticity in three-way interactions yet."))
table_scaling_apa
# Print the resulting table
#print(table_scaling_apa, preview = "docx")



####### 3. Set Negative Control Analyses #######

#### Data Preparation #####

        #add PGI for height
        dat_gene_raw <- read.table( file = "~/GSOEP_PGIrepo_v1.1.txt", header = T)
        
        #Choose PGI
        dat_gene <- dat_gene_raw[c("IID","PGI_HEIGHT.single",
                                   "PC1","PC2","PC3","PC4", "PC5",
                                   "PC6","PC7","PC8", "PC9","PC10",
                                   "PC11","PC12", "PC13","PC14","PC15",
                                   "PC16", "PC17","PC18","PC19","PC20")]


## Residualise PGI-Height on PCs ####
  

        # Fit linear regression model with PCs
        reg <- lm( PGI_HEIGHT.single ~
                     PC1  + PC2  + PC3  + PC4  + PC5  + PC6  + PC7  + PC8  + PC9  + PC10
                   + PC11 + PC12 + PC13 + PC14 + PC15 + PC16 + PC17 + PC18 + PC19 + PC20
                   , data = dat_gene)
        
        # Create a new column with the residuals
        dat_gene <- dat_gene %>% mutate("PGI_HEIGHT.single_res" := residuals(reg) )
        
        # and then standardize
        dat_gene <- dat_gene %>% mutate('PGI_HEIGHT.single_res_std' = scale(PGI_HEIGHT.single_res) )
        
        
        #plot(dat_gene$PGI_HEIGHT.single,dat_gene$PGI_HEIGHT.single_res)
        #hist(dat_gene$PGI_HEIGHT.single_res)
        
        #Make ID the same as in main data set
        dat_gene$IID  <-  substr(dat_gene$IID, 6, 10)
        colnames(dat_gene)[1] <- "ID"
        
        #reduce to relevant standardized and residualised PGIs 
        dat_gene <- dat_gene[c("ID", "PGI_HEIGHT.single_res_std")]
        
        #merge PGI-Height with main dataset
        dat_gene$ID <- as.numeric(dat_gene$ID)
        df <- left_join(df,dat_gene, by = c("ID"))
        
        #change name to 
        df <- df %>% mutate(PGI_height = PGI_HEIGHT.single_res_std)

        
## PGI-Height Control Analyses Reunification & EA ####

result_Height_EA_Control <- lm(std_owneduyears_old25 ~
                                 
                                 PGI_height *east_west*reunification  #interaction of interest
                                 
                               + PGI_height    * east_west     * gender     #control for gender * G
                               + reunification * east_west     * gender     #control for gender * Time
                               + PGI_height    * reunification * gender     #control for gender * E
                               
                               , data = df)

summary(result_Height_EA_Control)


table_Height_EA_Control <- tidy(summary(result_Height_EA_Control), conf.int = TRUE)
table_Height_EA_Control <- table_Height_EA_Control %>%  dplyr::select(-statistic) %>%  rename("Term" = term, "b" = estimate) #LR: exclude "t" in the columns


#delete covariate checks
table_Height_EA_Control <-  table_Height_EA_Control[c(2:4,6:8, 12),]      

#round values
table_Height_EA_Control <- table_Height_EA_Control %>% mutate(across(where(is.numeric), ~ round(., 3)))

#make apa table
table_Height_EA_Control_apa <-  nice_table(table_Height_EA_Control,
                                           title = c("Model with threeway-interaction: PGI-Education x pre- vs. post-reunification x East vs. West Germany"),
                                           note = (paste("N = ", length(result_Height_EA_Control[["model"]][,1]) ,
                                                         ";We controlled for the two covariates Gender, and PGI x Gender, Birth Year x Gender and EastWest x Gender, as well as 
                                                       PGI x Birth Year x Gender, EastWest x Birth Year x Gender and PGI x EastWest x Gender")))

table_Height_EA_Control_apa
#print(table_Height_EA_Control_apa, preview = "docx")


### Plot the std. Beta ####

# get estimated (marginal) means
emm_height_EA <- emmeans(result_Height_EA_Control, ~ PGI_height | east_west * reunification, at = list(PGI_height = 1))

# Extract the betas for PGI_education in each group
betas_height_EA <- as.data.frame(emtrends(result_Height_EA_Control, ~ east_west * reunification, var = "PGI_height"))

#plot betas
gg_betas_height_EA <- ggplot( betas_height_EA, aes(x = reunification, y = PGI_height.trend, color = east_west, group = east_west)) +  
  
  geom_point(position = position_dodge(width=0.1)) +    
  geom_line(position = position_dodge(width=0.1)) +    
  geom_errorbar(aes(ymin = lower.CL , ymax = upper.CL), width = 0.2, position = position_dodge(width=0.1)) +  # Add error bars    
  
  # no labs for manuscript
  #labs(x = NULL, y = NULL) +
  
  # Axis labels
  xlab("Reunification") +    
  ylab("Std. beta coefficients of PGI-Height") +  
  
  # Customize color and legend
  scale_color_manual(values = c("#000080", "#be1918"),     
                     name = "German Region",     
                     labels = c("West Germany", "East Germany")) +
  
  # Minimal theme
  theme_minimal() +  
  
  # set y-scale
  scale_y_continuous(limits = c(-0.2, 0.5), breaks = seq(-0.2, 0.5, 0.1)) +  # Set the y-axis limits and breaks
  
  # Font size and family customizations based on APA guidelines
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 9, family = "Helvetica"),  # X-axis text in 9-point font
    axis.text.y = element_text(size = 9, family = "Helvetica"),  # Y-axis text in 9-point font
    axis.title.x = element_text(size = 10, family = "Helvetica"),  # X-axis label in 10-point font
    axis.title.y = element_text(size = 10, family = "Helvetica", vjust = 2.5),  # Y-axis label in 10-point font, with vertical adjustment
    
    legend.position = "top",  # Place legend at the top
    legend.text = element_text(size = 9, family = "Helvetica"),  # Legend text in 9-point font
    legend.title = element_text(size = 9, family = "Helvetica"),  # Legend title in 9-point font
    
    plot.title = element_text(hjust = 0.5, size = 12, family = "Helvetica"),  # Title in 12-point font
    strip.text = element_text(size = 18, family = "Helvetica") # Panel labels in 18-point font
    
  )+
  geom_hline(yintercept = 0, color = "black",size =  0.1)

gg_betas_height_EA

# ggsave(paste0(dir_plot,"FraemkeFig2A_", format(Sys.Date(),"%Y%m%d" ),".pdf"),  gg_betas_height_EA, width = 150, height = 100, units = "mm")







## PGI-Height Control Analyses Reunification & Height in cm ####


result_Height_Height_Control <- lm(scale(cmheight) ~
                                     PGI_height*east_west*reunification + #interaction of interest
                                     
                                   + PGI_height    *east_west     *gender     #control for gender * G
                                   + reunification *east_west     *gender     #control for gender * Time
                                   + PGI_height    *reunification *gender #control for gender * E
                                   
                                   , data = df)

summary(result_Height_Height_Control)


table_Height_Height_Control <- tidy(summary(result_Height_Height_Control), conf.int = TRUE)
table_Height_Height_Control <- table_Height_Height_Control %>%  dplyr::select(-statistic) %>%  rename("Term" = term, "b" = estimate) #LR: exclude "t" in the columns


#delete covariate checks
table_Height_Height_Control <-  table_Height_Height_Control[c(2:4,6:8,12),]      

#round values
table_Height_Height_Control <- table_Height_Height_Control %>% mutate(across(where(is.numeric), ~ round(., 3)))

#make apa table
table_Height_Height_Control_apa <-  nice_table(table_Height_Height_Control,
                                               title = c("Model with threeway-interaction: PGI-Education x pre- vs. post-reunification x East vs. West Germany"),
                                               note = (paste("N = ", length(result_Height_Height_Control[["model"]][,1]) ,"; We controlled for the two covariates Gender, and PGI x Gender, Birth Year x Gender and EastWest x Gender, as well as 
                                                       PGI x Birth Year x Gender, EastWest x Birth Year x Gender and PGI x EastWest x Gender")))

table_Height_Height_Control_apa
 #print(table_Height_Height_Control_apa, preview = "docx")
 
 
 ### Plot the std. Beta ####
 
 # get estimated (marginal) means
 emm_height_Height <- emmeans(result_Height_Height_Control, ~ PGI_height | east_west * reunification, at = list(PGI_height = 1))
 
 # Extract the betas for PGI_education in each group
 betas_height_Height <- as.data.frame(emtrends(result_Height_Height_Control, ~ east_west * reunification, var = "PGI_height"))
 
 gg_betas_height_Height <- ggplot( betas_height_Height, aes(x = reunification, y = PGI_height.trend, color = east_west, group = east_west)) +  
   
   geom_point(position = position_dodge(width=0.1)) +    
   geom_line(position = position_dodge(width=0.1)) +    
   geom_errorbar(aes(ymin = lower.CL , ymax = upper.CL), width = 0.2, position = position_dodge(width=0.1)) +  # Add error bars    
   
   # no labs for manuscript
   # labs(x = NULL, y = NULL) +
   
   
   # Axis labels
   xlab("Reunification") +    
   ylab("Std. beta coefficients of PGI-Height") +
   
   
   # Customize color and legend
   scale_color_manual(values = c("#000080", "#be1918"),     
                      name = "German Region",     
                      labels = c("West Germany", "East Germany")) +
   
   # Minimal theme
   theme_minimal() +  
   
   # set y-scale
   scale_y_continuous(limits = c(-0.2, 0.5), breaks = seq(-0.2, 0.5, 0.1)) +  # Set the y-axis limits and breaks
   
   # Font size and family customizations based on APA guidelines
   theme(
     axis.text.x = element_text(angle = 45, hjust = 1, size = 9, family = "Helvetica"),  # X-axis text in 9-point font
     axis.text.y = element_text(size = 9, family = "Helvetica"),  # Y-axis text in 9-point font
     axis.title.x = element_text(size = 10, family = "Helvetica"),  # X-axis label in 10-point font
     axis.title.y = element_text(size = 10, family = "Helvetica", vjust = 2.5),  # Y-axis label in 10-point font, with vertical adjustment
     
     legend.position = "none",  # Place legend at the top
     legend.text = element_text(size = 9, family = "Helvetica"),  # Legend text in 9-point font
     legend.title = element_text(size = 9, family = "Helvetica"),  # Legend title in 9-point font
     
     plot.title = element_text(hjust = 0.5, size = 12, family = "Helvetica"),  # Title in 12-point font
     strip.text = element_text(size = 18, family = "Helvetica") # Panel labels in 18-point font
     
   )+
   geom_hline(yintercept = 0, color = "black",size =  0.1)
 gg_betas_height_Height
 
 
  # ggsave(paste0(dir_plot,"FraemkeFig2B_", format(Sys.Date(),"%Y%m%d" ),".pdf"),  gg_betas_height_Height, width = 150, height = 100, units = "mm")



#### Supplemental Analysis & Results #### 
 
 # Model with Birth year and Reunification in one model
  
  result_eastwest_prepost_birth <- lm(std_owneduyears_old25 ~ 
                                  
                                #it is needed to test for threewayy interactions with covariate
                                + PGI_education*east_west*gender  #control for gender * G
                                + reunification*east_west*gender  #control for gender * E1
                                + PGI_education*reunification*gender      #control for gender * E2
                                
                                + std_birth*east_west*gender  #control for gender * E1
                                + PGI_education*std_birth*gender      #control for gender * E2
                                
                                + PGI_education*reunification*east_west #interaction of interest
                                + PGI_education*std_birth*east_west #interaction of interest
                                
                                
                                , data = df)
  
  summary(result_eastwest_prepost_birth)
  
  
  
  result_eastwest_prepost_birth_clean <- tidy(summary(result_eastwest_prepost_birth), conf.int = TRUE)
  result_eastwest_prepost_birth_clean <- result_eastwest_prepost_birth_clean %>%  dplyr::select(-statistic) %>%  rename("Term" = term, "b" = estimate)
  
  
  #delete covariate checks
  result_eastwest_prepost_birth_clean <-  result_eastwest_prepost_birth_clean[c(2,3,7, #main effect terms
                                                                                5, 12, 10, 21, # reunification terms
                                                                                6, 15, 13, 22 ),] # birth year terms
  
  #round values
  result_eastwest_prepost_birth_clean <- result_eastwest_prepost_birth_clean %>% mutate(across(where(is.numeric), ~ round(., 3)))
  
  #make apa table
  result_eastwest_prepost_birth_clean_apa <-  nice_table(result_eastwest_prepost_birth_clean,
                                                  title = c("Model with threeway-interaction: PGI-Education x pre- vs. post-reunification x East vs. West Germany"),
                                                  note = (paste("N = ", length(result_eastwest_prepost_birth[["model"]][,1]) , "; We controlled for the covariate Gender, and GenexGender, PrePostXGender and EastWestXGender.")))
  #
  result_eastwest_prepost_birth_clean_apa
 # print(result_eastwest_prepost_birth_clean_apa, preview = "docx")

  
  
  
  
## Model in unrelated individuals
  
  # exclude all children who are not the only ones in there families, as they have no related.
  # By this exclusion all individuals in the sample were unrelated
  
  df_unrel <- df %>%
    group_by(hid) %>%
    filter(!(any(reltohead %in% c("child", "grandchild", "sibling")) & n() > 1) | reltohead != "child")

  #run model
  result_eastwest_prepost_unrel <- lm(std_owneduyears_old25 ~ 
                                        
                                        #it is needed to test for threewayy interactions with covariate
                                        + PGI_education*east_west*gender  #control for gender * G
                                      + reunification*east_west*gender  #control for gender * E1
                                      + PGI_education*reunification*gender      #control for gender * E2
                                      
                                      + PGI_education*reunification*east_west #interaction of interest

                                      
                                      
                                      , data = df_unrel)
  
  summary(result_eastwest_prepost_unrel)
  
  
  result_eastwest_prepost_unrel_clean <- tidy(summary(result_eastwest_prepost_unrel), conf.int = TRUE)
  result_eastwest_prepost_unrel_clean <- result_eastwest_prepost_unrel_clean %>%  dplyr::select(-statistic) %>%  rename("Term" = term, "b" = estimate) #Laurel doesnt want "t" in the columns
  
  
  #delete covariate checks
  result_eastwest_prepost_unrel_clean <-  result_eastwest_prepost_unrel_clean[c(2,5,3,
                                                                                11,6,9,
                                                                                15),]
  
  #round values
  result_eastwest_prepost_unrel_clean <- result_eastwest_prepost_unrel_clean %>% mutate(across(where(is.numeric), ~ round(., 3)))
  
  #make apa table
  result_eastwest_prepost_unrel_clean_apa <-  nice_table(result_eastwest_prepost_unrel_clean,
                                                         title = c("Model with threeway-interaction: PGI-Education x pre- vs. post-reunification x East vs. West Germany"),
                                                         note = (paste("N = ", length(result_eastwest_prepost_unrel[["model"]][,1]) , ";  We controlled for the covariate Gender, and GenexGender, PrePostXGender and EastWestXGender.")))
  #
  result_eastwest_prepost_unrel_clean_apa
  #print(result_eastwest_prepost_unrel_clean_apa, preview = "docx")

  
  
    
# Model with only strict Quality control genetic samples
  result_eastwest_prepost_QC <- lm(std_owneduyears_old25 ~ 
                                        
                                        #it is needed to test for threewayy interactions with covariate
                                        + PGI_education*east_west*gender  #control for gender * G
                                      + reunification*east_west*gender  #control for gender * E1
                                      + PGI_education*reunification*gender      #control for gender * E2
                                      
                                      + PGI_education*reunification*east_west #interaction of interest
                                      
                                      
                                      
                                      , data =df[df$QCpass == "Y",])
  
  summary(result_eastwest_prepost_QC)
  
  
  result_eastwest_prepost_QC_clean <- tidy(summary(result_eastwest_prepost_QC), conf.int = TRUE)
  result_eastwest_prepost_QC_clean <- result_eastwest_prepost_QC_clean %>%  dplyr::select(-statistic) %>%  rename("Term" = term, "b" = estimate) #Laurel doesnt want "t" in the columns
  
  
  #delete covariate checks
  result_eastwest_prepost_QC_clean <-  result_eastwest_prepost_QC_clean[c(2,5,3,
                                                                                11,6,9,
                                                                                15),]
  
  #round values
  result_eastwest_prepost_QC_clean <- result_eastwest_prepost_QC_clean %>% mutate(across(where(is.numeric), ~ round(., 3)))
  
  #make apa table
  result_eastwest_prepost_QC_clean_apa <-  nice_table(result_eastwest_prepost_QC_clean,
                                                         title = c("Model with threeway-interaction: PGI-Education x pre- vs. post-reunification x East vs. West Germany"),
                                                         note = (paste("N = ", length(result_eastwest_prepost_QC[["model"]][,1]) , ";  We controlled for the covariate Gender, and GenexGender, PrePostXGender and EastWestXGender.")))
  #
  result_eastwest_prepost_QC_clean_apa
  #print(result_eastwest_prepost_QC_clean_apa, preview = "docx")
  


# Model with BMI as a covariate
  
  result_eastwest_prepost_bmi <- lm(std_owneduyears_old25 ~ 
                                  
                                #it is needed to test for threewayy interactions with covariate
                                + PGI_education*east_west    *gender  #control for gender * G
                                + reunification*east_west    *gender  #control for gender * E1
                                + PGI_education*reunification*gender      #control for gender * E2
                                
                                # with covariate bmi
                                + PGI_education*east_west     *std_bmi   #control for bmi * E2
                                + reunification*east_west     *std_bmi #control for bmi * E1
                                + PGI_education*reunification *std_bmi  #control for bmi * G
                                
                                + PGI_education*reunification*east_west #interaction of interest
                                , data = df)
  
  summary(result_eastwest_prepost_bmi)
  
  
  result_eastwest_prepost_bmi_clean <- tidy(summary(result_eastwest_prepost_bmi), conf.int = TRUE)
  result_eastwest_prepost_bmi_clean <- result_eastwest_prepost_bmi_clean %>%  dplyr::select(-statistic) %>%  rename("Term" = term, "b" = estimate)
  
  
  #take out covariate checks for reporting
  result_eastwest_prepost_bmi_clean <-  result_eastwest_prepost_bmi_clean[c(2,3,5,
                                                                    7,10,12, 
                                                                    22),]
  
  #round values
  result_eastwest_prepost_bmi_clean <- result_eastwest_prepost_bmi_clean %>% mutate(across(where(is.numeric), ~ round(., 3)))
  
  #make apa table
  result_eastwest_prepost_bmi_clean_apa <-  nice_table(result_eastwest_prepost_bmi_clean,
                                                  title = c("Model with threeway-interaction: PGI-Education x pre- vs. post-reunification x East vs. West Germany"),
                                                  note = (paste("N = ", length(result_eastwest_prepost[["model"]][,1]) , "; We controlled for the two covariates Gender, and GenexGender, PrePostXGender and EastWestXGender.")))
  #
  result_eastwest_prepost_bmi_clean_apa
  #print(result_eastwest_prepost_bmi_clean_apa, preview = "docx")

  
  
# Migration x Reunification
 
 
 migration_eastwest_prepost <- lm(PGI_education ~ 

                                 reunification*east_west
                                 + reunification*gender  #control for gender * reuni
                                 + east_west*gender      #control for gender * east_west
                                
                               , data = df)
 
 summary(migration_eastwest_prepost)

 
 migration_eastwest_prepost_clean <- tidy(summary(migration_eastwest_prepost), conf.int = TRUE)
 migration_eastwest_prepost_clean <- migration_eastwest_prepost_clean %>%  dplyr::select(-statistic) %>%  rename("Term" = term, "b" = estimate)
 
 
 #delete covariate checks
 migration_eastwest_prepost_clean <-  migration_eastwest_prepost_clean[c(2,3,5),]
 
 #round values
 migration_eastwest_prepost_clean <- migration_eastwest_prepost_clean %>% mutate(across(where(is.numeric), ~ round(., 3)))
 
 #make apa table
 migration_eastwest_prepost_clean_apa <-  nice_table(migration_eastwest_prepost_clean,
                                                 title = c(""),
                                                 note = (paste("N = ", length(result_eastwest_prepost[["model"]][,1]))))
 #
 migration_eastwest_prepost_clean_apa
 #print(migration_eastwest_prepost_clean_apa, preview = "docx")
 
 # Migration x Birth Year
 migration_eastwest_birth <- lm(PGI_education ~ 
                                 
                                 std_birth*east_west
                               + std_birth*gender  #control for gender * reuni
                               + east_west*gender      #control for gender * east_west
                               
                               , data = df)
 
 summary(migration_eastwest_birth)
 
 migration_eastwest_birth_clean <- tidy(summary(migration_eastwest_birth), conf.int = TRUE)
 migration_eastwest_birth_clean <- migration_eastwest_birth_clean %>%  dplyr::select(-statistic) %>%  rename("Term" = term, "b" = estimate)
 
 
 #delete covariate checks
 migration_eastwest_birth_clean <-  migration_eastwest_birth_clean[c(2,3,5),]
 
 #round values
 migration_eastwest_birth_clean <- migration_eastwest_birth_clean %>% mutate(across(where(is.numeric), ~ round(., 3)))
 
 #make apa table
 migration_eastwest_birth_clean_apa <-  nice_table(migration_eastwest_birth_clean,
                                                     title = c(""),
                                                     note = (paste("N = ", length(migration_eastwest_birth[["model"]][,1]))))
 #
 migration_eastwest_birth_clean_apa
 









