##########################################
#Programmer: `Rachel Sendor
#Last update: Mar 2023
#Purpose:     DRC Non-falciparum Descriptive Epidemiology Analysis
#             
##########################################

##########################################################
#SETTING PARAMETERS / LOADING PACKAGES FOR USE

options(max.print=999999)
options(scipen = 999)
.libPaths()

#Loading R Packages:
library(tidyverse) 
library(dplyr)
library(tidyr)
library(readr)
library(readxl)
library(sf)
library(tableone)
library(devtools)
library(PropCIs)
library(ggplot2)
library(ggbreak)
library(ggthemes)
library(resample)
library(ggpubr)
library(haven)
library(lubridate)
library(plyr)
library(survminer)
library(purrr)
library(stringr)
library(forcats)
library(broom)
library(gee)
library(geepack)
library(ggExtra)
library(janitor)
library(linelist)
library(lme4)
library(lmerTest)
library(ggsurvfit)
library(gtsummary)
library(tidycmprsk)

##########################################################
# SET WORKING DIRECTORY

#setwd("XXXXXX")

# IMPORTING DATASETS FOR ANALYSIS --- DATASET CREATED IN SAS FOR DATA LINKAGE, CLEANING, & PRELIMINARY WRANGLING (SEE PROGRAM: AnalysisDatasetBuild_DRCNonPf_Mar22_RS.sas)
AD_All_BL_FU_VNP_Long_Sub <- read_excel("../Dataset_AD/AD_All_BL_FU_VNP_Long_Sub_Jul22.xlsx", NULL)
View(AD_All_BL_FU_VNP_Long_Sub)


##########################################################
# DEFINING NEW VARS FOR ANALYSIS    

AD_All_BL_FU_VNP_Long_Sub<-AD_All_BL_FU_VNP_Long_Sub %>%
                           mutate(Pm_Species= ifelse(Pm==1&Po==0&Pf==0, "Pm Mono", 
                                              ifelse(Pm==1&Po==1&Pf==0, "Pm Mixed", 
                                              ifelse(Pm==1&Po==0&Pf==1, "Pm Mixed",
                                              ifelse(Pm==1&Po==1&Pf==1, "Pm Mixed", NA)))))%>%
                           mutate(Po_Species= ifelse(Pm==0&Po==1&Pf==0, "Po Mono", 
                                              ifelse(Pm==1&Po==1&Pf==0, "Po Mixed", 
                                              ifelse(Pm==0&Po==1&Pf==1, "Po Mixed",
                                              ifelse(Pm==1&Po==1&Pf==1, "Po Mixed", NA)))))%>%
                           mutate(Pf_Species= ifelse(Pm==0&Po==0&Pf==1, "Pf Mono", 
                                              ifelse(Pm==1&Po==0&Pf==1, "Pf Mixed", 
                                              ifelse(Pm==0&Po==1&Pf==1, "Pf Mixed",
                                              ifelse(Pm==1&Po==1&Pf==1, "Pf Mixed", NA)))))%>%
  
                           mutate(PSpecies_Detail= ifelse(Pm==1&Po==0&Pf==0, "Pm Mono", 
                                                   ifelse(Pm==1&Po==1&Pf==0, "Pm Po", 
                                                   ifelse(Pm==1&Po==0&Pf==1, "Pm Pf",
                                                   ifelse(Pm==1&Po==1&Pf==1, "Pm Po Pf", 
                                                   ifelse(Pm==0&Po==1&Pf==0, "Po Mono", 
                                                   ifelse(Pm==0&Po==1&Pf==1, "Po Pf",
                                                   ifelse(Pm==0&Po==0&Pf==1, "Pf Mono", NA))))))))%>%
                          
                           mutate_at(vars("Rehydrated"), ~replace_na(.,0))%>%
  
                           mutate(Pm_Species_Mono=ifelse(Pm==1&Pm_Species=="Pm Mono", 1,
                                                  ifelse(Pm==1&Pm_Species=="Pm Mixed", 0, NA)))%>%
                           mutate(Po_Species_Mono=ifelse(Po==1&Po_Species=="Po Mono", 1,
                                                  ifelse(Po==1&Po_Species=="Po Mixed", 0, NA)))%>%                

                           mutate(Pf_Species_Mono=ifelse(Pf==1&Pf_Species=="Pf Mono", 1,
                                                  ifelse(Pf==1&Pf_Species=="Pf Mixed", 0, NA)))%>%
                           mutate(Sx_Prior6mo_new=ifelse(Sx_Prior6mo==1, 1, 
                                                  ifelse(Sx_Prior6mo==2, 1, 
                                                  ifelse(Sx_Prior6mo==0, 0, NA))))%>%
                           mutate(AntiMalTx_Prior6mo_new=ifelse(AntiMalTx_Prior6mo==1, 1, 
                                                         ifelse(AntiMalTx_Prior6mo==2, 1, 
                                                         ifelse(AntiMalTx_Prior6mo==0, 0, NA))))%>%
                           mutate(WealthCat = ifelse(WealthQuintile==1, 2,
                                              ifelse(WealthQuintile==2, 2,
                                              ifelse(WealthQuintile==3, 1, 
                                              ifelse(WealthQuintile==4, 3,
                                              ifelse(WealthQuintile==5, 3,NA))))))%>%
                           mutate(HealthArea= ifelse(Village_Lingwala==1, 1,
                                              ifelse(Village_Pema==1, 2, 
                                              ifelse(Village_Impuru==1, 2, 
                                              ifelse(Village_Bu==1, 2,
                                              ifelse(Village_Kimpoko==1, 3, 
                                              ifelse(Village_Ngamanzo==1, 3,
                                              ifelse(Village_Iye==1, 3, NA))))))))
                                                 
##COnfirming New Variable Coding
addmargins(table(AD_All_BL_FU_VNP_Long_Sub$Pm_Species, AD_All_BL_FU_VNP_Long_Sub$Visit_Type2))
addmargins(table(AD_All_BL_FU_VNP_Long_Sub$Po_Species, AD_All_BL_FU_VNP_Long_Sub$Visit_Type2))
addmargins(table(AD_All_BL_FU_VNP_Long_Sub$Pf_Species, AD_All_BL_FU_VNP_Long_Sub$Visit_Type2))
addmargins(table(AD_All_BL_FU_VNP_Long_Sub$HealthArea, AD_All_BL_FU_VNP_Long_Sub$Village))
addmargins(table(AD_All_BL_FU_VNP_Long_Sub$PSpecies_Detail, AD_All_BL_FU_VNP_Long_Sub$Visit_Type2, useNA = "always"))


##ASSESSING MISSING VISITS BY OUTCOME FREQUENCY AND EXCLUDING MISSING VISITS FROM ANALYSIS DATASET
addmargins(table(AD_All_BL_FU_VNP_Long_Sub$PSpecies_Detail, AD_All_BL_FU_VNP_Long_Sub$Visit_Type2, useNA = "always"))
MissingVisits <-AD_All_BL_FU_VNP_Long_Sub%>%filter(is.na(Visit_date))

AD_All_BL_FU_VNP_Long_Sub <-AD_All_BL_FU_VNP_Long_Sub%>%filter(!is.na(Visit_date))


##########################################################
##Separate visits out to Active Survey, Passive Unscheduled Clinic (ie VNP), and Total (Active + Passive) Datasets for Analyses

#Each dataset constructed represents analysis cohorts - survey-based; clinic sub-population, and total (ie all visits)

#Active   = By-visit dataset for BL, FU1, FU2, and FU3 (N=1,565 subs, 6260 visits possible-- but delete out instances of missing visits)
#Passive  = By-visit dataset for all VNP visits (N=1,050 subs, 3,407 VNP visits through end of Dec 2017)
#Total    = By-visit dataset for all Visits (Active and Passive) (N=1,565 subs, 9667 visits possible, excluding missings from active survey)

AD_Active <-AD_All_BL_FU_VNP_Long_Sub%>%filter(Visit_Type1=="Act")
AD_Passive <-AD_All_BL_FU_VNP_Long_Sub%>%filter(Visit_Type1=="Pas")
AD_Total <-AD_All_BL_FU_VNP_Long_Sub

##Quick look at malaria species infections by visit type in new datasets 
addmargins(table(AD_Active$PSpecies_Detail, AD_Active$Pm_Species, useNA = "always"))
addmargins(table(AD_Active$PSpecies_Detail, AD_Active$Po_Species, useNA = "always"))
addmargins(table(AD_Active$PSpecies_Detail, AD_Active$Pf_Species, useNA = "always"))

addmargins(table(AD_Passive$PSpecies_Detail, AD_Passive$Pm_Species, useNA = "always"))
addmargins(table(AD_Passive$PSpecies_Detail, AD_Passive$Po_Species, useNA = "always"))
addmargins(table(AD_Passive$PSpecies_Detail, AD_Passive$Pf_Species, useNA = "always"))


##########################################################
# ASSESSING DIFFERENCES BETWEEN ACTIVE AND PASSIVE COHORTS TO DETERMINE VALIDITY OF INFERENCES FROM PASSIVE POP, & COMPARING TO ACTIVE POP.   
##########################################################

#Create comparison between Active and Passive, and those who did vs. did not have a Passive Visit
#Differences in those who did vs. did not have a Passive Visit might necessitate IPSW to make more similar when comparing the active and passive populations

##Comparing demographics among the active vs. passive populations 

##Comparing demographics at the visit level: 
addmargins(table(AD_Total$Visit_Type1, AD_Total$Visit_Type2, useNA = "always"))

Compare_VNP_Visits_ActvsPass<-CreateTableOne(vars = c("AgeCat_Visit", "Sex", "Village", "HealthArea", "Visit_Type2",  "WealthQuintile", "Feverpos", "RDTpos", "BedNetPrior", "season_dry", "Sx_Prior6mo_new", "AntiMalTx_Prior6mo_new", "Pf", "Pm", "Po", "Pf_Species", "Pm_Species", "Po_Species", "Pd_Pf", "Pd_Pm", "Pd_Po"), 
                                                  strata = "Visit_Type1", 
                                                  data = AD_Total, 
                                                  factorVars = c("AgeCat_Visit", "Sex", "Village", "HealthArea", "Visit_Type2",  "WealthQuintile", "Feverpos", "RDTpos", "BedNetPrior", "season_dry", "Sx_Prior6mo_new", "AntiMalTx_Prior6mo_new", "Pf", "Pm", "Po", "Pf_Species", "Pm_Species", "Po_Species"), 
                                                  smd=TRUE, 
                                                  addOverall=TRUE)

P_Compare_VNPpop1<-print(Compare_VNP_Visits_ActvsPass, nonnormal = c("Pd_Pf", "Pd_Pm", "Pd_Po"), smd=TRUE, showAllLevels = TRUE, noSpaces = T)%>%write.table("clipboard", sep="\t", row.names = T)
summary(Compare_VNP_Visits_ActvsPass)

##Comparing demographics at baseline between subjects who did vs. did not have at least 1 VNP visit during the study:
#Flagging subjects at baseline who had at least 1 passive visit during the study
AD_Passive_Sub<-AD_Passive%>%mutate(PassPop_Flg=1)%>%dplyr::distinct(Subject_ID, PassPop_Flg)
AD_Active_BL<-AD_Active%>%filter(Visit_Type2=="BL")
PassivePop_YN<-left_join(AD_Active_BL, AD_Passive_Sub, by = c("Subject_ID"))
PassivePop_YN_Flg<-PassivePop_YN%>%mutate(PassPop_Flg = ifelse(is.na(PassPop_Flg), 0, PassPop_Flg)) 


addmargins(table(PassivePop_YN_Flg$PassPop_Flg, useNA = "always"))

Compare_VNP_Pop_YesvsNo<-CreateTableOne(vars = c("AgeCat_Visit", "Sex", "Village", "HealthArea", "Visit_Type2", "elevation_m",  "WealthQuintile", "Feverpos", "RDTpos", "BedNetPrior", "season_dry", "Sx_Prior6mo_new", "AntiMalTx_Prior6mo_new", "Pf", "Pm", "Po", "Pf_Species", "Pm_Species", "Po_Species", "Pd_Pf", "Pd_Pm", "Pd_Po"), 
                                        strata = "PassPop_Flg", 
                                        data = PassivePop_YN_Flg, 
                                        factorVars = c("AgeCat_Visit", "Sex", "Village", "HealthArea", "Visit_Type2",  "WealthQuintile", "Feverpos", "RDTpos", "BedNetPrior", "season_dry", "Sx_Prior6mo_new", "AntiMalTx_Prior6mo_new", "Pf", "Pm", "Po", "Pf_Species", "Pm_Species", "Po_Species"), 
                                        smd=TRUE, 
                                        addOverall=TRUE)

P_Compare_VNPpop2<-print(Compare_VNP_Pop_YesvsNo, exact = c("AgeCat_Visit", "Sex", "Feverpos", "Pf", "Pm", "Po"),
                                                  nonnormal = c("Pd_Pf", "Pd_Pm", "Pd_Po"), 
                                                  smd=TRUE, 
                                                  showAllLevels = TRUE, noSpaces = T)%>%write.table("clipboard", sep="\t", row.names = T)
summary(Compare_VNP_Pop_YesvsNo)

##Comparing demographics at baseline at subject-level between subjects who did vs. did not have at least 1 VNP visit during the study:
PassivePop_YN_Total<-left_join(AD_Total, AD_Passive_Sub, by = c("Subject_ID"))
PassivePop_YN_Total_Flg<-PassivePop_YN_Total%>%mutate(PassPop_Flg = ifelse(is.na(PassPop_Flg), 0, PassPop_Flg)) 


Compare_VNP_Subjects_ActvsPass<-CreateTableOne(vars = c("AgeCat_Visit", "Sex", "Village", "HealthArea", "Visit_Type2",  "WealthQuintile", "Feverpos", "Pf", "Pm", "Po"), 
                                             strata = "PassPop_Flg", data = AD_Total, 
                                             factorVars = c("AgeCat_Visit", "Sex", "Village", "Visit_Type2",  "WealthQuintile", "Feverpos", "RDTpos", "BedNetPrior", "season_dry", "Sx_Prior6mo_new", "AntiMalTx_Prior6mo_new", "Pf", "Pm", "Po", "Pf_Species", "Pm_Species", "Po_Species"))

P_Compare_VNPpop1_SubLevel<-print(Compare_VNP_Subjects_ActvsPass, showAllLevels = TRUE, noSpaces = T)%>%write.table("clipboard", sep="\t", row.names = T)


##Comparing demographics in VNP vists between those that occurred during active follow-up period (last FU3 visit 10-20-16), vs. those that continued past active FU
##Comparing demographics at the visit level: 

table(AD_Active$Visit_date, AD_Active$Visit)
table(AD_Passive$Visit_date, AD_Passive$Visit)

AD_Passive2<-AD_Passive%>%mutate(Flg_VNP_afteractive=ifelse(Visit_date<"2016-11-01", 0,
                                                     ifelse(Visit_date>"2016-10-30", 1, NA)))
addmargins(table(AD_Passive2$Visit_date, AD_Passive2$Flg_VNP_afteractive, useNA = "always"))

Compare_VNP_Visits_BeforevsAfterAct<-CreateTableOne(vars = c("AgeCat_Visit", "Sex", "Village", "Visit_Type2",  "WealthQuintile", "Feverpos", "Pf", "Pm", "Po", "anemia_WHO"), 
                                             strata = "Flg_VNP_afteractive", data = AD_Passive2, factorVars = c("AgeCat_Visit", "Sex", "Village", "Visit_Type2", "WealthQuintile", "Feverpos", "Pf", "Pm", "Po", "anemia_WHO"))

P_Compare_VNPpop3<-print(Compare_VNP_Visits_BeforevsAfterAct,  showAllLevels = TRUE, noSpaces = T)%>%write.table("clipboard", sep="\t", row.names = T)


##Summarizing demographics at baseline between active and passive surveillance populations: 
Compare_VNP_Pop_YesvsNo<-CreateTableOne(vars = c("AgeCat_Visit", "Sex", "Village", "Visit_Type2",  "WealthQuintile", "Feverpos", "Pf", "Pm", "Po"), 
                                        strata = "PassPop_Flg", data = PassivePop_YN_Flg, factorVars = c("AgeCat_Visit", "Sex", "Village", "Visit_Type2", "WealthQuintile", "Feverpos", "Pf", "Pm", "Po"), includeNA = F, addOverall=TRUE)

PassivePop_YN_Flg_BL<-PassivePop_YN_Flg%>%filter(Visit == 0)
Compare_VNP_Pop_YesvsNo<-CreateTableOne(vars = c("AgeCat_Visit", "Sex", "Village", "Visit_Type2",  "WealthQuintile", "Feverpos", "RDTpos", "BedNetPrior", "season_dry", "Sx_Prior6mo_new", "AntiMalTx_Prior6mo_new", "Pf", "Pm", "Po", "Pf_Species", "Pm_Species", "Po_Species", "Pd_Pf", "Pd_Pm", "Pd_Po"), 
               strata = "PassPop_Flg", 
               data = PassivePop_YN_Flg_BL, 
               #includeNA = T,
               factorVars = c("AgeCat_Visit", "Sex", "Village", "HealthArea", "Visit_Type2",  "WealthQuintile", "Feverpos", "RDTpos", "BedNetPrior", "season_dry", "Sx_Prior6mo_new", "AntiMalTx_Prior6mo_new", "Pf", "Pm", "Po", "Pf_Species", "Pm_Species", "Po_Species"))


P_Compare_VNPpop2<-print(Compare_VNP_Pop_YesvsNo, nonnormal = c("Pd_Pf", "Pd_Pm", "Pd_Po", "elevation_m"), showAllLevels = TRUE, noSpaces = T)%>%write.table("clipboard", sep="\t", row.names = T)


##########################################################
# Descriptive Tables of Demographic and Clinical Characteristics:    
##########################################################

##Creating a new calendar time variable for passive visit to categorize according to 6-month intervals
#Merge in baseline visit date to passive visits, then calculate difference between baseline and passive visit (in months), and categorize into 6-month intervals

AD_Active_BL2<-AD_Active_BL%>%dplyr::select(Subject_ID, Visit_date)%>%dplyr::rename(Visit_date_BL=Visit_date)
AD_Passive3<-left_join(AD_Passive2, AD_Active_BL2, by = c("Subject_ID"))
AD_Passive4<-AD_Passive3%>%mutate(MonthDiff_VisDate_BL_VNP=((Visit_date-Visit_date_BL)/30.4))%>%
                           mutate(Time_VNP_6moninterval_BL=ifelse(MonthDiff_VisDate_BL_VNP<6,  1, 
                                                           ifelse(MonthDiff_VisDate_BL_VNP>=6&MonthDiff_VisDate_BL_VNP<12, 2, 
                                                           ifelse(MonthDiff_VisDate_BL_VNP>=12&MonthDiff_VisDate_BL_VNP<18, 3,
                                                           ifelse(MonthDiff_VisDate_BL_VNP>=18&MonthDiff_VisDate_BL_VNP<24, 4,
                                                           ifelse(MonthDiff_VisDate_BL_VNP>=24&MonthDiff_VisDate_BL_VNP<30, 5,
                                                           ifelse(MonthDiff_VisDate_BL_VNP>=30&MonthDiff_VisDate_BL_VNP<36, 6, NA)))))))%>%
                            mutate(Time_VNP_12moninterval_BL=ifelse(Time_VNP_6moninterval_BL<3, 1, #visits within 6 and 12 months
                                                            ifelse(Time_VNP_6moninterval_BL>2&Time_VNP_6moninterval_BL<5, 2, #visits between 12 and 24 months
                                                            ifelse(Time_VNP_6moninterval_BL>4, 3, NA)))) #visits after 25 months       


#### By Analysis Population - Overall and by Follow-up Visit & Species: 

#Active Population 
##Look by visit across same species, and between species
##exclude missing visits 
AD_Active_nonmissvisits<-AD_Active%>%filter(!is.na(Visit_date))

#overall - all species in active follow-up. 
Demog_Active_AllVisits<-CreateTableOne(vars = c("AgeCat_Visit", "Sex", "Village", "HealthArea", "Visit_Type2",  "WealthQuintile", "Feverpos", "RDTpos", "BedNetPrior", "season_dry", "Sx_Prior6mo_new", "AntiMalTx_Prior6mo_new", "Pf", "Pm", "Po", "Pf_Species", "Pm_Species", "Po_Species", "Pd_Pf", "Pd_Pm", "Pd_Po"), 
                                                    strata = "Visit", 
                                                    data = AD_Active_nonmissvisits, 
                                                    includeNA = F,
                                                    factorVars = c("AgeCat_Visit", "Sex", "Village", "HealthArea", "Visit_Type2",  "WealthQuintile", "Feverpos", "RDTpos", "BedNetPrior", "season_dry", "Sx_Prior6mo_new", "AntiMalTx_Prior6mo_new", "Pf", "Pm", "Po", "Pf_Species", "Pm_Species", "Po_Species"))

P_Demog_Active_AllVisits<-print(Demog_Active_AllVisits, exact = c( "season_dry", "Sx_Prior6mo_new", "Pm", "Po", "Pf_Species", "Pm_Species", "Po_Species"), nonnormal = c("Pd_Pf", "Pd_Pm", "Pd_Po"), showAllLevels = TRUE, is.na=F, noSpaces = T)%>%write.table("clipboard", sep="\t", row.names = T)

summary(Demog_Active_AllVisits)


#overall - Pf species in active follow-up, by visit 
Demog_Active_PfVisits<-CreateTableOne(vars = c("AgeCat_Visit", "Sex", "Village", "Visit_Type2",  "WealthQuintile", "Feverpos", "RDTpos", "BedNetPrior", "season_dry", "Sx_Prior6mo_new", "AntiMalTx_Prior6mo_new", "Pf", "Pm", "Po", "Pf_Species", "Pm_Species", "Po_Species", "Pd_Pf", "Pd_Pm", "Pd_Po"), 
                                      strata = "Pf",
                                      # strata = "Pf_Species", 
                                       data = AD_Active_nonmissvisits, 
                                       includeNA = T,
                                       factorVars = c("AgeCat_Visit", "Sex", "Village", "Visit_Type2",  "WealthQuintile", "Feverpos", "RDTpos", "BedNetPrior", "season_dry", "Sx_Prior6mo_new", "AntiMalTx_Prior6mo_new", "Pf", "Pm", "Po", "Pf_Species", "Pm_Species", "Po_Species"))

Pf_Demog_Active_AllVisits<-print(Demog_Active_PfVisits, nonnormal = c("Pd_Pf", "Pd_Pm", "Pd_Po"), 
                                                        #exact = c("AgeCat_Visit", "Sex", "Village", "Visit_Type2",  "WealthQuintile", "Feverpos", "RDTpos", "BedNetPrior", "season_dry", "Sx_Prior6mo", "AntiMalTx_Prior6mo", "Pf", "Pm", "Po", "Pf_Species", "Pm_Species", "Po_Species"), 
                                                        showAllLevels = TRUE, is.na=T, noSpaces = T)%>%write.table("clipboard", sep="\t", row.names = T)


###Baseline Visit Only 
AD_Active_nonmiss_BL<-AD_Active%>%filter(Visit_Type2=="BL")
Demog_Active_Pf_BL<-CreateTableOne(vars = c("AgeCat_Visit", "Sex", "Village", "Visit_Type2",  "WealthQuintile", "Feverpos", "RDTpos", "BedNetPrior", "season_dry", "Sx_Prior6mo_new", "AntiMalTx_Prior6mo_new", "Pf", "Pm", "Po", "Pf_Species", "Pm_Species", "Po_Species", "Pd_Pf", "Pd_Pm", "Pd_Po"), 
                                      strata = "Pf",
                                      # strata = "Pf_Species", 
                                      data = AD_Active_nonmiss_BL, 
                                      #includeNA = T,
                                      factorVars = c("AgeCat_Visit", "Sex", "Village", "Visit_Type2",  "WealthQuintile", "Feverpos", "RDTpos", "BedNetPrior", "season_dry", "Sx_Prior6mo_new", "AntiMalTx_Prior6mo_new", "Pf", "Pm", "Po", "Pf_Species", "Pm_Species", "Po_Species"))

Demog_Active_Pf_BL_print<-print(Demog_Active_Pf_BL, nonnormal = c("Pd_Pf", "Pd_Pm", "Pd_Po"), 
                                #exact = c("AgeCat_Visit", "Sex", "Village", "Visit_Type2",  "WealthQuintile", "Feverpos", "RDTpos", "BedNetPrior", "season_dry", "Sx_Prior6mo", "AntiMalTx_Prior6mo", "Pf", "Pm", "Po", "Pf_Species", "Pm_Species", "Po_Species"), 
                                showAllLevels = TRUE, 
                          #Turn on and off the below line to assess missingness by var, vs. calculate p-values excluding missingness.  
                                #is.na=T, 
                                noSpaces = T)%>%write.table("clipboard", sep="\t", row.names = T)


#overall - Pm species in active follow-up, by visit 
Demog_Active_PmVisits<-CreateTableOne(vars = c("AgeCat_Visit", "Sex", "Village", "Visit_Type2",  "WealthQuintile", "Feverpos", "RDTpos", "BedNetPrior", "season_dry", "Sx_Prior6mo_new", "AntiMalTx_Prior6mo_new", "Pf", "Pm", "Po", "Pf_Species", "Pm_Species", "Po_Species", "Pd_Pf", "Pd_Pm", "Pd_Po"), 
                                      strata = "Pm", 
                                      data = AD_Active_nonmissvisits, 
                                      includeNA = T,
                                      factorVars = c("AgeCat_Visit", "Sex", "Village", "Visit_Type2",  "WealthQuintile", "Feverpos", "RDTpos", "BedNetPrior", "season_dry", "Sx_Prior6mo_new", "AntiMalTx_Prior6mo_new", "Pf", "Pm", "Po", "Pf_Species", "Pm_Species", "Po_Species"))

Pm_Demog_Active_AllVisits<-print(Demog_Active_PmVisits, nonnormal = c("Pd_Pf", "Pd_Pm", "Pd_Po"), exact = c("AgeCat_Visit", "Sex", "Village", "Visit_Type2",  "WealthQuintile", "Feverpos", "RDTpos", "BedNetPrior", "season_dry", "Sx_Prior6mo_new", "AntiMalTx_Prior6mo_new", "Pf", "Pm", "Po", "Pf_Species", "Pm_Species", "Po_Species"), showAllLevels = TRUE, is.na=T, noSpaces = T)%>%write.table("clipboard", sep="\t", row.names = T)

###Baseline Visit Only 
Demog_Active_Pm_BL<-CreateTableOne(vars = c("AgeCat_Visit", "Sex", "Village", "Visit_Type2",  "WealthQuintile", "Feverpos", "RDTpos", "BedNetPrior", "season_dry", "Sx_Prior6mo_new", "AntiMalTx_Prior6mo_new", "Pf", "Pm", "Po", "Pf_Species", "Pm_Species", "Po_Species", "Pd_Pf", "Pd_Pm", "Pd_Po"), 
                                   strata = "Pm",
                                   # strata = "Pf_Species", 
                                   data = AD_Active_nonmiss_BL, 
                                   #includeNA = T,
                                   factorVars = c("AgeCat_Visit", "Sex", "Village", "Visit_Type2",  "WealthQuintile", "Feverpos", "RDTpos", "BedNetPrior", "season_dry", "Sx_Prior6mo_new", "AntiMalTx_Prior6mo_new", "Pf", "Pm", "Po", "Pf_Species", "Pm_Species", "Po_Species"))

Demog_Active_Pm_BL_print<-print(Demog_Active_Pm_BL, nonnormal = c("Pd_Pf", "Pd_Pm", "Pd_Po"), 
                                #exact = c("AgeCat_Visit", "Sex", "Village", "Visit_Type2",  "WealthQuintile", "Feverpos", "RDTpos", "BedNetPrior", "season_dry", "Sx_Prior6mo", "AntiMalTx_Prior6mo", "Pf", "Pm", "Po", "Pf_Species", "Pm_Species", "Po_Species"), 
                                showAllLevels = TRUE, 
                                #Turn on and off the below line to assess missingness by var, vs. calculate p-values excluding missingness.  
                                #is.na=T, 
                                noSpaces = T)%>%write.table("clipboard", sep="\t", row.names = T)


#overall - Po species in active follow-up, by visit 
Demog_Active_PoVisits<-CreateTableOne(vars = c("AgeCat_Visit", "Sex", "Village", "Visit_Type2",  "WealthQuintile", "Feverpos", "RDTpos", "BedNetPrior", "season_dry", "Sx_Prior6mo_new", "AntiMalTx_Prior6mo_new", "Pf", "Pm", "Po", "Pf_Species", "Pm_Species", "Po_Species", "Pd_Pf", "Pd_Pm", "Pd_Po"), 
                                      strata = "Po_Species", 
                                      data = AD_Active_nonmissvisits, 
                                      includeNA = T,
                                      factorVars = c("AgeCat_Visit", "Sex", "Village", "Visit_Type2",  "WealthQuintile", "Feverpos", "RDTpos", "BedNetPrior", "season_dry", "Sx_Prior6mo_new", "AntiMalTx_Prior6mo_new", "Pf", "Pm", "Po", "Pf_Species", "Pm_Species", "Po_Species"))

Po_Demog_Active_AllVisits<-print(Demog_Active_PoVisits, nonnormal = c("Pd_Pf", "Pd_Pm", "Pd_Po"), exact = c("AgeCat_Visit", "Sex", "Village", "Visit_Type2",  "WealthQuintile", "Feverpos", "RDTpos", "BedNetPrior", "season_dry", "Sx_Prior6mo_new", "AntiMalTx_Prior6mo_new", "Pf", "Pm", "Po", "Pf_Species", "Pm_Species", "Po_Species"), showAllLevels = TRUE, is.na=T, noSpaces = T)%>%write.table("clipboard", sep="\t", row.names = T)

###Baseline Visit Only 
Demog_Active_Po_BL<-CreateTableOne(vars = c("AgeCat_Visit", "Sex", "Village", "Visit_Type2",  "WealthQuintile", "Feverpos", "RDTpos", "BedNetPrior", "season_dry", "Sx_Prior6mo_new", "AntiMalTx_Prior6mo_new", "Pf", "Pm", "Po", "Pf_Species", "Pm_Species", "Po_Species", "Pd_Pf", "Pd_Pm", "Pd_Po"), 
                                   strata = "Po",
                                   # strata = "Po_Species", 
                                   data = AD_Active_nonmiss_BL, 
                                   includeNA = T,
                                   factorVars = c("AgeCat_Visit", "Sex", "Village", "Visit_Type2",  "WealthQuintile", "Feverpos", "RDTpos", "BedNetPrior", "season_dry", "Sx_Prior6mo_new", "AntiMalTx_Prior6mo_new", "Pf", "Pm", "Po", "Pf_Species", "Pm_Species", "Po_Species"))

Demog_Active_Po_BL_print<-print(Demog_Active_Po_BL, nonnormal = c("Pd_Po", "Pd_Pm", "Pd_Po"), 
                                #exact = c("AgeCat_Visit", "Sex", "Village", "Visit_Type2",  "WealthQuintile", "Feverpos", "RDTpos", "BedNetPrior", "season_dry", "Sx_Prior6mo", "AntiMalTx_Prior6mo", "Pf", "Pm", "Po", "Pf_Species", "Pm_Species", "Po_Species"), 
                                showAllLevels = TRUE, 
                                #Turn on and off the below line to assess missingness by var, vs. calculate p-values excluding missingness.  
                                is.na=T, 
                                noSpaces = T)%>%write.table("clipboard", sep="\t", row.names = T)


##
#Passive Population
##

# Looking at number of visits and number of subjects across 1-year, 2-years, and 2+ years from baseline 
# visits by timepoint from baseline 
addmargins(table(AD_Passive4$Time_VNP_6moninterval_BL, useNA = "always"))
addmargins(table(AD_Passive4$Time_VNP_12moninterval_BL, useNA = "always"))
#addmargins(table(AD_Passive4$Visit_date, useNA = "always"))
#addmargins(table(AD_Passive4$Visit_date_BL, useNA = "always"))

# subjects by timepoint from baseline 
AD_Passive4_subjectlvl_6mon<-AD_Passive4%>%
                        group_by(Time_VNP_6moninterval_BL)%>%
                        distinct(Subject_ID)
addmargins(table(AD_Passive4_subjectlvl_6mon$Time_VNP_6moninterval_BL))


AD_Passive4_subjectlvl_year<-AD_Passive4%>%
                        group_by(Time_VNP_12moninterval_BL)%>%
                        distinct(Subject_ID)
addmargins(table(AD_Passive4_subjectlvl_year$Time_VNP_12moninterval_BL))


#overall - all species in passive follow-up. 
Demog_Passive_AllVisits<-CreateTableOne(vars = c("AgeCat_Visit", "Sex", "Village", "HealthArea", "Visit_Type2",  "WealthQuintile", "Feverpos", "RDTpos", "anemia_WHO", "season_dry",  "Pf", "Pm", "Po", "Pf_Species", "Pm_Species", "Po_Species", "Pd_Pf", "Pd_Pm", "Pd_Po"), 
                                       strata = "Time_VNP_12moninterval_BL", 
                                       data = AD_Passive4, 
                                       includeNA = F,
                                       factorVars = c("AgeCat_Visit", "Sex", "Village", "HealthArea", "Visit_Type2",  "WealthQuintile", "Feverpos", "RDTpos", "anemia_WHO", "season_dry", "Pf", "Pm", "Po", "Pf_Species", "Pm_Species", "Po_Species"))

P_Demog_Passive_AllVisits<-print(Demog_Passive_AllVisits, exact = c( "season_dry", "Feverpos", "RDTpos",  "Pm", "Po", "Pf_Species", "Pm_Species", "Po_Species"), nonnormal = c("Pd_Pf", "Pd_Pm", "Pd_Po"), showAllLevels = TRUE, is.na=F, noSpaces = T)%>%write.table("clipboard", sep="\t", row.names = T)

summary(Demog_Passive_AllVisits)


# Median (IQR) number of passive visits by subject and by household -- in-text numbers for manuscript
AD_Passive_Subvisitcount<-AD_Passive4%>%
                                        group_by(Subject_ID) %>%
                                        dplyr::summarise(count=n())
summary(AD_Passive_Subvisitcount$count)

      #RESULTS FOR MEDIAN # VNP VISITS PER SUBJECT IN PASSIVE POP (n=1050)
      #Min.   1st Qu.  Median    Mean 3rd Qu.    Max. 
      #1.000   1.000   2.000   3.245   4.000  19.000 

AD_Passive_HHvisitcount<-AD_Passive4%>%
                                      group_by(Household) %>%
                                      dplyr::summarise(count=n())
summary(AD_Passive_HHvisitcount$count)

      #RESULTS FOR MEDIAN # VNP VISITS PER HOUSEHOLD IN PASSIVE POP (n=1050)
      #Min.  st Qu. Median  Mean    3rd Qu.    Max. 
      #1.00  5.00   11.00   15.63   21.75     74.00 

      ##Looking at which household had 74 VNP visits (max)
      print(AD_Passive_HHvisitcount$Household[AD_Passive_HHvisitcount$count==74])
      
      
######
#Checking distribution of visits over time 
  
      #Active Pop
      ggplot(AD_Active_nonmissvisits, aes(Visit_date))+
        geom_histogram()
      
      
      ggplot(AD_Active_nonmissvisits, aes(Visit))+
        geom_histogram()
      
      #Active Pop by Health Area
         ggplot(AD_Active_nonmissvisits, aes(Visit))+
            geom_histogram()+
            facet_grid(rows = vars(Village))
      
      #VNP Pop
      AD_Passive4<-apply_labels(AD_Passive4, 
                                village=c("Bu" = 1, "Impuru" = 2, "Pema" = 3, "Kimpoko" = 4, "Ngamanzo" = 5, "Iye" = 6, "Voix du Peuple" = 7))
      
        ggplot(AD_Passive4, aes(Time_VNP_6moninterval_BL))+
        geom_histogram()
      
        ggplot(AD_Passive4, aes(Time_VNP_12moninterval_BL))+
        geom_histogram()

      ggplot(AD_Passive4, aes(Visit_date))+
          geom_histogram()+
          facet_grid(rows = vars(Village))
      
      
 ##Look by visit across same species, and between species
  ##exclude missing visits 
PassivePop_Y_nonmissvisits<-PassivePop_YN_Flg%>%filter(PassPop_Flg==1)
      
#overall - all species in passive population (baseline visit only)
Demog_Passive_BL<-CreateTableOne(vars = c("AgeCat_Visit", "Sex", "Village", "Visit_Type2",  "WealthQuintile", "Feverpos", "RDTpos", "BedNetPrior", "season_dry", "Sx_Prior6mo_new", "AntiMalTx_Prior6mo_new", "Pf", "Pm", "Po", "Pf_Species", "Pm_Species", "Po_Species", "Pd_Pf", "Pd_Pm", "Pd_Po"), 
                                             data = PassivePop_Y_nonmissvisits, 
                                             includeNA = T,
                                             factorVars = c("AgeCat_Visit", "Sex", "Village", "Visit_Type2",  "WealthQuintile", "Feverpos", "RDTpos", "BedNetPrior", "season_dry", "Sx_Prior6mo_new", "AntiMalTx_Prior6mo_new", "Pf", "Pm", "Po", "Pf_Species", "Pm_Species", "Po_Species"))
      
P_Demog_Passive_BL<-print(Demog_Passive_BL, nonnormal = c("Pd_Pf", "Pd_Pm", "Pd_Po"), showAllLevels = TRUE, is.na=T, noSpaces = T)%>%write.table("clipboard", sep="\t", row.names = T)
      
      
      #overall - Pf species in active population 
      Demog_Active_PfVisits<-CreateTableOne(vars = c("AgeCat_Visit", "Sex", "Village", "Visit_Type2",  "WealthQuintile", "Feverpos", "RDTpos", "BedNetPrior", "season_dry", "Sx_Prior6mo_new", "AntiMalTx_Prior6mo_new", "Pf", "Pm", "Po", "Pf_Species", "Pm_Species", "Po_Species", "Pd_Pf", "Pd_Pm", "Pd_Po"), 
                                            strata = "Pf",
                                            # strata = "Pf_Species", 
                                            data = AD_Active_nonmissvisits, 
                                            includeNA = T,
                                            factorVars = c("AgeCat_Visit", "Sex", "Village", "Visit_Type2",  "WealthQuintile", "Feverpos", "RDTpos", "BedNetPrior", "season_dry", "Sx_Prior6mo_new", "AntiMalTx_Prior6mo_new", "Pf", "Pm", "Po", "Pf_Species", "Pm_Species", "Po_Species"))
      
      
      Pf_Demog_Active_AllVisits<-print(Demog_Active_PfVisits, nonnormal = c("Pd_Pf", "Pd_Pm", "Pd_Po"), 
                                       #exact = c("AgeCat_Visit", "Sex", "Village", "Visit_Type2",  "WealthQuintile", "Feverpos", "RDTpos", "BedNetPrior", "season_dry", "Sx_Prior6mo", "AntiMalTx_Prior6mo", "Pf", "Pm", "Po", "Pf_Species", "Pm_Species", "Po_Species"), 
                                       showAllLevels = TRUE, is.na=T, noSpaces = T)%>%write.table("clipboard", sep="\t", row.names = T)
      
      
      #overall - Pm species in active follow-up, by visit 
      Demog_Active_PmVisits<-CreateTableOne(vars = c("AgeCat_Visit", "Sex", "Village", "Visit_Type2",  "WealthQuintile", "Feverpos", "RDTpos", "BedNetPrior", "season_dry", "Sx_Prior6mo_new", "AntiMalTx_Prior6mo_new", "Pf", "Pm", "Po", "Pf_Species", "Pm_Species", "Po_Species", "Pd_Pf", "Pd_Pm", "Pd_Po"), 
                                            strata = "Pm", 
                                            data = AD_Active_nonmissvisits, 
                                            includeNA = T,
                                            factorVars = c("AgeCat_Visit", "Sex", "Village", "Visit_Type2",  "WealthQuintile", "Feverpos", "RDTpos", "BedNetPrior", "season_dry", "Sx_Prior6mo_new", "AntiMalTx_Prior6mo_new", "Pf", "Pm", "Po", "Pf_Species", "Pm_Species", "Po_Species"))
      
      Pm_Demog_Active_AllVisits<-print(Demog_Active_PmVisits, nonnormal = c("Pd_Pf", "Pd_Pm", "Pd_Po"), exact = c("AgeCat_Visit", "Sex", "Village", "Visit_Type2",  "WealthQuintile", "Feverpos", "RDTpos", "BedNetPrior", "season_dry", "Sx_Prior6mo_new", "AntiMalTx_Prior6mo_new", "Pf", "Pm", "Po", "Pf_Species", "Pm_Species", "Po_Species"), showAllLevels = TRUE, is.na=T, noSpaces = T)%>%write.table("clipboard", sep="\t", row.names = T)
      
      
      ###Baseline Visit Only 
      Demog_Active_Pm_BL<-CreateTableOne(vars = c("AgeCat_Visit", "Sex", "Village", "Visit_Type2",  "WealthQuintile", "Feverpos", "RDTpos", "BedNetPrior", "season_dry", "Sx_Prior6mo_new", "AntiMalTx_Prior6mo_new", "Pf", "Pm", "Po", "Pf_Species", "Pm_Species", "Po_Species", "Pd_Pf", "Pd_Pm", "Pd_Po", "elevation_m"), 
                                         strata = "Pm",
                                         # strata = "Pf_Species", 
                                         data = AD_Active_nonmiss_BL, 
                                         #includeNA = T,
                                         factorVars = c("AgeCat_Visit", "Sex", "Village", "Visit_Type2",  "WealthQuintile", "Feverpos", "RDTpos", "BedNetPrior", "season_dry", "Sx_Prior6mo_new", "AntiMalTx_Prior6mo_new", "Pf", "Pm", "Po", "Pf_Species", "Pm_Species", "Po_Species"))
      
      Demog_Active_Pm_BL_print<-print(Demog_Active_Pm_BL, nonnormal = c("Pd_Pf", "Pd_Pm", "Pd_Po","elevation_m"), 
                                      #exact = c("AgeCat_Visit", "Sex", "Village", "Visit_Type2",  "WealthQuintile", "Feverpos", "RDTpos", "BedNetPrior", "season_dry", "Sx_Prior6mo", "AntiMalTx_Prior6mo", "Pf", "Pm", "Po", "Pf_Species", "Pm_Species", "Po_Species"), 
                                      showAllLevels = TRUE, 
                                      #Turn on and off the below line to assess missingness by var, vs. calculate p-values excluding missingness.  
                                      #is.na=T, 
                                      noSpaces = T)%>%write.table("clipboard", sep="\t", row.names = T)
      
      
      
      #overall - Po species in active follow-up, by visit 
      Demog_Active_PoVisits<-CreateTableOne(vars = c("AgeCat_Visit", "Sex", "Village", "Visit_Type2",  "WealthQuintile", "Feverpos", "RDTpos", "BedNetPrior", "season_dry", "Sx_Prior6mo_new", "AntiMalTx_Prior6mo_new", "Pf", "Pm", "Po", "Pf_Species", "Pm_Species", "Po_Species", "Pd_Pf", "Pd_Pm", "Pd_Po"), 
                                            strata = "Po_Species", 
                                            data = AD_Active_nonmissvisits, 
                                            includeNA = T,
                                            factorVars = c("AgeCat_Visit", "Sex", "Village", "Visit_Type2",  "WealthQuintile", "Feverpos", "RDTpos", "BedNetPrior", "season_dry", "Sx_Prior6mo_new", "AntiMalTx_Prior6mo_new", "Pf", "Pm", "Po", "Pf_Species", "Pm_Species", "Po_Species"))
      
      Po_Demog_Active_AllVisits<-print(Demog_Active_PoVisits, nonnormal = c("Pd_Pf", "Pd_Pm", "Pd_Po"), exact = c("AgeCat_Visit", "Sex", "Village", "Visit_Type2",  "WealthQuintile", "Feverpos", "RDTpos", "BedNetPrior", "season_dry", "Sx_Prior6mo_new", "AntiMalTx_Prior6mo_new", "Pf", "Pm", "Po", "Pf_Species", "Pm_Species", "Po_Species"), showAllLevels = TRUE, is.na=T, noSpaces = T)%>%write.table("clipboard", sep="\t", row.names = T)
      
      
      
      ###Baseline Visit Only 
      Demog_Active_Po_BL<-CreateTableOne(vars = c("AgeCat_Visit", "Sex", "Village", "Visit_Type2",  "WealthQuintile", "Feverpos", "RDTpos", "BedNetPrior", "season_dry", "Sx_Prior6mo_new", "AntiMalTx_Prior6mo_new", "Pf", "Pm", "Po", "Pf_Species", "Pm_Species", "Po_Species", "Pd_Pf", "Pd_Pm", "Pd_Po", "elevation_m"), 
                                         strata = "Po",
                                         # strata = "Po_Species", 
                                         data = AD_Active_nonmiss_BL, 
                                         includeNA = T,
                                         factorVars = c("AgeCat_Visit", "Sex", "Village", "Visit_Type2",  "WealthQuintile", "Feverpos", "RDTpos", "BedNetPrior", "season_dry", "Sx_Prior6mo_new", "AntiMalTx_Prior6mo_new", "Pf", "Pm", "Po", "Pf_Species", "Pm_Species", "Po_Species"))
      
      Demog_Active_Po_BL_print<-print(Demog_Active_Po_BL, nonnormal = c("Pd_Po", "Pd_Pm", "Pd_Po","elevation_m"), 
                                      #exact = c("AgeCat_Visit", "Sex", "Village", "Visit_Type2",  "WealthQuintile", "Feverpos", "RDTpos", "BedNetPrior", "season_dry", "Sx_Prior6mo", "AntiMalTx_Prior6mo", "Pf", "Pm", "Po", "Pf_Species", "Pm_Species", "Po_Species"), 
                                      showAllLevels = TRUE, 
                                      #Turn on and off the below line to assess missingness by var, vs. calculate p-values excluding missingness.  
                                      is.na=T, 
                                      noSpaces = T)%>%write.table("clipboard", sep="\t", row.names = T)
      
    
##########################################################
# SUMMARIZING NUMBER OF REPEATED PM, PO, AND PF INFECTIONS OVER TIME 
##########################################################
      
#Count total number of each species' infections by subject (non-missings)
AD_Total_Counts <- AD_Total %>% 
                    group_by(Subject_ID) %>%
                    dplyr::summarise(Pm_sum = sum((Pm), na.rm = TRUE), Po_sum = sum((Po), na.rm = TRUE), Pf_sum = sum((Pf), na.rm = TRUE))
                               
AD_Active_Counts <- AD_Active %>% 
                    group_by(Subject_ID) %>%
                    dplyr::summarise(Pm_sum = sum((Pm), na.rm = TRUE), Po_sum = sum((Po), na.rm = TRUE), Pf_sum = sum((Pf), na.rm = TRUE))

AD_Passive_Counts <- AD_Passive %>% 
                    group_by(Subject_ID) %>%
                    dplyr::summarise(Pm_sum = sum((Pm), na.rm = TRUE), Po_sum = sum((Po), na.rm = TRUE), Pf_sum = sum((Pf), na.rm = TRUE))


AD_Total_Counts<-AD_Total_Counts%>%mutate(Pm_multiple=ifelse(Pm_sum>1, 1,0))%>%
                                   mutate(Pm_any=ifelse(Pm_sum>0, 1,0))%>%
                                   mutate(Po_multiple=ifelse(Po_sum>1, 1,0))%>%
                                   mutate(Po_any=ifelse(Po_sum>0, 1,0))%>%
                                   mutate(Pf_multiple=ifelse(Pf_sum>1, 1,0))%>%
                                   mutate(Pf_any=ifelse(Pf_sum>0, 1,0))%>%
                                   mutate(Pf_multiple_GT5=ifelse(Pf_sum>4, 1,0))%>%
                                   mutate(Pf_any=ifelse(Pf_sum>0, 1,0))

addmargins(table(AD_Total_Counts$Pm_sum, useNA = "always"))
addmargins(table(AD_Active_Counts$Pm_sum, useNA = "always"))
addmargins(table(AD_Passive_Counts$Pm_sum, useNA = "always"))


##Proportion of multiple infections out of total
addmargins(table(AD_Total_Counts$Pm_multiple[AD_Total_Counts$Pm_any==1]))
prop.table(table(AD_Total_Counts$Pm_multiple[AD_Total_Counts$Pm_any==1]))


addmargins(table(AD_Total_Counts$Po_sum, useNA = "always"))
addmargins(table(AD_Active_Counts$Po_sum, useNA = "always"))
addmargins(table(AD_Passive_Counts$Po_sum, useNA = "always"))


##Proportion of multiple infections out of total
addmargins(table(AD_Total_Counts$Po_multiple[AD_Total_Counts$Po_any==1]))
prop.table(table(AD_Total_Counts$Po_multiple[AD_Total_Counts$Po_any==1]))


addmargins(table(AD_Total_Counts$Pf_sum, useNA = "always"))
addmargins(table(AD_Active_Counts$Pf_sum, useNA = "always"))
addmargins(table(AD_Passive_Counts$Pf_sum, useNA = "always"))


##Proportion of multiple infections out of total
addmargins(table(AD_Total_Counts$Pf_multiple[AD_Total_Counts$Pf_any==1]))
prop.table(table(AD_Total_Counts$Pf_multiple[AD_Total_Counts$Pf_any==1]))

addmargins(table(AD_Total_Counts$Pf_multiple_GT5[AD_Total_Counts$Pf_any==1]))
prop.table(table(AD_Total_Counts$Pf_multiple_GT5[AD_Total_Counts$Pf_any==1]))



Pm_multiple<-ggplot(data=AD_Total_Counts)+
              geom_histogram(aes(Pm_sum))
plot(Pm_multiple)

Po_multiple<-ggplot(data=AD_Total_Counts)+
  geom_histogram(aes(Po_sum))
plot(Po_multiple)

Pf_multiple<-ggplot(data=AD_Total_Counts)+
  geom_histogram(aes(Pf_sum))
plot(Pf_multiple)



##########################################################
# Parasitemia Analyses:    
##########################################################

##First, need to filter out the rehydrated samples and the missing visit dates to exclude those lost to FU  

##UPDATE: over-ride variable by multiplying all non-Pf PD's by 4 because of dilution factor during PCR assay - need to correct the Pd for non-pf only so it is more valid comparison with Pf Pd (since Pf was not diluted during PCR)
      
AD_Total<-AD_Total%>%mutate(Pd_Po_mult4 = Pd_Po*4)%>%
                     mutate(Pd_Pm_mult4 = Pd_Pm*4)
      
#Perform a quick comparison of rehydrated vs. non-rehydrated samples by species and demographics to see if there are important differences.  
#Looking in Total Pop.
AD_Total_nonmissvisits<-AD_Total%>%filter(!is.na(Visit_date))

addmargins(table(AD_Total_nonmissvisits$Rehydrated, AD_Total_nonmissvisits$Visit_Type2, useNA = "always"))

Compare_RehydratedSamples_YesvsNo<-CreateTableOne(vars = c("AgeCat_Visit", "Sex", "Village", "Visit_Type2",  "WealthQuintile", "Feverpos", "Pf", "Pm", "Po"), 
                                         strata = "Rehydrated", data = AD_Total_nonmissvisits, factorVars = c("AgeCat_Visit", "Sex", "Village", "Visit_Type2", "WealthQuintile", "Feverpos", "Pf", "Pm", "Po"))

P_Compare_Rehydrate<-print(Compare_RehydratedSamples_YesvsNo, factorvars = c("AgeCat_Visit", "Sex", "Village", "Visit_Type2", "Wealthquintile", "Feverpos", "Pf", "Pm", "Po"), exact  = c("AgeCat_Visit", "Sex", "Visit_Type2", "Feverpos", "Pf", "Pm", "Po"), showAllLevels = TRUE, noSpaces = T)%>%write.table("clipboard", sep="\t", row.names = T)

        #Result: In Total Pop, the % of visits with samples that were rehydrated were higher for Bu, Baseline and VNP visit types, and Fever =N. 


##Now exclude rehydrated samples from parasitemia estimation since the concentration has been affected
#Also subset data for individual species' parasite density graphs
po_pd<-AD_Total_nonmissvisits%>%filter(Po==1&Rehydrated!=1)
pm_pd<-AD_Total_nonmissvisits%>%filter(Pm==1&Rehydrated!=1)
  pm_pd<-pm_pd%>%filter(Pm_Species==as.factor('Pm Mono') | Pm_Species==as.factor('Pm Mixed'))
pf_pd<-AD_Total_nonmissvisits%>%filter(Pf==1&Rehydrated!=1)
pf_pd2<-pf_pd%>%filter(Pf_Species==as.factor('Pf Mono') | Pf_Species==as.factor('Pf Mixed'))
pall_pd<-AD_Total_nonmissvisits%>%filter(Rehydrated!=1)


##Looking at excluded due to rehydrated: 
po_pd_rehyd<-AD_Total_nonmissvisits%>%filter(Po==1&Rehydrated==1)
pm_pd_rehyd<-AD_Total_nonmissvisits%>%filter(Pm==1&Rehydrated==1)
pf_pd_rehyd<-AD_Total_nonmissvisits%>%filter(Pf==1&Rehydrated==1)


#####TOTAL POPULATION -- ALL VISITS NON-Rehydrated 

#####Basic Descriptive Stats (Median because non-normally distributed parasitemia)
#Po
po_pd %>% 
  dplyr::summarize(n(), median(Pd_Po_mult4), quantile(Pd_Po_mult4, c(0.25)), quantile(Pd_Po_mult4, c(0.75)), min(Pd_Po_mult4), max(Pd_Po_mult4))

    ##rehydrated summary: 
    po_pd_rehyd%>%group_by(Po_Species)%>%dplyr::summarise(n())

    #mixed vs. mono summary: 
    po_pd_mixed<-po_pd%>%filter(Po_Species=="Po Mixed")%>%dplyr::summarize(n(), median(Pd_Po_mult4),  quantile(Pd_Po_mult4, c(0.25)), quantile(Pd_Po_mult4, c(0.75)), min(Pd_Po_mult4), max(Pd_Po_mult4))
    po_pd_mono<-po_pd%>%filter(Po_Species=="Po Mono")%>%dplyr::summarize(n(), median(Pd_Po_mult4),  quantile(Pd_Po_mult4, c(0.25)), quantile(Pd_Po_mult4, c(0.75)), min(Pd_Po_mult4), max(Pd_Po_mult4))
    rbind(po_pd_mixed, po_pd_mono)
    
    Compare_Pd_Po_mult4Species<-CreateTableOne(vars = c("Pd_Po_mult4"), strata = "Po_Species", data = po_pd)
    print(Compare_Pd_Po_mult4Species, exact = c("Pd_Po_mult4"), nonnormal=c("Pd_Po_mult4"), showAllLevels = TRUE, noSpaces = T)%>%write.table("clipboard", sep="\t", row.names = T)
    
   
#Pm
pm_pd %>% 
  dplyr::summarize(n(), median(Pd_Pm_mult4), quantile(Pd_Pm_mult4, c(0.25)), quantile(Pd_Pm_mult4, c(0.75)), min(Pd_Pm_mult4), max(Pd_Pm_mult4))

    ##rehydrated summary: 
    pm_pd_rehyd%>%group_by(Pm_Species)%>%dplyr::summarise(n())


pm_pd_mono<-pm_pd%>%filter(Pm_Species=="Pm Mono")%>%dplyr::summarize(n(), median(Pd_Pm_mult4), quantile(Pd_Pm_mult4, c(0.25)), quantile(Pd_Pm_mult4, c(0.75)), min(Pd_Pm_mult4), max(Pd_Pm_mult4))
pm_pd_mixed<-pm_pd%>%filter(Pm_Species=="Pm Mixed")%>%dplyr::summarize(n(), median(Pd_Pm_mult4),quantile(Pd_Pm_mult4, c(0.25)), quantile(Pd_Pm_mult4, c(0.75)), min(Pd_Pm_mult4), max(Pd_Pm_mult4))
    rbind(pm_pd_mixed, pm_pd_mono)

Compare_Pd_Pm_mult4Species<-CreateTableOne(vars = c("Pd_Pm_mult4"), strata = "Pm_Species", data = pm_pd)
print(Compare_Pd_Pm_mult4Species, exact = c("Pd_Pm_mult4"), nonnormal=c("Pd_Pm_mult4"), showAllLevels = TRUE, noSpaces = T)%>%write.table("clipboard", sep="\t", row.names = T)

  

#Pf
pf_pd %>% 
  dplyr::summarize(n(), median(Pd_Pf), quantile(Pd_Pf, c(0.25)), quantile(Pd_Pf, c(0.75)), min(Pd_Pf), max(Pd_Pf))


pf_pd_mono<-pf_pd%>%filter(Pf_Species=="Pf Mono")%>%dplyr::summarize(n(), median(Pd_Pf), quantile(Pd_Pf, c(0.25)), quantile(Pd_Pf, c(0.75)), min(Pd_Pf), max(Pd_Pf))
pf_pd_mixed<-pf_pd%>%filter(Pf_Species=="Pf Mixed")%>%dplyr::summarize(n(), median(Pd_Pf), quantile(Pd_Pf, c(0.25)), quantile(Pd_Pf, c(0.75)), min(Pd_Pf), max(Pd_Pf))
  rbind( pf_pd_mixed, pf_pd_mono)

##rehydrated summary: 
pf_pd_rehyd%>%group_by(Pf_Species)%>%dplyr::summarise(n())


Compare_Pd_PfSpecies<-CreateTableOne(vars = c("Pd_Pf"), strata = "Pf_Species", data = pf_pd)
print(Compare_Pd_PfSpecies, exact = c("Pd_Pf"), nonnormal=c("Pd_Pf"), showAllLevels = TRUE, noSpaces = T)%>%write.table("clipboard", sep="\t", row.names = T)



###comparing Pd's in Pf mono vs. Pf+Pm mixed, and Pf mono vs. Pf+Po mixed: 

#Pf mono vs. Pf+Po   (no Pm infections)
pf_pd_mono<-pf_pd%>%filter(Pf_Species=="Pf Mono")
pf_pd_mixed_Po<-pf_pd%>%filter(Pf_Species=="Pf Mixed"&Po==1&Pm!=1)
  PfvsPfPo<- rbind( pf_pd_mixed_Po, pf_pd_mono)

  PfvsPfPo%>%group_by(Pf_Species)%>%dplyr::summarize(n(), median(Pd_Pf), quantile(Pd_Pf, c(0.25)), quantile(Pd_Pf, c(0.75)), min(Pd_Pf), max(Pd_Pf))
   
    Compare_Pd_PfSpecies<-CreateTableOne(vars = c("Pd_Pf"), strata = "Pf_Species", data = PfvsPfPo)
    print(Compare_Pd_PfSpecies, exact = c("Pd_Pf"), nonnormal=c("Pd_Pf"), showAllLevels = TRUE, noSpaces = T)%>%write.table("clipboard", sep="\t", row.names = T)
    
    
    #RESULT - Pf mono vs. Pf+Po mixed -- TOTAL POP. 
        #Stratified by Pf_Species
        #                       level Pf Mixed                Pf Mono                 p     test   
        #n                          96                      3418                                 
        #Pd_Pf (median [IQR])       324.98 [47.52, 4103.50] 279.60 [17.38, 5014.62] 0.468 nonnorm
    
    
#Pf mono vs. Pf+Pm  (no Po infections)
pf_pd_mono<-pf_pd%>%filter(Pf_Species=="Pf Mono")
pf_pd_mixed_Pm<-pf_pd%>%filter(Pf_Species=="Pf Mixed"&Pm==1&Po!=1)
  PfvsPfPm<- rbind( pf_pd_mixed_Pm, pf_pd_mono)
    
  PfvsPfPm%>%group_by(Pf_Species)%>%dplyr::summarize(n(), median(Pd_Pf), quantile(Pd_Pf, c(0.25)), quantile(Pd_Pf, c(0.75)), min(Pd_Pf), max(Pd_Pf))
    
    Compare_Pd_PfSpecies<-CreateTableOne(vars = c("Pd_Pf"), strata = "Pf_Species", data = PfvsPfPm)
    print(Compare_Pd_PfSpecies, exact = c("Pd_Pf"), nonnormal=c("Pd_Pf"), showAllLevels = TRUE, noSpaces = T)%>%write.table("clipboard", sep="\t", row.names = T)
    
    #RESULT - Pf mono vs. Pf+Pm mixed -- TOTAL POP. 
    #Stratified by Pf_Species
    #                   level Pf Mixed                Pf Mono                 p     test   
    #n                          206                     3418                                 
    #Pd_Pf (median [IQR])       166.50 [33.39, 1041.74] 279.60 [17.38, 5014.62] 0.059 nonnorm
    


######### NOW REPEAT FOR PASSIVE AND ACTIVE POPULATIONS 

#PASSIVE POPULATION  VISITS NON-Rehydrated 

##Basic Descriptive Stats (Median because non-normally distributed parasitemia)

### filter to only passive pop
po_pd_passive<-po_pd%>%filter(Visit_Type2=="VNP")
po_pd_rehyd_passive<-po_pd_rehyd%>%filter(Visit_Type2=="VNP")

#Po
po_pd_passive %>% 
  dplyr::summarize(n(), median(Pd_Po_mult4), quantile(Pd_Po_mult4, c(0.25)), quantile(Pd_Po_mult4, c(0.75)), min(Pd_Po_mult4), max(Pd_Po_mult4))

##rehydrated summary: 
po_pd_rehyd_passive%>%group_by(Po_Species)%>%dplyr::summarise(n())

#mixed vs. mono summary: 
po_pd_mixed<-po_pd_passive%>%filter(Po_Species=="Po Mixed")%>%dplyr::summarize(n(), median(Pd_Po_mult4),  quantile(Pd_Po_mult4, c(0.25)), quantile(Pd_Po_mult4, c(0.75)), min(Pd_Po_mult4), max(Pd_Po_mult4))
po_pd_mono<-po_pd_passive%>%filter(Po_Species=="Po Mono")%>%dplyr::summarize(n(), median(Pd_Po_mult4),  quantile(Pd_Po_mult4, c(0.25)), quantile(Pd_Po_mult4, c(0.75)), min(Pd_Po_mult4), max(Pd_Po_mult4))
  rbind(po_pd_mixed, po_pd_mono)

Compare_Pd_Po_mult4Species<-CreateTableOne(vars = c("Pd_Po_mult4"), strata = "Po_Species", data = po_pd_passive)
print(Compare_Pd_Po_mult4Species, exact = c("Pd_Po_mult4"), nonnormal=c("Pd_Po_mult4"), showAllLevels = TRUE, noSpaces = T)%>%write.table("clipboard", sep="\t", row.names = T)


#Pm
    
### filter to only passive pop
pm_pd_passive<-pm_pd%>%filter(Visit_Type2=="VNP")
pm_pd_rehyd_passive<-pm_pd_rehyd%>%filter(Visit_Type2=="VNP")    
    
pm_pd_passive %>% 
  dplyr::summarize(n(), median(Pd_Pm_mult4), quantile(Pd_Pm_mult4, c(0.25)), quantile(Pd_Pm_mult4, c(0.75)), min(Pd_Pm_mult4), max(Pd_Pm_mult4))

##rehydrated summary: 
pm_pd_rehyd_passive%>%group_by(Pm_Species)%>%dplyr::summarise(n())


pm_pd_mono<-pm_pd_passive%>%filter(Pm_Species=="Pm Mono")%>%dplyr::summarize(n(), median(Pd_Pm_mult4), quantile(Pd_Pm_mult4, c(0.25)), quantile(Pd_Pm_mult4, c(0.75)), min(Pd_Pm_mult4), max(Pd_Pm_mult4))
pm_pd_mixed<-pm_pd_passive%>%filter(Pm_Species=="Pm Mixed")%>%dplyr::summarize(n(), median(Pd_Pm_mult4),quantile(Pd_Pm_mult4, c(0.25)), quantile(Pd_Pm_mult4, c(0.75)), min(Pd_Pm_mult4), max(Pd_Pm_mult4))
  rbind(pm_pd_mixed, pm_pd_mono)

Compare_Pd_Pm_mult4Species<-CreateTableOne(vars = c("Pd_Pm_mult4"), strata = "Pm_Species", data = pm_pd_passive)
print(Compare_Pd_Pm_mult4Species, exact = c("Pd_Pm_mult4"), nonnormal=c("Pd_Pm_mult4"), showAllLevels = TRUE, noSpaces = T)%>%write.table("clipboard", sep="\t", row.names = T)


#Pf

### filter to only passive pop
pf_pd_passive<-pf_pd%>%filter(Visit_Type2=="VNP")
pf_pd_rehyd_passive<-pf_pd_rehyd%>%filter(Visit_Type2=="VNP")    


pf_pd_passive %>% 
  dplyr::summarize(n(), median(Pd_Pf), quantile(Pd_Pf, c(0.25)), quantile(Pd_Pf, c(0.75)), min(Pd_Pf), max(Pd_Pf))

pf_pd_mono<-pf_pd_passive%>%filter(Pf_Species=="Pf Mono")%>%dplyr::summarize(n(), median(Pd_Pf), quantile(Pd_Pf, c(0.25)), quantile(Pd_Pf, c(0.75)), min(Pd_Pf), max(Pd_Pf))
pf_pd_mixed<-pf_pd_passive%>%filter(Pf_Species=="Pf Mixed")%>%dplyr::summarize(n(), median(Pd_Pf), quantile(Pd_Pf, c(0.25)), quantile(Pd_Pf, c(0.75)), min(Pd_Pf), max(Pd_Pf))
  rbind( pf_pd_mixed, pf_pd_mono)
  
  
  Compare_Pd_PfSpecies<-CreateTableOne(vars = c("Pd_Pf"), strata = "Pf_Species", data = pf_pd_passive)
  print(Compare_Pd_PfSpecies, exact = c("Pd_Pf"), nonnormal=c("Pd_Pf"), showAllLevels = TRUE, noSpaces = T)%>%write.table("clipboard", sep="\t", row.names = T)
  
  
##rehydrated summary: 
  pf_pd_rehyd_passive%>%group_by(Pf_Species)%>%dplyr::summarise(n())

  Compare_Pd_PfSpecies<-CreateTableOne(vars = c("Pd_Pf"), strata = "Pf_Species", data = pf_pd_rehyd_passive)
  print(Compare_Pd_PfSpecies, exact = c("Pd_Pf"), nonnormal=c("Pd_Pf"), showAllLevels = TRUE, noSpaces = T)%>%write.table("clipboard", sep="\t", row.names = T)



###comparing Pd's in Pf mono vs. Pf+Pm mixed, and Pf mono vs. Pf+Po mixed: 

#Pf mono vs. Pf+Po   (no Pm infections)
pf_pd_mono<-pf_pd_passive%>%filter(Pf_Species=="Pf Mono")
pf_pd_mixed_Po<-pf_pd_passive%>%filter(Pf_Species=="Pf Mixed"&Po==1&Pm!=1)
PfvsPfPo_passive<- rbind( pf_pd_mixed_Po, pf_pd_mono)

PfvsPfPo_passive%>%group_by(Pf_Species)%>%dplyr::summarize(n(), median(Pd_Pf), quantile(Pd_Pf, c(0.25)), quantile(Pd_Pf, c(0.75)), min(Pd_Pf), max(Pd_Pf))

Compare_Pd_PfSpecies<-CreateTableOne(vars = c("Pd_Pf"), strata = "Pf_Species", data = PfvsPfPo_passive)
print(Compare_Pd_PfSpecies, exact = c("Pd_Pf"), nonnormal=c("Pd_Pf"), showAllLevels = TRUE, noSpaces = T)%>%write.table("clipboard", sep="\t", row.names = T)

    #RESULT - Pf mono vs. Pf+Po mixed -- PASSIVE POP. 
      #Stratified by Pf_Species
      #                     level Pf Mixed                   Pf Mono                    p     test   
      #n                          47                         1834                                    
      #Pd_Pf (median [IQR])       1710.00 [165.18, 13082.50] 2897.00 [126.51, 18058.25] 0.596 nonnorm


#Pf mono vs. Pf+Pm   (no Po infections)
pf_pd_mono<-pf_pd_passive%>%filter(Pf_Species=="Pf Mono")
pf_pd_mixed_Pm<-pf_pd_passive%>%filter(Pf_Species=="Pf Mixed"&Pm==1&Po!=1)
PfvsPfPm_passive<- rbind( pf_pd_mixed_Pm, pf_pd_mono)

PfvsPfPm_passive%>%group_by(Pf_Species)%>%dplyr::summarize(n(), median(Pd_Pf), quantile(Pd_Pf, c(0.25)), quantile(Pd_Pf, c(0.75)), min(Pd_Pf), max(Pd_Pf))

Compare_Pd_PfSpecies<-CreateTableOne(vars = c("Pd_Pf"), strata = "Pf_Species", data = PfvsPfPm_passive)
print(Compare_Pd_PfSpecies, exact = c("Pd_Pf"), nonnormal=c("Pd_Pf"), showAllLevels = TRUE, noSpaces = T)%>%write.table("clipboard", sep="\t", row.names = T)

    #RESULT - Pf mono vs. Pf+Pm mixed -- PASSIVE POP. 
      # Stratified by Pf_Species
      #                     level Pf Mixed                Pf Mono                    p      test   
      #n                          84                      1834                                     
      #Pd_Pf (median [IQR])       287.78 [56.96, 4318.60] 2897.00 [126.51, 18058.25] <0.001 nonnorm



##### ACTIVE POPULATION VISITS NON-Rehydrated 

#####Basic Descriptive Stats (Median because non-normally distributed parasitemia)

### filter to only active pop
po_pd_active<-po_pd%>%filter(Visit_Type1=="Act")
po_pd_rehyd_active<-po_pd_rehyd%>%filter(Visit_Type1=="Act")

#Po
po_pd_active %>% 
  dplyr::summarize(n(), median(Pd_Po_mult4), quantile(Pd_Po_mult4, c(0.25)), quantile(Pd_Po_mult4, c(0.75)), min(Pd_Po_mult4), max(Pd_Po_mult4))

##rehydrated summary: 
po_pd_rehyd_active%>%group_by(Po_Species)%>%dplyr::summarise(n())

#mixed vs. mono summary: 
po_pd_mixed<-po_pd_active%>%filter(Po_Species=="Po Mixed")%>%dplyr::summarize(n(), median(Pd_Po_mult4),  quantile(Pd_Po_mult4, c(0.25)), quantile(Pd_Po_mult4, c(0.75)), min(Pd_Po_mult4), max(Pd_Po_mult4))
po_pd_mono<-po_pd_active%>%filter(Po_Species=="Po Mono")%>%dplyr::summarize(n(), median(Pd_Po_mult4),  quantile(Pd_Po_mult4, c(0.25)), quantile(Pd_Po_mult4, c(0.75)), min(Pd_Po_mult4), max(Pd_Po_mult4))
  rbind(po_pd_mixed, po_pd_mono)

Compare_Pd_Po_mult4Species<-CreateTableOne(vars = c("Pd_Po_mult4"), strata = "Po_Species", data = po_pd_active)
print(Compare_Pd_Po_mult4Species, exact = c("Pd_Po_mult4"), nonnormal=c("Pd_Po_mult4"), showAllLevels = TRUE, noSpaces = T)%>%write.table("clipboard", sep="\t", row.names = T)


#Pm

### filter to only active pop
pm_pd_active<-pm_pd%>%filter(Visit_Type1=="Act")
pm_pd_rehyd_active<-pm_pd_rehyd%>%filter(Visit_Type1=="Act")    

pm_pd_active %>% 
  dplyr::summarize(n(), median(Pd_Pm_mult4), quantile(Pd_Pm_mult4, c(0.25)), quantile(Pd_Pm_mult4, c(0.75)), min(Pd_Pm_mult4), max(Pd_Pm_mult4))

##rehydrated summary: 
pm_pd_rehyd_active%>%group_by(Pm_Species)%>%dplyr::summarise(n())


pm_pd_mono<-pm_pd_active%>%filter(Pm_Species=="Pm Mono")%>%dplyr::summarize(n(), median(Pd_Pm_mult4), quantile(Pd_Pm_mult4, c(0.25)), quantile(Pd_Pm_mult4, c(0.75)), min(Pd_Pm_mult4), max(Pd_Pm_mult4))
pm_pd_mixed<-pm_pd_active%>%filter(Pm_Species=="Pm Mixed")%>%dplyr::summarize(n(), median(Pd_Pm_mult4),quantile(Pd_Pm_mult4, c(0.25)), quantile(Pd_Pm_mult4, c(0.75)), min(Pd_Pm_mult4), max(Pd_Pm_mult4))
rbind(pm_pd_mixed, pm_pd_mono)

Compare_Pd_Pm_mult4Species<-CreateTableOne(vars = c("Pd_Pm_mult4"), strata = "Pm_Species", data = pm_pd_active)
print(Compare_Pd_Pm_mult4Species, exact = c("Pd_Pm_mult4"), nonnormal=c("Pd_Pm_mult4"), showAllLevels = TRUE, noSpaces = T)%>%write.table("clipboard", sep="\t", row.names = T)


#Pf

### filter to only active pop
pf_pd_active<-pf_pd%>%filter(Visit_Type1=="Act")
pf_pd_rehyd_active<-pf_pd_rehyd%>%filter(Visit_Type1=="Act")    


pf_pd_active %>% 
  dplyr::summarize(n(), median(Pd_Pf), quantile(Pd_Pf, c(0.25)), quantile(Pd_Pf, c(0.75)), min(Pd_Pf), max(Pd_Pf))

pf_pd_mono<-pf_pd_active%>%filter(Pf_Species=="Pf Mono")%>%dplyr::summarize(n(), median(Pd_Pf), quantile(Pd_Pf, c(0.25)), quantile(Pd_Pf, c(0.75)), min(Pd_Pf), max(Pd_Pf))
pf_pd_mixed<-pf_pd_active%>%filter(Pf_Species=="Pf Mixed")%>%dplyr::summarize(n(), median(Pd_Pf), quantile(Pd_Pf, c(0.25)), quantile(Pd_Pf, c(0.75)), min(Pd_Pf), max(Pd_Pf))
  rbind( pf_pd_mixed, pf_pd_mono)

  Compare_Pd_PfSpecies<-CreateTableOne(vars = c("Pd_Pf"), strata = "Pf_Species", data = pf_pd_active)
  print(Compare_Pd_PfSpecies, exact = c("Pd_Pf"), nonnormal=c("Pd_Pf"), showAllLevels = TRUE, noSpaces = T)%>%write.table("clipboard", sep="\t", row.names = T)
  
  
##rehydrated summary: 
  pf_pd_rehyd_active%>%group_by(Pf_Species)%>%dplyr::summarise(n())

    
    Compare_Pd_PfSpecies<-CreateTableOne(vars = c("Pd_Pf"), strata = "Pf_Species", data = pf_pd_rehyd_active)
    print(Compare_Pd_PfSpecies, exact = c("Pd_Pf"), nonnormal=c("Pd_Pf"), showAllLevels = TRUE, noSpaces = T)%>%write.table("clipboard", sep="\t", row.names = T)



###comparing Pd's in Pf mono vs. Pf+Pm mixed, and Pf mono vs. Pf+Po mixed: 

#Pf mono vs. Pf+Po   (no Pm infections)
pf_pd_mono<-pf_pd_active%>%filter(Pf_Species=="Pf Mono")
pf_pd_mixed_Po<-pf_pd_active%>%filter(Pf_Species=="Pf Mixed"&Po==1&Pm!=1)
PfvsPfPo_active<- rbind( pf_pd_mixed_Po, pf_pd_mono)

PfvsPfPo_active%>%group_by(Pf_Species)%>%dplyr::summarize(n(), median(Pd_Pf), quantile(Pd_Pf, c(0.25)), quantile(Pd_Pf, c(0.75)), min(Pd_Pf), max(Pd_Pf))

Compare_Pd_PfSpecies<-CreateTableOne(vars = c("Pd_Pf"), strata = "Pf_Species", data = PfvsPfPo_active)
print(Compare_Pd_PfSpecies, exact = c("Pd_Pf"), nonnormal=c("Pd_Pf"), showAllLevels = TRUE, noSpaces = T)%>%write.table("clipboard", sep="\t", row.names = T)

  #RESULT - Pf mono vs. Pf+Po mixed -- ACTIVE POP. 
    #Stratified by Pf_Species
    #level                      Pf Mixed               Pf Mono              p     test   
    #n                          49                     1584                              
    #Pd_Pf (median [IQR])       122.11 [42.16, 485.90] 46.95 [7.40, 311.12] 0.010 nonnorm


#Pf mono vs. Pf+Pm   (no Po infections)
pf_pd_mono<-pf_pd_active%>%filter(Pf_Species=="Pf Mono")
pf_pd_mixed_Pm<-pf_pd_active%>%filter(Pf_Species=="Pf Mixed"&Pm==1&Po!=1)
PfvsPfPm_active<- rbind( pf_pd_mixed_Pm, pf_pd_mono)

PfvsPfPm_active%>%group_by(Pf_Species)%>%dplyr::summarize(n(), median(Pd_Pf), quantile(Pd_Pf, c(0.25)), quantile(Pd_Pf, c(0.75)), min(Pd_Pf), max(Pd_Pf))

Compare_Pd_PfSpecies<-CreateTableOne(vars = c("Pd_Pf"), strata = "Pf_Species", data = PfvsPfPm_active)
print(Compare_Pd_PfSpecies, exact = c("Pd_Pf"), nonnormal=c("Pd_Pf"), showAllLevels = TRUE, noSpaces = T)%>%write.table("clipboard", sep="\t", row.names = T)

  #RESULT - Pf mono vs. Pf+Pm mixed -- ACTIVE POP. 
    #Stratified by Pf_Species
    #level                      Pf Mixed               Pf Mono              p     test   
    #n                          122                    1584                              
    #Pd_Pf (median [IQR])       126.88 [24.94, 496.40] 46.95 [7.40, 311.12] 0.001 nonnorm



##########################
 
#CREATING PARASITEMIA FIGURES FOR MANUSCRIPT 


#1. Density plots comparing Pf parasitemia in Pf mono vs. Pf+Po  and Pf mono vs. Pf+Pm  (with background histogram for raw counts)

## SEPARATELY BY VISIT TYPE (ACTIVE VS. PASSIVE POP.)   ALSO CREATE FOR TOTAL POP BUT DON'T INCLUDE IN MANUSCRIPT 

xlabels_manual<-c("0.01", "0.1", "1", "10", "100", "1,000", "10,000", "100,000", "1,000,000") 

##ACTIVE POP.

##Pf vs. Pm 

#Density plot using log scale 
  dplot_PfbyPm<-ggplot(PfvsPfPm_active)+
    geom_density(aes(x = Pd_Pf, fill = Pf_Species), alpha=0.5)+
    scale_x_log10(name="Estimated Parasite Density (p/uL)")
  plot(dplot_PfbyPm)
  
  barplot_PfPm<-ggplot(PfvsPfPm_active)+
    geom_bar(aes(x=Pd_Pf, y=(count)/sum(count), fill=Pf_Species))+
    scale_x_log10()
  plot(barplot_PfPm)
  
  
#histogram plot using log scale
  hplot_PfbyPm<-ggplot(PfvsPfPm_active)+
    geom_histogram(aes(x = Pd_Pf, fill = Pf_Species), position="dodge", alpha=0.3)+
    scale_x_log10(name="Estimated Parasite Density (p/uL)")
  plot(hplot_PfbyPm)
  
  hplot_PfbyPo<-ggplot(PfvsPfPo_active)+
    geom_histogram(aes(x = Pd_Pf, fill = Pf_Species), position="dodge", alpha=0.3)+
    scale_x_log10(name="Estimated Parasite Density (p/uL)")
  plot(hplot_PfbyPo)

#overlaying density plot onto histogram to show the raw counts too
#un-stratified counts in background: 
PfbyPm_overlay<-ggplot(PfvsPfPm_active, aes(x=Pd_Pf, bins=10))+
 # geom_histogram(aes(y=..density..), alpha=0.5, col='gray', lwd=0.4)+
 # scale_x_log10(name="Estimated Pf Parasite Density (p/uL)", breaks=c(.01,.1,1,10, 100, 1000, 10000, 100000, 1000000),labels=xlabels_manual)+
  scale_x_log10(name="", breaks=c(.01,.1,1,10, 100, 1000, 10000, 100000, 1000000),labels=xlabels_manual)+
  geom_density(aes(fill=factor(Pf_Species)), col='black', lwd=0.6, alpha=0.4)
  #labs(title="Pf mono vs. Pf+Pm mixed infections - Active visits", fontsize = 8) 
PfbyPm_overlay_fig <- PfbyPm_overlay + guides(fill=guide_legend(title=""))
plot(PfbyPm_overlay_fig)


#Stratified (but stacked) counts in background:
PfbyPm_overlay<-ggplot(PfvsPfPm_active, aes(x=Pd_Pf, fill=Pf_Species, bins=10))+
  geom_histogram(aes(y=..density..), position="identity", alpha=0.3, col='gray', lwd=0.2)+
  #scale_x_log10(name="Estimated Pf Parasite Density (p/uL)", breaks=c(.01,.1,1,10, 100, 1000, 10000, 100000, 1000000),labels=xlabels_manual)+
  scale_x_log10(name="", breaks=c(.01,.1,1,10, 100, 1000, 10000, 100000, 1000000),labels=xlabels_manual)+
  geom_density(aes(fill=factor(Pf_Species)), col='black', lwd=0.6, alpha=0.2)+
  scale_fill_manual(values=c("#5b9bf5", "#f5cc5b"))
  #+labs(title="Pf mono vs. Pf+Pm mixed infections - Survey Pop.", fontsize = 8)

PfbyPm_overlay_fig <- PfbyPm_overlay + guides(fill=guide_legend(title="")) +
                     # geom_vline(xintercept = 10, linetype="solid", color = "dark gray", size=1.3) +
                     # annotate("text", label = "LoD", x = 7, y = 0.87, size = 5, colour = "dark gray")+
                    theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                                       panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), 
                                       text=element_text(size=14), legend.position = c(0.79, 0.65))

PfbyPm_overlay_fig2 <- PfbyPm_overlay_fig + expand_limits(x=c(0.1,3000000), y=c(0.0, 0.80)) 
plot(PfbyPm_overlay_fig2)



#####Creating a scaled plot to depict differences in sample size
ggplot(PfvsPfPm_active) +
  geom_density(aes(x = Pd_Pf, y = after_stat(count), fill=Pf_Species), alpha = 0.8)+
  scale_x_log10(name="Estimated Pf Parasite Density (p/uL)", breaks=c(.01,.1,1,10, 100, 1000, 10000, 100000, 1000000),labels=xlabels_manual)
  


## Pf vs. Po

#Density plot on log scale
dplot_PfbyPo<-ggplot(PfvsPfPo_active)+
  geom_density(aes(x = Pd_Pf, fill = Pf_Species), alpha=0.5)+
  scale_x_log10(name="Estimated Parasite Density (p/uL)")
plot(dplot_PfbyPo)

#histogram plot using log scale
hplot_PfbyPo<-ggplot(PfvsPfPo_active)+
  geom_histogram(aes(x = Pd_Pf, fill = Pf_Species), alpha=0.3)+
  scale_x_log10(name="Estimated Parasite Density (p/uL)")
plot(hplot_PfbyPo)

#overlaying density plot onto histogram to show the raw counts too
#un-stratified counts in background: 
PfbyPo_overlay<-ggplot(PfvsPfPo_active, aes(x=Pd_Pf, bins=10))+
  geom_histogram(aes(y=..density..), alpha=0.5, col='gray', lwd=0.4)+
  scale_x_log10(name="Estimated Pf Parasite Density (p/uL)", breaks=c(.01,.1,1,10, 100, 1000, 10000, 100000, 1000000),labels=xlabels_manual)+
  geom_density(aes(fill=factor(Pf_Species)), col='black', lwd=0.6, alpha=0.4)
  #labs(title="Pf mono vs. Pf+Po mixed infections - Active visits", fontsize = 8)

PfbyPo_overlay_fig <- PfbyPo_overlay + guides(fill=guide_legend(title=""))
plot(PfbyPo_overlay_fig)

#Stratified (but stacked) counts in background:
PfbyPo_overlay<-ggplot(PfvsPfPo_active, aes(x=Pd_Pf, fill=Pf_Species, bins=10))+
  geom_histogram(aes(y=..density..), position="identity", alpha=0.3, col='gray', lwd=0.2)+
  #scale_x_log10(name="Estimated Pf Parasite Density (p/uL)", breaks=c(.01,.1,1,10, 100, 1000, 10000, 100000, 1000000),labels=xlabels_manual)+
  scale_x_log10(name="", breaks=c(.01,.1,1,10, 100, 1000, 10000, 100000, 1000000),labels=xlabels_manual)+
  geom_density(aes(fill=factor(Pf_Species)), col='black', lwd=0.6, alpha=0.2)+
  scale_fill_manual(values=c("#c44343", "#f5cc5b"))
  #+labs(title="Pf mono vs. Pf+Po mixed infections - Survey Pop.", fontsize = 8)

PfbyPo_overlay_fig <- PfbyPo_overlay + guides(fill=guide_legend(title="")) +
                      # geom_vline(xintercept = 10, linetype="solid", color = "dark gray", size=1.3) +
                      # annotate("text", label = "LoD", x = 7, y = 0.87, size = 5, colour = "dark gray")+
                      theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                                         panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), 
                                         text=element_text(size=14), legend.position = c(0.79, 0.65))

PfbyPo_overlay_fig2 <- PfbyPo_overlay_fig + expand_limits(x=c(0.1,3000000), y=c(0.0, 0.80))
plot(PfbyPo_overlay_fig2)



 
##PASSIVE POP.

##Pf vs. Pm 

#Density plot using log scale
dplot_PfbyPm<-ggplot(PfvsPfPm_passive)+
  geom_density(aes(x = Pd_Pf, fill = Pf_Species), alpha=0.5)+
  scale_x_log10(name="Estimated Parasite Density (p/uL)")
plot(dplot_PfbyPm)

#histogram plot using log scale
hplot_PfbyPm<-ggplot(PfvsPfPm_passive)+
  geom_histogram(aes(x = Pd_Pf, fill = Pf_Species), alpha=0.3)+
  scale_x_log10(name="Estimated Parasite Density (p/uL)")
plot(hplot_PfbyPm)

#overlaying density plot onto histogram to show the raw counts too
#un-stratified counts in background: 
PfbyPm_overlay_Pas<-ggplot(PfvsPfPm_passive, aes(x=Pd_Pf, bins=10))+
  geom_histogram(aes(y=..density..), alpha=0.5, col='gray', lwd=0.4)+
  #scale_x_log10(name="Estimated Pf Parasite Density (p/uL)")+
  scale_x_log10(name="Estimated Pf Parasite Density (p/uL)", breaks=c(.01,.1,1,10, 100, 1000, 10000, 100000, 1000000),labels=xlabels_manual)+
  geom_density(aes(fill=factor(Pf_Species)), col='black', lwd=0.6, alpha=0.4)
  #+labs(title="Pf mono vs. Pf+Pm mixed infections - Passive visits", fontsize = 8) 

PfbyPm_overlay_Pas_fig <- PfbyPm_overlay_Pas + guides(fill=guide_legend(title=""))
plot(PfbyPm_overlay_Pas_fig)

#Stratified (but stacked) counts in background:
PfbyPm_overlay_Pas<-ggplot(PfvsPfPm_passive, aes(x=Pd_Pf, fill=Pf_Species, bins=10))+
  geom_histogram(aes(y=..density..), position="identity", alpha=0.3, col='gray', lwd=0.2)+
  scale_x_log10(name="Estimated Pf Parasite Density (p/uL)", breaks=c(.01,.1,1,10, 100, 1000, 10000, 100000, 1000000),labels=xlabels_manual)+
  geom_density(aes(fill=factor(Pf_Species)), col='black', lwd=0.6, alpha=0.2)+
  scale_fill_manual(values=c("#5b9bf5", "#f5cc5b"))
  #+labs(title="Pf mono vs. Pf+Pm mixed infections - Clinic Sub-Pop.", fontsize = 8)


PfbyPm_overlay_Pas_fig <- PfbyPm_overlay_Pas + guides(fill=guide_legend(title="")) +
                            # annotate("text", label = "LoD", x = 7, y = 0.87, size = 5, gend(title="")) +
                            # geom_vline(xintercept = 10, linetype="solid", color = "dark grcolour = "dark gray")+
                            theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                                               panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), 
                                               text=element_text(size=14), legend.position = c(0.79, 0.65))


PfbyPm_overlay_Pas_fig2 <- PfbyPm_overlay_Pas_fig + expand_limits(x=c(0.1 ,3000000), y=c(0.0, 0.80))
plot(PfbyPm_overlay_Pas_fig2)


## Pf vs. Po

#Density plot using log scale
dplot_PfbyPo<-ggplot(PfvsPfPo_passive)+
  geom_density(aes(x = Pd_Pf, fill = Pf_Species), alpha=0.5)+
  scale_x_log10(name="Estimated Parasite Density (p/uL)")
plot(dplot_PfbyPo)

#histogram plot using log scale
hplot_PfbyPo<-ggplot(PfvsPfPo_passive)+
  geom_histogram(aes(x = Pd_Pf, fill = Pf_Species), alpha=0.3)+
  scale_x_log10(name="Estimated Parasite Density (p/uL)")
plot(hplot_PfbyPo)

#overlaying density plot onto histogram to show the raw counts too
#un-stratified counts in background: 
PfbyPo_overlay_Pas<-ggplot(PfvsPfPo_passive, aes(x=Pd_Pf, bins=10))+
  geom_histogram(aes(y=..density..), alpha=0.5, col='gray', lwd=0.4)+
  scale_x_log10(name="Estimated Pf Parasite Density (p/uL)", breaks=c(.01,.1,1,10, 100, 1000, 10000, 100000, 1000000),labels=xlabels_manual)+
  geom_density(aes(fill=factor(Pf_Species)), col='black', lwd=0.6, alpha=0.4)
  #+labs(title="Pf mono vs. Pf+Po mixed infections - Passive visits", fontsize = 8)

PfbyPo_overlay_Pas_fig <- PfbyPo_overlay_Pas + guides(fill=guide_legend(title=""))
plot(PfbyPo_overlay_Pas_fig)

#Stratified (but stacked) counts in background:
PfbyPo_overlay_Pas<-ggplot(PfvsPfPo_passive, aes(x=Pd_Pf, fill=Pf_Species, bins=10))+
  geom_histogram(aes(y=..density..), position="identity", alpha=0.3, col='gray', lwd=0.2)+
  scale_x_log10(name="Estimated Pf Parasite Density (p/uL)", breaks=c(.01,.1,1,10, 100, 1000, 10000, 100000, 1000000),labels=xlabels_manual)+
  geom_density(aes(fill=factor(Pf_Species)), col='black', lwd=0.6, alpha=0.2)+
  scale_fill_manual(values=c("#c44343", "#f5cc5b"))
  #+labs(title="Pf mono vs. Pf+Po mixed infections - Clinic Sub-Pop.", fontsize = 8)


PfbyPo_overlay_Pas_fig <- PfbyPo_overlay_Pas + guides(fill=guide_legend(title="")) +
                          # annotate("text", label = "LoD", x = 7, y = 0.87, size = 5, gend(title="")) +
                          # geom_vline(xintercept = 10, linetype="solid", color = "dark grcolour = "dark gray")+
                          theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                                             panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), 
                                             text=element_text(size=14), legend.position = c(0.79, 0.65))


PfbyPo_overlay_Pas_fig2 <- PfbyPo_overlay_Pas_fig + expand_limits(x=c(0.1 ,3000000), y=c(0.0, 0.80))
plot(PfbyPo_overlay_Pas_fig2)



###Aggregating the Pf parasitemia figures
gridExtra::grid.arrange(PfbyPm_overlay_fig2, PfbyPo_overlay_fig2, 
                        PfbyPm_overlay_Pas_fig2, PfbyPo_overlay_Pas_fig2,   ncol=2, nrow=2)   #print Pf species density graphs in one frame

## Exporting aggregate image as a high-res file
png("Pf_mixedvsmono_ParasiteDensity_PublicationFig.png", width =14, height = 14, units = "cm", res = 600)
dev.off()



#######  Now Plotting Pm and Po density + Histogram plots comparing mixed vs. mono in Active and Passive visits 


##### ACTIVE VISITS

## Pm 
Pm_overlay_Act<-ggplot(pm_pd_active, aes(x=Pd_Pm_mult4, fill=Pm_Species, bins=10))+
  geom_histogram(aes(y=..density..), alpha=0.3, col='gray', lwd=0.2)+
  scale_x_log10(name="Estimated Pm Parasite Density (p/uL)", breaks=c(.01,.1,1,10, 100, 1000, 10000, 100000, 1000000),labels=xlabels_manual)+
  geom_density(aes(fill=factor(Pm_Species)), color=Pm_Species, lwd=1.0, alpha=0.4)+
  scale_fill_manual(values=c("#acbdf2", "#294ab3"))
  #+labs(title="Pm mono vs. mixed infections - Active visits", fontsize = 6)

Pm_overlay_Act_fig <- Pm_overlay_Act + guides(fill=guide_legend(title="")) +
   geom_vline(xintercept = 10, linetype="solid", color = "dark gray", size=0.6) +
   annotate("text", label = "Pm LoD", x = 3.0, y = 1.8, size = 5, colour = "dark gray")+
    theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), 
                     text=element_text(size=20)) 
                      #legend.position = c(0.87, 0.25)))

Pm_overlay_Act_fig2 <- Pm_overlay_Act_fig + expand_limits(x=c(0.001,3000000))

plot(Pm_overlay_Act_fig2)



## Po 
Po_overlay_Act<-ggplot(po_pd_active, aes(x=Pd_Po_mult4, fill=Po_Species, bins=10))+
 # geom_histogram(aes(y=..density..), alpha=0.3, col='gray', lwd=0.2)+
  scale_x_log10(name="Estimated Po Parasite Density (p/uL)", breaks=c(.01,.1,1,10, 100, 1000, 10000, 100000, 1000000),labels=xlabels_manual)+
  geom_density(aes(fill=factor(Po_Species)), col="black", lwd=1.2, alpha=0.6)+
  scale_fill_manual(values=c("#edbbbb", "#780101"))
  #+labs(title="Po mono vs. mixed infections - Active visits", fontsize = 6)

Po_overlay_Act_fig <- Po_overlay_Act + guides(fill=guide_legend(title="")) +
  geom_vline(xintercept = 10, linetype="solid", color = "dark gray", size=0.6) +
  annotate("text", label = "Po LoD", x = 3.5, y = 1.8, size = 5, colour = "dark gray")+
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), 
                     text=element_text(size=20)) 
                      #legend.position = c(0.87, 0.25)))
Po_overlay_Act_fig2 <- Po_overlay_Act_fig + expand_limits(x=c(0.001,3000000))

plot(Po_overlay_Act_fig2)



##### PASSIVE VISITS

## Pm 
Pm_overlay_Pas<-ggplot(pm_pd_passive, aes(x=Pd_Pm_mult4, fill=Pm_Species, bins=10))+
  geom_histogram(aes(y=..density..), alpha=0.3, col='gray', lwd=0.2)+
  scale_x_log10(name="Estimated Pm Parasite Density (p/uL)", breaks=c(.01,.1,1,10, 100, 1000, 10000, 100000, 1000000),labels=xlabels_manual)+
  geom_density(aes(fill=factor(Pm_Species)), col='black', lwd=1, alpha=0.4)
  #+labs(title="Pm mono vs. mixed infections - Passive visits", fontsize = 8)

Pm_overlay_Pas_fig <- Pm_overlay_Pas + guides(fill=guide_legend(title="")) +
  geom_vline(xintercept = 10, linetype="solid", color = "dark gray", size=1.0) +
  annotate("text", label = "Pm LoD", x = 3.0, y = 1.8, size = 3, colour = "dark gray")+
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+
  expand_limits(x=c(0,3000000))
              #legend.position = c(0.87, 0.25)))

Pm_overlay_Pas_fig2 <- Pm_overlay_Pas_fig + expand_limits(x=c(0,3000000))

plot(Pm_overlay_Pas_fig2)


## Po 
Po_overlay_Pas<-ggplot(po_pd_passive, aes(x=Pd_Po_mult4, fill=Po_Species, bins=10))+
  geom_histogram(aes(y=..density..), alpha=0.3, col='gray', lwd=0.2)+
  scale_x_log10(name="Estimated Po Parasite Density (p/uL)", breaks=c(.01,.1,1,10, 100, 1000, 10000, 100000, 1000000),labels=xlabels_manual)+
  geom_density(aes(fill=factor(Po_Species)), col='black', lwd=1, alpha=0.4)
  #+labs(title="Po mono vs. mixed infections - Passive visits", fontsize = 8)

Po_overlay_Pas_fig <- Po_overlay_Pas + guides(fill=guide_legend(title="")) +
  geom_vline(xintercept = 10, linetype="solid", color = "dark gray", size=1.0) +
  annotate("text", label = "Po LoD", x = 4.0, y = 1.8, size = 3, colour = "dark gray")+
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) 
#legend.position = c(0.87, 0.25)))
Po_overlay_Pas_fig2 <- Po_overlay_Pas_fig + expand_limits(x=c(0,3000000))

plot(Po_overlay_Pas_fig2)




###Aggregating ALL parasitemia figures
gridExtra::grid.arrange(PfbyPm_overlay_fig2, PfbyPo_overlay_fig2, 
                        PfbyPm_overlay_Pas_fig2, PfbyPo_overlay_Pas_fig2,   ncol=2, nrow=2)   #print Pf species density graphs in one frame


gridExtra::grid.arrange(Pm_overlay_Act_fig2, Po_overlay_Act_fig2, 
                        Pm_overlay_Pas_fig2, Po_overlay_Pas_fig2,   ncol=2, nrow=2)   #print Pm and Po species density graphs in one frame



##### Creating additional plots for visualization and descriptive review of data 
# parasitemia for total pop. 
#P.malariae - mixed vs. mono
pm<-ggplot(pm_pd)+
  geom_histogram(aes(Pd_Pm_mult4, fill=Pm_Species), color="gray80",bins=10)+
  scale_fill_brewer(palette = "Blues", direction=1, name="Species")+
  scale_x_log10(name="Estimated Parasite Density (p/uL)")+
  guides(fill = guide_legend(reverse=T))+
  ggtitle(expression(italic("P. malariae (Total Pop.)")))+
  geom_vline(aes(xintercept = median(Pd_Pm_mult4[Pm_Species=="Pm Mono"]))) + 
  geom_vline(aes(xintercept = median(Pd_Pm_mult4[Pm_Species=="Pm Mixed"])))+
  theme_bw()+
  theme(legend.position = c(0.93, 0.8),
        plot.title = element_text(hjust = 0.5))

print(pm)

#P.ovale - mixed vs. mono
        po<-ggplot(po_pd)+
          geom_histogram(aes(Pd_Po_mult4, fill=Po_Species), color="gray80",bins=10)+
          scale_fill_brewer(palette = "Reds", direction=1, name="Species")+
          scale_x_log10(name="Estimated Parasite Density (p/uL)")+
          guides(fill = guide_legend(reverse=T))+
          geom_vline(aes(xintercept = median(Pd_Po_mult4[Po_Species=="Po Mono"]))) + 
          geom_vline(aes(xintercept = median(Pd_Po_mult4[Po_Species=="Po Mixed"])))+
          ggtitle(expression(italic("P. ovale spp. (Total Pop.)")))+
          theme_bw()+
          theme(legend.position = c(0.93, 0.8),
                plot.title = element_text(hjust = 0.5))
        
        print(po)
        
      
#P.falciparum - mixed vs. mono
pf<-ggplot(pf_pd)+
  geom_histogram(aes(Pd_Pf, fill=Pf_Species), color="gray80",bins=10)+
  scale_fill_brewer(palette = "Greens", direction=1, name="Species")+
  scale_x_log10(name="Estimated Parasite Density (p/uL)")+
  guides(fill = guide_legend(reverse=T))+
  ggtitle(expression(italic("P. falciparum (Total Pop.)")))+
  geom_vline(aes(xintercept = median(Pd_Pf[Pf_Species=="Pf Mono"]))) + 
  geom_vline(aes(xintercept = median(Pd_Pf[Pf_Species=="Pf Mixed"])))+
  theme_bw()+
  theme(legend.position = c(0.93, 0.8),
        plot.title = element_text(hjust = 0.5))

print(pf)


gridExtra::grid.arrange(pm, po, pf, ncol=3)   #print all 3 species density graphs in one frame


ggsave(filename=".../DRC_nonfalcip_active_pd.png", device="png",
       height=4, width=8, units="in", dpi=600, bg="transparent")




##########################################################
# Cumulative Incidence Analyses - Risk Curves - Overall and Stratified by different factors    
##########################################################

##Analysis Notes: 
                  #All datasets are comprised of subjects negative for the species-specific infection at baseline
                  #Active + Passive dataset includes the time to first active OR passive infection, only among those negative for infection at baseline AND who had at least 1 VNP visit during the study

#Analysis Datasets created in SAS: (program: AnalysisDatasetBuild_DRCNonPf_Mar22_RS.sas)


#Read in incidence-specific analytic datasets   (separate datasets created for each species, and each analysis population (ie 9 total datasets))

###PM INFECTIONS###
#Active only 
Pm_Risk_Act<-read_excel("../Pm_Risk_Cohort_Active_Sub_Jul22.xlsx")

#Active & Passive  -VNP pop
Pm_Risk_VNP<-read_excel("../Pm_Risk_Cohort_VNP_Sub_Jul22.xlsx")

#Active & Passive  - TOtal pop
Pm_Risk_Tot<-read_excel("../Pm_Risk_Cohort_Total_Sub_Jul22.xlsx")


###PO INFECTIONS###
#Active only 
Po_Risk_Act<-read_excel("../Po_Risk_Cohort_Active_Sub_Jul22.xlsx")

#Active & Passive  
Po_Risk_VNP<-read_excel("../Po_Risk_Cohort_VNP_Sub_Jul22.xlsx")

#Active & Passive  - TOtal pop
Po_Risk_Tot<-read_excel("../Po_Risk_Cohort_Total_Sub_Jul22.xlsx")


###PF INFECTIONS###
#Active only 
Pf_Risk_Act<-read_excel("../Pf_Risk_Cohort_Active_Sub_Jul22.xlsx")

#Active & Passive  
Pf_Risk_VNP<-read_excel("../Pf_Risk_Cohort_VNP_Sub_Jul22.xlsx")

#Active & Passive  - TOtal pop
Pf_Risk_Tot<-read_excel("../Pf_Risk_Cohort_Total_Sub_Jul22.xlsx")



###DEFINING NEW VARIABLES FOR ANALYSIS###
Pm_Risk_Act<- Pm_Risk_Act%>%mutate(age_cat2= ifelse(age_cat_FU0==1, 1, 
                                                    ifelse(age_cat_FU0==2, 2, 
                                                           ifelse(age_cat_FU0==3, 2, 
                                                                  ifelse(age_cat_FU0==4, 3, 
                                                                         ifelse(age_cat_FU0==5, 3, NA))))))%>%
                         mutate(Time_active_mon=Time_active/30.4)
              

Pm_Risk_VNP<- Pm_Risk_VNP%>%mutate(age_cat2= ifelse(age_cat_FU0==1, 1, 
                                                    ifelse(age_cat_FU0==2, 2, 
                                                           ifelse(age_cat_FU0==3, 2, 
                                                                  ifelse(age_cat_FU0==4, 3, 
                                                                         ifelse(age_cat_FU0==5, 3, NA))))))%>%
                          mutate(Time_VNP_end_mon=Time_VNP_end/30.4)%>%
                          mutate(Time_all_end_mon=Time_all_end/30.4)

Pm_Risk_Tot<- Pm_Risk_Tot%>%mutate(age_cat2= ifelse(age_cat_FU0==1, 1, 
                                                    ifelse(age_cat_FU0==2, 2, 
                                                           ifelse(age_cat_FU0==3, 2, 
                                                                  ifelse(age_cat_FU0==4, 3, 
                                                                         ifelse(age_cat_FU0==5, 3, NA))))))%>%
                          mutate(Time_VNP_end_mon=Time_VNP_end/30.4)%>%
                          mutate(Time_all_end_mon=Time_all_end/30.4)%>%
                          mutate(Pm_Event_All_CE= ifelse(Pm_Event_All==0, 0, 
                                                   ifelse(Pm_Event_All==1, 1, 2)))

Po_Risk_Act<- Po_Risk_Act%>%mutate(age_cat2= ifelse(age_cat_FU0==1, 1, 
                                                    ifelse(age_cat_FU0==2, 2, 
                                                           ifelse(age_cat_FU0==3, 2, 
                                                                  ifelse(age_cat_FU0==4, 3, 
                                                                         ifelse(age_cat_FU0==5, 3, NA))))))%>%
                          mutate(Time_active_mon=Time_active/30.4)

Po_Risk_VNP<- Po_Risk_VNP%>%mutate(age_cat2= ifelse(age_cat_FU0==1, 1, 
                                                    ifelse(age_cat_FU0==2, 2, 
                                                           ifelse(age_cat_FU0==3, 2, 
                                                                  ifelse(age_cat_FU0==4, 3, 
                                                                         ifelse(age_cat_FU0==5, 3, NA))))))%>%
                          mutate(Time_VNP_end_mon=Time_VNP_end/30.4)%>%
                          mutate(Time_all_end_mon=Time_all_end/30.4)
                
Po_Risk_Tot<- Po_Risk_Tot%>%mutate(age_cat2= ifelse(age_cat_FU0==1, 1, 
                                                    ifelse(age_cat_FU0==2, 2, 
                                                           ifelse(age_cat_FU0==3, 2, 
                                                                  ifelse(age_cat_FU0==4, 3, 
                                                                         ifelse(age_cat_FU0==5, 3, NA))))))%>%
                          mutate(Time_VNP_end_mon=Time_VNP_end/30.4)%>%
                          mutate(Time_all_end_mon=Time_all_end/30.4)%>%
                          mutate(Po_Event_All_CE= ifelse(Po_Event_All==0, 0, 
                                                  ifelse(Po_Event_All==1, 1, 2)))
  

Pf_Risk_Act<- Pf_Risk_Act%>%mutate(age_cat2= ifelse(age_cat_FU0==1, 1, 
                                                    ifelse(age_cat_FU0==2, 2, 
                                                           ifelse(age_cat_FU0==3, 2, 
                                                                  ifelse(age_cat_FU0==4, 3, 
                                                                         ifelse(age_cat_FU0==5, 3, NA))))))%>%
                          mutate(Time_active_mon=Time_active/30.4)

Pf_Risk_VNP<- Pf_Risk_VNP%>%mutate(age_cat2= ifelse(age_cat_FU0==1, 1, 
                                                    ifelse(age_cat_FU0==2, 2, 
                                                           ifelse(age_cat_FU0==3, 2, 
                                                                  ifelse(age_cat_FU0==4, 3, 
                                                                         ifelse(age_cat_FU0==5, 3, NA))))))%>%
                          mutate(Time_VNP_end_mon=Time_VNP_end/30.4)%>%
                          mutate(Time_all_end_mon=Time_all_end/30.4)
                  

Pf_Risk_Tot<- Pf_Risk_Tot%>%mutate(age_cat2= ifelse(age_cat_FU0==1, 1, 
                                                    ifelse(age_cat_FU0==2, 2, 
                                                           ifelse(age_cat_FU0==3, 2, 
                                                                  ifelse(age_cat_FU0==4, 3, 
                                                                         ifelse(age_cat_FU0==5, 3, NA))))))%>%
                          mutate(Time_VNP_end_mon=Time_VNP_end/30.4)%>%
                          mutate(Time_all_end_mon=Time_all_end/30.4)%>%
                          mutate(Pf_Event_All_CE= ifelse(Pf_Event_All==0, 0, 
                                                  ifelse(Pf_Event_All==1, 1, 2)))
                        
  
##########################################################
# COMBINED SURVIVAL CURVES OF PM, PO, AND PF INFECTION    
##########################################################

#Estimate the Kaplan Meier Crude Risk Function for All Datasets, where event = 1 and Time = time to event or censoring due to LTFU 
##Estimating cumulative incidence = 1-KM survival function (assuming no competing risks and non-informative censoring)

Pf_Act_Surv<-survfit(Surv(Time_active_mon, Pf_Event_Active) ~ 1, data = Pf_Risk_Act)
Pm_Act_Surv<-survfit(Surv(Time_active_mon, Pm_Event_Active) ~ 1, data = Pm_Risk_Act)
Po_Act_Surv<-survfit(Surv(Time_active_mon, Po_Event_Active) ~ 1, data = Po_Risk_Act)


fit <- list(pfs=Pf_Act_Surv, pos=Po_Act_Surv, pms=Pm_Act_Surv)
ggsurvplot(fit, combine = TRUE, 
           legend.title = "Survey-based Pop.",
         #  legend.labs = c(""),
           break.x.by = 6, 
           # Add p-value and tervals
          # pval = TRUE,
           conf.int = TRUE,
           #surv.median.line = c("hv"),
           censor = TRUE, 
           # Add risk table
           risk.table = TRUE,
           tables.height = 0.17,
           cumevents = TRUE, 
          # cumcensor = TRUE, 
           #cumcensor.title = "Cumulative number censored",
           cumevent.title = "Cumulative infections",
           #risk.table.fontsize = 2.5,
          # font.x=2.5,
           tables.theme = theme_cleantable(),
           xlab = "Months from Baseline", 
           ylab = ("Prob. un-infected"), 
           xlim = c(0,24))
           

Pf_Act_Surv<-survfit(Surv(Time_active_mon, Pf_Event_Active) ~ 1, data = Pf_Risk_Tot)
Pm_Act_Surv<-survfit(Surv(Time_active_mon, Pm_Event_Active) ~ 1, data = Pm_Risk_Tot)
Po_Act_Surv<-survfit(Surv(Time_active_mon, Po_Event_Active) ~ 1, data = Po_Risk_Tot)


fit <- list(pfs=Pf_Act_Surv, pos=Po_Act_Surv, pms=Pm_Act_Surv)
ggsurvplot(fit, combine = TRUE, 
           legend.title = "Survey-based Pop.",
           #  legend.labs = c(""),
           break.x.by = 6, 
           # Add p-value and tervals
           # pval = TRUE,
           conf.int = TRUE,
           #surv.median.line = c("hv"),
           censor = TRUE, 
           # Add risk table
           risk.table = TRUE,
           tables.height = 0.17,
           cumevents = TRUE, 
           # cumcensor = TRUE, 
           #cumcensor.title = "Cumulative number censored",
           cumevent.title = "Cumulative infections",
           #risk.table.fontsize = 2.5,
           # font.x=2.5,
           tables.theme = theme_cleantable(),
           xlab = "Months from Baseline", 
           ylab = ("Prob. un-infected"), 
           xlim = c(0,24))
           

#Crude Pm Risk among Active Follow-ups

##Look at Time in days and months to compare (months is likely easier to interpret in plots)
km <- survfit(Surv(Time_active_mon, Pm_Event_Active) ~ 1, data=Pm_Risk_Act, stype=2, id=Subject_ID)
delta_risk_Pm_act <- data.frame(time = summary(km)$time, 
                                surv = summary(km)$surv, 
                                risk = 1 - summary(km)$surv, 
                                LB_CI = 1- summary(km)$upper,
                                UB_CI = 1- summary(km)$lower) 


ggplot(data = delta_risk_Pm_act) + 
  geom_step(aes(x = time, y = risk), color = "turquoise", size=1.4)+

  
  geom_ribbon(aes(ymin=LB_CI, ymax=UB_CI, x=time),  fill = "turquoise", alpha=0.3)+
  ylab("Cumulative Incidence") +
  xlab("Months from Baseline")+
  labs(title="Risk of Pm Infxn during Active Visits")+
  theme_clean()

ggsurvplot(
  fit = survfit(Surv(Time_active_mon, Pm_Event_Active) ~ 1, data = Pm_Risk_Act), 
  legend.title = "P.malariae",
  legend.labs = c(""),
  break.x.by = 6, 
  # Add p-value and tervals
  pval = TRUE,
  conf.int = TRUE,
  #surv.median.line = c("hv"),
  censor = TRUE, 
  # Add risk table
  risk.table = TRUE,
  tables.height = 0.15,
  cumevents = TRUE, 
  cumcensor = TRUE, 
  risk.table.fontsize = 4,
  font.x=4,
  tables.theme = theme_cleantable(),
  xlab = "Months from Baseline", 
  ylab = ("Survival prob."), 
  xlim = c(0,20))

#Summary with 95% CIs for survival estimate
summary(survfit(Surv(Time_active_mon, Pm_Event_Active) ~ 1, data = Pm_Risk_Act))
summary(delta_risk_Pm_act)

#__________________________________________________________________________________________________#

#Crude Po Risk among Active Follow-ups

km <- survfit(Surv(Time_active_mon, Po_Event_Active) ~ 1, data=Po_Risk_Act, stype=2, id=Subject_ID)
delta_risk_Po_act <- data.frame(time = summary(km)$time, 
                                surv = summary(km)$surv, 
                                risk = 1 - summary(km)$surv, 
                                LB_CI = 1- summary(km)$upper,
                                UB_CI = 1- summary(km)$lower) 


ggplot(data = delta_risk_Po_act) + 
geom_step(aes(x = time, y = risk), color = "red", size=1.4)+
  
  
  geom_ribbon(aes(ymin=LB_CI, ymax=UB_CI, x=time),  fill = "red", alpha=0.3)+
  ylab("Cumulative Incidence") +
  xlab("Months from Baseline")+
  labs(title="Risk of Po Infxn during Active Visits")+
  theme_clean()

ggsurvplot(
  fit = survfit(Surv(Time_active_mon, Po_Event_Active) ~ 1, data = Po_Risk_Act), 
  legend.title = "P.ovale",
  legend.labs = c(""),
  break.x.by = 6, 
  # Add p-value and tervals
  pval = TRUE,
  conf.int = TRUE,
  #surv.median.line = c("hv"),
  censor = TRUE, 
  # Add risk table
  risk.table = TRUE,
  tables.height = 0.15,
  cumevents = TRUE, 
  cumcensor = TRUE, 
  risk.table.fontsize = 4,
  font.x=4,
  tables.theme = theme_cleantable(),
  xlab = "Months from Baseline", 
  ylab = ("Survival prob."), 
  xlim = c(0,20))

#Summary with 95% CIs for survival estimate
summary(survfit(Surv(Time_active_mon, Po_Event_Active) ~ 1, data = Po_Risk_Act))
summary(delta_risk_Po_act)


#__________________________________________________________________________________________________#

#Crude Pf Risk among Active Follow-ups

km <- survfit(Surv(Time_active_mon, Pf_Event_Active) ~ 1, data=Pf_Risk_Act, stype=2, id=Subject_ID)
delta_risk_Pf_act <- data.frame(time = summary(km)$time, 
                                surv = summary(km)$surv, 
                                risk = 1 - summary(km)$surv, 
                                LB_CI = 1- summary(km)$upper,
                                UB_CI = 1- summary(km)$lower) 


ggplot(data = delta_risk_Pf_act) + 
geom_step(aes(x = time, y = risk), color = "red", size=1.4)+
  
  
  geom_ribbon(aes(ymin=LB_CI, ymax=UB_CI, x=time),  fill = "orange", alpha=0.3)+
  ylab("Cumulative Incidence") +
  xlab("Months from Baseline")+
  labs(title="Risk of Pf Infxn during Active Visits")+
  theme_clean()

ggsurvplot(
  fit = survfit(Surv(Time_active_mon, Pf_Event_Active) ~ 1, data = Pf_Risk_Act), 
  legend.title = "P.falciparum",
  legend.labs = c(""),
  break.x.by = 6, 
  # Add p-value and tervals
  pval = TRUE,
  conf.int = TRUE,
  #surv.median.line = c("hv"),
  censor = TRUE, 
  # Add risk table
  risk.table = TRUE,
  tables.height = 0.15,
  cumevents = TRUE, 
  cumcensor = TRUE, 
  risk.table.fontsize = 4,
  font.x=14,
  tables.theme = theme_cleantable(),
  xlab = "Months from Baseline", 
  ylab = ("Probability un-infected"), 
  xlim = c(0,20))

#Summary with 95% CIs for survival estimate
summary(survfit(Surv(Time_active_mon, Pf_Event_Active) ~ 1, data = Pf_Risk_Act))
summary(delta_risk_Pf_act)


#_________________________________________________________________________________________________#

# Pm risk in passive visits 

km <- survfit(Surv(Time_VNP_end_mon, Pm_event_VNP) ~ 1, data=Pm_Risk_VNP, stype=2, id=Subject_ID)
delta_risk_Pm_vnp <- data.frame(time = summary(km)$time, 
                                surv = summary(km)$surv, 
                                risk = 1 - summary(km)$surv, 
                                LB_CI = 1- summary(km)$upper,
                                UB_CI = 1- summary(km)$lower) 


ggplot(data = delta_risk_Pm_vnp) + 
  geom_step(aes(x = time, y = risk), color = "blue", size=1.4)+
  ylab("Risk") +
  labs(title="Risk of Pm Infxn during Passive Visits")

ggsurvplot(
  fit = survfit(Surv(Time_VNP_end_mon, Pm_event_VNP) ~ 1, data = Pm_Risk_VNP), 
  legend.title = "P.malariae",
  legend.labs = c(""),
  break.x.by = 6, 
  # Add p-value and tervals
  pval = TRUE,
  conf.int = TRUE,
  #surv.median.line = c("hv"),
  censor = TRUE, 
  # Add risk table
  risk.table = TRUE,
  tables.height = 0.15,
  cumevents = TRUE, 
  cumcensor = TRUE, 
  risk.table.fontsize = 4,
  font.x=14,
  tables.theme = theme_cleantable(),
  xlab = "Months from Baseline", 
  ylab = ("Prob. un-infected"), 
  xlim = c(0,36))

#Summary with 95% CIs for survival estimate
summary(survfit(Surv(Time_VNP_end_mon, Pm_event_VNP) ~ 1, data = Pm_Risk_VNP))
summary(delta_risk_Pm_vnp)


#_________________________________________________________________________________________________#
# Po risk in passive visits 

km <- survfit(Surv(Time_VNP_end_mon, Po_event_VNP) ~ 1, data=Po_Risk_VNP, stype=2, id=Subject_ID)
delta_risk_Po_vnp <- data.frame(time = summary(km)$time, 
                                surv = summary(km)$surv, 
                                risk = 1 - summary(km)$surv, 
                                LB_CI = 1- summary(km)$upper,
                                UB_CI = 1- summary(km)$lower) 


ggplot(data = delta_risk_Po_vnp) + 
  geom_step(aes(x = time, y = risk), color = "red", size=1.4)+
  ylab("Risk") +
  labs(title="Risk of Po Infxn during Passive Visits")

ggsurvplot(
  fit = survfit(Surv(Time_VNP_end_mon, Po_event_VNP) ~ 1, data = Po_Risk_VNP), 
  legend.title = "P.ovale",
  legend.labs = c(""),
  break.x.by = 6, 
  # Add p-value and tervals
  pval = TRUE,
  conf.int = TRUE,
  #surv.median.line = c("hv"),
  censor = TRUE, 
  # Add risk table
  risk.table = TRUE,
  tables.height = 0.15,
  cumevents = TRUE, 
  cumcensor = TRUE, 
  risk.table.fontsize = 4,
  font.x=4,
  tables.theme = theme_cleantable(),
  xlab = "Months from Baseline", 
  ylab = ("Survival prob."), 
  xlim = c(0,36))

#Summary with 95% CIs for survival estimate
summary(survfit(Surv(Time_VNP_end_mon, Po_event_VNP) ~ 1, data = Po_Risk_VNP))
summary(delta_risk_Po_vnp)



#_________________________________________________________________________________________________#
# Pf risk in passive visits 

km <- survfit(Surv(Time_VNP_end_mon, Pf_event_VNP) ~ 1, data=Pf_Risk_VNP, stype=2, id=Subject_ID)
delta_risk_Pf_vnp <- data.frame(time = summary(km)$time, 
                                surv = summary(km)$surv, 
                                risk = 1 - summary(km)$surv, 
                                LB_CI = 1- summary(km)$upper,
                                UB_CI = 1- summary(km)$lower) 


ggplot(data = delta_risk_Pf_vnp) + 
  geom_step(aes(x = time, y = risk), color = "orange", size=1.4)+
  ylab("Risk") +
  labs(title="Risk of Pf Infxn during Passive Visits")

ggsurvplot(
  fit = survfit(Surv(Time_VNP_end_mon, Pf_event_VNP) ~ 1, data = Pf_Risk_VNP), 
  legend.title = "P.falciparum",
  legend.labs = c(""),
  break.x.by = 6, 
  # Add p-value and tervals
  pval = TRUE,
  conf.int = TRUE,
  #surv.median.line = c("hv"),
  censor = TRUE, 
  # Add risk table
  risk.table = TRUE,
  tables.height = 0.15,
  cumevents = TRUE, 
  cumcensor = TRUE, 
  risk.table.fontsize = 4,
  font.x=4,
  tables.theme = theme_cleantable(),
  xlab = "Months from Baseline", 
  ylab = ("Survival prob."), 
  xlim = c(0,36))

#Summary with 95% CIs for survival estimate
summary(survfit(Surv(Time_VNP_end_mon, Pf_event_VNP) ~ 1, data = Pf_Risk_VNP))
summary(delta_risk_Pf_vnp)




#_________________________________________________________________________________________________#
# TOTAL STUDY POPULATION 


# Pm risk in total study visits 

km <- survfit(Surv(Time_all_end_mon, Pm_Event_All) ~ 1, data=Pm_Risk_Tot, stype=2, id=Subject_ID)
delta_risk_Pm_tot <- data.frame(time = summary(km)$time, 
                                surv = summary(km)$surv, 
                                risk = 1 - summary(km)$surv, 
                                LB_CI = 1- summary(km)$upper,
                                UB_CI = 1- summary(km)$lower, 
                                n_atrisk = summary(km)$n.risk,
                                n_event = summary(km)$n.event, 
                                n_censor = summary(km)$n.censor) 


ggplot(data = delta_risk_Pm_tot) + 
  geom_step(aes(x = time, y = risk), color = "blue", size=1.4)+
  ylab("Risk") +
  labs(title="Risk of Pm Infxn during Active + Passive Visits")

ggsurvplot(
  fit = survfit(Surv(Time_all_end_mon, Pm_Event_All) ~ 1, data = Pm_Risk_Tot), 
  legend.title = "P.malariae",
  legend.labs = c(""),
  break.x.by = 6, 
  # Add p-value and tervals
  pval = TRUE,
  conf.int = TRUE,
  #surv.median.line = c("hv"),
  censor = TRUE, 
  # Add risk table
  risk.table = TRUE,
  tables.height = 0.15,
  cumevents = TRUE, 
  cumcensor = TRUE, 
  risk.table.fontsize = 4,
  font.x=4,
  tables.theme = theme_cleantable(),
  xlab = "Months from Baseline", 
  ylab = ("Survival prob."), 
  xlim = c(0,36))

#Summary with 95% CIs for survival estimate
summary(survfit(Surv(Time_all_end_mon, Pm_Event_All) ~ 1, data = Pm_Risk_Tot))
summary(delta_risk_Pm_tot)


#_________________________________________________________________________________________________#
# Po risk in total study visits 

km <- survfit(Surv(Time_all_end_mon, Po_Event_All) ~ 1, data=Po_Risk_Tot, stype=2, id=Subject_ID)
delta_risk_Po_tot <- data.frame(time = summary(km)$time, 
                                surv = summary(km)$surv, 
                                risk = 1 - summary(km)$surv, 
                                LB_CI = 1- summary(km)$upper,
                                UB_CI = 1- summary(km)$lower) 


ggplot(data = delta_risk_Po_tot) + 
  geom_step(aes(x = time, y = risk), color = "red", size=1.4)+
  ylab("Risk") +
  labs(title="Risk of Po Infxn during Active + Passive Visits")

ggsurvplot(
  fit = survfit(Surv(Time_all_end_mon, Po_Event_All) ~ 1, data = Po_Risk_Tot), 
  legend.title = "P.ovale",
  legend.labs = c(""),
  break.x.by = 6, 
  # Add p-value and tervals
  pval = TRUE,
  conf.int = TRUE,
  #surv.median.line = c("hv"),
  censor = TRUE, 
  # Add risk table
  risk.table = TRUE,
  tables.height = 0.15,
  cumevents = TRUE, 
  cumcensor = TRUE, 
  risk.table.fontsize = 4,
  font.x=4,
  tables.theme = theme_cleantable(),
  xlab = "Months from Baseline", 
  ylab = ("Survival prob."), 
  xlim = c(0,36))

#Summary with 95% CIs for survival estimate
summary(survfit(Surv(Time_all_end_mon, Po_Event_All) ~ 1, data = Po_Risk_Tot))
summary(delta_risk_Po_tot)

survfit2(Surv)


#_________________________________________________________________________________________________#
# Pf risk in total study visits 

km <- survfit(Surv(Time_all_end_mon, Pf_Event_All) ~ 1, data=Pf_Risk_Tot, stype=2, id=Subject_ID)
delta_risk_Pf_tot <- data.frame(time = summary(km)$time, 
                                surv = summary(km)$surv, 
                                risk = 1 - summary(km)$surv, 
                                LB_CI = 1- summary(km)$upper,
                                UB_CI = 1- summary(km)$lower) 


ggplot(data = delta_risk_Pf_tot) + 
  geom_step(aes(x = time, y = risk), color = "orange", size=1.4)+
  ylab("Risk") +
  labs(title="Risk of Pf Infxn during Active + Passive Visits")

ggsurvplot(
  fit = survfit(Surv(Time_all_end_mon, Pf_Event_All) ~ 1, data = Pf_Risk_Tot), 
  legend.title = "P.falciparum",
  legend.labs = c(""),
  break.x.by = 6, 
  # Add p-value and tervals
  pval = TRUE,
  conf.int = TRUE,
  #surv.median.line = c("hv"),
  censor = TRUE, 
  # Add risk table
  risk.table = TRUE,
  tables.height = 0.15,
  cumevents = TRUE, 
  cumcensor = TRUE, 
  risk.table.fontsize = 4,
  font.x=4,
  tables.theme = theme_cleantable(),
  xlab = "Months from Baseline", 
  ylab = ("Survival prob."), 
  xlim = c(0,36))

#Summary with 95% CIs for survival estimate
summary(survfit(Surv(Time_all_end_mon, Pf_Event_All) ~ 1, data = Pf_Risk_Tot))
summary(delta_risk_Pf_tot)




#_________________________________________________________________________________________________#

#######  ACTIVE STUDY POPULATION ONLY 
colors <- c("Pm" = "blue", "Po" = "red", "Pf" = "orange")


p1_Act<- ggplot() + 
          geom_step(data = delta_risk_Pm_act, aes(x = time, y = risk, color = "Pm"), size=1.4)+
          geom_ribbon(data = delta_risk_Pm_act, aes(ymin=LB_CI, ymax=UB_CI, x=time),  fill = "blue", alpha=0.3)+
          geom_step(data = delta_risk_Po_act, aes(x = time, y = risk, color = "Po"), size=1.4)+
          geom_ribbon(data = delta_risk_Po_act, aes(ymin=LB_CI, ymax=UB_CI, x=time),  fill = "red", alpha=0.3)+
          geom_step(data = delta_risk_Pf_act, aes(x = time, y = risk, color = "Pf"), size=1.4)+
          geom_ribbon(data = delta_risk_Pf_act, aes(ymin=LB_CI, ymax=UB_CI, x=time),  fill = "orange", alpha=0.3)+
          ylab("Cumulative Incidence") +
          xlab("Months from Baseline")+
          ylim(0,1.0)+
          scale_x_continuous(limits = c(0, 24), breaks = c(0, 6, 12, 18, 24))+
          labs(title="Risk of Malaria Infection by Species, Active Visits", 
               x = "Months from Baseline", 
               y = "Cumulative Incidence", 
               color = "Legend")+
          scale_color_manual(values = colors)+
          theme_clean()
          #scale_color_manual(name = "Species", 
           #          breaks= c("P.malariae", "P.ovale", "P.falciparum"), 
            #         values= c("Pm" = "blue", "Po" = "red", "Pf" = "orange"))
plot(p1_Act)



#######  PASSIVE STUDY POPULATION ONLY 
p1_VNP<- ggplot() + 
  geom_step(data = delta_risk_Pm_vnp, aes(x = time, y = risk, color = "Pm"), size=1.4)+
  geom_ribbon(data = delta_risk_Pm_vnp, aes(ymin=LB_CI, ymax=UB_CI, x=time),  fill = "blue", alpha=0.3)+
  geom_step(data = delta_risk_Po_vnp, aes(x = time, y = risk, color = "Po"), size=1.4)+
  geom_ribbon(data = delta_risk_Po_vnp, aes(ymin=LB_CI, ymax=UB_CI, x=time),  fill = "red", alpha=0.3)+
  geom_step(data = delta_risk_Pf_vnp, aes(x = time, y = risk, color = "Pf"), size=1.4)+
  geom_ribbon(data = delta_risk_Pf_vnp, aes(ymin=LB_CI, ymax=UB_CI, x=time),  fill = "orange", alpha=0.3)+
  ylab("Cumulative Incidence") +
  xlab("Months from Baseline")+
  ylim(0,1.0)+
  scale_x_continuous(limits = c(0, 36), breaks = c(0, 6, 12, 18, 24, 30, 36))+
  labs(title="Risk of Malaria Infection by Species, Passive Visits", 
       x = "Months from Baseline", 
       y = "Cumulative Incidence", 
       color = "Species")+
  scale_color_manual(values = colors)+
  theme_clean()
#scale_color_manual(name = "Species", 
#          breaks= c("P.malariae", "P.ovale", "P.falciparum"), 
#         values= c("Pm" = "blue", "Po" = "red", "Pf" = "orange"))
plot(p1_VNP)



####FULL PLOT - TOTAL STUDY POPULATION - VNP and Active - THROUGH 31 DEC 17 - All Species
p1_Tot<-ggplot() + 
  geom_step(data=delta_risk_Po_tot, aes(x = time, y = risk, color = "Po"), size=1.3)+
  geom_ribbon(data=delta_risk_Po_tot, aes(ymin=LB_CI, ymax=UB_CI, x=time),  fill = "red", alpha=0.1)+
  geom_step(data=delta_risk_Pm_tot, aes(x = time, y = risk, color = "Pm"), size=1.3)+
  geom_ribbon(data=delta_risk_Pm_tot, aes(ymin=LB_CI, ymax=UB_CI, x=time), fill = "blue", alpha=0.1)+
  geom_step(data=delta_risk_Pf_tot, aes(x = time, y = risk, color = "Pf"), size=1.3)+
  geom_ribbon(data=delta_risk_Pf_tot, aes(ymin=LB_CI, ymax=UB_CI, x=time), fill = "orange", alpha=0.1)+
  # geom_vline(xintercept = 365, color = "black", lwd = 1, alpha = 0.5)+
  # geom_vline(xintercept = 730, color = "black", lwd = 1, alpha = 0.5)+
  ylim(0,1.0)+
  scale_x_continuous(limits = c(0, 36), breaks = c(0, 6, 12, 18, 24, 30, 36))+
  labs(title="", 
       x = "Months from Baseline", 
       y = "Cumulative Incidence", 
       color = "Species")+
  scale_color_manual(values = colors)+
  theme_set(
      theme_clean(base_size = 15))

p1_Tot <- p1_Tot + theme(legend.position = c(0.84, 0.6))
plot(p1_Tot)


#####TOTAL STUDY POPULATION: ZOOMED INSERT FOR PLOT WITH MULTIPLE CURVES
p2_Tot<-ggplot() + 
  geom_step(data=delta_risk_Po_tot, aes(x = time, y = risk, color = "Po"), size=0.8)+
  geom_ribbon(data=delta_risk_Po_tot, aes(ymin=LB_CI, ymax=UB_CI, x=time),  fill = "red", alpha=0.1)+
  geom_step(data=delta_risk_Pm_tot, aes(x = time, y = risk, color = "Pm"), size=0.8)+
  geom_ribbon(data=delta_risk_Pm_tot, aes(ymin=LB_CI, ymax=UB_CI, x=time), fill = "blue", alpha=0.1)+
  #   geom_vline(xintercept = 365, color = "black", lwd = 1, alpha = 0.5)+
  #   geom_vline(xintercept = 730, color = "black", lwd = 1, alpha = 0.5)+
  ylim(0,0.25)+
 # labs(title="Pm and Po infection incidence, Total Population", fontsize = 4)+
  theme_clean()+
  scale_x_continuous(limits = c(0, 36), breaks = c(0, 6, 12, 18, 24, 30, 36))+
  labs(x = "Months", 
       y = "Cum. Incid.")+
  scale_color_manual(values = c("Pm"= "blue", "Po"="red"), guide="none")

plot(p2_Tot)


#######NOW CREATING A ZOOMED INSERT

p1_Tot+
  annotation_custom(ggplotGrob(p2_Tot), xmin = -1.0, xmax=10.8, ymin=0.70, ymax=1.0)+
  geom_rect(aes(xmin = -1.0, xmax=10.8, ymin=0.70, ymax = 1.0), color = 'black', linetype = "solid", alpha = 0 )



####All species: CI Curves with 95% CIs 
jpeg("../Analysis/Figures/Draft Figures_Manuscript/KMRiskPlot_TotalPop_AllSpecies_Zoomed.jpg", quality = "100", 
     res = 600, bg = "transparent",  height=5, width=6, units = "in" )





####FULL PLOT - SURVEY POPULATION -Active ONLY - THROUGH FU3  - All Species
p1_Act<-ggplot() + 
  geom_step(data=delta_risk_Po_act, aes(x = time, y = risk, color = "Po"), size=1.3)+
  geom_ribbon(data=delta_risk_Po_act, aes(ymin=LB_CI, ymax=UB_CI, x=time),  fill = "red", alpha=0.1)+
  geom_step(data=delta_risk_Pm_act, aes(x = time, y = risk, color = "Pm"), size=1.3)+
  geom_ribbon(data=delta_risk_Pm_act, aes(ymin=LB_CI, ymax=UB_CI, x=time), fill = "blue", alpha=0.1)+
  geom_step(data=delta_risk_Pf_act, aes(x = time, y = risk, color = "Pf"), size=1.3)+
  geom_ribbon(data=delta_risk_Pf_act, aes(ymin=LB_CI, ymax=UB_CI, x=time), fill = "orange", alpha=0.1)+
  # geom_vline(xintercept = 365, color = "black", lwd = 1, alpha = 0.5)+
  # geom_vline(xintercept = 730, color = "black", lwd = 1, alpha = 0.5)+
  ylim(0,1.0)+
  scale_x_continuous(limits = c(0, 36), breaks = c(0, 6, 12, 18, 24, 30, 36))+
  labs(title="", 
       x = "Months from Baseline", 
       y = "Cumulative Incidence", 
       color = "Species")+
  scale_color_manual(values = colors)+
  theme_set(
    theme_classic(base_size = 15))

plot(p1_Act)


#####TOTAL STUDY POPULATION: ZOOMED INSERT FOR PLOT WITH MULTIPLE CURVES
p2_Tot<-ggplot() + 
  geom_step(data=delta_risk_Po_tot, aes(x = time, y = risk, color = "Po"), size=0.8)+
  geom_ribbon(data=delta_risk_Po_tot, aes(ymin=LB_CI, ymax=UB_CI, x=time),  fill = "red", alpha=0.1)+
  geom_step(data=delta_risk_Pm_tot, aes(x = time, y = risk, color = "Pm"), size=0.8)+
  geom_ribbon(data=delta_risk_Pm_tot, aes(ymin=LB_CI, ymax=UB_CI, x=time), fill = "blue", alpha=0.1)+
  #   geom_vline(xintercept = 365, color = "black", lwd = 1, alpha = 0.5)+
  #   geom_vline(xintercept = 730, color = "black", lwd = 1, alpha = 0.5)+
  ylim(0,0.25)+
  # labs(title="Pm and Po infection incidence, Total Population", fontsize = 4)+
  theme_clean()+
  scale_x_continuous(limits = c(0, 36), breaks = c(0, 6, 12, 18, 24, 30, 36))+
  labs(x = "Months", 
       y = "Cum. Incid.")+
  scale_color_manual(values = c("Pm"= "blue", "Po"="red"), guide="none")

plot(p2_Tot)


#######NOW CREATING A ZOOMED INSERT

p1_Tot+
  annotation_custom(ggplotGrob(p2_Tot), xmin = -1.0, xmax=10.8, ymin=0.70, ymax=1.0)+
  geom_rect(aes(xmin = -1.0, xmax=10.8, ymin=0.70, ymax = 1.0), color = 'black', linetype = "solid", alpha = 0 )



####All species: CI Curves with 95% CIs 
jpeg("../Analysis/Figures/Draft Figures_Manuscript/KMRiskPlot_TotalPop_AllSpecies_Zoomed.jpg", quality = "100", 
     res = 600, bg = "transparent",  height=5, width=6, units = "in" )




##########################################################
# CRUDE RISK PLOTS ACROSS STRATA OF PREDICTOR VARS  
##########################################################

#####
#Age Categories (<5, 5-15, >15)
#####

#Pm Crude Risk among Total Pop (active + passive) stratified by age at baseline
km <- survfit(Surv(Time_all_end_mon, Pm_Event_All) ~ age_cat2, data=Pm_Risk_Tot, stype=2, id=Subject_ID)
delta_risk_Pm_Tot1a <- data.frame(time = summary(km)$time, 
                                  surv = summary(km)$surv, 
                                  risk = 1 - summary(km)$surv,
                                  LB_CI = 1- summary(km)$upper,
                                  UB_CI = 1- summary(km)$lower, 
                                  agecat = summary(km)$strata)  
  
  #Plotting Pm survival by age categories at baseline
  delta_risk_Pm_Tot1a <- survfit(Surv(Time_all_end_mon, Pm_Event_All)~ age_cat2, data=Pm_Risk_Tot)
  ggsurvplot(delta_risk_Pm_Tot1a, conf.int=TRUE, pval=TRUE, risk.table=TRUE,)
  

#Po Crude Risk among Total Pop (active + passive) stratified by age at baseline
  km <- survfit(Surv(Time_all_end_mon, Po_Event_All) ~ age_cat2, data=Po_Risk_Tot, stype=2, id=Subject_ID)
  delta_risk_Po_Tot1a <- data.frame(time = summary(km)$time, 
                                    surv = summary(km)$surv, 
                                    risk = 1 - summary(km)$surv,
                                    LB_CI = 1- summary(km)$upper,
                                    UB_CI = 1- summary(km)$lower, 
                                    agecat = summary(km)$strata)  
  
  #Plotting Po survival by age categories at baseline
  delta_risk_Po_Tot1a <- survfit(Surv(Time_all_end_mon, Po_Event_All)~ age_cat2, data=Po_Risk_Tot)
  ggsurvplot(delta_risk_Po_Tot1a, conf.int=TRUE, pval=TRUE, risk.table=TRUE,)
  
  
#Pf Crude Risk among Total Pop (active + passive) stratified by age at baseline
  km <- survfit(Surv(Time_all_end_mon, Pf_Event_All) ~ age_cat2, data=Pf_Risk_Tot, stype=2, id=Subject_ID)
  delta_risk_Pf_Tot1a <- data.frame(time = summary(km)$time, 
                                    surv = summary(km)$surv, 
                                    risk = 1 - summary(km)$surv,
                                    LB_CI = 1- summary(km)$upper,
                                    UB_CI = 1- summary(km)$lower, 
                                    agecat = summary(km)$strata)  
  
  #Plotting Pf survival by age categories at baseline
  delta_risk_Pf_Tot1a <- survfit(Surv(Time_all_end_mon, Pf_Event_All)~ age_cat2, data=Pf_Risk_Tot)
  ggsurvplot(delta_risk_Pf_Tot1a, conf.int=TRUE, pval=TRUE, risk.table=TRUE,)
  
  
  
  #####
  #Sex  Gender (1=F; 0=M)
  #####
  
  #Pm Crude Risk among Total Pop (active + passive) stratified by sex
  km <- survfit(Surv(Time_all_end_mon, Pm_Event_All) ~ gender, data=Pm_Risk_Tot, stype=2, id=Subject_ID)
  delta_risk_Pm_Tot1a <- data.frame(time = summary(km)$time, 
                                    surv = summary(km)$surv, 
                                    risk = 1 - summary(km)$surv,
                                    LB_CI = 1- summary(km)$upper,
                                    UB_CI = 1- summary(km)$lower, 
                                    gender = summary(km)$strata)  
  
  #Plotting Pm survival by sex
  delta_risk_Pm_Tot1a <- survfit(Surv(Time_all_end_mon, Pm_Event_All)~ gender, data=Pm_Risk_Tot)
  ggsurvplot(delta_risk_Pm_Tot1a, conf.int=TRUE, pval=TRUE, risk.table=TRUE,)
  
  
  
  #Po Crude Risk among Total Pop (active + passive) stratified by sex
  km <- survfit(Surv(Time_all_end_mon, Po_Event_All) ~ gender, data=Po_Risk_Tot, stype=2, id=Subject_ID)
  delta_risk_Po_Tot1a <- data.frame(time = summary(km)$time, 
                                    surv = summary(km)$surv, 
                                    risk = 1 - summary(km)$surv,
                                    LB_CI = 1- summary(km)$upper,
                                    UB_CI = 1- summary(km)$lower, 
                                    gender = summary(km)$strata)  
  
  #Plotting Po survival by sex
  delta_risk_Po_Tot1a <- survfit(Surv(Time_all_end_mon, Po_Event_All)~ gender, data=Po_Risk_Tot)
  ggsurvplot(delta_risk_Po_Tot1a, conf.int=TRUE, pval=TRUE, risk.table=TRUE,)
  

  #Pf Crude Risk among Total Pop (active + passive) stratified by sex
  km <- survfit(Surv(Time_all_end_mon, Pf_Event_All) ~ gender, data=Pf_Risk_Tot, stype=2, id=Subject_ID)
  delta_risk_Pf_Tot1a <- data.frame(time = summary(km)$time, 
                                    surv = summary(km)$surv, 
                                    risk = 1 - summary(km)$surv,
                                    LB_CI = 1- summary(km)$upper,
                                    UB_CI = 1- summary(km)$lower, 
                                    gender = summary(km)$strata)  
  
  #Plotting Pf survival by sex
  delta_risk_Pf_Tot1a <- survfit(Surv(Time_all_end_mon, Pf_Event_All)~ gender, data=Pf_Risk_Tot)
  ggsurvplot(delta_risk_Pf_Tot1a, conf.int=TRUE, pval=TRUE, risk.table=TRUE,)
  
  
  
  
  
  #####
  #Wealth Quintiles (wtd quantile scores)
  #####
  
  #Pm Crude Risk among Total Pop (active + passive) stratified by wealth
  km <- survfit(Surv(Time_all_end_mon, Pm_Event_All) ~ WealthCat, data=Pm_Risk_Tot, stype=2, id=Subject_ID)
  delta_risk_Pm_Tot1a <- data.frame(time = summary(km)$time, 
                                    surv = summary(km)$surv, 
                                    risk = 1 - summary(km)$surv,
                                    LB_CI = 1- summary(km)$upper,
                                    UB_CI = 1- summary(km)$lower, 
                                    wealth = summary(km)$strata)  
  
  #Plotting Pm survival by wealth
  delta_risk_Pm_Tot1a <- survfit(Surv(Time_all_end_mon, Pm_Event_All)~ WealthCat, data=Pm_Risk_Tot)
  ggsurvplot(delta_risk_Pm_Tot1a, conf.int=TRUE, pval=TRUE, risk.table=TRUE,)
  
  
  
  #Po Crude Risk among Total Pop (active + passive) stratified by wealth
  km <- survfit(Surv(Time_all_end_mon, Po_Event_All) ~ WealthCat, data=Po_Risk_Tot, stype=2, id=Subject_ID)
  delta_risk_Po_Tot1a <- data.frame(time = summary(km)$time, 
                                    surv = summary(km)$surv, 
                                    risk = 1 - summary(km)$surv,
                                    LB_CI = 1- summary(km)$upper,
                                    UB_CI = 1- summary(km)$lower, 
                                    wealth = summary(km)$strata)  
  
  #Plotting Po survival by wealth
  delta_risk_Po_Tot1a <- survfit(Surv(Time_all_end_mon, Po_Event_All)~ WealthCat, data=Po_Risk_Tot)
  ggsurvplot(delta_risk_Po_Tot1a, conf.int=TRUE, pval=TRUE, risk.table=TRUE,)
  
  
  
  
  #Pf Crude Risk among Total Pop (active + passive) stratified by wealth
  km <- survfit(Surv(Time_all_end_mon, Pf_Event_All) ~ WealthCat, data=Pf_Risk_Tot, stype=2, id=Subject_ID)
  delta_risk_Pf_Tot1a <- data.frame(time = summary(km)$time, 
                                    surv = summary(km)$surv, 
                                    risk = 1 - summary(km)$surv,
                                    LB_CI = 1- summary(km)$upper,
                                    UB_CI = 1- summary(km)$lower, 
                                    wealth = summary(km)$strata)  
  
  #Plotting Pf survival by wealth
  delta_risk_Pf_Tot1a <- survfit(Surv(Time_all_end_mon, Pf_Event_All)~ WealthCat, data=Pf_Risk_Tot)
  ggsurvplot(delta_risk_Pf_Tot1a, conf.int=TRUE, pval=TRUE, risk.table=TRUE,)
  
  
  
  
  #####
  #Health Area (urbanicity)
  #####
  
  #Pm Crude Risk among Total Pop (active + passive) stratified by urbanicity
  km <- survfit(Surv(Time_all_end_mon, Pm_Event_All) ~ healtharea, data=Pm_Risk_Tot, stype=2, id=Subject_ID)
  delta_risk_Pm_Tot1a <- data.frame(time = summary(km)$time, 
                                    surv = summary(km)$surv, 
                                    risk = 1 - summary(km)$surv,
                                    LB_CI = 1- summary(km)$upper,
                                    UB_CI = 1- summary(km)$lower, 
                                    healtharea = summary(km)$strata)  
  
  #Plotting Pm survival by urbanicity
  delta_risk_Pm_Tot1a <- survfit(Surv(Time_all_end_mon, Pm_Event_All)~ healtharea, data=Pm_Risk_Tot)
  ggsurvplot(delta_risk_Pm_Tot1a,
             pval = TRUE,
             conf.int = TRUE,
             #surv.median.line = c("hv"),
             censor = TRUE, 
             # Add risk table
             risk.table = TRUE,
             tables.height = 0.15,
             cumevents = TRUE, 
             cumcensor = TRUE, )
  
  
  
  #Po Crude Risk among Total Pop (active + passive) stratified by urbanicity
  km <- survfit(Surv(Time_all_end_mon, Po_Event_All) ~ healtharea, data=Po_Risk_Tot, stype=2, id=Subject_ID)
  delta_risk_Po_Tot1a <- data.frame(time = summary(km)$time, 
                                    surv = summary(km)$surv, 
                                    risk = 1 - summary(km)$surv,
                                    LB_CI = 1- summary(km)$upper,
                                    UB_CI = 1- summary(km)$lower, 
                                    healtharea = summary(km)$strata)  
  
  #Plotting Po survival by urbanicity
  delta_risk_Po_Tot1a <- survfit(Surv(Time_all_end_mon, Po_Event_All)~ healtharea, data=Po_Risk_Tot)
  ggsurvplot(delta_risk_Po_Tot1a,
             pval = TRUE,
             conf.int = TRUE,
             #surv.median.line = c("hv"),
             censor = TRUE, 
             # Add risk table
             risk.table = TRUE,
             tables.height = 0.15,
             cumevents = TRUE, 
             cumcensor = TRUE, )

  
  
  
  
  #Pf Crude Risk among Total Pop (active + passive) stratified by urbanicity
  km <- survfit(Surv(Time_all_end_mon, Pf_Event_All) ~ healtharea, data=Pf_Risk_Tot, stype=2, id=Subject_ID)
  delta_risk_Pf_Tot1a <- data.frame(time = summary(km)$time, 
                                    surv = summary(km)$surv, 
                                    risk = 1 - summary(km)$surv,
                                    LB_CI = 1- summary(km)$upper,
                                    UB_CI = 1- summary(km)$lower, 
                                    healtharea = summary(km)$strata)  
  
  #Plotting Pf survival by urbanicity
  delta_risk_Pf_Tot1a <- survfit(Surv(Time_all_end_mon, Pf_Event_All)~ healtharea, data=Pf_Risk_Tot)
  ggsurvplot(delta_risk_Pf_Tot1a, conf.int=TRUE, pval=TRUE, risk.table=TRUE,)
  
  
  
  
  
  #####
  #Mixed vs. Mono Infection 
  #####
  
  #Pm Crude Risk among Total Pop (active + passive) stratified by species type
  km <- survfit(Surv(Time_all_end_mon, Pm_Event_All) ~ Pm_Species, data=Pm_Risk_Tot, stype=2, id=Subject_ID)
  delta_risk_Pm_Tot1a <- data.frame(time = summary(km)$time, 
                                    surv = summary(km)$surv, 
                                    risk = 1 - summary(km)$surv,
                                    LB_CI = 1- summary(km)$upper,
                                    UB_CI = 1- summary(km)$lower, 
                                    species = summary(km)$strata)  
  
  #Plotting Pm survival by urbanicity
  delta_risk_Pm_Tot1a <- survfit(Surv(Time_all_end_mon, Pm_Event_All)~ Pm_Species, data=Pm_Risk_Tot)
  ggsurvplot(delta_risk_Pm_Tot1a,
             pval = TRUE,
             conf.int = TRUE,
             #surv.median.line = c("hv"),
             censor = TRUE, 
             # Add risk table
             risk.table = TRUE,
             tables.height = 0.15,
             cumevents = TRUE, 
             cumcensor = TRUE, )
  
  
  
  #Po Crude Risk among Total Pop (active + passive) stratified by species type
  km <- survfit(Surv(Time_all_end_mon, Po_Event_All) ~ Po_Species, data=Po_Risk_Tot, stype=2, id=Subject_ID)
  delta_risk_Po_Tot1a <- data.frame(time = summary(km)$time, 
                                    surv = summary(km)$surv, 
                                    risk = 1 - summary(km)$surv,
                                    LB_CI = 1- summary(km)$upper,
                                    UB_CI = 1- summary(km)$lower, 
                                    species = summary(km)$strata)  
  
  #Plotting Po survival by species type
  delta_risk_Po_Tot1a <- survfit(Surv(Time_all_end_mon, Po_Event_All)~ Po_Species, data=Po_Risk_Tot)
  ggsurvplot(delta_risk_Po_Tot1a,
             pval = TRUE,
             conf.int = TRUE,
             #surv.median.line = c("hv"),
             censor = TRUE, 
             # Add risk table
             risk.table = TRUE,
             tables.height = 0.15,
             cumevents = TRUE, 
             cumcensor = TRUE, )
  
  
  
  #Pf Crude Risk among Total Pop (active + passive) stratified by species type
  km <- survfit(Surv(Time_all_end_mon, Pf_Event_All) ~ Pf_Species, data=Pf_Risk_Tot, stype=2, id=Subject_ID)
  delta_risk_Pf_Tot1a <- data.frame(time = summary(km)$time, 
                                    surv = summary(km)$surv, 
                                    risk = 1 - summary(km)$surv,
                                    LB_CI = 1- summary(km)$upper,
                                    UB_CI = 1- summary(km)$lower, 
                                    species = summary(km)$strata)  
  
  #Plotting Pf survival by species type
  delta_risk_Pf_Tot1a <- survfit(Surv(Time_all_end_mon, Pf_Event_All)~ Pf_Species, data=Pf_Risk_Tot)
  ggsurvplot(delta_risk_Pf_Tot1a, conf.int=TRUE, pval=TRUE, risk.table=TRUE,)
  
  
  
  
  ##########################################################
  # Cumulative Incidence Curves - Final - OF PM, PO, AND PF INFECTION    
  ##########################################################
  
  #Table - total cumulative incidence for all species 
  survfit2(Surv(Time_all_end_mon, Pf_Event_All_CE) ~ 1, data = Pf_Risk_Tot) %>%
  ggsurvfit()+
    labs(
      x="Months from Baseline", 
      y="Overall Prob. Uninfected)", 
    )+
    add_confidence_interval()+
    add_risktable()
    
  
  Pf_Risk_Tot <- Pf_Risk_Tot %>% mutate(Pf_Event_All_CE = as.factor(recode(Pf_Event_All_CE, `0` = 0, `1` = 1, `2` = 2)))
  Pm_Risk_Tot <- Pm_Risk_Tot %>% mutate(Pm_Event_All_CE = as.factor(recode(Pm_Event_All_CE, `0` = 0, `1` = 1, `2` = 2)))
  Po_Risk_Tot <- Po_Risk_Tot %>% mutate(Po_Event_All_CE = as.factor(recode(Po_Event_All_CE, `0` = 0, `1` = 1, `2` = 2)))
  
  cuminc(Surv(Time_all_end_mon, Pf_Event_All_CE) ~ 1, data = Pf_Risk_Tot) %>% 
   ggcuminc()+
    labs(
      x = "Months from Baseline"
    ) + 
    add_confidence_interval() +
    add_risktable()
  
  
  
  cuminc(Surv(Time_all_end_mon, Pf_Event_All_CE) ~ age_cat2, data = Pf_Risk_Tot) %>% 
    ggcuminc() + 
    labs(
      x = "Months from Baseline") + 
    add_confidence_interval() +
    add_risktable()
  
  
  
  #Table - total cumulative incidence for all species 
  survfit2(Surv(Time_all_end_mon, Pf_Event_All_CE) ~ 1, data = Pf_Risk_Tot, stype=2, id=Subject_ID) 
    ggcuminc(outcome = "Plasmodium Infection")+
    add_confidence_interval() +
    add_risktable()
  
    
  ##Combine all species together for a 3-species lot of cumulative incidence in survey and total population 
    fit_Surv <- list(falciparum=Pf_Act_Surv, ovale=Po_Act_Surv, malariae=Pm_Act_Surv)
    ggcuminc(fit_Surv, combine = TRUE, 
               legend.title = "Survey-based Pop.",
               #  legend.labs = c(""),
               break.x.by = 6, 
               # Add p-value and tervals
               # pval = TRUE,
               conf.int = TRUE,
               #surv.median.line = c("hv"),
               censor = TRUE, 
               # Add risk table
               risk.table = TRUE,
               tables.height = 0.17,
               cumevents = TRUE, 
               # cumcensor = TRUE, 
               #cumcensor.title = "Cumulative number censored",
               cumevent.title = "Cumulative infections",
               #risk.table.fontsize = 2.5,
               # font.x=2.5,
               tables.theme = theme_cleantable(),
               xlab = "Months from Baseline", 
               ylab = ("Prob. un-infected"), 
               xlim = c(0,24))
  
    cuminc(Surv(Time_all_end_mon, Pf_Event_All_CE) ~ 1, data = Pf_Risk_Tot) %>% 
      ggcuminc() + 
      labs(
        x = "Months from Baseline"
      ) + 
      add_confidence_interval() +
      add_risktable()
  
  
  
  
  

##########################################################
# PREVALENCE OF PM, PO, AND PF INFECTION    
##########################################################

table(AD_Active_nonmissvisits$Household, AD_Active_nonmissvisits$Pm)
table(AD_Active_nonmissvisits$Household, AD_Active_nonmissvisits$Po)
  
  
Prev_Data<-AD_Active_nonmissvisits%>% 
                   group_by(Visit)%>% 
                   dplyr::summarise(Prevalence_Pm = sum(Pm)/n())


##Prevalence of Pm, Po, and Pf -- Num Subjects with infection 
addmargins(table(AD_Active_nonmissvisits$Pm, AD_Active_nonmissvisits$Visit))
addmargins(table(AD_Active_nonmissvisits$Po, AD_Active_nonmissvisits$Visit))
addmargins(table(AD_Active_nonmissvisits$Pf, AD_Active_nonmissvisits$Visit))


#calculate 95% confidence intervals for binomial proportion (prevalences) at each FU visit 

##Pm Prev at each FU visit and in total
prop.test(x=47, n=1565, conf.level=.95, correct=FALSE)
prop.test(x=35, n=1443, conf.level=.95, correct=FALSE)
prop.test(x=56, n=1356, conf.level=.95, correct=FALSE)
prop.test(x=48, n=1295, conf.level=.95, correct=FALSE)
prop.test(x=186, n=5659, conf.level=.95, correct=FALSE)


##Po Prev at each FU visit and in total
prop.test(x=6, n=1565, conf.level=.95, correct=FALSE)
prop.test(x=27, n=1443, conf.level=.95, correct=FALSE)
prop.test(x=27, n=1356, conf.level=.95, correct=FALSE)
prop.test(x=18, n=1295, conf.level=.95, correct=FALSE)
prop.test(x=78, n=5659, conf.level=.95, correct=FALSE)


##Pf Prev at each FU visit and in total
prop.test(x=484, n=1565, conf.level=.95, correct=FALSE)
prop.test(x=512, n=1442, conf.level=.95, correct=FALSE)
prop.test(x=538, n=1358, conf.level=.95, correct=FALSE)
prop.test(x=442, n=1295, conf.level=.95, correct=FALSE)
prop.test(x=1976, n=5660, conf.level=.95, correct=FALSE)


##Cross-tabulation for Infections by Village by Visit 
addmargins(table(AD_Active_nonmissvisits$Pm, AD_Active_nonmissvisits$Visit, AD_Active_nonmissvisits$Village ))
prop.table(AD_Active_nonmissvisits$Pm, AD_Active_nonmissvisits$Visit, AD_Active_nonmissvisits$Village )


##########################################################
# ASSESSING FACTORS ASSOCIATED WITH PREVALENCE OF PM, PO, AND PF INFECTION    
##########################################################

## Method:  Modeling as Binomial GEE with Identity Link (for prev. diff), accounting for correlations between repeated testing across longitudinal follow-up visits within subjects. 
#           Using robust standard errors. 
#           Used Exch. correlation matrix

## 
## Relevant Sources: 1)  https://data.library.virginia.edu/getting-started-with-generalized-estimating-equations/


#### Total Population PDs ####

# N=1,595 subjects / 5,689 visits (BL, FU1, Fu2, Fu3) 
# Dataset: AD_Total

#Pm Infection (Any):  N=187 infections across 5688 non-missing Total visits   (572 FU visits missing as expected)
#Po Infection (Any):  N=78 infections across 5688 non-missing Total visits   (572 FU visits missing as expected)
#Pf Infection (Any):  N=1976 infections across 5688 non-missing Total visits   (572 FU visits missing as expected)


##Soring data by Subject_ID and then by Visit number to ensure it's formatted correctly for the clustering analysis. 
AD_Active <- AD_Active[order(AD_Active$Subject_ID, AD_Active$Visit, AD_Active$Visit_date),]


##Crude prevalence (accounting for repeated testing;  not accounting for household clustering)

##Pm

##PD = Prevalence Difference 
CrudePrev_GEE_Pm_Active_Pd<-geeglm(Pm ~ 1, 
                                   data = AD_Active, 
                                   id = Subject_ID, 
                                   family = binomial(link="identity"),
                                   corstr = "ex")

summary(CrudePrev_GEE_Pm_Active_Pd)
confint(fit) # 95% CI for the coefficients

##PR = Prevalence Ratio
CrudePrev_GEE_Pm_Active_PR<-geeglm(Pm ~ 1, 
                                   data = AD_Active, 
                                   id = Subject_ID, 
                                   family = binomial(link="log"),
                                   corstr = "ex")

summary(CrudePrev_GEE_Pm_Active_PR)


### Checking Mixed model accounting for within-subject and household clustering
CrudePrev_ME_Pm <- lmer(Pm ~ 1 + (1 | Subject_ID) + (1 | Household)  ,
                        data = AD_Active,
                        family = binomial(link="identity"),
                        REML = FALSE)
summary(CrudePrev_ME_Pm)

CrudePrev_ME_Pm <- lmer(Pm ~ 1 + (1 | Subject_ID) + (1 | Household)  ,
                        data = AD_Active,
                        family = binomial(link="log"),
                        REML = FALSE)
summary(CrudePrev_ME_Pm)



##Po
CrudePrev_GEE_Po<-geeglm(Po ~ 1, 
                         data = AD_Active, 
                         id = Subject_ID, 
                         family = binomial(link="identity"),
                         corstr = "ex")
summary(CrudePrev_GEE_Po)
confint(fit) # 95% CI for the coefficients



### Mixed model accounting for within-subject and household clustering
CrudePrev_ME_Po <- lmer(Po ~ 1 + (1 | Subject_ID) + (1 | Household),
                        data = AD_Active,
                        family = binomial(link="logit"),
                        REML = FALSE)
summary(CrudePrev_ME_Po)



##Pf
CrudePrev_GEE_Pf<-geeglm(Pf ~ 1, 
                         data = AD_Active, 
                         id = Subject_ID, 
                         family = binomial(link="identity"),
                         corstr = "ex")
summary(CrudePrev_GEE_Pf)
confint(fit) # 95% CI for the coefficients


### Mixed model accounting forwithin-subject and household clutering
CrudePrev_ME_Pf <- lmer(Pf ~ 1 + (1 | Subject_ID) + (1 | Household),
                        data = AD_Active,
                        REML = FALSE)
summary(CrudePrev_ME_Pf)






##
##AGE CATEGORY (Age >=15 years = Ref)
##
AD_Active_AgeNoMiss<-AD_Active%>%filter(!is.na(AgeCat_Visit))
addmargins(table(AD_Active_AgeNoMiss$AgeCat_Visit, AD_Active_AgeNoMiss$Pm))


#Pm#
Age_GEE_Pm_PD<-geeglm(Pm ~ relevel(as.factor(AgeCat_Visit), ref='3'), 
                      data = AD_Active_AgeNoMiss, 
                      id = Subject_ID, 
                      family = binomial(link="identity"),
                      corstr = "ex")
summary(Age_GEE_Pm_PD)

Age_GEE_Pm_PR<-geeglm(Pm ~ relevel(as.factor(AgeCat_Visit), ref='3'), 
                      data = AD_Active_AgeNoMiss, 
                      id = Subject_ID, 
                      family = binomial(link="log"),
                      corstr = "ex")
summary(Age_GEE_Pm_PR)


#Age_GEE_Pm<-geeglm(Pm ~ agecat_LT5 + agecat_5_14, 
#                   data = AD_Active_AgeNoMiss, 
#                   id = Subject_ID, 
#                   family = binomial(link="identity"),
#                   corstr = "ex")
#summary(Age_GEE_Pm)


#Po#

Age_GEE_Po_PD<-geeglm(Po ~ relevel(as.factor(AgeCat_Visit), ref='3'), 
                      data = AD_Active_AgeNoMiss, 
                      id = Subject_ID, 
                      family = binomial(link="identity"),
                      corstr = "ex")
summary(Age_GEE_Po_PD)

Age_GEE_Po_PR<-geeglm(Po ~ relevel(as.factor(AgeCat_Visit), ref='3'), 
                      data = AD_Active_AgeNoMiss, 
                      id = Subject_ID, 
                      family = binomial(link="log"),
                      corstr = "ex")
summary(Age_GEE_Po_PR)


#Age_GEE_Po<-geeglm(Po ~ agecat_LT5 + agecat_5_14, 
#                   data = AD_Active_AgeNoMiss, 
#                   id = Subject_ID, 
#                   family = binomial(link="identity"),
#                   corstr = "ex")
#summary(Age_GEE_Po)


#Pf#
Age_GEE_Pf_PD<-geeglm(Pf ~ relevel(as.factor(AgeCat_Visit), ref='3'), 
                      data = AD_Active_AgeNoMiss, 
                      id = Subject_ID, 
                      family = binomial(link="identity"),
                      corstr = "ex")
summary(Age_GEE_Pf_PD)

Age_GEE_Pf_PR<-geeglm(Pf ~ relevel(as.factor(AgeCat_Visit), ref='3'), 
                      data = AD_Active_AgeNoMiss, 
                      id = Subject_ID, 
                      family = binomial(link="log"),
                      corstr = "ex")
summary(Age_GEE_Pf_PR)


#Age_GEE_Pf<-geeglm(Pf ~ agecat_LT5 + agecat_5_14, 
#                   data = AD_Active_AgeNoMiss, 
#                   id = Subject_ID, 
#                   family = binomial(link="identity"),
#                   corstr = "ex")
#summary(Age_GEE_Pf)

##Outputing GEE Results: Estimate + 95% CI from Robust Sandwich Estimator
#Prevalence Differences 
Coeff_Active_Age_Pm_PD<-tidy(Age_GEE_Pm_PD, exponentiate=FALSE, conf.int=TRUE)%>%dplyr::mutate(Ifxn="Pm")%>%filter(!term=="(Intercept)")
Coeff_Active_Age_Po_PD<-tidy(Age_GEE_Po_PD, exponentiate=FALSE, conf.int=TRUE)%>%dplyr::mutate(Ifxn="Po")%>%filter(!term=="(Intercept)")
Coeff_Active_Age_Pf_PD<-tidy(Age_GEE_Pf_PD, exponentiate=FALSE, conf.int=TRUE)%>%dplyr::mutate(Ifxn="Pf")%>%filter(!term=="(Intercept)")

#Prevalence Ratios 
Coeff_Active_Age_Pm_PR<-tidy(Age_GEE_Pm_PR, exponentiate=TRUE, conf.int=TRUE)%>%dplyr::mutate(Ifxn="Pm")%>%filter(!term=="(Intercept)")
Coeff_Active_Age_Po_PR<-tidy(Age_GEE_Po_PR, exponentiate=TRUE, conf.int=TRUE)%>%dplyr::mutate(Ifxn="Po")%>%filter(!term=="(Intercept)")
Coeff_Active_Age_Pf_PR<-tidy(Age_GEE_Pf_PR, exponentiate=TRUE, conf.int=TRUE)%>%dplyr::mutate(Ifxn="Pf")%>%filter(!term=="(Intercept)")



##
##SEX CATEGORY (Male = Ref)
##
AD_Active_SexNoMiss<-AD_Active%>%filter(!is.na(Sex))

#Pm#
Sex_GEE_Pm_PD<-geeglm(Pm ~ Sex, 
                      data = AD_Active_SexNoMiss, 
                      id = Subject_ID, 
                      family = binomial(link="identity"),
                      corstr = "ex")
summary(Sex_GEE_Pm_PD)

Sex_GEE_Pm_PR<-geeglm(Pm ~ Sex, 
                      data = AD_Active_SexNoMiss, 
                      id = Subject_ID, 
                      family = binomial(link="log"),
                      corstr = "ex")
summary(Sex_GEE_Pm_PR)

#Po#
Sex_GEE_Po_PD<-geeglm(Po ~ Sex, 
                      data = AD_Active_SexNoMiss, 
                      id = Subject_ID, 
                      family = binomial(link="identity"),
                      corstr = "ex")
summary(Sex_GEE_Po_PD)

Sex_GEE_Po_PR<-geeglm(Po ~ Sex, 
                      data = AD_Active_SexNoMiss, 
                      id = Subject_ID, 
                      family = binomial(link="log"),
                      corstr = "ex")
summary(Sex_GEE_Po_PR)

#Pf#
Sex_GEE_Pf_PD<-geeglm(Pf ~ Sex, 
                      data = AD_Active_SexNoMiss, 
                      id = Subject_ID, 
                      family = binomial(link="identity"),
                      corstr = "ex")
summary(Sex_GEE_Pf_PD)

Sex_GEE_Pf_PR<-geeglm(Pf ~ Sex, 
                      data = AD_Active_SexNoMiss, 
                      id = Subject_ID, 
                      family = binomial(link="log"),
                      corstr = "ex")
summary(Sex_GEE_Pf_PR)



##Outputing GEE Results: Estimate + 95% CI from Robust Sandwich Estimator

#Prevalence Differences 
Coeff_Active_Sex_Pm_PD<-tidy(Sex_GEE_Pm_PD, exponentiate=FALSE, conf.int=TRUE)%>%dplyr::mutate(Ifxn="Pm")%>%filter(!term=="(Intercept)")
Coeff_Active_Sex_Po_PD<-tidy(Sex_GEE_Po_PD, exponentiate=FALSE, conf.int=TRUE)%>%dplyr::mutate(Ifxn="Po")%>%filter(!term=="(Intercept)")
Coeff_Active_Sex_Pf_PD<-tidy(Sex_GEE_Pf_PD, exponentiate=FALSE, conf.int=TRUE)%>%dplyr::mutate(Ifxn="Pf")%>%filter(!term=="(Intercept)")

#Prevalence Ratios
Coeff_Active_Sex_Pm_PR<-tidy(Sex_GEE_Pm_PR, exponentiate=TRUE, conf.int=TRUE)%>%dplyr::mutate(Ifxn="Pm")%>%filter(!term=="(Intercept)")
Coeff_Active_Sex_Po_PR<-tidy(Sex_GEE_Po_PR, exponentiate=TRUE, conf.int=TRUE)%>%dplyr::mutate(Ifxn="Po")%>%filter(!term=="(Intercept)")
Coeff_Active_Sex_Pf_PR<-tidy(Sex_GEE_Pf_PR, exponentiate=TRUE, conf.int=TRUE)%>%dplyr::mutate(Ifxn="Pf")%>%filter(!term=="(Intercept)")




##
##HEALTH AREA CATEGORY (1 = LINGWALA = REF CAT (urban);  Bu = 2 = Ref ; (rural);  3 = KIMPOKO  (peri-urban))
##
AD_Active_HealthAreaNoMiss<-AD_Active%>%filter(!is.na(HealthArea))

#Pm#
HealthArea_GEE_Pm_PD<-geeglm(Pm ~ as.factor(HealthArea), 
                             data = AD_Active_HealthAreaNoMiss, 
                             id = Subject_ID, 
                             family = binomial(link="identity"),
                             corstr = "ex")
summary(HealthArea_GEE_Pm_PD)

HealthArea_GEE_Pm_PR<-geeglm(Pm ~ as.factor(HealthArea), 
                             data = AD_Active_HealthAreaNoMiss, 
                             id = Subject_ID, 
                             family = binomial(link="log"),
                             corstr = "ex")
summary(HealthArea_GEE_Pm_PR)

#Po#
HealthArea_GEE_Po_PD<-geeglm(Po ~ as.factor(HealthArea),  
                             data = AD_Active_HealthAreaNoMiss, 
                             id = Subject_ID, 
                             family = binomial(link="identity"),
                             corstr = "ex")
summary(HealthArea_GEE_Po_PD)

HealthArea_GEE_Po_PR<-geeglm(Po ~ as.factor(HealthArea),  
                             data = AD_Active_HealthAreaNoMiss, 
                             id = Subject_ID, 
                             family = binomial(link="log"),
                             corstr = "ex")
summary(HealthArea_GEE_Po_PR)

#Pf#
HealthArea_GEE_Pf_PD<-geeglm(Pf ~ as.factor(HealthArea),  
                             data = AD_Active_HealthAreaNoMiss, 
                             id = Subject_ID, 
                             family = binomial(link="identity"),
                             corstr = "ex")
summary(HealthArea_GEE_Pf_PD)

HealthArea_GEE_Pf_PR<-geeglm(Pf ~ as.factor(HealthArea),  
                             data = AD_Active_HealthAreaNoMiss, 
                             id = Subject_ID, 
                             family = binomial(link="log"),
                             corstr = "ex")
summary(HealthArea_GEE_Pf_PR)


##Outputing GEE Results: Estimate + 95% CI from Robust Sandwich Estimator
#Prevalence Differences 
Coeff_Active_HealthArea_Pm_PD<-tidy(HealthArea_GEE_Pm_PD, exponentiate=FALSE, conf.int=TRUE)%>%dplyr::mutate(Ifxn="Pm")%>%filter(!term=="(Intercept)")
Coeff_Active_HealthArea_Po_PD<-tidy(HealthArea_GEE_Po_PD, exponentiate=FALSE, conf.int=TRUE)%>%dplyr::mutate(Ifxn="Po")%>%filter(!term=="(Intercept)")
Coeff_Active_HealthArea_Pf_PD<-tidy(HealthArea_GEE_Pf_PD, exponentiate=FALSE, conf.int=TRUE)%>%dplyr::mutate(Ifxn="Pf")%>%filter(!term=="(Intercept)")

#Prevalence Ratios
Coeff_Active_HealthArea_Pm_PR<-tidy(HealthArea_GEE_Pm_PR, exponentiate=TRUE, conf.int=TRUE)%>%dplyr::mutate(Ifxn="Pm")%>%filter(!term=="(Intercept)")
Coeff_Active_HealthArea_Po_PR<-tidy(HealthArea_GEE_Po_PR, exponentiate=TRUE, conf.int=TRUE)%>%dplyr::mutate(Ifxn="Po")%>%filter(!term=="(Intercept)")
Coeff_Active_HealthArea_Pf_PR<-tidy(HealthArea_GEE_Pf_PR, exponentiate=TRUE, conf.int=TRUE)%>%dplyr::mutate(Ifxn="Pf")%>%filter(!term=="(Intercept)")





##
##WEALTH CATEGORY (Average Wealth [3] = Ref)
##

AD_Active_WealthCatNoMiss<-AD_Active%>%filter(!is.na(WealthCat))
## comparing poor vs. average, and rich vs. average  (average = 1; poor = 2; rich = 3)

#Pm#
Wealth_GEE_Pm_PD<-geeglm(Pm ~ as.factor(WealthCat), 
                         data = AD_Active_WealthCatNoMiss, 
                         id = Subject_ID, 
                         family = binomial(link="identity"),
                         corstr = "ex")
summary(Wealth_GEE_Pm_PD)

Wealth_GEE_Pm_PR<-geeglm(Pm ~ as.factor(WealthCat), 
                         data = AD_Active_WealthCatNoMiss, 
                         id = Subject_ID, 
                         family = binomial(link="log"),
                         corstr = "ex")
summary(Wealth_GEE_Pm_PR)


#Po#
Wealth_GEE_Po_PD<-geeglm(Po ~ as.factor(WealthCat), 
                         data = AD_Active_WealthCatNoMiss, 
                         id = Subject_ID, 
                         family = binomial(link="identity"),
                         corstr = "ex")
summary(Wealth_GEE_Po_PD)

Wealth_GEE_Po_PR<-geeglm(Po ~ as.factor(WealthCat), 
                         data = AD_Active_WealthCatNoMiss, 
                         id = Subject_ID, 
                         family = binomial(link="log"),
                         corstr = "ex")
summary(Wealth_GEE_Po_PR)

#Pf#
Wealth_GEE_Pf_PD<-geeglm(Pf ~ as.factor(WealthCat), 
                         data = AD_Active_WealthCatNoMiss, 
                         id = Subject_ID, 
                         family = binomial(link="identity"),
                         corstr = "ex")
summary(Wealth_GEE_Pf_PD)

Wealth_GEE_Pf_PR<-geeglm(Pf ~ as.factor(WealthCat), 
                         data = AD_Active_WealthCatNoMiss, 
                         id = Subject_ID, 
                         family = binomial(link="log"),
                         corstr = "ex")
summary(Wealth_GEE_Pf_PR)


##Outputing GEE Results: Estimate + 95% CI from Robust Sandwich Estimator
#Prevalence Difference 
Coeff_Active_Wealth_Pm_PD<-tidy(Wealth_GEE_Pm_PD, exponentiate=FALSE, conf.int=TRUE)%>%dplyr::mutate(Ifxn="Pm")%>%filter(!term=="(Intercept)")
Coeff_Active_Wealth_Po_PD<-tidy(Wealth_GEE_Po_PD, exponentiate=FALSE, conf.int=TRUE)%>%dplyr::mutate(Ifxn="Po")%>%filter(!term=="(Intercept)")
Coeff_Active_Wealth_Pf_PD<-tidy(Wealth_GEE_Pf_PD, exponentiate=FALSE, conf.int=TRUE)%>%dplyr::mutate(Ifxn="Pf")%>%filter(!term=="(Intercept)")

#Prevalence Ratios 
Coeff_Active_Wealth_Pm_PR<-tidy(Wealth_GEE_Pm_PR, exponentiate=TRUE, conf.int=TRUE)%>%dplyr::mutate(Ifxn="Pm")%>%filter(!term=="(Intercept)")
Coeff_Active_Wealth_Po_PR<-tidy(Wealth_GEE_Po_PR, exponentiate=TRUE, conf.int=TRUE)%>%dplyr::mutate(Ifxn="Po")%>%filter(!term=="(Intercept)")
Coeff_Active_Wealth_Pf_PR<-tidy(Wealth_GEE_Pf_PR, exponentiate=TRUE, conf.int=TRUE)%>%dplyr::mutate(Ifxn="Pf")%>%filter(!term=="(Intercept)")



##
##FEVER CATEGORY (No Fever = Ref)
##
AD_Active_FeverNoMiss<-AD_Active%>%filter(!is.na(Feverpos))

addmargins(table(AD_Active_FeverNoMiss$Feverpos, AD_Active_FeverNoMiss$Pm, useNA = "always"))
addmargins(table(AD_Active_FeverNoMiss$Feverpos, AD_Active_FeverNoMiss$Po, useNA = "always"))

#Pm#
Fever_GEE_Pm_PD<-geeglm(Pm ~ Feverpos, 
                        data = AD_Active_FeverNoMiss, 
                        id = Subject_ID, 
                        family = binomial(link="identity"),
                        corstr = "ex")
summary(Fever_GEE_Pm_PD)

Fever_GEE_Pm_PR<-geeglm(Pm ~ Feverpos, 
                        data = AD_Active_FeverNoMiss, 
                        id = Subject_ID, 
                        family = binomial(link="log"),
                        corstr = "ex")
summary(Fever_GEE_Pm_PR)


#Po#
Fever_GEE_Po_PD<-geeglm(Po ~ Feverpos, 
                        data = AD_Active_FeverNoMiss, 
                        id = Subject_ID, 
                        family = binomial(link="identity"),
                        corstr = "ex")
summary(Fever_GEE_Po_PD)

Fever_GEE_Po_PR<-geeglm(Po ~ Feverpos, 
                        data = AD_Active_FeverNoMiss, 
                        id = Subject_ID, 
                        family = binomial(link="log"),
                        corstr = "ex")
summary(Fever_GEE_Po_PR)

#Pf#
Fever_GEE_Pf_PD<-geeglm(Pf ~ Feverpos, 
                        data = AD_Active_FeverNoMiss, 
                        id = Subject_ID, 
                        family = binomial(link="identity"),
                        corstr = "ex")
summary(Fever_GEE_Pf_PD)

Fever_GEE_Pf_PR<-geeglm(Pf ~ Feverpos, 
                        data = AD_Active_FeverNoMiss, 
                        id = Subject_ID, 
                        family = binomial(link="log"),
                        corstr = "ex")
summary(Fever_GEE_Pf_PR)

##Outputing GEE Results: Estimate + 95% CI from Robust Sandwich Estimator
#Prevalence Differences 
Coeff_Active_Fever_Pm_PD<-tidy(Fever_GEE_Pm_PD, exponentiate=FALSE, conf.int=TRUE)%>%dplyr::mutate(Ifxn="Pm")%>%filter(!term=="(Intercept)")
Coeff_Active_Fever_Po_PD<-tidy(Fever_GEE_Po_PD, exponentiate=FALSE, conf.int=TRUE)%>%dplyr::mutate(Ifxn="Po")%>%filter(!term=="(Intercept)")
Coeff_Active_Fever_Pf_PD<-tidy(Fever_GEE_Pf_PD, exponentiate=FALSE, conf.int=TRUE)%>%dplyr::mutate(Ifxn="Pf")%>%filter(!term=="(Intercept)")

#Prevalence Ratios 
Coeff_Active_Fever_Pm_PR<-tidy(Fever_GEE_Pm_PR, exponentiate=TRUE, conf.int=TRUE)%>%dplyr::mutate(Ifxn="Pm")%>%filter(!term=="(Intercept)")
Coeff_Active_Fever_Po_PR<-tidy(Fever_GEE_Po_PR, exponentiate=TRUE, conf.int=TRUE)%>%dplyr::mutate(Ifxn="Po")%>%filter(!term=="(Intercept)")
Coeff_Active_Fever_Pf_PR<-tidy(Fever_GEE_Pf_PR, exponentiate=TRUE, conf.int=TRUE)%>%dplyr::mutate(Ifxn="Pf")%>%filter(!term=="(Intercept)")




write.table(Coeff_Active_Fever_Pm_PD, "clipboard", sep="\t", col.names = F )
write.table(Coeff_Active_Fever_Po_PD, "clipboard", sep="\t", col.names = F )
write.table(Coeff_Active_Fever_Pf_PD, "clipboard", sep="\t", col.names = F )

write.table(Coeff_Active_Fever_Pm_PR, "clipboard", sep="\t", col.names = F )
write.table(Coeff_Active_Fever_Po_PR, "clipboard", sep="\t", col.names = F )
write.table(Coeff_Active_Fever_Pf_PR, "clipboard", sep="\t", col.names = F )



##
##RDT-positive CATEGORY (Not RDT Positive = Ref)
##
AD_Active_RDTNoMiss<-AD_Active%>%filter(!is.na(RDTpos))

#Pm#
RDT_GEE_Pm_PD<-geeglm(Pm ~ RDTpos, 
                      data = AD_Active_RDTNoMiss, 
                      id = Subject_ID, 
                      family = binomial(link="identity"),
                      corstr = "ex")
summary(RDT_GEE_Pm_PD)

RDT_GEE_Pm_PR<-geeglm(Pm ~ RDTpos, 
                      data = AD_Active_RDTNoMiss, 
                      id = Subject_ID, 
                      family = binomial(link="log"),
                      corstr = "ex")
summary(RDT_GEE_Pm_PR)


#Po#
RDT_GEE_Po_PD<-geeglm(Po ~ RDTpos, 
                      data = AD_Active_RDTNoMiss, 
                      id = Subject_ID, 
                      family = binomial(link="identity"),
                      corstr = "ex")
summary(RDT_GEE_Po_PD)

RDT_GEE_Po_PR<-geeglm(Po ~ RDTpos, 
                      data = AD_Active_RDTNoMiss, 
                      id = Subject_ID, 
                      family = binomial(link="log"),
                      corstr = "ex")
summary(RDT_GEE_Po_PR)

#Pf#
RDT_GEE_Pf_PD<-geeglm(Pf ~ RDTpos, 
                      data = AD_Active_RDTNoMiss, 
                      id = Subject_ID, 
                      family = binomial(link="identity"),
                      corstr = "ex")
summary(RDT_GEE_Pf_PD)

RDT_GEE_Pf_PR<-geeglm(Pf ~ RDTpos, 
                      data = AD_Active_RDTNoMiss, 
                      id = Subject_ID, 
                      family = binomial(link="log"),
                      corstr = "ex")
summary(RDT_GEE_Pf_PR)

##Outputing GEE Results: Estimate + 95% CI from Robust Sandwich Estimator
#Prevalence Differences 
Coeff_Active_RDT_Pm_PD<-tidy(RDT_GEE_Pm_PD, exponentiate=FALSE, conf.int=TRUE)%>%dplyr::mutate(Ifxn="Pm")%>%filter(!term=="(Intercept)")
Coeff_Active_RDT_Po_PD<-tidy(RDT_GEE_Po_PD, exponentiate=FALSE, conf.int=TRUE)%>%dplyr::mutate(Ifxn="Po")%>%filter(!term=="(Intercept)")
Coeff_Active_RDT_Pf_PD<-tidy(RDT_GEE_Pf_PD, exponentiate=FALSE, conf.int=TRUE)%>%dplyr::mutate(Ifxn="Pf")%>%filter(!term=="(Intercept)")

#Prevalence Ratios 
Coeff_Active_RDT_Pm_PR<-tidy(RDT_GEE_Pm_PR, exponentiate=TRUE, conf.int=TRUE)%>%dplyr::mutate(Ifxn="Pm")%>%filter(!term=="(Intercept)")
Coeff_Active_RDT_Po_PR<-tidy(RDT_GEE_Po_PR, exponentiate=TRUE, conf.int=TRUE)%>%dplyr::mutate(Ifxn="Po")%>%filter(!term=="(Intercept)")
Coeff_Active_RDT_Pf_PR<-tidy(RDT_GEE_Pf_PR, exponentiate=TRUE, conf.int=TRUE)%>%dplyr::mutate(Ifxn="Pf")%>%filter(!term=="(Intercept)")



##
##P.falciparum Co-infection (Neg Pf = Ref)
##
AD_Active_PfNoMiss<-AD_Active%>%filter(!is.na(Pf))
#Pm#
Pf_GEE_Pm_PD<-geeglm(Pm ~ Pf, 
                     data = AD_Active_PfNoMiss, 
                     id = Subject_ID, 
                     family = binomial(link="identity"),
                     corstr = "ex")
summary(Pf_GEE_Pm_PD)

Pf_GEE_Pm_PR<-geeglm(Pm ~ Pf, 
                     data = AD_Active_PfNoMiss, 
                     id = Subject_ID, 
                     family = binomial(link="log"),
                     corstr = "ex")
summary(Pf_GEE_Pm_PR)

#Po#
Pf_GEE_Po_PD<-geeglm(Po ~ Pf, 
                     data = AD_Active_PfNoMiss, 
                     id = Subject_ID, 
                     family = binomial(link="identity"),
                     corstr = "ex")
summary(Pf_GEE_Po_PD)

Pf_GEE_Po_PR<-geeglm(Po ~ Pf, 
                     data = AD_Active_PfNoMiss, 
                     id = Subject_ID, 
                     family = binomial(link="log"),
                     corstr = "ex")
summary(Pf_GEE_Po_PR)


##Outputing GEE Results: Estimate + 95% CI from Robust Sandwich Estimator
#Prevalence DIfferences 
Coeff_Active_Pfcoinfec_Pm_PD<-tidy(Pf_GEE_Pm_PD, exponentiate=FALSE, conf.int=TRUE)%>%dplyr::mutate(Ifxn="Pm")%>%filter(!term=="(Intercept)")
Coeff_Active_Pfcoinfec_Po_PD<-tidy(Pf_GEE_Po_PD, exponentiate=FALSE, conf.int=TRUE)%>%dplyr::mutate(Ifxn="Po")%>%filter(!term=="(Intercept)")

#Prevalence Ratios 
Coeff_Active_Pfcoinfec_Pm_PR<-tidy(Pf_GEE_Pm_PR, exponentiate=TRUE, conf.int=TRUE)%>%dplyr::mutate(Ifxn="Pm")%>%filter(!term=="(Intercept)")
Coeff_Active_Pfcoinfec_Po_PR<-tidy(Pf_GEE_Po_PR, exponentiate=TRUE, conf.int=TRUE)%>%dplyr::mutate(Ifxn="Po")%>%filter(!term=="(Intercept)")



##
##SEASONALITY CATEGORY (Rainy Season = Ref)
##
AD_Active_SeasonNoMiss<-AD_Active%>%filter(!is.na(season_dry))
#Pm#
DrySeason_GEE_Pm_PD<-geeglm(Pm ~ season_dry, 
                            data = AD_Active_SeasonNoMiss, 
                            id = Subject_ID, 
                            family = binomial(link="identity"),
                            corstr = "ex")
summary(DrySeason_GEE_Pm_PD)

DrySeason_GEE_Pm_PR<-geeglm(Pm ~ season_dry, 
                            data = AD_Active_SeasonNoMiss, 
                            id = Subject_ID, 
                            family = binomial(link="log"),
                            corstr = "ex")
summary(DrySeason_GEE_Pm_PR)

#Po#
DrySeason_GEE_Po_PD<-geeglm(Po ~ season_dry, 
                            data = AD_Active_SeasonNoMiss, 
                            id = Subject_ID, 
                            family = binomial(link="identity"),
                            corstr = "ex")
summary(DrySeason_GEE_Po_PD)

DrySeason_GEE_Po_PR<-geeglm(Po ~ season_dry, 
                            data = AD_Active_SeasonNoMiss, 
                            id = Subject_ID, 
                            family = binomial(link="log"),
                            corstr = "ex")
summary(DrySeason_GEE_Po_PR)

#Pf#
DrySeason_GEE_Pf_PD<-geeglm(Pf ~ season_dry, 
                            data = AD_Active_SeasonNoMiss, 
                            id = Subject_ID, 
                            family = binomial(link="identity"),
                            corstr = "ex")
summary(DrySeason_GEE_Pf_PD)

DrySeason_GEE_Pf_PR<-geeglm(Pf ~ season_dry, 
                            data = AD_Active_SeasonNoMiss, 
                            id = Subject_ID, 
                            family = binomial(link="log"),
                            corstr = "ex")
summary(DrySeason_GEE_Pf_PR)

##Outputing GEE Results: Estimate + 95% CI from Robust Sandwich Estimator
#Prevalecne Differneces 
Coeff_Active_DrySeason_Pm_PD<-tidy(DrySeason_GEE_Pm_PD, exponentiate=FALSE, conf.int=TRUE)%>%dplyr::mutate(Ifxn="Pm")%>%filter(!term=="(Intercept)")
Coeff_Active_DrySeason_Po_PD<-tidy(DrySeason_GEE_Po_PD, exponentiate=FALSE, conf.int=TRUE)%>%dplyr::mutate(Ifxn="Po")%>%filter(!term=="(Intercept)")
Coeff_Active_DrySeason_Pf_PD<-tidy(DrySeason_GEE_Pf_PD, exponentiate=FALSE, conf.int=TRUE)%>%dplyr::mutate(Ifxn="Pf")%>%filter(!term=="(Intercept)")

#Prevalecne Ratios 
Coeff_Active_DrySeason_Pm_PR<-tidy(DrySeason_GEE_Pm_PR, exponentiate=TRUE, conf.int=TRUE)%>%dplyr::mutate(Ifxn="Pm")%>%filter(!term=="(Intercept)")
Coeff_Active_DrySeason_Po_PR<-tidy(DrySeason_GEE_Po_PR, exponentiate=TRUE, conf.int=TRUE)%>%dplyr::mutate(Ifxn="Po")%>%filter(!term=="(Intercept)")
Coeff_Active_DrySeason_Pf_PR<-tidy(DrySeason_GEE_Pf_PR, exponentiate=TRUE, conf.int=TRUE)%>%dplyr::mutate(Ifxn="Pf")%>%filter(!term=="(Intercept)")



##
##BED NET USE PRIOR NIGHT (No Use = Ref)
##
AD_Active_BedNetNoMiss<-AD_Active%>%filter(!is.na(BedNetPrior))
#Pm#
BedNet_GEE_Pm_PD<-geeglm(Pm ~ BedNetPrior, 
                         data = AD_Active_BedNetNoMiss, 
                         id = Subject_ID, 
                         family = binomial(link="identity"),
                         corstr = "ex")
summary(BedNet_GEE_Pm_PD)

BedNet_GEE_Pm_PR<-geeglm(Pm ~ BedNetPrior, 
                         data = AD_Active_BedNetNoMiss, 
                         id = Subject_ID, 
                         family = binomial(link="log"),
                         corstr = "ex")
summary(BedNet_GEE_Pm_PR)

#Po#
BedNet_GEE_Po_PD<-geeglm(Po ~ BedNetPrior, 
                         data = AD_Active_BedNetNoMiss, 
                         id = Subject_ID, 
                         family = binomial(link="identity"),
                         corstr = "ex")
summary(BedNet_GEE_Po_PD)

BedNet_GEE_Po_PR<-geeglm(Po ~ BedNetPrior, 
                         data = AD_Active_BedNetNoMiss, 
                         id = Subject_ID, 
                         family = binomial(link="log"),
                         corstr = "ex")
summary(BedNet_GEE_Po_PR)
#Pf#
BedNet_GEE_Pf_PD<-geeglm(Pf ~ BedNetPrior, 
                         data = AD_Active_BedNetNoMiss, 
                         id = Subject_ID, 
                         family = binomial(link="identity"),
                         corstr = "ex")
summary(BedNet_GEE_Pf_PD)

BedNet_GEE_Pf_PR<-geeglm(Pf ~ BedNetPrior, 
                         data = AD_Active_BedNetNoMiss, 
                         id = Subject_ID, 
                         family = binomial(link="log"),
                         corstr = "ex")
summary(BedNet_GEE_Pf_PR)

##Outputing GEE Results: Estimate + 95% CI from Robust Sandwich Estimator
#Prevalence Differences 
Coeff_Active_BedNet_Pm_PD<-tidy(BedNet_GEE_Pm_PD, exponentiate=FALSE, conf.int=TRUE)%>%dplyr::mutate(Ifxn="Pm")%>%filter(!term=="(Intercept)")
Coeff_Active_BedNet_Po_PD<-tidy(BedNet_GEE_Po_PD, exponentiate=FALSE, conf.int=TRUE)%>%dplyr::mutate(Ifxn="Po")%>%filter(!term=="(Intercept)")
Coeff_Active_BedNet_Pf_PD<-tidy(BedNet_GEE_Pf_PD, exponentiate=FALSE, conf.int=TRUE)%>%dplyr::mutate(Ifxn="Pf")%>%filter(!term=="(Intercept)")

#Prevalence Ratios 
Coeff_Active_BedNet_Pm_PR<-tidy(BedNet_GEE_Pm_PR, exponentiate=TRUE, conf.int=TRUE)%>%dplyr::mutate(Ifxn="Pm")%>%filter(!term=="(Intercept)")
Coeff_Active_BedNet_Po_PR<-tidy(BedNet_GEE_Po_PR, exponentiate=TRUE, conf.int=TRUE)%>%dplyr::mutate(Ifxn="Po")%>%filter(!term=="(Intercept)")
Coeff_Active_BedNet_Pf_PR<-tidy(BedNet_GEE_Pf_PR, exponentiate=TRUE, conf.int=TRUE)%>%dplyr::mutate(Ifxn="Pf")%>%filter(!term=="(Intercept)")






######
#Active Visits:  Stacking all GEE Output for Estimate (95% CI) Plots

#Prevalence Differences

Active_GEE_Output_PD<-rbind(
  Coeff_Active_Age_Pm_PD, Coeff_Active_Age_Po_PD, Coeff_Active_Age_Pf_PD,
  Coeff_Active_Sex_Pm_PD, Coeff_Active_Sex_Po_PD, Coeff_Active_Sex_Pf_PD,
  Coeff_Active_HealthArea_Pm_PD, Coeff_Active_HealthArea_Po_PD, Coeff_Active_HealthArea_Pf_PD,
  #Coeff_Active_Village_Pm_PD, Coeff_Active_Village_Po_PD, Coeff_Active_Village_Pf_PD,
  Coeff_Active_Wealth_Pm_PD, Coeff_Active_Wealth_Po_PD, Coeff_Active_Wealth_Pf_PD,
  Coeff_Active_Fever_Pm_PD, Coeff_Active_Fever_Po_PD, Coeff_Active_Fever_Pf_PD,
  Coeff_Active_RDT_Pm_PD, Coeff_Active_RDT_Po_PD, Coeff_Active_RDT_Pf_PD,
  Coeff_Active_Pfcoinfec_Pm_PD, Coeff_Active_Pfcoinfec_Po_PD,
  Coeff_Active_DrySeason_Pm_PD, Coeff_Active_DrySeason_Po_PD, Coeff_Active_DrySeason_Pf_PD,
  Coeff_Active_BedNet_Pm_PD, Coeff_Active_BedNet_Po_PD, Coeff_Active_BedNet_Pf_PD
  # Coeff_Active_Visit_Pm_PD, Coeff_Active_Visit_Po_PD, Coeff_Active_Visit_Pf_PD
)

write.table(Active_GEE_Output_PD, "clipboard", sep="\t", col.names = T )



#Prevalence Ratios

Active_GEE_Output_PR<-rbind(
  Coeff_Active_Age_Pm_PR, Coeff_Active_Age_Po_PR, Coeff_Active_Age_Pf_PR,
  Coeff_Active_Sex_Pm_PR, Coeff_Active_Sex_Po_PR, Coeff_Active_Sex_Pf_PR,
  Coeff_Active_HealthArea_Pm_PR, Coeff_Active_HealthArea_Po_PR, Coeff_Active_HealthArea_Pf_PR,
  #Coeff_Active_Village_Pm_PD, Coeff_Active_Village_Po_PD, Coeff_Active_Village_Pf_PD,
  Coeff_Active_Wealth_Pm_PR, Coeff_Active_Wealth_Po_PR, Coeff_Active_Wealth_Pf_PR,
  Coeff_Active_Fever_Pm_PR, Coeff_Active_Fever_Po_PR, Coeff_Active_Fever_Pf_PR,
  Coeff_Active_RDT_Pm_PR, Coeff_Active_RDT_Po_PR, Coeff_Active_RDT_Pf_PR,
  Coeff_Active_Pfcoinfec_Pm_PR, Coeff_Active_Pfcoinfec_Po_PR,
  Coeff_Active_DrySeason_Pm_PR, Coeff_Active_DrySeason_Po_PR, Coeff_Active_DrySeason_Pf_PR,
  Coeff_Active_BedNet_Pm_PR, Coeff_Active_BedNet_Po_PR, Coeff_Active_BedNet_Pf_PR
  # Coeff_Active_Visit_Pm_PD, Coeff_Active_Visit_Po_PD, Coeff_Active_Visit_Pf_PD
)

write.table(Active_GEE_Output_PR, "clipboard", sep="\t", col.names = T )




#############
##  Plotting Prevalence Differences and 95% CIs by Population

Active_GEE_Output_PD$Ifxn <- factor(Active_GEE_Output_PD$Ifxn, levels=c("Pm", "Po", "Pf"))
Active_GEE_Output_PD$Ifxn <- factor(Active_GEE_Output_PD$Ifxn, levels=c("Pm", "Po", "Pf"))

Active_GEE_Output_PD$term <- recode_factor(Active_GEE_Output_PD$term, 
                                           "relevel(as.factor(AgeCat_Visit), ref = \"3\")1" = "Age <5 vs 15+", 
                                           "relevel(as.factor(AgeCat_Visit), ref = \"3\")2" = "Age 5-14 vs 15+",
                                           "Sex" = "Sex (F vs M)",
                                           "as.factor(HealthArea)2" = "Rural vs. Urban", 
                                           "as.factor(HealthArea)3" = "Peri-urban vs. Urban", 
                                           "as.factor(WealthCat)2" = "Poor vs Avg. Wealth",
                                           "as.factor(WealthCat)3" = "Wealthy vs Avg. Wealth",
                                           "season_dry" =  "Dry vs Rainy Season",
                                           "RDTpos" = "RDT+ vs RDT-",
                                           "Pf" = "Pf coinfection (Y vs N)",
                                           "Feverpos" = "Fever (Y vs N)",
                                           "BedNetPrior" = "Bed Net Use (Y vs N)")

Active_GEE_Output_PR$Ifxn <- factor(Active_GEE_Output_PR$Ifxn, levels=c("Pm", "Po", "Pf"))
Active_GEE_Output_PR$Ifxn <- factor(Active_GEE_Output_PR$Ifxn, levels=c("Pm", "Po", "Pf"))

Active_GEE_Output_PR$term <- recode_factor(Active_GEE_Output_PR$term, 
                                           "relevel(as.factor(AgeCat_Visit), ref = \"3\")1" = "Age <5 vs 15+", 
                                           "relevel(as.factor(AgeCat_Visit), ref = \"3\")2" = "Age 5-14 vs 15+",
                                           "Sex" = "Sex (F vs M)",
                                           "as.factor(HealthArea)2" = "Rural vs. Urban", 
                                           "as.factor(HealthArea)3" = "Peri-urban vs. Urban", 
                                           "as.factor(WealthCat)2" = "Poor vs Avg. Wealth",
                                           "as.factor(WealthCat)3" = "Wealthy vs Avg. Wealth",
                                           "season_dry" =  "Dry vs Rainy Season",
                                           "RDTpos" = "RDT+ vs RDT-",
                                           "Pf" = "Pf coinfection (Y vs N)",
                                           "Feverpos" = "Fever (Y vs N)",
                                           "BedNetPrior" = "Bed Net Use (Y vs N)")



Active_GEE_Output$term <- recode_factor(Active_GEE_Output$term, 
                                        "agecat_LT5" = "Age <5 vs 15+", 
                                        "agecat_5_14" = "Age 5-14 vs 15+",
                                        "Sex" = "Sex (F vs M)",
                                        "as.factor(HealthArea)2" = "Rural vs. Urban", 
                                        "as.factor(HealthArea)3" = "Peri-urban vs. Urban", 
                                        "Village_Impuru" = "Site-Impuru vs Bu",
                                        "Village_Pema" = "Site-Pema vs Bu",
                                        "Village_Kimpoko" = "Site-Kimpoko vs Bu",
                                        "Village_Ngamanzo" = "Site-Ngamanzo vs Bu",
                                        "Village_Iye" =  "Site-Iye vs Bu",
                                        "Village_Lingwala" = "Site-Voix de Peuple vs Bu",
                                        "Wealth_Poorest" = "Poorest vs Avg. Wealth",
                                        "Wealth_Poorer" = "Poorer vs Avg. Wealth",
                                        "Wealth_Wealthier" = "Wealthier vs Avg. Wealth",
                                        "Wealth_Wealthiest" = "Wealthiest vs Avg. Wealth",
                                        "season_dry" =  "Dry vs Rainy Season",
                                        "RDTpos" = "RDT+ vs RDT-",
                                        "Pf" = "Pf coinfection (Y vs N)",
                                        "Feverpos" = "Fever (Y vs N)",
                                        "BedNetPrior" = "Bed Net Use (Y vs N)",
                                        "Visit" = "Calendar Time")


##checking the ordering of variables for gggplotting: 
Active_GEE_Output_PD$term %>%levels()
Active_GEE_Output_PR$term %>%levels()
#Passive_GEE_Output_PD$term %>%levels()
Active_GEE_Output_PD$term %>%levels()


#set order of levels to appear along y-axis
Active_GEE_Output_PD <- Active_GEE_Output_PD%>%
  mutate(term = term %>% 
           fct_relevel("Wealthy vs Avg. Wealth",
                       "Poor vs Avg. Wealth",
                       "Dry vs Rainy Season",
                       "Bed Net Use (Y vs N)",
                       "Fever (Y vs N)",
                       "Pf coinfection (Y vs N)",
                       "RDT+ vs RDT-",
                       "Peri-urban vs. Urban",
                       "Rural vs. Urban",
                       "Sex (F vs M)",
                       "Age 5-14 vs 15+",
                       "Age <5 vs 15+", 
           ))

Active_GEE_Output_PR <- Active_GEE_Output_PR%>%
  mutate(term = term %>% 
           fct_relevel("Wealthy vs Avg. Wealth",
                       "Poor vs Avg. Wealth",
                       "Dry vs Rainy Season",
                       "Bed Net Use (Y vs N)",
                       "Fever (Y vs N)",
                       "Pf coinfection (Y vs N)",
                       "RDT+ vs RDT-",
                       "Peri-urban vs. Urban",
                       "Rural vs. Urban",
                       "Sex (F vs M)",
                       "Age 5-14 vs 15+",
                       "Age <5 vs 15+", 
           ))



ActivePop_PD <-ggplot(Active_GEE_Output_PD, (aes(x=estimate, y=term, col=Ifxn, fill=Ifxn))) + 
  geom_errorbar(aes(xmin=conf.low, xmax=conf.high, col=Ifxn), size=0.65, width=0.6, position=position_dodge(width = 0.7))+
  geom_point(size=3, shape=21, colour="black", stroke = 0.8, position=position_dodge(width = 0.7)) +
  geom_vline(xintercept=0.0, lty=2)+
  scale_fill_manual(values=c("blue", "red", "orange"))+
  scale_color_manual(values=c("black", "black", "black"))+
  guides(fill = guide_legend(title = "Species"), col=FALSE)+
  # ggtitle("Survey-based Pop. (N=1,565; 5,682 visits)")+
  xlab("Prevalence Difference")+
  ylab("")+
  #labs(title = "Active Pop.",
  #     subtitle = "n=1,565 participants across 5,682 visits")+
  #guides(fill = guide_legend(title = "Species"), col=FALSE)+
  theme(plot.title = element_text(size=11, color="black"), plot.subtitle = element_text(size=9.5), axis.text.x=element_text(size=9, color="black"), axis.text.y.left=element_text(size=10,  color="black"),  legend.text=element_text(size=10, color="black"), legend.title = element_text(size=10, color="black")
        #                   , legend.position = c(0.82, 0.15), legend.background = element_rect(size=0.3, linetype="solid", colour ="black")
        , legend.position = "none"
  )
#ActivePop_PD_noPf_2<-ActivePop_PD_noPf + expand_limits(x=c(-0.08, 0.08))
ActivePop_PD_2<-ActivePop_PD + xlim(-0.29, 0.69)
plot(ActivePop_PD_2) 



##Now without Pf also for a zoomed version on just Pm and Po differences
Active_GEE_Output_PD_NoPf<-Active_GEE_Output_PD%>%filter(Ifxn!= "Pf")
ActivePop_PD_noPf<-ggplot(Active_GEE_Output_PD_NoPf, (aes(x=estimate, y=term, col=Ifxn, fill=Ifxn))) + 
  geom_errorbar(aes(xmin=conf.low, xmax=conf.high, col=Ifxn), size=0.5,  width=0.4, position=position_dodge(width = 0.3))+
  geom_point(size=3, shape=21, colour="black", stroke = 0.8, position=position_dodge(width = 0.3)) +
  geom_vline(xintercept=0.0, lty=2)+
  scale_fill_manual(values=c("blue", "red", "orange"))+
  scale_color_manual(values=c("black", "black", "black"))+
  guides(fill = guide_legend(title = "Species"), col=FALSE)+
  # ggtitle("Survey-based Pop. (N=1,565; 5,682 visits)")+
  xlab("Prevalence Difference")+
  ylab("")+
  #labs(title = "Active Pop.",
  #     subtitle = "n=1,565 participants across 5,682 visits")+
  #guides(fill = guide_legend(title = "Species"), col=FALSE)+
  theme(plot.title = element_text(size=11, color="black"), plot.subtitle = element_text(size=9.5), axis.text.x=element_text(size=9, color="black"), axis.text.y.left=element_text(size=10,  color="black"),  legend.text=element_text(size=10, color="black"), legend.title = element_text(size=10, color="black")
        #                   , legend.position = c(0.82, 0.15), legend.background = element_rect(size=0.3, linetype="solid", colour ="black")
        , legend.position = "none"
  )
#ActivePop_PD_noPf_2<-ActivePop_PD_noPf + expand_limits(x=c(-0.08, 0.08))
ActivePop_PD_noPf_2<-ActivePop_PD_noPf + xlim(-0.05, 0.075)
plot(ActivePop_PD_noPf_2) 



ActivePop_PR <-ggplot(Active_GEE_Output_PR, (aes(x=estimate, y=term, col=Ifxn, fill=Ifxn))) + 
  geom_errorbar(aes(xmin=conf.low, xmax=conf.high, col=Ifxn), size=0.7, width=0.6, position=position_dodge(width = 0.7))+
  geom_point(size=4, shape=21, colour="black", stroke = 0.8, position=position_dodge(width = 0.7)) +
  geom_vline(xintercept=0.0, lty=2)+
  scale_fill_manual(values=c("blue", "red", "orange"))+
  scale_color_manual(values=c("black", "black", "black"))+
  ggtitle("Active population (n=1,565 participants across 5,682 visits)")+
  xlab("Prevalence Ratio")+
  ylab("")+
  guides(fill = guide_legend(title = "Species"), col=FALSE)+
  theme(plot.title =element_text(size=16, color="black"), axis.title = element_text(size =14), axis.text.x=element_text(size=11, color="black"), axis.text.y.left=element_text(size=11,  color="black"), legend.text=element_text(size=14, color="black"), legend.title = element_text(size=14, color="black"))
#ActivePop_PR_2<-ActivePop_PR + xlim(-0.9, 0.9)
plot(ActivePop_PR) 


##Now without Pf also for a zoomed version on just Pm and Po differences
Active_GEE_Output_PR_NoPf<-Active_GEE_Output_PR%>%filter(Ifxn!= "Pf")
ActivePop_PR_noPf<-ggplot(Active_GEE_Output_PR_NoPf, (aes(x=estimate, y=term, col=Ifxn, fill=Ifxn))) + 
  geom_errorbar(aes(xmin=conf.low, xmax=conf.high, col=Ifxn), size=0.5,  width=0.4, position=position_dodge(width = 0.3))+
  geom_point(size=3, shape=21, colour="black", stroke = 0.8, position=position_dodge(width = 0.3)) +
  geom_vline(xintercept=0.0, lty=2)+
  scale_fill_manual(values=c("blue", "red", "orange"))+
  scale_color_manual(values=c("black", "black", "black"))+
  guides(fill = guide_legend(title = "Species"), col=FALSE)+
  # ggtitle("Survey-based Pop. (N=1,565; 5,682 visits)")+
  xlab("Prevalence Ratio")+
  ylab("")+
  labs(title = "Active Pop.",
       subtitle = "n=1,565 participants across 5,682 visits")+
  #guides(fill = guide_legend(title = "Species"), col=FALSE)+
  theme(plot.title =element_text(size=11, color="black"), plot.subtitle = element_text(size=9.5), axis.text.x=element_text(size=9, color="black"), axis.text.y.left=element_text(size=10,  color="black"),  legend.text=element_text(size=10, color="black"), legend.title = element_text(size=10, color="black"))
ActivePop_PR_noPf_2<-ActivePop_PR_noPf + expand_limits(x=c(-3, 6))
plot(ActivePop_PR_noPf_2) 






## Passive Visit Prevalence Association 


##Soring data by Subject_ID and then by Visit number to ensure it's formatted correctly for the clustering analysis. 
AD_Passive <- AD_Passive[order(AD_Passive$Subject_ID, AD_Passive$Visit, AD_Passive$Visit_date),]


##Crude prevalence (accounting for repeated testing;  not accounting for household clustering)

##Pm

##PD
CrudePrev_GEE_Pm_Passive_Pd<-geeglm(Pm ~ 1, 
                                    data = AD_Passive, 
                                    id = Subject_ID, 
                                    family = binomial(link="identity"),
                                    corstr = "ex")

summary(CrudePrev_GEE_Pm_Passive_Pd)

##PR
CrudePrev_GEE_Pm_Passive_PR<-geeglm(Pm ~ 1, 
                                    data = AD_Passive, 
                                    id = Subject_ID, 
                                    family = binomial(link="log"),
                                    corstr = "ex")

summary(CrudePrev_GEE_Pm_Passive_PR)


### Mixed model accounting for within-subject and household clustering
CrudePrev_ME_Pm <- lmer(Pm ~ 1 + (1 | Subject_ID) + (1 | Household)  ,
                        data = AD_Passive,
                        family = binomial(link="identity"),
                        REML = FALSE)
summary(CrudePrev_ME_Pm)

CrudePrev_ME_Pm <- lmer(Pm ~ 1 + (1 | Subject_ID) + (1 | Household)  ,
                        data = AD_Passive,
                        family = binomial(link="log"),
                        REML = FALSE)
summary(CrudePrev_ME_Pm)



##Po
CrudePrev_GEE_Po<-geeglm(Po ~ 1, 
                         data = AD_Passive, 
                         id = Subject_ID, 
                         family = binomial(link="identity"),
                         corstr = "ex")
summary(CrudePrev_GEE_Po)


### Mixed model accounting for within-subject and household clustering
CrudePrev_ME_Po <- lmer(Po ~ 1 + (1 | Subject_ID) + (1 | Household),
                        data = AD_Passive,
                        REML = FALSE)
summary(CrudePrev_ME_Po)



##Pf
CrudePrev_GEE_Pf<-geeglm(Pf ~ 1, 
                         data = AD_Passive, 
                         id = Subject_ID, 
                         family = binomial(link="identity"),
                         corstr = "ex")
summary(CrudePrev_GEE_Pf)


### Mixed model accounting forwithin-subject and household clutering
CrudePrev_ME_Pf <- lmer(Pf ~ 1 + (1 | Subject_ID) + (1 | Household),
                        data = AD_Passive,
                        REML = FALSE)
summary(CrudePrev_ME_Pf)






##
##AGE CATEGORY (Age >=15 years = Ref)
##
AD_Passive_AgeNoMiss<-AD_Passive%>%filter(!is.na(AgeCat_Visit))
addmargins(table(AD_Passive_AgeNoMiss$AgeCat_Visit, AD_Passive_AgeNoMiss$Pm))
addmargins(table(AD_Passive$AgeCat_Visit, AD_Passive$Pm))


#Pm#
Age_GEE_Pm_PD<-geeglm(Pm ~ relevel(as.factor(AgeCat_Visit), ref='3'), 
                      data = AD_Passive_AgeNoMiss, 
                      id = Subject_ID, 
                      family = binomial(link="identity"),
                      corstr = "ex")
summary(Age_GEE_Pm_PD)

Age_GEE_Pm_PR<-geeglm(Pm ~ relevel(as.factor(AgeCat_Visit), ref='3'), 
                      data = AD_Passive_AgeNoMiss, 
                      id = Subject_ID, 
                      family = binomial(link="log"),
                      corstr = "ex")
summary(Age_GEE_Pm_PR)



#Age_GEE_Pm<-geeglm(Pm ~ agecat_LT5 + agecat_5_14, 
#                   data = AD_Passive_AgeNoMiss, 
#                   id = Subject_ID, 
#                   family = binomial(link="identity"),
#                   corstr = "ex")
#summary(Age_GEE_Pm)


#Po#

Age_GEE_Po_PD<-geeglm(Po ~ relevel(as.factor(AgeCat_Visit), ref='3'), 
                      data = AD_Passive_AgeNoMiss, 
                      id = Subject_ID, 
                      family = binomial(link="identity"),
                      corstr = "ex")
summary(Age_GEE_Po_PD)

Age_GEE_Po_PR<-geeglm(Po ~ relevel(as.factor(AgeCat_Visit), ref='3'), 
                      data = AD_Passive_AgeNoMiss, 
                      id = Subject_ID, 
                      family = binomial(link="log"),
                      corstr = "ex")
summary(Age_GEE_Po_PR)


#Age_GEE_Po<-geeglm(Po ~ agecat_LT5 + agecat_5_14, 
#                   data = AD_Passive_AgeNoMiss, 
#                   id = Subject_ID, 
#                   family = binomial(link="identity"),
#                   corstr = "ex")
#summary(Age_GEE_Po)


#Pf#
Age_GEE_Pf_PD<-geeglm(Pf ~ relevel(as.factor(AgeCat_Visit), ref='3'), 
                      data = AD_Passive_AgeNoMiss, 
                      id = Subject_ID, 
                      family = binomial(link="identity"),
                      corstr = "ex")
summary(Age_GEE_Pf_PD)

Age_GEE_Pf_PR<-geeglm(Pf ~ relevel(as.factor(AgeCat_Visit), ref='3'), 
                      data = AD_Passive_AgeNoMiss, 
                      id = Subject_ID, 
                      family = binomial(link="log"),
                      corstr = "ex")
summary(Age_GEE_Pf_PR)


#Age_GEE_Pf<-geeglm(Pf ~ agecat_LT5 + agecat_5_14, 
#                   data = AD_Passive_AgeNoMiss, 
#                   id = Subject_ID, 
#                   family = binomial(link="identity"),
#                   corstr = "ex")
#summary(Age_GEE_Pf)

##Outputing GEE Results: Estimate + 95% CI from Robust Sandwich Estimator
#Prevalence Differences 
Coeff_Passive_Age_Pm_PD<-tidy(Age_GEE_Pm_PD, exponentiate=FALSE, conf.int=TRUE)%>%dplyr::mutate(Ifxn="Pm")%>%filter(!term=="(Intercept)")
Coeff_Passive_Age_Po_PD<-tidy(Age_GEE_Po_PD, exponentiate=FALSE, conf.int=TRUE)%>%dplyr::mutate(Ifxn="Po")%>%filter(!term=="(Intercept)")
Coeff_Passive_Age_Pf_PD<-tidy(Age_GEE_Pf_PD, exponentiate=FALSE, conf.int=TRUE)%>%dplyr::mutate(Ifxn="Pf")%>%filter(!term=="(Intercept)")

#Prevalence Ratios 
Coeff_Passive_Age_Pm_PR<-tidy(Age_GEE_Pm_PR, exponentiate=TRUE, conf.int=TRUE)%>%dplyr::mutate(Ifxn="Pm")%>%filter(!term=="(Intercept)")
Coeff_Passive_Age_Po_PR<-tidy(Age_GEE_Po_PR, exponentiate=TRUE, conf.int=TRUE)%>%dplyr::mutate(Ifxn="Po")%>%filter(!term=="(Intercept)")
Coeff_Passive_Age_Pf_PR<-tidy(Age_GEE_Pf_PR, exponentiate=TRUE, conf.int=TRUE)%>%dplyr::mutate(Ifxn="Pf")%>%filter(!term=="(Intercept)")



##
##SEX CATEGORY (Male = Ref)
##
AD_Passive_SexNoMiss<-AD_Passive%>%filter(!is.na(Sex))

#Pm#
Sex_GEE_Pm_PD<-geeglm(Pm ~ Sex, 
                      data = AD_Passive_SexNoMiss, 
                      id = Subject_ID, 
                      family = binomial(link="identity"),
                      corstr = "ex")
summary(Sex_GEE_Pm_PD)

Sex_GEE_Pm_PR<-geeglm(Pm ~ Sex, 
                      data = AD_Passive_SexNoMiss, 
                      id = Subject_ID, 
                      family = binomial(link="log"),
                      corstr = "ex")
summary(Sex_GEE_Pm_PR)

#Po#
Sex_GEE_Po_PD<-geeglm(Po ~ Sex, 
                      data = AD_Passive_SexNoMiss, 
                      id = Subject_ID, 
                      family = binomial(link="identity"),
                      corstr = "ex")
summary(Sex_GEE_Po_PD)

Sex_GEE_Po_PR<-geeglm(Po ~ Sex, 
                      data = AD_Passive_SexNoMiss, 
                      id = Subject_ID, 
                      family = binomial(link="log"),
                      corstr = "ex")
summary(Sex_GEE_Po_PR)

#Pf#
Sex_GEE_Pf_PD<-geeglm(Pf ~ Sex, 
                      data = AD_Passive_SexNoMiss, 
                      id = Subject_ID, 
                      family = binomial(link="identity"),
                      corstr = "ex")
summary(Sex_GEE_Pf_PD)

Sex_GEE_Pf_PR<-geeglm(Pf ~ Sex, 
                      data = AD_Passive_SexNoMiss, 
                      id = Subject_ID, 
                      family = binomial(link="log"),
                      corstr = "ex")
summary(Sex_GEE_Pf_PR)



##Outputing GEE Results: Estimate + 95% CI from Robust Sandwich Estimator

#Prevalence Differences 
Coeff_Passive_Sex_Pm_PD<-tidy(Sex_GEE_Pm_PD, exponentiate=FALSE, conf.int=TRUE)%>%dplyr::mutate(Ifxn="Pm")%>%filter(!term=="(Intercept)")
Coeff_Passive_Sex_Po_PD<-tidy(Sex_GEE_Po_PD, exponentiate=FALSE, conf.int=TRUE)%>%dplyr::mutate(Ifxn="Po")%>%filter(!term=="(Intercept)")
Coeff_Passive_Sex_Pf_PD<-tidy(Sex_GEE_Pf_PD, exponentiate=FALSE, conf.int=TRUE)%>%dplyr::mutate(Ifxn="Pf")%>%filter(!term=="(Intercept)")

#Prevalence Ratios
Coeff_Passive_Sex_Pm_PR<-tidy(Sex_GEE_Pm_PR, exponentiate=TRUE, conf.int=TRUE)%>%dplyr::mutate(Ifxn="Pm")%>%filter(!term=="(Intercept)")
Coeff_Passive_Sex_Po_PR<-tidy(Sex_GEE_Po_PR, exponentiate=TRUE, conf.int=TRUE)%>%dplyr::mutate(Ifxn="Po")%>%filter(!term=="(Intercept)")
Coeff_Passive_Sex_Pf_PR<-tidy(Sex_GEE_Pf_PR, exponentiate=TRUE, conf.int=TRUE)%>%dplyr::mutate(Ifxn="Pf")%>%filter(!term=="(Intercept)")




##
##HEALTH AREA CATEGORY (1 = LINGWALA = REF CAT (urban);  Bu = 2 = Ref ; (rural);  3 = KIMPOKO  (peri-urban))
##
AD_Passive_HealthAreaNoMiss<-AD_Passive%>%filter(!is.na(HealthArea))

#Pm#
HealthArea_GEE_Pm_PD<-geeglm(Pm ~ as.factor(HealthArea), 
                             data = AD_Passive_HealthAreaNoMiss, 
                             id = Subject_ID, 
                             family = binomial(link="identity"),
                             corstr = "ex")
summary(HealthArea_GEE_Pm_PD)

HealthArea_GEE_Pm_PR<-geeglm(Pm ~ as.factor(HealthArea), 
                             data = AD_Passive_HealthAreaNoMiss, 
                             id = Subject_ID, 
                             family = binomial(link="log"),
                             corstr = "ex")
summary(HealthArea_GEE_Pm_PR)

#Po#
HealthArea_GEE_Po_PD<-geeglm(Po ~ as.factor(HealthArea),  
                             data = AD_Passive_HealthAreaNoMiss, 
                             id = Subject_ID, 
                             family = binomial(link="identity"),
                             corstr = "ex")
summary(HealthArea_GEE_Po_PD)

HealthArea_GEE_Po_PR<-geeglm(Po ~ as.factor(HealthArea),  
                             data = AD_Passive_HealthAreaNoMiss, 
                             id = Subject_ID, 
                             family = binomial(link="log"),
                             corstr = "ex")
summary(HealthArea_GEE_Po_PR)

#Pf#
HealthArea_GEE_Pf_PD<-geeglm(Pf ~ as.factor(HealthArea),  
                             data = AD_Passive_HealthAreaNoMiss, 
                             id = Subject_ID, 
                             family = binomial(link="identity"),
                             corstr = "ex")
summary(HealthArea_GEE_Pf_PD)

HealthArea_GEE_Pf_PR<-geeglm(Pf ~ as.factor(HealthArea),  
                             data = AD_Passive_HealthAreaNoMiss, 
                             id = Subject_ID, 
                             family = binomial(link="log"),
                             corstr = "ex")
summary(HealthArea_GEE_Pf_PR)


##Outputing GEE Results: Estimate + 95% CI from Robust Sandwich Estimator
#Prevalence Differences 
Coeff_Passive_HealthArea_Pm_PD<-tidy(HealthArea_GEE_Pm_PD, exponentiate=FALSE, conf.int=TRUE)%>%dplyr::mutate(Ifxn="Pm")%>%filter(!term=="(Intercept)")
Coeff_Passive_HealthArea_Po_PD<-tidy(HealthArea_GEE_Po_PD, exponentiate=FALSE, conf.int=TRUE)%>%dplyr::mutate(Ifxn="Po")%>%filter(!term=="(Intercept)")
Coeff_Passive_HealthArea_Pf_PD<-tidy(HealthArea_GEE_Pf_PD, exponentiate=FALSE, conf.int=TRUE)%>%dplyr::mutate(Ifxn="Pf")%>%filter(!term=="(Intercept)")

#Prevalence Ratios
Coeff_Passive_HealthArea_Pm_PR<-tidy(HealthArea_GEE_Pm_PR, exponentiate=TRUE, conf.int=TRUE)%>%dplyr::mutate(Ifxn="Pm")%>%filter(!term=="(Intercept)")
Coeff_Passive_HealthArea_Po_PR<-tidy(HealthArea_GEE_Po_PR, exponentiate=TRUE, conf.int=TRUE)%>%dplyr::mutate(Ifxn="Po")%>%filter(!term=="(Intercept)")
Coeff_Passive_HealthArea_Pf_PR<-tidy(HealthArea_GEE_Pf_PR, exponentiate=TRUE, conf.int=TRUE)%>%dplyr::mutate(Ifxn="Pf")%>%filter(!term=="(Intercept)")





##
##WEALTH CATEGORY (Average Wealth [3] = Ref)
##

AD_Passive_WealthCatNoMiss<-AD_Passive%>%filter(!is.na(WealthCat))
## comparing poor vs. average, and rich vs. average  (average = 1; poor = 2; rich = 3)

#Pm#
Wealth_GEE_Pm_PD<-geeglm(Pm ~ as.factor(WealthCat), 
                         data = AD_Passive_WealthCatNoMiss, 
                         id = Subject_ID, 
                         family = binomial(link="identity"),
                         corstr = "ex")
summary(Wealth_GEE_Pm_PD)

Wealth_GEE_Pm_PR<-geeglm(Pm ~ as.factor(WealthCat), 
                         data = AD_Passive_WealthCatNoMiss, 
                         id = Subject_ID, 
                         family = binomial(link="log"),
                         corstr = "ex")
summary(Wealth_GEE_Pm_PR)


#Po#
Wealth_GEE_Po_PD<-geeglm(Po ~ as.factor(WealthCat), 
                         data = AD_Passive_WealthCatNoMiss, 
                         id = Subject_ID, 
                         family = binomial(link="identity"),
                         corstr = "ex")
summary(Wealth_GEE_Po_PD)

Wealth_GEE_Po_PR<-geeglm(Po ~ as.factor(WealthCat), 
                         data = AD_Passive_WealthCatNoMiss, 
                         id = Subject_ID, 
                         family = binomial(link="log"),
                         corstr = "ex")
summary(Wealth_GEE_Po_PR)

#Pf#
Wealth_GEE_Pf_PD<-geeglm(Pf ~ as.factor(WealthCat), 
                         data = AD_Passive_WealthCatNoMiss, 
                         id = Subject_ID, 
                         family = binomial(link="identity"),
                         corstr = "ex")
summary(Wealth_GEE_Pf_PD)

Wealth_GEE_Pf_PR<-geeglm(Pf ~ as.factor(WealthCat), 
                         data = AD_Passive_WealthCatNoMiss, 
                         id = Subject_ID, 
                         family = binomial(link="log"),
                         corstr = "ex")
summary(Wealth_GEE_Pf_PR)


##Outputing GEE Results: Estimate + 95% CI from Robust Sandwich Estimator
#Prevalence Difference 
Coeff_Passive_Wealth_Pm_PD<-tidy(Wealth_GEE_Pm_PD, exponentiate=FALSE, conf.int=TRUE)%>%dplyr::mutate(Ifxn="Pm")%>%filter(!term=="(Intercept)")
Coeff_Passive_Wealth_Po_PD<-tidy(Wealth_GEE_Po_PD, exponentiate=FALSE, conf.int=TRUE)%>%dplyr::mutate(Ifxn="Po")%>%filter(!term=="(Intercept)")
Coeff_Passive_Wealth_Pf_PD<-tidy(Wealth_GEE_Pf_PD, exponentiate=FALSE, conf.int=TRUE)%>%dplyr::mutate(Ifxn="Pf")%>%filter(!term=="(Intercept)")

#Prevalence Ratios 
Coeff_Passive_Wealth_Pm_PR<-tidy(Wealth_GEE_Pm_PR, exponentiate=TRUE, conf.int=TRUE)%>%dplyr::mutate(Ifxn="Pm")%>%filter(!term=="(Intercept)")
Coeff_Passive_Wealth_Po_PR<-tidy(Wealth_GEE_Po_PR, exponentiate=TRUE, conf.int=TRUE)%>%dplyr::mutate(Ifxn="Po")%>%filter(!term=="(Intercept)")
Coeff_Passive_Wealth_Pf_PR<-tidy(Wealth_GEE_Pf_PR, exponentiate=TRUE, conf.int=TRUE)%>%dplyr::mutate(Ifxn="Pf")%>%filter(!term=="(Intercept)")



##
##FEVER CATEGORY (No Fever = Ref)
##
AD_Passive_FeverNoMiss<-AD_Passive%>%filter(!is.na(Feverpos))

addmargins(table(AD_Passive_FeverNoMiss$Feverpos, AD_Passive_FeverNoMiss$Pm, useNA = "always"))
addmargins(table(AD_Passive_FeverNoMiss$Feverpos, AD_Passive_FeverNoMiss$Po, useNA = "always"))

#Pm#
Fever_GEE_Pm_PD<-geeglm(Pm ~ Feverpos, 
                        data = AD_Passive_FeverNoMiss, 
                        id = Subject_ID, 
                        family = binomial(link="identity"),
                        corstr = "ex")
summary(Fever_GEE_Pm_PD)

Fever_GEE_Pm_PR<-geeglm(Pm ~ Feverpos, 
                        data = AD_Passive_FeverNoMiss, 
                        id = Subject_ID, 
                        family = binomial(link="log"),
                        corstr = "ex")
summary(Fever_GEE_Pm_PR)


#Po#
Fever_GEE_Po_PD<-geeglm(Po ~ Feverpos, 
                        data = AD_Passive_FeverNoMiss, 
                        id = Subject_ID, 
                        family = binomial(link="identity"),
                        corstr = "ex")
summary(Fever_GEE_Po_PD)

Fever_GEE_Po_PR<-geeglm(Po ~ Feverpos, 
                        data = AD_Passive_FeverNoMiss, 
                        id = Subject_ID, 
                        family = binomial(link="log"),
                        corstr = "ex")
summary(Fever_GEE_Po_PR)

#Pf#
Fever_GEE_Pf_PD<-geeglm(Pf ~ Feverpos, 
                        data = AD_Passive_FeverNoMiss, 
                        id = Subject_ID, 
                        family = binomial(link="identity"),
                        corstr = "ex")
summary(Fever_GEE_Pf_PD)

Fever_GEE_Pf_PR<-geeglm(Pf ~ Feverpos, 
                        data = AD_Passive_FeverNoMiss, 
                        id = Subject_ID, 
                        family = binomial(link="log"),
                        corstr = "ex")
summary(Fever_GEE_Pf_PR)

##Outputing GEE Results: Estimate + 95% CI from Robust Sandwich Estimator
#Prevalence Differences 
Coeff_Passive_Fever_Pm_PD<-tidy(Fever_GEE_Pm_PD, exponentiate=FALSE, conf.int=TRUE)%>%dplyr::mutate(Ifxn="Pm")%>%filter(!term=="(Intercept)")
Coeff_Passive_Fever_Po_PD<-tidy(Fever_GEE_Po_PD, exponentiate=FALSE, conf.int=TRUE)%>%dplyr::mutate(Ifxn="Po")%>%filter(!term=="(Intercept)")
Coeff_Passive_Fever_Pf_PD<-tidy(Fever_GEE_Pf_PD, exponentiate=FALSE, conf.int=TRUE)%>%dplyr::mutate(Ifxn="Pf")%>%filter(!term=="(Intercept)")

#Prevalence Ratios 
Coeff_Passive_Fever_Pm_PR<-tidy(Fever_GEE_Pm_PR, exponentiate=TRUE, conf.int=TRUE)%>%dplyr::mutate(Ifxn="Pm")%>%filter(!term=="(Intercept)")
Coeff_Passive_Fever_Po_PR<-tidy(Fever_GEE_Po_PR, exponentiate=TRUE, conf.int=TRUE)%>%dplyr::mutate(Ifxn="Po")%>%filter(!term=="(Intercept)")
Coeff_Passive_Fever_Pf_PR<-tidy(Fever_GEE_Pf_PR, exponentiate=TRUE, conf.int=TRUE)%>%dplyr::mutate(Ifxn="Pf")%>%filter(!term=="(Intercept)")




write.table(Coeff_Passive_Fever_Pm_PD, "clipboard", sep="\t", col.names = F )
write.table(Coeff_Passive_Fever_Po_PD, "clipboard", sep="\t", col.names = F )
write.table(Coeff_Passive_Fever_Pf_PD, "clipboard", sep="\t", col.names = F )

write.table(Coeff_Passive_Fever_Pm_PR, "clipboard", sep="\t", col.names = F )
write.table(Coeff_Passive_Fever_Po_PR, "clipboard", sep="\t", col.names = F )
write.table(Coeff_Passive_Fever_Pf_PR, "clipboard", sep="\t", col.names = F )



##
##RDT-positive CATEGORY (Not RDT Positive = Ref)
##
AD_Passive_RDTNoMiss<-AD_Passive%>%filter(!is.na(RDTpos))

#Pm#
RDT_GEE_Pm_PD<-geeglm(Pm ~ RDTpos, 
                      data = AD_Passive_RDTNoMiss, 
                      id = Subject_ID, 
                      family = binomial(link="identity"),
                      corstr = "ex")
summary(RDT_GEE_Pm_PD)

RDT_GEE_Pm_PR<-geeglm(Pm ~ RDTpos, 
                      data = AD_Passive_RDTNoMiss, 
                      id = Subject_ID, 
                      family = binomial(link="log"),
                      corstr = "ex")
summary(RDT_GEE_Pm_PR)


#Po#
RDT_GEE_Po_PD<-geeglm(Po ~ RDTpos, 
                      data = AD_Passive_RDTNoMiss, 
                      id = Subject_ID, 
                      family = binomial(link="identity"),
                      corstr = "ex")
summary(RDT_GEE_Po_PD)

RDT_GEE_Po_PR<-geeglm(Po ~ RDTpos, 
                      data = AD_Passive_RDTNoMiss, 
                      id = Subject_ID, 
                      family = binomial(link="log"),
                      corstr = "ex")
summary(RDT_GEE_Po_PR)

#Pf#
RDT_GEE_Pf_PD<-geeglm(Pf ~ RDTpos, 
                      data = AD_Passive_RDTNoMiss, 
                      id = Subject_ID, 
                      family = binomial(link="identity"),
                      corstr = "ex")
summary(RDT_GEE_Pf_PD)

RDT_GEE_Pf_PR<-geeglm(Pf ~ RDTpos, 
                      data = AD_Passive_RDTNoMiss, 
                      id = Subject_ID, 
                      family = binomial(link="log"),
                      corstr = "ex")
summary(RDT_GEE_Pf_PR)

##Outputing GEE Results: Estimate + 95% CI from Robust Sandwich Estimator
#Prevalence Differences 
Coeff_Passive_RDT_Pm_PD<-tidy(RDT_GEE_Pm_PD, exponentiate=FALSE, conf.int=TRUE)%>%dplyr::mutate(Ifxn="Pm")%>%filter(!term=="(Intercept)")
Coeff_Passive_RDT_Po_PD<-tidy(RDT_GEE_Po_PD, exponentiate=FALSE, conf.int=TRUE)%>%dplyr::mutate(Ifxn="Po")%>%filter(!term=="(Intercept)")
Coeff_Passive_RDT_Pf_PD<-tidy(RDT_GEE_Pf_PD, exponentiate=FALSE, conf.int=TRUE)%>%dplyr::mutate(Ifxn="Pf")%>%filter(!term=="(Intercept)")

#Prevalence Ratios 
Coeff_Passive_RDT_Pm_PR<-tidy(RDT_GEE_Pm_PR, exponentiate=TRUE, conf.int=TRUE)%>%dplyr::mutate(Ifxn="Pm")%>%filter(!term=="(Intercept)")
Coeff_Passive_RDT_Po_PR<-tidy(RDT_GEE_Po_PR, exponentiate=TRUE, conf.int=TRUE)%>%dplyr::mutate(Ifxn="Po")%>%filter(!term=="(Intercept)")
Coeff_Passive_RDT_Pf_PR<-tidy(RDT_GEE_Pf_PR, exponentiate=TRUE, conf.int=TRUE)%>%dplyr::mutate(Ifxn="Pf")%>%filter(!term=="(Intercept)")



##
##P.falciparum Co-infection (Neg Pf = Ref)
##
AD_Passive_PfNoMiss<-AD_Passive%>%filter(!is.na(Pf))
#Pm#
Pf_GEE_Pm_PD<-geeglm(Pm ~ Pf, 
                     data = AD_Passive_PfNoMiss, 
                     id = Subject_ID, 
                     family = binomial(link="identity"),
                     corstr = "ex")
summary(Pf_GEE_Pm_PD)

Pf_GEE_Pm_PR<-geeglm(Pm ~ Pf, 
                     data = AD_Passive_PfNoMiss, 
                     id = Subject_ID, 
                     family = binomial(link="log"),
                     corstr = "ex")
summary(Pf_GEE_Pm_PR)

#Po#
Pf_GEE_Po_PD<-geeglm(Po ~ Pf, 
                     data = AD_Passive_PfNoMiss, 
                     id = Subject_ID, 
                     family = binomial(link="identity"),
                     corstr = "ex")
summary(Pf_GEE_Po_PD)

Pf_GEE_Po_PR<-geeglm(Po ~ Pf, 
                     data = AD_Passive_PfNoMiss, 
                     id = Subject_ID, 
                     family = binomial(link="log"),
                     corstr = "ex")
summary(Pf_GEE_Po_PR)


##Outputing GEE Results: Estimate + 95% CI from Robust Sandwich Estimator
#Prevalence DIfferences 
Coeff_Passive_Pfcoinfec_Pm_PD<-tidy(Pf_GEE_Pm_PD, exponentiate=FALSE, conf.int=TRUE)%>%dplyr::mutate(Ifxn="Pm")%>%filter(!term=="(Intercept)")
Coeff_Passive_Pfcoinfec_Po_PD<-tidy(Pf_GEE_Po_PD, exponentiate=FALSE, conf.int=TRUE)%>%dplyr::mutate(Ifxn="Po")%>%filter(!term=="(Intercept)")

#Prevalence Ratios 
Coeff_Passive_Pfcoinfec_Pm_PR<-tidy(Pf_GEE_Pm_PR, exponentiate=TRUE, conf.int=TRUE)%>%dplyr::mutate(Ifxn="Pm")%>%filter(!term=="(Intercept)")
Coeff_Passive_Pfcoinfec_Po_PR<-tidy(Pf_GEE_Po_PR, exponentiate=TRUE, conf.int=TRUE)%>%dplyr::mutate(Ifxn="Po")%>%filter(!term=="(Intercept)")



##
##SEASONALITY CATEGORY (Rainy Season = Ref)
##
AD_Passive_SeasonNoMiss<-AD_Passive%>%filter(!is.na(season_dry))
#Pm#
DrySeason_GEE_Pm_PD<-geeglm(Pm ~ season_dry, 
                            data = AD_Passive_SeasonNoMiss, 
                            id = Subject_ID, 
                            family = binomial(link="identity"),
                            corstr = "ex")
summary(DrySeason_GEE_Pm_PD)

DrySeason_GEE_Pm_PR<-geeglm(Pm ~ season_dry, 
                            data = AD_Passive_SeasonNoMiss, 
                            id = Subject_ID, 
                            family = binomial(link="log"),
                            corstr = "ex")
summary(DrySeason_GEE_Pm_PR)

#Po#
DrySeason_GEE_Po_PD<-geeglm(Po ~ season_dry, 
                            data = AD_Passive_SeasonNoMiss, 
                            id = Subject_ID, 
                            family = binomial(link="identity"),
                            corstr = "ex")
summary(DrySeason_GEE_Po_PD)

DrySeason_GEE_Po_PR<-geeglm(Po ~ season_dry, 
                            data = AD_Passive_SeasonNoMiss, 
                            id = Subject_ID, 
                            family = binomial(link="log"),
                            corstr = "ex")
summary(DrySeason_GEE_Po_PR)

#Pf#
DrySeason_GEE_Pf_PD<-geeglm(Pf ~ season_dry, 
                            data = AD_Passive_SeasonNoMiss, 
                            id = Subject_ID, 
                            family = binomial(link="identity"),
                            corstr = "ex")
summary(DrySeason_GEE_Pf_PD)

DrySeason_GEE_Pf_PR<-geeglm(Pf ~ season_dry, 
                            data = AD_Passive_SeasonNoMiss, 
                            id = Subject_ID, 
                            family = binomial(link="log"),
                            corstr = "ex")
summary(DrySeason_GEE_Pf_PR)

##Outputing GEE Results: Estimate + 95% CI from Robust Sandwich Estimator
#Prevalecne Differneces 
Coeff_Passive_DrySeason_Pm_PD<-tidy(DrySeason_GEE_Pm_PD, exponentiate=FALSE, conf.int=TRUE)%>%dplyr::mutate(Ifxn="Pm")%>%filter(!term=="(Intercept)")
Coeff_Passive_DrySeason_Po_PD<-tidy(DrySeason_GEE_Po_PD, exponentiate=FALSE, conf.int=TRUE)%>%dplyr::mutate(Ifxn="Po")%>%filter(!term=="(Intercept)")
Coeff_Passive_DrySeason_Pf_PD<-tidy(DrySeason_GEE_Pf_PD, exponentiate=FALSE, conf.int=TRUE)%>%dplyr::mutate(Ifxn="Pf")%>%filter(!term=="(Intercept)")

#Prevalecne Ratios 
Coeff_Passive_DrySeason_Pm_PR<-tidy(DrySeason_GEE_Pm_PR, exponentiate=TRUE, conf.int=TRUE)%>%dplyr::mutate(Ifxn="Pm")%>%filter(!term=="(Intercept)")
Coeff_Passive_DrySeason_Po_PR<-tidy(DrySeason_GEE_Po_PR, exponentiate=TRUE, conf.int=TRUE)%>%dplyr::mutate(Ifxn="Po")%>%filter(!term=="(Intercept)")
Coeff_Passive_DrySeason_Pf_PR<-tidy(DrySeason_GEE_Pf_PR, exponentiate=TRUE, conf.int=TRUE)%>%dplyr::mutate(Ifxn="Pf")%>%filter(!term=="(Intercept)")



####
##Anemia(No Anemia= Ref)

##
AD_Passive_AnemiaNoMiss<-AD_Passive%>%filter(!is.na(anemia_WHO))


addmargins(table(AD_Passive$anemia_WHO, AD_Passive$Pm, useNA="always"))
addmargins(table(AD_Passive$anemia_WHO, AD_Passive$Po, useNA="always"))

addmargins(table(AD_Passive$anemia_WHO, AD_Passive$Pm_Species_Mono, useNA="always"))
addmargins(table(AD_Passive$anemia_WHO, AD_Passive$Po_Species_Mono, useNA="always"))

addmargins(table(AD_Passive_AnemiaNoMiss$anemia_WHO, AD_Passive_AnemiaNoMiss$Pm, useNA="always"))
addmargins(table(AD_Passive_AnemiaNoMiss$anemia_WHO, AD_Passive_AnemiaNoMiss$Po, useNA="always"))

AD_Passive_AnemiaNoMiss<-AD_Passive_AnemiaNoMiss%>%mutate(Anemia_ModSevere=ifelse(Anemia_None==1|Anemia_Mild==1, 0, 
                                                                                  ifelse(Anemia_Moderate==1|Anemia_Severe==1, 1, NA)))%>%
  mutate(Anemia_Any = ifelse(Anemia_Mild==1 | Anemia_Moderate==1|Anemia_Severe==1, 1,
                             ifelse(Anemia_None==1, 0, NA)))

addmargins(table(AD_Passive_AnemiaNoMiss$Anemia_ModSevere, AD_Passive_AnemiaNoMiss$Pm, useNA="always"))
addmargins(table(AD_Passive_AnemiaNoMiss$Anemia_ModSevere, AD_Passive_AnemiaNoMiss$Po, useNA="always"))
addmargins(table(AD_Passive_AnemiaNoMiss$Anemia_Any, AD_Passive_AnemiaNoMiss$Pm, useNA="always"))
addmargins(table(AD_Passive_AnemiaNoMiss$Anemia_Any, AD_Passive_AnemiaNoMiss$Po, useNA="always"))


#Pm#
#ModSevere vs. Mild/None

Anemia_GEE_Passive_Pm_PD<-geeglm(Pm ~ Anemia_ModSevere, 
                                 data = AD_Passive_AnemiaNoMiss, 
                                 id = Subject_ID, 
                                 family = binomial(link="identity"),
                                 corstr = "ex")
summary(Anemia_GEE_Passive_Pm_PD)

Anemia_GEE_Passive_Pm_PR<-geeglm(Pm ~ Anemia_ModSevere, 
                                 data = AD_Passive_AnemiaNoMiss, 
                                 id = Subject_ID, 
                                 family = binomial(link="log"),
                                 corstr = "ex")
summary(Anemia_GEE_Passive_Pm_PR)

#Any Anemia vs. No Anemia
Anemia_GEE_Passive_Pm2_PD<-geeglm(Pm ~ Anemia_Any, 
                                  data = AD_Passive_AnemiaNoMiss, 
                                  id = Subject_ID, 
                                  family = binomial(link="identity"),
                                  corstr = "ex")
summary(Anemia_GEE_Passive_Pm2_PD)

Anemia_GEE_Passive_Pm2_PR<-geeglm(Pm ~ Anemia_Any, 
                                  data = AD_Passive_AnemiaNoMiss, 
                                  id = Subject_ID, 
                                  family = binomial(link="log"),
                                  corstr = "ex")
summary(Anemia_GEE_Passive_Pm2_PR)                                      


##Now with clustering accounted for
Anemia_GEE_Passive_Pm2_PD2 <- glmer(Pm ~ Anemia_Any + (1 | Subject_ID) + (1 | Household),
                                    data = AD_Passive_AnemiaNoMiss,
                                    family = binomial(link = "log"))
summary(Anemia_GEE_Passive_Pm2_PD2)


#Po#
#ModSevere vs. Mild/None

Anemia_GEE_Passive_Po_PD<-geeglm(Po ~ Anemia_ModSevere, 
                                 data = AD_Passive_AnemiaNoMiss, 
                                 id = Subject_ID, 
                                 family = binomial(link="identity"),
                                 corstr = "ex")
summary(Anemia_GEE_Passive_Po_PD)

Anemia_GEE_Passive_Po_PR<-geeglm(Po ~ Anemia_ModSevere, 
                                 data = AD_Passive_AnemiaNoMiss, 
                                 id = Subject_ID, 
                                 family = binomial(link="log"),
                                 corstr = "ex")
summary(Anemia_GEE_Passive_Po_PR)

#Any Anemia vs. No Anemia
Anemia_GEE_Passive_Po2_PD<-geeglm(Po ~ Anemia_Any, 
                                  data = AD_Passive_AnemiaNoMiss, 
                                  id = Subject_ID, 
                                  family = binomial(link="identity"),
                                  corstr = "ex")
summary(Anemia_GEE_Passive_Po2_PD)

Anemia_GEE_Passive_Po2_PR<-geeglm(Po ~ Anemia_Any, 
                                  data = AD_Passive_AnemiaNoMiss, 
                                  id = Subject_ID, 
                                  family = binomial(link="log"),
                                  corstr = "ex")
summary(Anemia_GEE_Passive_Po2_PR)    

#Pf#
#ModSevere vs. Mild/None

Anemia_GEE_Passive_Pf_PD<-geeglm(Pf ~ Anemia_ModSevere, 
                                 data = AD_Passive_AnemiaNoMiss, 
                                 id = Subject_ID, 
                                 family = binomial(link="identity"),
                                 corstr = "ex")
summary(Anemia_GEE_Passive_Pf_PD)

Anemia_GEE_Passive_Pf_PR<-geeglm(Pf ~ Anemia_ModSevere, 
                                 data = AD_Passive_AnemiaNoMiss, 
                                 id = Subject_ID, 
                                 family = binomial(link="log"),
                                 corstr = "ex")
summary(Anemia_GEE_Passive_Pf_PR)

#Any Anemia vs. No Anemia
Anemia_GEE_Passive_Pf2_PD<-geeglm(Pf ~ Anemia_Any, 
                                  data = AD_Passive_AnemiaNoMiss, 
                                  id = Subject_ID, 
                                  family = binomial(link="identity"),
                                  corstr = "ex")
summary(Anemia_GEE_Passive_Pf2_PD)

Anemia_GEE_Passive_Pf2_PR<-geeglm(Pf ~ Anemia_Any, 
                                  data = AD_Passive_AnemiaNoMiss, 
                                  id = Subject_ID, 
                                  family = binomial(link="log"),
                                  corstr = "ex")
summary(Anemia_GEE_Passive_Pf2_PR)      



##Outputing GEE Results: Estimate + 95% CI from Robust Sandwich Estimator
#Moderate/Severe vs. Mild / No Anemia
#Prevalence Differneces 
Coeff_Passive_Anemia_Pm_PD<-tidy(Anemia_GEE_Passive_Pm_PD, exponentiate=FALSE, conf.int=TRUE)%>%dplyr::mutate(Ifxn="Pm")%>%filter(!term=="(Intercept)")
Coeff_Passive_Anemia_Po_PD<-tidy(Anemia_GEE_Passive_Po_PD, exponentiate=FALSE, conf.int=TRUE)%>%dplyr::mutate(Ifxn="Po")%>%filter(!term=="(Intercept)")
Coeff_Passive_Anemia_Pf_PD<-tidy(Anemia_GEE_Passive_Pf_PD, exponentiate=FALSE, conf.int=TRUE)%>%dplyr::mutate(Ifxn="Pf")%>%filter(!term=="(Intercept)")

#Prevalence Ratios 
Coeff_Passive_Anemia_Pm_PR<-tidy(Anemia_GEE_Passive_Pm_PR, exponentiate=TRUE, conf.int=TRUE)%>%dplyr::mutate(Ifxn="Pm")%>%filter(!term=="(Intercept)")
Coeff_Passive_Anemia_Po_PR<-tidy(Anemia_GEE_Passive_Po_PR, exponentiate=TRUE, conf.int=TRUE)%>%dplyr::mutate(Ifxn="Po")%>%filter(!term=="(Intercept)")
Coeff_Passive_Anemia_Pf_PR<-tidy(Anemia_GEE_Passive_Pf_PR, exponentiate=TRUE, conf.int=TRUE)%>%dplyr::mutate(Ifxn="Pf")%>%filter(!term=="(Intercept)")


#Any Anemia vs. No Anemia 
#Prevalence Differneces 
Coeff_Passive_Anemia_Pm2_PD<-tidy(Anemia_GEE_Passive_Pm2_PD, exponentiate=FALSE, conf.int=TRUE)%>%dplyr::mutate(Ifxn="Pm")%>%filter(!term=="(Intercept)")
Coeff_Passive_Anemia_Po2_PD<-tidy(Anemia_GEE_Passive_Po2_PD, exponentiate=FALSE, conf.int=TRUE)%>%dplyr::mutate(Ifxn="Po")%>%filter(!term=="(Intercept)")
Coeff_Passive_Anemia_Pf2_PD<-tidy(Anemia_GEE_Passive_Pf2_PD, exponentiate=FALSE, conf.int=TRUE)%>%dplyr::mutate(Ifxn="Pf")%>%filter(!term=="(Intercept)")

#Prevalence Ratios 
Coeff_Passive_Anemia_Pm2_PR<-tidy(Anemia_GEE_Passive_Pm2_PR, exponentiate=TRUE, conf.int=TRUE)%>%dplyr::mutate(Ifxn="Pm")%>%filter(!term=="(Intercept)")
Coeff_Passive_Anemia_Po2_PR<-tidy(Anemia_GEE_Passive_Po2_PR, exponentiate=TRUE, conf.int=TRUE)%>%dplyr::mutate(Ifxn="Po")%>%filter(!term=="(Intercept)")
Coeff_Passive_Anemia_Pf2_PR<-tidy(Anemia_GEE_Passive_Pf2_PR, exponentiate=TRUE, conf.int=TRUE)%>%dplyr::mutate(Ifxn="Pf")%>%filter(!term=="(Intercept)")



######
#Passive Visits:  Stacking all GEE Output for Estimate (95% CI) Plots

#Prevalence Differences

Passive_GEE_Output_PD<-rbind(
  Coeff_Passive_Age_Pm_PD, Coeff_Passive_Age_Po_PD, Coeff_Passive_Age_Pf_PD,
  Coeff_Passive_Sex_Pm_PD, Coeff_Passive_Sex_Po_PD, Coeff_Passive_Sex_Pf_PD,
  Coeff_Passive_HealthArea_Pm_PD, Coeff_Passive_HealthArea_Po_PD, Coeff_Passive_HealthArea_Pf_PD,
  Coeff_Passive_Wealth_Pm_PD, Coeff_Passive_Wealth_Po_PD, Coeff_Passive_Wealth_Pf_PD,
  Coeff_Passive_Fever_Pm_PD, Coeff_Passive_Fever_Po_PD, Coeff_Passive_Fever_Pf_PD,
  Coeff_Passive_RDT_Pm_PD, Coeff_Passive_RDT_Po_PD, Coeff_Passive_RDT_Pf_PD,
  Coeff_Passive_Pfcoinfec_Pm_PD, Coeff_Passive_Pfcoinfec_Po_PD,
  Coeff_Passive_DrySeason_Pm_PD, Coeff_Passive_DrySeason_Po_PD, Coeff_Passive_DrySeason_Pf_PD,
  Coeff_Passive_Anemia_Pm2_PD, Coeff_Passive_Anemia_Po2_PD, Coeff_Passive_Anemia_Pf2_PD,
  Coeff_Passive_Anemia_Pm_PD, Coeff_Passive_Anemia_Po_PD, Coeff_Passive_Anemia_Pf_PD
)

write.table(Passive_GEE_Output_PD, "clipboard", sep="\t", col.names = T )



#Prevalence Ratios

Passive_GEE_Output_PR<-rbind(
  Coeff_Passive_Age_Pm_PR, Coeff_Passive_Age_Po_PR, Coeff_Passive_Age_Pf_PR,
  Coeff_Passive_Sex_Pm_PR, Coeff_Passive_Sex_Po_PR, Coeff_Passive_Sex_Pf_PR,
  Coeff_Passive_HealthArea_Pm_PR, Coeff_Passive_HealthArea_Po_PR, Coeff_Passive_HealthArea_Pf_PR,
  #Coeff_Passive_Village_Pm_PD, Coeff_Passive_Village_Po_PD, Coeff_Passive_Village_Pf_PD,
  Coeff_Passive_Wealth_Pm_PR, Coeff_Passive_Wealth_Po_PR, Coeff_Passive_Wealth_Pf_PR,
  Coeff_Passive_Fever_Pm_PR, Coeff_Passive_Fever_Po_PR, Coeff_Passive_Fever_Pf_PR,
  Coeff_Passive_RDT_Pm_PR, Coeff_Passive_RDT_Po_PR, Coeff_Passive_RDT_Pf_PR,
  Coeff_Passive_Pfcoinfec_Pm_PR, Coeff_Passive_Pfcoinfec_Po_PR,
  Coeff_Passive_DrySeason_Pm_PR, Coeff_Passive_DrySeason_Po_PR, Coeff_Passive_DrySeason_Pf_PR,
  Coeff_Passive_Anemia_Pm2_PR, Coeff_Passive_Anemia_Po2_PR, Coeff_Passive_Anemia_Pf2_PR,
  Coeff_Passive_Anemia_Pm_PR, Coeff_Passive_Anemia_Po_PR, Coeff_Passive_Anemia_Pf_PR
)

write.table(Passive_GEE_Output_PR, "clipboard", sep="\t", col.names = T )




#############
##  Plotting Prevalence Differences and 95% CIs by Population

Passive_GEE_Output_PD$Ifxn <- factor(Passive_GEE_Output_PD$Ifxn, levels=c("Pm", "Po", "Pf"))
Passive_GEE_Output_PD$Ifxn <- factor(Passive_GEE_Output_PD$Ifxn, levels=c("Pm", "Po", "Pf"))

Passive_GEE_Output_PD$term <- recode_factor(Passive_GEE_Output_PD$term, 
                                            "relevel(as.factor(AgeCat_Visit), ref = \"3\")1" = "Age <5 vs 15+", 
                                            "relevel(as.factor(AgeCat_Visit), ref = \"3\")2" = "Age 5-14 vs 15+",
                                            "Sex" = "Sex (F vs M)",
                                            "as.factor(HealthArea)2" = "Rural vs Urban", 
                                            "as.factor(HealthArea)3" = "Peri-urban vs Urban", 
                                            "as.factor(WealthCat)2" = "Poor vs Avg. Wealth",
                                            "as.factor(WealthCat)3" = "Wealthy vs Avg. Wealth",
                                            "season_dry" =  "Dry vs Rainy Season",
                                            "RDTpos" = "RDT+ vs RDT-",
                                            "Pf" = "Pf coinfection (Y vs N)",
                                            "Feverpos" = "Fever (Y vs N)",
                                            "Anemia_Any" = "Anemic vs Not Anemic",
                                            "Anemia_ModSevere" = "Mod./Severe vs Mild/No Anemia")

Passive_GEE_Output_PR$Ifxn <- factor(Passive_GEE_Output_PR$Ifxn, levels=c("Pm", "Po", "Pf"))
Passive_GEE_Output_PR$Ifxn <- factor(Passive_GEE_Output_PR$Ifxn, levels=c("Pm", "Po", "Pf"))

Passive_GEE_Output_PR$term <- recode_factor(Passive_GEE_Output_PR$term, 
                                            "relevel(as.factor(AgeCat_Visit), ref = \"3\")1" = "Age <5 vs 15+", 
                                            "relevel(as.factor(AgeCat_Visit), ref = \"3\")2" = "Age 5-14 vs 15+",
                                            "Sex" = "Sex (F vs M)",
                                            "as.factor(HealthArea)2" = "Rural vs Urban", 
                                            "as.factor(HealthArea)3" = "Peri-urban vs Urban", 
                                            "as.factor(WealthCat)2" = "Poor vs Avg. Wealth",
                                            "as.factor(WealthCat)3" = "Wealthy vs Avg. Wealth",
                                            "season_dry" =  "Dry vs Rainy Season",
                                            "RDTpos" = "RDT+ vs RDT-",
                                            "Pf" = "Pf coinfection (Y vs N)",
                                            "Feverpos" = "Fever (Y vs N)",
                                            "Anemia_Any" = "Anemic vs Not Anemic", 
                                            "Anemia_ModSevere" = "Mod./Severe vs Mild/No Anemia")



##checking the ordering of variables for gggplotting: 
Passive_GEE_Output_PD$term %>%levels()
Passive_GEE_Output_PR$term %>%levels()
Passive_GEE_Output_PD$term %>%levels()
#  Total_GEE_Output_PD$term %>%levels()


#set order of levels to appear along y-axis
Passive_GEE_Output_PD <- Passive_GEE_Output_PD%>%
  mutate(term = term %>% 
           fct_relevel("Wealthy vs Avg. Wealth",
                       "Poor vs Avg. Wealth",
                       "Dry vs Rainy Season",
                       "Mod./Severe vs Mild/No Anemia",
                       "Anemic vs Not Anemic",
                       "Fever (Y vs N)",
                       "Pf coinfection (Y vs N)",
                       "RDT+ vs RDT-",
                       "Peri-urban vs Urban",
                       "Rural vs Urban",
                       "Sex (F vs M)",
                       "Age 5-14 vs 15+",
                       "Age <5 vs 15+", 
           ))

Passive_GEE_Output_PR <- Passive_GEE_Output_PR%>%
  mutate(term = term %>% 
           fct_relevel("Wealthy vs Avg. Wealth",
                       "Poor vs Avg. Wealth",
                       "Dry vs Rainy Season",
                       "Mod./Severe vs Mild/No Anemia",
                       "Anemic vs Not Anemic",
                       "Fever (Y vs N)",
                       "Pf coinfection (Y vs N)",
                       "RDT+ vs RDT-",
                       "Peri-urban vs Urban",
                       "Rural vs Urban",
                       "Sex (F vs M)",
                       "Age 5-14 vs 15+",
                       "Age <5 vs 15+", 
           ))



PassivePop_PD <-ggplot(Passive_GEE_Output_PD, (aes(x=estimate, y=term, col=Ifxn, fill=Ifxn))) + 
  geom_errorbar(aes(xmin=conf.low, xmax=conf.high, col=Ifxn), size=0.7, width=0.6, position=position_dodge(width = 0.7))+
  geom_point(size=3, shape=21, colour="black", stroke = 0.8, position=position_dodge(width = 0.7)) +
  geom_vline(xintercept=0.0, lty=2)+
  scale_fill_manual(values=c("blue", "red", "orange"))+
  scale_color_manual(values=c("black", "black", "black"))+
  #          ggtitle("Survey-based population (n=1,565 participants across 5,682 visits)")+
  xlab("Prevalence Difference")+
  ylab("")+
  #labs(title = "Clinic Sub-Pop.",
  #     subtitle = "n=1,050 participants across 3,407 visits")+
  guides(fill = guide_legend(title = "Species"), col=FALSE)+
  theme(plot.title =element_text(size=11, color="black"), plot.subtitle = element_text(size=9.5), axis.text.x=element_text(size=9, color="black"), axis.text.y.left=element_text(size=10,  color="black"),  legend.text=element_text(size=9, color="black"), legend.title = element_text(size=10, color="black")
        #        , panel.background = element_rect(fill = "white", color = "black")
        #       , panel.grid.major.x = element_line(size = 0.3, linetype = 'solid', colour = "#D3D3D3"), panel.grid.minor.x = element_line(size = 0.2, linetype = 'solid', colour = "#D3D3D3"), panel.grid.major.y = element_blank()
        , legend.position = c(0.89, 0.88), legend.background = element_rect(size=0.3, linetype="solid", colour ="black")
        #          , legend.position = "none"
  )

plot(PassivePop_PD) 


##Now without Pf also for a zoomed version on just Pm and Po differences
Passive_GEE_Output_PD_NoPf<-Passive_GEE_Output_PD%>%filter(Ifxn!= "Pf")
PassivePop_PD_noPf<-ggplot(Passive_GEE_Output_PD_NoPf, (aes(x=estimate, y=term, col=Ifxn, fill=Ifxn)))+ 
  geom_errorbar(aes(xmin=conf.low, xmax=conf.high, col=Ifxn), size=0.5,  width=0.4, position=position_dodge(width = 0.3))+
  geom_point(size=3, shape=21, colour="black", stroke = 0.8, position=position_dodge(width = 0.3)) +
  geom_vline(xintercept=0.0, lty=2)+
  scale_fill_manual(values=c("blue", "red", "orange"))+
  scale_color_manual(values=c("black", "black", "black"))+
  guides(fill = guide_legend(title = "Species"), col=FALSE)+
  # ggtitle("Survey-based Pop. (N=1,565; 5,682 visits)")+
  xlab("Prevalence Difference")+
  ylab("")+
  #labs(title = "Clinic Sub-Pop.",
  #     subtitle = "n=1,050 participants across 3,407 visits")+
  #guides(fill = guide_legend(title = "Species"), col=FALSE)+
  theme(plot.title =element_text(size=11, color="black"), plot.subtitle = element_text(size=9.5), axis.text.x=element_text(size=9, color="black"), axis.text.y.left=element_text(size=10,  color="black"),  legend.text=element_text(size=9, color="black"), legend.title = element_text(size=10, color="black")
        #        , panel.background = element_rect(fill = "white", color = "black")
        #       , panel.grid.major.x = element_line(size = 0.3, linetype = 'solid', colour = "#D3D3D3"), panel.grid.minor.x = element_line(size = 0.2, linetype = 'solid', colour = "#D3D3D3"), panel.grid.major.y = element_blank()
        , legend.position = c(0.89, 0.88), legend.background = element_rect(size=0.3, linetype="solid", colour ="black")
        #          , legend.position = "none"
  )

#PassivePop_PD_noPf_2<-PassivePop_PD_noPf + expand_limits(x=c(-0.08, 0.08))
PassivePop_PD_noPf_2<-PassivePop_PD_noPf + xlim(-0.05, 0.075)
plot(PassivePop_PD_noPf_2) 



PassivePop_PR <-ggplot(Passive_GEE_Output_PR, (aes(x=estimate, y=term, col=Ifxn, fill=Ifxn))) + 
  geom_errorbar(aes(xmin=conf.low, xmax=conf.high, col=Ifxn), size=0.7, width=0.6, position=position_dodge(width = 0.7))+
  geom_point(size=4, shape=21, colour="black", stroke = 0.8, position=position_dodge(width = 0.7)) +
  geom_vline(xintercept=0.0, lty=2)+
  scale_fill_manual(values=c("blue", "red", "orange"))+
  scale_color_manual(values=c("black", "black", "black"))+
  #ggtitle("Survey-based population (n=1,565 participants across 5,682 visits)")+
  xlab("Prevalence Ratio")+
  ylab("")+
  labs(title = "Clinic Sub-Pop.",
       subtitle = "n=1,050 participants across 3,407 visits")+
  guides(fill = guide_legend(title = "Species"), col=FALSE)+
  theme(plot.title =element_text(size=16, color="black"), axis.title = element_text(size =14), axis.text.x=element_text(size=11, color="black"), axis.text.y.left=element_text(size=11,  color="black"), legend.text=element_text(size=14, color="black"), legend.title = element_text(size=14, color="black"))
plot(PassivePop_PR) 


##Now without Pf also for a zoomed version on just Pm and Po differences
Passive_GEE_Output_PR_NoPf<-Passive_GEE_Output_PR%>%filter(Ifxn!= "Pf")
PassivePop_PR_noPf<-ggplot(Passive_GEE_Output_PR_NoPf, (aes(x=estimate, y=term, col=Ifxn, fill=Ifxn))) + 
  geom_errorbar(aes(xmin=conf.low, xmax=conf.high, col=Ifxn), size=0.5,  width=0.4, position=position_dodge(width = 0.3))+
  geom_point(size=3, shape=21, colour="black", stroke = 0.8, position=position_dodge(width = 0.3)) +
  geom_vline(xintercept=0.0, lty=2)+
  scale_fill_manual(values=c("blue", "red", "orange"))+
  scale_color_manual(values=c("black", "black", "black"))+
  guides(fill = guide_legend(title = "Species"), col=FALSE)+
  # ggtitle("Survey-based Pop. (N=1,565; 5,682 visits)")+
  xlab("Prevalence Ratio")+
  ylab("")+
  labs(title = "Clinic Sub-Pop.",
       subtitle = "n=1,050 participants across 3,407 visits")+
  #guides(fill = guide_legend(title = "Species"), col=FALSE)+
  theme(plot.title =element_text(size=11, color="black"), plot.subtitle = element_text(size=9.5), axis.text.x=element_text(size=9, color="black"), axis.text.y.left=element_text(size=10,  color="black"),  legend.text=element_text(size=10, color="black"), legend.title = element_text(size=10, color="black"))
PassivePop_PR_noPf_2<-PassivePop_PR_noPf + expand_limits(x=c(-3, 6))
plot(PassivePop_PR_noPf_2) 




plot(ActivePop_PD_noPf_2)
plot(PassivePop_PD_noPf_2)


###Aggregating the non-pf Risk Factor Plots as 1 figure 
gridExtra::grid.arrange(ActivePop_PD_noPf_2, PassivePop_PD_noPf_2, ncol=2, nrow=1) 


gridExtra::grid.arrange(ActivePop_PD_2, PassivePop_PD, ncol=2, nrow=1) 









## TOTAL  Visit Prevalence Association 


##Soring data by Subject_ID and then by Visit number to ensure it's formatted correctly for the clustering analysis. 
AD_Total <- AD_Total[order(AD_Total$Subject_ID, AD_Total$Visit, AD_Total$Visit_date),]


##Crude prevalence (accounting for repeated testing;  not accounting for household clustering)

##Pm

##PD
CrudePrev_GEE_Pm_Total_Pd<-geeglm(Pm ~ 1, 
                                  data = AD_Total, 
                                  id = Subject_ID, 
                                  family = binomial(link="identity"),
                                  corstr = "ex")

summary(CrudePrev_GEE_Pm_Total_Pd)

##PR
CrudePrev_GEE_Pm_Total_PR<-geeglm(Pm ~ 1, 
                                  data = AD_Total, 
                                  id = Subject_ID, 
                                  family = binomial(link="log"),
                                  corstr = "ex")

summary(CrudePrev_GEE_Pm_Total_PR)


### Mixed model accounting for within-subject and household clustering
CrudePrev_ME_Pm <- lmer(Pm ~ 1 + (1 | Subject_ID) + (1 | Household)  ,
                        data = AD_Total,
                        family = binomial(link="identity"),
                        REML = FALSE)
summary(CrudePrev_ME_Pm)

CrudePrev_ME_Pm <- lmer(Pm ~ 1 + (1 | Subject_ID) + (1 | Household)  ,
                        data = AD_Total,
                        family = binomial(link="log"),
                        REML = FALSE)
summary(CrudePrev_ME_Pm)



##Po
CrudePrev_GEE_Po<-geeglm(Po ~ 1, 
                         data = AD_Total, 
                         id = Subject_ID, 
                         family = binomial(link="identity"),
                         corstr = "ex")
summary(CrudePrev_GEE_Po)


### Mixed model accounting for within-subject and household clustering
CrudePrev_ME_Po <- lmer(Po ~ 1 + (1 | Subject_ID) + (1 | Household),
                        data = AD_Total,
                        REML = FALSE)
summary(CrudePrev_ME_Po)



##Pf
CrudePrev_GEE_Pf<-geeglm(Pf ~ 1, 
                         data = AD_Total, 
                         id = Subject_ID, 
                         family = binomial(link="identity"),
                         corstr = "ex")
summary(CrudePrev_GEE_Pf)


### Mixed model accounting forwithin-subject and household clutering
CrudePrev_ME_Pf <- lmer(Pf ~ 1 + (1 | Subject_ID) + (1 | Household),
                        data = AD_Total,
                        REML = FALSE)
summary(CrudePrev_ME_Pf)






##
##AGE CATEGORY (Age >=15 years = Ref)
##
AD_Total_AgeNoMiss<-AD_Total%>%filter(!is.na(AgeCat_Visit))
addmargins(table(AD_Total_AgeNoMiss$AgeCat_Visit, AD_Total_AgeNoMiss$Pm))
addmargins(table(AD_Total$AgeCat_Visit, AD_Total$Pm))


#Pm#
Age_GEE_Pm_PD<-geeglm(Pm ~ relevel(as.factor(AgeCat_Visit), ref='3'), 
                      data = AD_Total_AgeNoMiss, 
                      id = Subject_ID, 
                      family = binomial(link="identity"),
                      corstr = "ex")
summary(Age_GEE_Pm_PD)

Age_GEE_Pm_PR<-geeglm(Pm ~ relevel(as.factor(AgeCat_Visit), ref='3'), 
                      data = AD_Total_AgeNoMiss, 
                      id = Subject_ID, 
                      family = binomial(link="log"),
                      corstr = "ex")
summary(Age_GEE_Pm_PR)



#Age_GEE_Pm<-geeglm(Pm ~ agecat_LT5 + agecat_5_14, 
#                   data = AD_Total_AgeNoMiss, 
#                   id = Subject_ID, 
#                   family = binomial(link="identity"),
#                   corstr = "ex")
#summary(Age_GEE_Pm)


#Po#

Age_GEE_Po_PD<-geeglm(Po ~ relevel(as.factor(AgeCat_Visit), ref='3'), 
                      data = AD_Total_AgeNoMiss, 
                      id = Subject_ID, 
                      family = binomial(link="identity"),
                      corstr = "ex")
summary(Age_GEE_Po_PD)

Age_GEE_Po_PR<-geeglm(Po ~ relevel(as.factor(AgeCat_Visit), ref='3'), 
                      data = AD_Total_AgeNoMiss, 
                      id = Subject_ID, 
                      family = binomial(link="log"),
                      corstr = "ex")
summary(Age_GEE_Po_PR)


#Age_GEE_Po<-geeglm(Po ~ agecat_LT5 + agecat_5_14, 
#                   data = AD_Total_AgeNoMiss, 
#                   id = Subject_ID, 
#                   family = binomial(link="identity"),
#                   corstr = "ex")
#summary(Age_GEE_Po)


#Pf#
Age_GEE_Pf_PD<-geeglm(Pf ~ relevel(as.factor(AgeCat_Visit), ref='3'), 
                      data = AD_Total_AgeNoMiss, 
                      id = Subject_ID, 
                      family = binomial(link="identity"),
                      corstr = "ex")
summary(Age_GEE_Pf_PD)

Age_GEE_Pf_PR<-geeglm(Pf ~ relevel(as.factor(AgeCat_Visit), ref='3'), 
                      data = AD_Total_AgeNoMiss, 
                      id = Subject_ID, 
                      family = binomial(link="log"),
                      corstr = "ex")
summary(Age_GEE_Pf_PR)


#Age_GEE_Pf<-geeglm(Pf ~ agecat_LT5 + agecat_5_14, 
#                   data = AD_Total_AgeNoMiss, 
#                   id = Subject_ID, 
#                   family = binomial(link="identity"),
#                   corstr = "ex")
#summary(Age_GEE_Pf)

##Outputing GEE Results: Estimate + 95% CI from Robust Sandwich Estimator
#Prevalence Differences 
Coeff_Total_Age_Pm_PD<-tidy(Age_GEE_Pm_PD, exponentiate=FALSE, conf.int=TRUE)%>%dplyr::mutate(Ifxn="Pm")%>%filter(!term=="(Intercept)")
Coeff_Total_Age_Po_PD<-tidy(Age_GEE_Po_PD, exponentiate=FALSE, conf.int=TRUE)%>%dplyr::mutate(Ifxn="Po")%>%filter(!term=="(Intercept)")
Coeff_Total_Age_Pf_PD<-tidy(Age_GEE_Pf_PD, exponentiate=FALSE, conf.int=TRUE)%>%dplyr::mutate(Ifxn="Pf")%>%filter(!term=="(Intercept)")

#Prevalence Ratios 
Coeff_Total_Age_Pm_PR<-tidy(Age_GEE_Pm_PR, exponentiate=TRUE, conf.int=TRUE)%>%dplyr::mutate(Ifxn="Pm")%>%filter(!term=="(Intercept)")
Coeff_Total_Age_Po_PR<-tidy(Age_GEE_Po_PR, exponentiate=TRUE, conf.int=TRUE)%>%dplyr::mutate(Ifxn="Po")%>%filter(!term=="(Intercept)")
Coeff_Total_Age_Pf_PR<-tidy(Age_GEE_Pf_PR, exponentiate=TRUE, conf.int=TRUE)%>%dplyr::mutate(Ifxn="Pf")%>%filter(!term=="(Intercept)")



##
##SEX CATEGORY (Male = Ref)
##
AD_Total_SexNoMiss<-AD_Total%>%filter(!is.na(Sex))

#Pm#
Sex_GEE_Pm_PD<-geeglm(Pm ~ Sex, 
                      data = AD_Total_SexNoMiss, 
                      id = Subject_ID, 
                      family = binomial(link="identity"),
                      corstr = "ex")
summary(Sex_GEE_Pm_PD)

Sex_GEE_Pm_PR<-geeglm(Pm ~ Sex, 
                      data = AD_Total_SexNoMiss, 
                      id = Subject_ID, 
                      family = binomial(link="log"),
                      corstr = "ex")
summary(Sex_GEE_Pm_PR)

#Po#
Sex_GEE_Po_PD<-geeglm(Po ~ Sex, 
                      data = AD_Total_SexNoMiss, 
                      id = Subject_ID, 
                      family = binomial(link="identity"),
                      corstr = "ex")
summary(Sex_GEE_Po_PD)

Sex_GEE_Po_PR<-geeglm(Po ~ Sex, 
                      data = AD_Total_SexNoMiss, 
                      id = Subject_ID, 
                      family = binomial(link="log"),
                      corstr = "ex")
summary(Sex_GEE_Po_PR)

#Pf#
Sex_GEE_Pf_PD<-geeglm(Pf ~ Sex, 
                      data = AD_Total_SexNoMiss, 
                      id = Subject_ID, 
                      family = binomial(link="identity"),
                      corstr = "ex")
summary(Sex_GEE_Pf_PD)

Sex_GEE_Pf_PR<-geeglm(Pf ~ Sex, 
                      data = AD_Total_SexNoMiss, 
                      id = Subject_ID, 
                      family = binomial(link="log"),
                      corstr = "ex")
summary(Sex_GEE_Pf_PR)



##Outputing GEE Results: Estimate + 95% CI from Robust Sandwich Estimator

#Prevalence Differences 
Coeff_Total_Sex_Pm_PD<-tidy(Sex_GEE_Pm_PD, exponentiate=FALSE, conf.int=TRUE)%>%dplyr::mutate(Ifxn="Pm")%>%filter(!term=="(Intercept)")
Coeff_Total_Sex_Po_PD<-tidy(Sex_GEE_Po_PD, exponentiate=FALSE, conf.int=TRUE)%>%dplyr::mutate(Ifxn="Po")%>%filter(!term=="(Intercept)")
Coeff_Total_Sex_Pf_PD<-tidy(Sex_GEE_Pf_PD, exponentiate=FALSE, conf.int=TRUE)%>%dplyr::mutate(Ifxn="Pf")%>%filter(!term=="(Intercept)")

#Prevalence Ratios
Coeff_Total_Sex_Pm_PR<-tidy(Sex_GEE_Pm_PR, exponentiate=TRUE, conf.int=TRUE)%>%dplyr::mutate(Ifxn="Pm")%>%filter(!term=="(Intercept)")
Coeff_Total_Sex_Po_PR<-tidy(Sex_GEE_Po_PR, exponentiate=TRUE, conf.int=TRUE)%>%dplyr::mutate(Ifxn="Po")%>%filter(!term=="(Intercept)")
Coeff_Total_Sex_Pf_PR<-tidy(Sex_GEE_Pf_PR, exponentiate=TRUE, conf.int=TRUE)%>%dplyr::mutate(Ifxn="Pf")%>%filter(!term=="(Intercept)")




##
##HEALTH AREA CATEGORY (1 = LINGWALA = REF CAT (urban);  Bu = 2 = Ref ; (rural);  3 = KIMPOKO  (peri-urban))
##
AD_Total_HealthAreaNoMiss<-AD_Total%>%filter(!is.na(HealthArea))

#Pm#
HealthArea_GEE_Pm_PD<-geeglm(Pm ~ as.factor(HealthArea), 
                             data = AD_Total_HealthAreaNoMiss, 
                             id = Subject_ID, 
                             family = binomial(link="identity"),
                             corstr = "ex")
summary(HealthArea_GEE_Pm_PD)

HealthArea_GEE_Pm_PR<-geeglm(Pm ~ as.factor(HealthArea), 
                             data = AD_Total_HealthAreaNoMiss, 
                             id = Subject_ID, 
                             family = binomial(link="log"),
                             corstr = "ex")
summary(HealthArea_GEE_Pm_PR)

#Po#
HealthArea_GEE_Po_PD<-geeglm(Po ~ as.factor(HealthArea),  
                             data = AD_Total_HealthAreaNoMiss, 
                             id = Subject_ID, 
                             family = binomial(link="identity"),
                             corstr = "ex")
summary(HealthArea_GEE_Po_PD)

HealthArea_GEE_Po_PR<-geeglm(Po ~ as.factor(HealthArea),  
                             data = AD_Total_HealthAreaNoMiss, 
                             id = Subject_ID, 
                             family = binomial(link="log"),
                             corstr = "ex")
summary(HealthArea_GEE_Po_PR)

#Pf#
HealthArea_GEE_Pf_PD<-geeglm(Pf ~ as.factor(HealthArea),  
                             data = AD_Total_HealthAreaNoMiss, 
                             id = Subject_ID, 
                             family = binomial(link="identity"),
                             corstr = "ex")
summary(HealthArea_GEE_Pf_PD)

HealthArea_GEE_Pf_PR<-geeglm(Pf ~ as.factor(HealthArea),  
                             data = AD_Total_HealthAreaNoMiss, 
                             id = Subject_ID, 
                             family = binomial(link="log"),
                             corstr = "ex")
summary(HealthArea_GEE_Pf_PR)


##Outputing GEE Results: Estimate + 95% CI from Robust Sandwich Estimator
#Prevalence Differences 
Coeff_Total_HealthArea_Pm_PD<-tidy(HealthArea_GEE_Pm_PD, exponentiate=FALSE, conf.int=TRUE)%>%dplyr::mutate(Ifxn="Pm")%>%filter(!term=="(Intercept)")
Coeff_Total_HealthArea_Po_PD<-tidy(HealthArea_GEE_Po_PD, exponentiate=FALSE, conf.int=TRUE)%>%dplyr::mutate(Ifxn="Po")%>%filter(!term=="(Intercept)")
Coeff_Total_HealthArea_Pf_PD<-tidy(HealthArea_GEE_Pf_PD, exponentiate=FALSE, conf.int=TRUE)%>%dplyr::mutate(Ifxn="Pf")%>%filter(!term=="(Intercept)")

#Prevalence Ratios
Coeff_Total_HealthArea_Pm_PR<-tidy(HealthArea_GEE_Pm_PR, exponentiate=TRUE, conf.int=TRUE)%>%dplyr::mutate(Ifxn="Pm")%>%filter(!term=="(Intercept)")
Coeff_Total_HealthArea_Po_PR<-tidy(HealthArea_GEE_Po_PR, exponentiate=TRUE, conf.int=TRUE)%>%dplyr::mutate(Ifxn="Po")%>%filter(!term=="(Intercept)")
Coeff_Total_HealthArea_Pf_PR<-tidy(HealthArea_GEE_Pf_PR, exponentiate=TRUE, conf.int=TRUE)%>%dplyr::mutate(Ifxn="Pf")%>%filter(!term=="(Intercept)")





##
##WEALTH CATEGORY (Average Wealth [3] = Ref)
##

AD_Total_WealthCatNoMiss<-AD_Total%>%filter(!is.na(WealthCat))
## comparing poor vs. average, and rich vs. average  (average = 1; poor = 2; rich = 3)

#Pm#
Wealth_GEE_Pm_PD<-geeglm(Pm ~ as.factor(WealthCat), 
                         data = AD_Total_WealthCatNoMiss, 
                         id = Subject_ID, 
                         family = binomial(link="identity"),
                         corstr = "ex")
summary(Wealth_GEE_Pm_PD)

Wealth_GEE_Pm_PR<-geeglm(Pm ~ as.factor(WealthCat), 
                         data = AD_Total_WealthCatNoMiss, 
                         id = Subject_ID, 
                         family = binomial(link="log"),
                         corstr = "ex")
summary(Wealth_GEE_Pm_PR)


#Po#
Wealth_GEE_Po_PD<-geeglm(Po ~ as.factor(WealthCat), 
                         data = AD_Total_WealthCatNoMiss, 
                         id = Subject_ID, 
                         family = binomial(link="identity"),
                         corstr = "ex")
summary(Wealth_GEE_Po_PD)

Wealth_GEE_Po_PR<-geeglm(Po ~ as.factor(WealthCat), 
                         data = AD_Total_WealthCatNoMiss, 
                         id = Subject_ID, 
                         family = binomial(link="log"),
                         corstr = "ex")
summary(Wealth_GEE_Po_PR)

#Pf#
Wealth_GEE_Pf_PD<-geeglm(Pf ~ as.factor(WealthCat), 
                         data = AD_Total_WealthCatNoMiss, 
                         id = Subject_ID, 
                         family = binomial(link="identity"),
                         corstr = "ex")
summary(Wealth_GEE_Pf_PD)

Wealth_GEE_Pf_PR<-geeglm(Pf ~ as.factor(WealthCat), 
                         data = AD_Total_WealthCatNoMiss, 
                         id = Subject_ID, 
                         family = binomial(link="log"),
                         corstr = "ex")
summary(Wealth_GEE_Pf_PR)


##Outputing GEE Results: Estimate + 95% CI from Robust Sandwich Estimator
#Prevalence Difference 
Coeff_Total_Wealth_Pm_PD<-tidy(Wealth_GEE_Pm_PD, exponentiate=FALSE, conf.int=TRUE)%>%dplyr::mutate(Ifxn="Pm")%>%filter(!term=="(Intercept)")
Coeff_Total_Wealth_Po_PD<-tidy(Wealth_GEE_Po_PD, exponentiate=FALSE, conf.int=TRUE)%>%dplyr::mutate(Ifxn="Po")%>%filter(!term=="(Intercept)")
Coeff_Total_Wealth_Pf_PD<-tidy(Wealth_GEE_Pf_PD, exponentiate=FALSE, conf.int=TRUE)%>%dplyr::mutate(Ifxn="Pf")%>%filter(!term=="(Intercept)")

#Prevalence Ratios 
Coeff_Total_Wealth_Pm_PR<-tidy(Wealth_GEE_Pm_PR, exponentiate=TRUE, conf.int=TRUE)%>%dplyr::mutate(Ifxn="Pm")%>%filter(!term=="(Intercept)")
Coeff_Total_Wealth_Po_PR<-tidy(Wealth_GEE_Po_PR, exponentiate=TRUE, conf.int=TRUE)%>%dplyr::mutate(Ifxn="Po")%>%filter(!term=="(Intercept)")
Coeff_Total_Wealth_Pf_PR<-tidy(Wealth_GEE_Pf_PR, exponentiate=TRUE, conf.int=TRUE)%>%dplyr::mutate(Ifxn="Pf")%>%filter(!term=="(Intercept)")



##
##FEVER CATEGORY (No Fever = Ref)
##
AD_Total_FeverNoMiss<-AD_Total%>%filter(!is.na(Feverpos))

addmargins(table(AD_Total_FeverNoMiss$Feverpos, AD_Total_FeverNoMiss$Pm, useNA = "always"))
addmargins(table(AD_Total_FeverNoMiss$Feverpos, AD_Total_FeverNoMiss$Po, useNA = "always"))

#Pm#
Fever_GEE_Pm_PD<-geeglm(Pm ~ Feverpos, 
                        data = AD_Total_FeverNoMiss, 
                        id = Subject_ID, 
                        family = binomial(link="identity"),
                        corstr = "ex")
summary(Fever_GEE_Pm_PD)

Fever_GEE_Pm_PR<-geeglm(Pm ~ Feverpos, 
                        data = AD_Total_FeverNoMiss, 
                        id = Subject_ID, 
                        family = binomial(link="log"),
                        corstr = "ex")
summary(Fever_GEE_Pm_PR)


#Po#
Fever_GEE_Po_PD<-geeglm(Po ~ Feverpos, 
                        data = AD_Total_FeverNoMiss, 
                        id = Subject_ID, 
                        family = binomial(link="identity"),
                        corstr = "ex")
summary(Fever_GEE_Po_PD)

Fever_GEE_Po_PR<-geeglm(Po ~ Feverpos, 
                        data = AD_Total_FeverNoMiss, 
                        id = Subject_ID, 
                        family = binomial(link="log"),
                        corstr = "ex")
summary(Fever_GEE_Po_PR)

#Pf#
Fever_GEE_Pf_PD<-geeglm(Pf ~ Feverpos, 
                        data = AD_Total_FeverNoMiss, 
                        id = Subject_ID, 
                        family = binomial(link="identity"),
                        corstr = "ex")
summary(Fever_GEE_Pf_PD)

Fever_GEE_Pf_PR<-geeglm(Pf ~ Feverpos, 
                        data = AD_Total_FeverNoMiss, 
                        id = Subject_ID, 
                        family = binomial(link="log"),
                        corstr = "ex")
summary(Fever_GEE_Pf_PR)

##Outputing GEE Results: Estimate + 95% CI from Robust Sandwich Estimator
#Prevalence Differences 
Coeff_Total_Fever_Pm_PD<-tidy(Fever_GEE_Pm_PD, exponentiate=FALSE, conf.int=TRUE)%>%dplyr::mutate(Ifxn="Pm")%>%filter(!term=="(Intercept)")
Coeff_Total_Fever_Po_PD<-tidy(Fever_GEE_Po_PD, exponentiate=FALSE, conf.int=TRUE)%>%dplyr::mutate(Ifxn="Po")%>%filter(!term=="(Intercept)")
Coeff_Total_Fever_Pf_PD<-tidy(Fever_GEE_Pf_PD, exponentiate=FALSE, conf.int=TRUE)%>%dplyr::mutate(Ifxn="Pf")%>%filter(!term=="(Intercept)")

#Prevalence Ratios 
Coeff_Total_Fever_Pm_PR<-tidy(Fever_GEE_Pm_PR, exponentiate=TRUE, conf.int=TRUE)%>%dplyr::mutate(Ifxn="Pm")%>%filter(!term=="(Intercept)")
Coeff_Total_Fever_Po_PR<-tidy(Fever_GEE_Po_PR, exponentiate=TRUE, conf.int=TRUE)%>%dplyr::mutate(Ifxn="Po")%>%filter(!term=="(Intercept)")
Coeff_Total_Fever_Pf_PR<-tidy(Fever_GEE_Pf_PR, exponentiate=TRUE, conf.int=TRUE)%>%dplyr::mutate(Ifxn="Pf")%>%filter(!term=="(Intercept)")




write.table(Coeff_Total_Fever_Pm_PD, "clipboard", sep="\t", col.names = F )
write.table(Coeff_Total_Fever_Po_PD, "clipboard", sep="\t", col.names = F )
write.table(Coeff_Total_Fever_Pf_PD, "clipboard", sep="\t", col.names = F )

write.table(Coeff_Total_Fever_Pm_PR, "clipboard", sep="\t", col.names = F )
write.table(Coeff_Total_Fever_Po_PR, "clipboard", sep="\t", col.names = F )
write.table(Coeff_Total_Fever_Pf_PR, "clipboard", sep="\t", col.names = F )



##
##RDT-positive CATEGORY (Not RDT Positive = Ref)
##
AD_Total_RDTNoMiss<-AD_Total%>%filter(!is.na(RDTpos))

#Pm#
RDT_GEE_Pm_PD<-geeglm(Pm ~ RDTpos, 
                      data = AD_Total_RDTNoMiss, 
                      id = Subject_ID, 
                      family = binomial(link="identity"),
                      corstr = "ex")
summary(RDT_GEE_Pm_PD)

RDT_GEE_Pm_PR<-geeglm(Pm ~ RDTpos, 
                      data = AD_Total_RDTNoMiss, 
                      id = Subject_ID, 
                      family = binomial(link="log"),
                      corstr = "ex")
summary(RDT_GEE_Pm_PR)


#Po#
RDT_GEE_Po_PD<-geeglm(Po ~ RDTpos, 
                      data = AD_Total_RDTNoMiss, 
                      id = Subject_ID, 
                      family = binomial(link="identity"),
                      corstr = "ex")
summary(RDT_GEE_Po_PD)

RDT_GEE_Po_PR<-geeglm(Po ~ RDTpos, 
                      data = AD_Total_RDTNoMiss, 
                      id = Subject_ID, 
                      family = binomial(link="log"),
                      corstr = "ex")
summary(RDT_GEE_Po_PR)

#Pf#
RDT_GEE_Pf_PD<-geeglm(Pf ~ RDTpos, 
                      data = AD_Total_RDTNoMiss, 
                      id = Subject_ID, 
                      family = binomial(link="identity"),
                      corstr = "ex")
summary(RDT_GEE_Pf_PD)

RDT_GEE_Pf_PR<-geeglm(Pf ~ RDTpos, 
                      data = AD_Total_RDTNoMiss, 
                      id = Subject_ID, 
                      family = binomial(link="log"),
                      corstr = "ex")
summary(RDT_GEE_Pf_PR)

##Outputing GEE Results: Estimate + 95% CI from Robust Sandwich Estimator
#Prevalence Differences 
Coeff_Total_RDT_Pm_PD<-tidy(RDT_GEE_Pm_PD, exponentiate=FALSE, conf.int=TRUE)%>%dplyr::mutate(Ifxn="Pm")%>%filter(!term=="(Intercept)")
Coeff_Total_RDT_Po_PD<-tidy(RDT_GEE_Po_PD, exponentiate=FALSE, conf.int=TRUE)%>%dplyr::mutate(Ifxn="Po")%>%filter(!term=="(Intercept)")
Coeff_Total_RDT_Pf_PD<-tidy(RDT_GEE_Pf_PD, exponentiate=FALSE, conf.int=TRUE)%>%dplyr::mutate(Ifxn="Pf")%>%filter(!term=="(Intercept)")

#Prevalence Ratios 
Coeff_Total_RDT_Pm_PR<-tidy(RDT_GEE_Pm_PR, exponentiate=TRUE, conf.int=TRUE)%>%dplyr::mutate(Ifxn="Pm")%>%filter(!term=="(Intercept)")
Coeff_Total_RDT_Po_PR<-tidy(RDT_GEE_Po_PR, exponentiate=TRUE, conf.int=TRUE)%>%dplyr::mutate(Ifxn="Po")%>%filter(!term=="(Intercept)")
Coeff_Total_RDT_Pf_PR<-tidy(RDT_GEE_Pf_PR, exponentiate=TRUE, conf.int=TRUE)%>%dplyr::mutate(Ifxn="Pf")%>%filter(!term=="(Intercept)")



##
##P.falciparum Co-infection (Neg Pf = Ref)
##
AD_Total_PfNoMiss<-AD_Total%>%filter(!is.na(Pf))
#Pm#
Pf_GEE_Pm_PD<-geeglm(Pm ~ Pf, 
                     data = AD_Total_PfNoMiss, 
                     id = Subject_ID, 
                     family = binomial(link="identity"),
                     corstr = "ex")
summary(Pf_GEE_Pm_PD)

Pf_GEE_Pm_PR<-geeglm(Pm ~ Pf, 
                     data = AD_Total_PfNoMiss, 
                     id = Subject_ID, 
                     family = binomial(link="log"),
                     corstr = "ex")
summary(Pf_GEE_Pm_PR)

#Po#
Pf_GEE_Po_PD<-geeglm(Po ~ Pf, 
                     data = AD_Total_PfNoMiss, 
                     id = Subject_ID, 
                     family = binomial(link="identity"),
                     corstr = "ex")
summary(Pf_GEE_Po_PD)

Pf_GEE_Po_PR<-geeglm(Po ~ Pf, 
                     data = AD_Total_PfNoMiss, 
                     id = Subject_ID, 
                     family = binomial(link="log"),
                     corstr = "ex")
summary(Pf_GEE_Po_PR)


##Outputing GEE Results: Estimate + 95% CI from Robust Sandwich Estimator
#Prevalence DIfferences 
Coeff_Total_Pfcoinfec_Pm_PD<-tidy(Pf_GEE_Pm_PD, exponentiate=FALSE, conf.int=TRUE)%>%dplyr::mutate(Ifxn="Pm")%>%filter(!term=="(Intercept)")
Coeff_Total_Pfcoinfec_Po_PD<-tidy(Pf_GEE_Po_PD, exponentiate=FALSE, conf.int=TRUE)%>%dplyr::mutate(Ifxn="Po")%>%filter(!term=="(Intercept)")

#Prevalence Ratios 
Coeff_Total_Pfcoinfec_Pm_PR<-tidy(Pf_GEE_Pm_PR, exponentiate=TRUE, conf.int=TRUE)%>%dplyr::mutate(Ifxn="Pm")%>%filter(!term=="(Intercept)")
Coeff_Total_Pfcoinfec_Po_PR<-tidy(Pf_GEE_Po_PR, exponentiate=TRUE, conf.int=TRUE)%>%dplyr::mutate(Ifxn="Po")%>%filter(!term=="(Intercept)")



##
##SEASONALITY CATEGORY (Rainy Season = Ref)
##
AD_Total_SeasonNoMiss<-AD_Total%>%filter(!is.na(season_dry))
#Pm#
DrySeason_GEE_Pm_PD<-geeglm(Pm ~ season_dry, 
                            data = AD_Total_SeasonNoMiss, 
                            id = Subject_ID, 
                            family = binomial(link="identity"),
                            corstr = "ex")
summary(DrySeason_GEE_Pm_PD)

DrySeason_GEE_Pm_PR<-geeglm(Pm ~ season_dry, 
                            data = AD_Total_SeasonNoMiss, 
                            id = Subject_ID, 
                            family = binomial(link="log"),
                            corstr = "ex")
summary(DrySeason_GEE_Pm_PR)

#Po#
DrySeason_GEE_Po_PD<-geeglm(Po ~ season_dry, 
                            data = AD_Total_SeasonNoMiss, 
                            id = Subject_ID, 
                            family = binomial(link="identity"),
                            corstr = "ex")
summary(DrySeason_GEE_Po_PD)

DrySeason_GEE_Po_PR<-geeglm(Po ~ season_dry, 
                            data = AD_Total_SeasonNoMiss, 
                            id = Subject_ID, 
                            family = binomial(link="log"),
                            corstr = "ex")
summary(DrySeason_GEE_Po_PR)

#Pf#
DrySeason_GEE_Pf_PD<-geeglm(Pf ~ season_dry, 
                            data = AD_Total_SeasonNoMiss, 
                            id = Subject_ID, 
                            family = binomial(link="identity"),
                            corstr = "ex")
summary(DrySeason_GEE_Pf_PD)

DrySeason_GEE_Pf_PR<-geeglm(Pf ~ season_dry, 
                            data = AD_Total_SeasonNoMiss, 
                            id = Subject_ID, 
                            family = binomial(link="log"),
                            corstr = "ex")
summary(DrySeason_GEE_Pf_PR)

##Outputing GEE Results: Estimate + 95% CI from Robust Sandwich Estimator
#Prevalecne Differneces 
Coeff_Total_DrySeason_Pm_PD<-tidy(DrySeason_GEE_Pm_PD, exponentiate=FALSE, conf.int=TRUE)%>%dplyr::mutate(Ifxn="Pm")%>%filter(!term=="(Intercept)")
Coeff_Total_DrySeason_Po_PD<-tidy(DrySeason_GEE_Po_PD, exponentiate=FALSE, conf.int=TRUE)%>%dplyr::mutate(Ifxn="Po")%>%filter(!term=="(Intercept)")
Coeff_Total_DrySeason_Pf_PD<-tidy(DrySeason_GEE_Pf_PD, exponentiate=FALSE, conf.int=TRUE)%>%dplyr::mutate(Ifxn="Pf")%>%filter(!term=="(Intercept)")

#Prevalecne Ratios 
Coeff_Total_DrySeason_Pm_PR<-tidy(DrySeason_GEE_Pm_PR, exponentiate=TRUE, conf.int=TRUE)%>%dplyr::mutate(Ifxn="Pm")%>%filter(!term=="(Intercept)")
Coeff_Total_DrySeason_Po_PR<-tidy(DrySeason_GEE_Po_PR, exponentiate=TRUE, conf.int=TRUE)%>%dplyr::mutate(Ifxn="Po")%>%filter(!term=="(Intercept)")
Coeff_Total_DrySeason_Pf_PR<-tidy(DrySeason_GEE_Pf_PR, exponentiate=TRUE, conf.int=TRUE)%>%dplyr::mutate(Ifxn="Pf")%>%filter(!term=="(Intercept)")




##
##BED NET USE PRIOR NIGHT (No Use = Ref)
##
AD_Total_BedNetNoMiss<-AD_Total%>%filter(!is.na(BedNetPrior))
#Pm#
BedNet_GEE_Pm_PD<-geeglm(Pm ~ BedNetPrior, 
                         data = AD_Total_BedNetNoMiss, 
                         id = Subject_ID, 
                         family = binomial(link="identity"),
                         corstr = "ex")
summary(BedNet_GEE_Pm_PD)

BedNet_GEE_Pm_PR<-geeglm(Pm ~ BedNetPrior, 
                         data = AD_Total_BedNetNoMiss, 
                         id = Subject_ID, 
                         family = binomial(link="log"),
                         corstr = "ex")
summary(BedNet_GEE_Pm_PR)

#Po#
BedNet_GEE_Po_PD<-geeglm(Po ~ BedNetPrior, 
                         data = AD_Total_BedNetNoMiss, 
                         id = Subject_ID, 
                         family = binomial(link="identity"),
                         corstr = "ex")
summary(BedNet_GEE_Po_PD)

BedNet_GEE_Po_PR<-geeglm(Po ~ BedNetPrior, 
                         data = AD_Total_BedNetNoMiss, 
                         id = Subject_ID, 
                         family = binomial(link="log"),
                         corstr = "ex")
summary(BedNet_GEE_Po_PR)
#Pf#
BedNet_GEE_Pf_PD<-geeglm(Pf ~ BedNetPrior, 
                         data = AD_Total_BedNetNoMiss, 
                         id = Subject_ID, 
                         family = binomial(link="identity"),
                         corstr = "ex")
summary(BedNet_GEE_Pf_PD)

BedNet_GEE_Pf_PR<-geeglm(Pf ~ BedNetPrior, 
                         data = AD_Total_BedNetNoMiss, 
                         id = Subject_ID, 
                         family = binomial(link="log"),
                         corstr = "ex")
summary(BedNet_GEE_Pf_PR)

##Outputing GEE Results: Estimate + 95% CI from Robust Sandwich Estimator
#Prevalence Differences 
Coeff_Total_BedNet_Pm_PD<-tidy(BedNet_GEE_Pm_PD, exponentiate=FALSE, conf.int=TRUE)%>%dplyr::mutate(Ifxn="Pm")%>%filter(!term=="(Intercept)")
Coeff_Total_BedNet_Po_PD<-tidy(BedNet_GEE_Po_PD, exponentiate=FALSE, conf.int=TRUE)%>%dplyr::mutate(Ifxn="Po")%>%filter(!term=="(Intercept)")
Coeff_Total_BedNet_Pf_PD<-tidy(BedNet_GEE_Pf_PD, exponentiate=FALSE, conf.int=TRUE)%>%dplyr::mutate(Ifxn="Pf")%>%filter(!term=="(Intercept)")

#Prevalence Ratios 
Coeff_Total_BedNet_Pm_PR<-tidy(BedNet_GEE_Pm_PR, exponentiate=TRUE, conf.int=TRUE)%>%dplyr::mutate(Ifxn="Pm")%>%filter(!term=="(Intercept)")
Coeff_Total_BedNet_Po_PR<-tidy(BedNet_GEE_Po_PR, exponentiate=TRUE, conf.int=TRUE)%>%dplyr::mutate(Ifxn="Po")%>%filter(!term=="(Intercept)")
Coeff_Total_BedNet_Pf_PR<-tidy(BedNet_GEE_Pf_PR, exponentiate=TRUE, conf.int=TRUE)%>%dplyr::mutate(Ifxn="Pf")%>%filter(!term=="(Intercept)")






####
##Anemia(No Anemia= Ref)

##
AD_Total_AnemiaNoMiss<-AD_Total%>%filter(!is.na(anemia_WHO))


addmargins(table(AD_Total$anemia_WHO, AD_Total$Pm, useNA="always"))
addmargins(table(AD_Total$anemia_WHO, AD_Total$Po, useNA="always"))

addmargins(table(AD_Total$anemia_WHO, AD_Total$Pm_Species_Mono, useNA="always"))
addmargins(table(AD_Total$anemia_WHO, AD_Total$Po_Species_Mono, useNA="always"))

addmargins(table(AD_Total_AnemiaNoMiss$anemia_WHO, AD_Total_AnemiaNoMiss$Pm, useNA="always"))
addmargins(table(AD_Total_AnemiaNoMiss$anemia_WHO, AD_Total_AnemiaNoMiss$Po, useNA="always"))

AD_Total_AnemiaNoMiss<-AD_Total_AnemiaNoMiss%>%mutate(Anemia_ModSevere=ifelse(Anemia_None==1|Anemia_Mild==1, 0, 
                                                                              ifelse(Anemia_Moderate==1|Anemia_Severe==1, 1, NA)))%>%
  mutate(Anemia_Any = ifelse(Anemia_Mild==1 | Anemia_Moderate==1|Anemia_Severe==1, 1,
                             ifelse(Anemia_None==1, 0, NA)))

#Pm#
#ModSevere vs. Mild/None

Anemia_GEE_Total_Pm_PD<-geeglm(Pm ~ Anemia_ModSevere, 
                               data = AD_Total_AnemiaNoMiss, 
                               id = Subject_ID, 
                               family = binomial(link="identity"),
                               corstr = "ex")
summary(Anemia_GEE_Total_Pm_PD)

Anemia_GEE_Total_Pm_PR<-geeglm(Pm ~ Anemia_ModSevere, 
                               data = AD_Total_AnemiaNoMiss, 
                               id = Subject_ID, 
                               family = binomial(link="log"),
                               corstr = "ex")
summary(Anemia_GEE_Total_Pm_PR)

#Any Anemia vs. No Anemia
Anemia_GEE_Total_Pm2_PD<-geeglm(Pm ~ Anemia_Any, 
                                data = AD_Total_AnemiaNoMiss, 
                                id = Subject_ID, 
                                family = binomial(link="identity"),
                                corstr = "ex")
summary(Anemia_GEE_Total_Pm2_PD)

Anemia_GEE_Total_Pm2_PR<-geeglm(Pm ~ Anemia_Any, 
                                data = AD_Total_AnemiaNoMiss, 
                                id = Subject_ID, 
                                family = binomial(link="log"),
                                corstr = "ex")
summary(Anemia_GEE_Total_Pm2_PR)                                      


#Po#
#ModSevere vs. Mild/None

Anemia_GEE_Total_Po_PD<-geeglm(Po ~ Anemia_ModSevere, 
                               data = AD_Total_AnemiaNoMiss, 
                               id = Subject_ID, 
                               family = binomial(link="identity"),
                               corstr = "ex")
summary(Anemia_GEE_Total_Po_PD)

Anemia_GEE_Total_Po_PR<-geeglm(Po ~ Anemia_ModSevere, 
                               data = AD_Total_AnemiaNoMiss, 
                               id = Subject_ID, 
                               family = binomial(link="log"),
                               corstr = "ex")
summary(Anemia_GEE_Total_Po_PR)

#Any Anemia vs. No Anemia
Anemia_GEE_Total_Po2_PD<-geeglm(Po ~ Anemia_Any, 
                                data = AD_Total_AnemiaNoMiss, 
                                id = Subject_ID, 
                                family = binomial(link="identity"),
                                corstr = "ex")
summary(Anemia_GEE_Total_Po2_PD)

Anemia_GEE_Total_Po2_PR<-geeglm(Po ~ Anemia_Any, 
                                data = AD_Total_AnemiaNoMiss, 
                                id = Subject_ID, 
                                family = binomial(link="log"),
                                corstr = "ex")
summary(Anemia_GEE_Total_Po2_PR)    

#Pf#
#ModSevere vs. Mild/None

Anemia_GEE_Total_Pf_PD<-geeglm(Pf ~ Anemia_ModSevere, 
                               data = AD_Total_AnemiaNoMiss, 
                               id = Subject_ID, 
                               family = binomial(link="identity"),
                               corstr = "ex")
summary(Anemia_GEE_Total_Pf_PD)

Anemia_GEE_Total_Pf_PR<-geeglm(Pf ~ Anemia_ModSevere, 
                               data = AD_Total_AnemiaNoMiss, 
                               id = Subject_ID, 
                               family = binomial(link="log"),
                               corstr = "ex")
summary(Anemia_GEE_Total_Pf_PR)

#Any Anemia vs. No Anemia
Anemia_GEE_Total_Pf2_PD<-geeglm(Pf ~ Anemia_Any, 
                                data = AD_Total_AnemiaNoMiss, 
                                id = Subject_ID, 
                                family = binomial(link="identity"),
                                corstr = "ex")
summary(Anemia_GEE_Total_Pf2_PD)

Anemia_GEE_Total_Pf2_PR<-geeglm(Pf ~ Anemia_Any, 
                                data = AD_Total_AnemiaNoMiss, 
                                id = Subject_ID, 
                                family = binomial(link="log"),
                                corstr = "ex")
summary(Anemia_GEE_Total_Pf2_PR)      



##Outputing GEE Results: Estimate + 95% CI from Robust Sandwich Estimator
#Moderate/Severe vs. Mild / No Anemia
#Prevalence Differneces 
Coeff_Total_Anemia_Pm_PD<-tidy(Anemia_GEE_Total_Pm_PD, exponentiate=FALSE, conf.int=TRUE)%>%dplyr::mutate(Ifxn="Pm")%>%filter(!term=="(Intercept)")
Coeff_Total_Anemia_Po_PD<-tidy(Anemia_GEE_Total_Po_PD, exponentiate=FALSE, conf.int=TRUE)%>%dplyr::mutate(Ifxn="Po")%>%filter(!term=="(Intercept)")
Coeff_Total_Anemia_Pf_PD<-tidy(Anemia_GEE_Total_Pf_PD, exponentiate=FALSE, conf.int=TRUE)%>%dplyr::mutate(Ifxn="Pf")%>%filter(!term=="(Intercept)")

#Prevalence Ratios 
Coeff_Total_Anemia_Pm_PR<-tidy(Anemia_GEE_Total_Pm_PR, exponentiate=TRUE, conf.int=TRUE)%>%dplyr::mutate(Ifxn="Pm")%>%filter(!term=="(Intercept)")
Coeff_Total_Anemia_Po_PR<-tidy(Anemia_GEE_Total_Po_PR, exponentiate=TRUE, conf.int=TRUE)%>%dplyr::mutate(Ifxn="Po")%>%filter(!term=="(Intercept)")
Coeff_Total_Anemia_Pf_PR<-tidy(Anemia_GEE_Total_Pf_PR, exponentiate=TRUE, conf.int=TRUE)%>%dplyr::mutate(Ifxn="Pf")%>%filter(!term=="(Intercept)")


#Any Anemia vs. No Anemia 
#Prevalence Differneces 
Coeff_Total_Anemia_Pm2_PD<-tidy(Anemia_GEE_Total_Pm2_PD, exponentiate=FALSE, conf.int=TRUE)%>%dplyr::mutate(Ifxn="Pm")%>%filter(!term=="(Intercept)")
Coeff_Total_Anemia_Po2_PD<-tidy(Anemia_GEE_Total_Po2_PD, exponentiate=FALSE, conf.int=TRUE)%>%dplyr::mutate(Ifxn="Po")%>%filter(!term=="(Intercept)")
Coeff_Total_Anemia_Pf2_PD<-tidy(Anemia_GEE_Total_Pf2_PD, exponentiate=FALSE, conf.int=TRUE)%>%dplyr::mutate(Ifxn="Pf")%>%filter(!term=="(Intercept)")

#Prevalence Ratios 
Coeff_Total_Anemia_Pm2_PR<-tidy(Anemia_GEE_Total_Pm2_PR, exponentiate=TRUE, conf.int=TRUE)%>%dplyr::mutate(Ifxn="Pm")%>%filter(!term=="(Intercept)")
Coeff_Total_Anemia_Po2_PR<-tidy(Anemia_GEE_Total_Po2_PR, exponentiate=TRUE, conf.int=TRUE)%>%dplyr::mutate(Ifxn="Po")%>%filter(!term=="(Intercept)")
Coeff_Total_Anemia_Pf2_PR<-tidy(Anemia_GEE_Total_Pf2_PR, exponentiate=TRUE, conf.int=TRUE)%>%dplyr::mutate(Ifxn="Pf")%>%filter(!term=="(Intercept)")



######
#Total Visits:  Stacking all GEE Output for Estimate (95% CI) Plots

#Prevalence Differences

Total_GEE_Output_PD<-rbind(
  Coeff_Total_Age_Pm_PD, Coeff_Total_Age_Po_PD, Coeff_Total_Age_Pf_PD,
  Coeff_Total_Sex_Pm_PD, Coeff_Total_Sex_Po_PD, Coeff_Total_Sex_Pf_PD,
  Coeff_Total_HealthArea_Pm_PD, Coeff_Total_HealthArea_Po_PD, Coeff_Total_HealthArea_Pf_PD,
  Coeff_Total_Wealth_Pm_PD, Coeff_Total_Wealth_Po_PD, Coeff_Total_Wealth_Pf_PD,
  Coeff_Total_Fever_Pm_PD, Coeff_Total_Fever_Po_PD, Coeff_Total_Fever_Pf_PD,
  Coeff_Total_RDT_Pm_PD, Coeff_Total_RDT_Po_PD, Coeff_Total_RDT_Pf_PD,
  Coeff_Total_Pfcoinfec_Pm_PD, Coeff_Total_Pfcoinfec_Po_PD,
  Coeff_Total_DrySeason_Pm_PD, Coeff_Total_DrySeason_Po_PD, Coeff_Total_DrySeason_Pf_PD,
  Coeff_Total_Anemia_Pm2_PD, Coeff_Total_Anemia_Po2_PD, Coeff_Total_Anemia_Pf2_PD, 
  Coeff_Total_Anemia_Pm_PD, Coeff_Total_Anemia_Po_PD, Coeff_Total_Anemia_Pf_PD, 
  Coeff_Total_BedNet_Pm_PD, Coeff_Total_BedNet_Po_PD, Coeff_Total_BedNet_Pf_PD
)

write.table(Total_GEE_Output_PD, "clipboard", sep="\t", col.names = T )



#Prevalence Ratios

Total_GEE_Output_PR<-rbind(
  Coeff_Total_Age_Pm_PR, Coeff_Total_Age_Po_PR, Coeff_Total_Age_Pf_PR,
  Coeff_Total_Sex_Pm_PR, Coeff_Total_Sex_Po_PR, Coeff_Total_Sex_Pf_PR,
  Coeff_Total_HealthArea_Pm_PR, Coeff_Total_HealthArea_Po_PR, Coeff_Total_HealthArea_Pf_PR,
  #Coeff_Total_Village_Pm_PD, Coeff_Total_Village_Po_PD, Coeff_Total_Village_Pf_PD,
  Coeff_Total_Wealth_Pm_PR, Coeff_Total_Wealth_Po_PR, Coeff_Total_Wealth_Pf_PR,
  Coeff_Total_Fever_Pm_PR, Coeff_Total_Fever_Po_PR, Coeff_Total_Fever_Pf_PR,
  Coeff_Total_RDT_Pm_PR, Coeff_Total_RDT_Po_PR, Coeff_Total_RDT_Pf_PR,
  Coeff_Total_Pfcoinfec_Pm_PR, Coeff_Total_Pfcoinfec_Po_PR,
  Coeff_Total_DrySeason_Pm_PR, Coeff_Total_DrySeason_Po_PR, Coeff_Total_DrySeason_Pf_PR,
  Coeff_Total_Anemia_Pm2_PR, Coeff_Total_Anemia_Po2_PR, Coeff_Total_Anemia_Pf2_PR,
  Coeff_Total_Anemia_Pm_PR, Coeff_Total_Anemia_Po_PR, Coeff_Total_Anemia_Pf_PR,
  Coeff_Total_BedNet_Pm_PR, Coeff_Total_BedNet_Po_PR, Coeff_Total_BedNet_Pf_PR
)

write.table(Total_GEE_Output_PR, "clipboard", sep="\t", col.names = T )




#############
##  Plotting Prevalence Differences and 95% CIs by Population

Total_GEE_Output_PD$Ifxn <- factor(Total_GEE_Output_PD$Ifxn, levels=c("Pm", "Po", "Pf"))
Total_GEE_Output_PD$Ifxn <- factor(Total_GEE_Output_PD$Ifxn, levels=c("Pm", "Po", "Pf"))

Total_GEE_Output_PD$term <- recode_factor(Total_GEE_Output_PD$term, 
                                          "relevel(as.factor(AgeCat_Visit), ref = \"3\")1" = "Age <5 vs 15+", 
                                          "relevel(as.factor(AgeCat_Visit), ref = \"3\")2" = "Age 5-14 vs 15+",
                                          "Sex" = "Sex (F vs M)",
                                          "as.factor(HealthArea)2" = "Rural vs Urban", 
                                          "as.factor(HealthArea)3" = "Peri-urban vs Urban", 
                                          "as.factor(WealthCat)2" = "Poor vs Avg. Wealth",
                                          "as.factor(WealthCat)3" = "Wealthy vs Avg. Wealth",
                                          "season_dry" =  "Dry vs Rainy Season",
                                          "BedNetPrior" = "Bed Net Use (Y vs N)",
                                          "RDTpos" = "RDT+ vs RDT-",
                                          "Pf" = "Pf coinfection (Y vs N)",
                                          "Feverpos" = "Fever (Y vs N)",
                                          "Anemia_Any" = "Anemic vs Not Anemic",
                                          "Anemia_ModSevere" = "Mod./Severe vs Mild/No Anemia")

Total_GEE_Output_PR$Ifxn <- factor(Total_GEE_Output_PR$Ifxn, levels=c("Pm", "Po", "Pf"))
Total_GEE_Output_PR$Ifxn <- factor(Total_GEE_Output_PR$Ifxn, levels=c("Pm", "Po", "Pf"))

Total_GEE_Output_PR$term <- recode_factor(Total_GEE_Output_PR$term, 
                                          "relevel(as.factor(AgeCat_Visit), ref = \"3\")1" = "Age <5 vs 15+", 
                                          "relevel(as.factor(AgeCat_Visit), ref = \"3\")2" = "Age 5-14 vs 15+",
                                          "Sex" = "Sex (F vs M)",
                                          "as.factor(HealthArea)2" = "Rural vs Urban", 
                                          "as.factor(HealthArea)3" = "Peri-urban vs Urban", 
                                          "as.factor(WealthCat)2" = "Poor vs Avg. Wealth",
                                          "as.factor(WealthCat)3" = "Wealthy vs Avg. Wealth",
                                          "season_dry" =  "Dry vs Rainy Season",
                                          "BedNetPrior" = "Bed Net Use (Y vs N)",
                                          "RDTpos" = "RDT+ vs RDT-",
                                          "Pf" = "Pf coinfection (Y vs N)",
                                          "Feverpos" = "Fever (Y vs N)",
                                          "Anemia_Any" = "Anemic vs Not Anemic",
                                          "Anemia_ModSevere" = "Mod./Severe vs Mild/No Anemia")



##checking the ordering of variables for gggplotting: 
Total_GEE_Output_PD$term %>%levels()
Total_GEE_Output_PR$term %>%levels()
Total_GEE_Output_PD$term %>%levels()
Total_GEE_Output_PD$term %>%levels()


#set order of levels to appear along y-axis
Total_GEE_Output_PD <- Total_GEE_Output_PD%>%
  mutate(term = term %>% 
           fct_relevel("Wealthy vs Avg. Wealth",
                       "Poor vs Avg. Wealth",
                       "Dry vs Rainy Season",
                       "Bed Net Use (Y vs N)",
                       "Mod./Severe vs Mild/No Anemia",
                       "Anemic vs Not Anemic",
                       "Fever (Y vs N)",
                       "Pf coinfection (Y vs N)",
                       "RDT+ vs RDT-",
                       "Peri-urban vs Urban",
                       "Rural vs Urban",
                       "Sex (F vs M)",
                       "Age 5-14 vs 15+",
                       "Age <5 vs 15+", 
           ))

Total_GEE_Output_PR <- Total_GEE_Output_PR%>%
  mutate(term = term %>% 
           fct_relevel("Wealthy vs Avg. Wealth",
                       "Poor vs Avg. Wealth",
                       "Dry vs Rainy Season",
                       "Bed Net Use (Y vs N)",
                       "Mod./Severe vs Mild/No Anemia",
                       "Anemic vs Not Anemic",
                       "Fever (Y vs N)",
                       "Pf coinfection (Y vs N)",
                       "RDT+ vs RDT-",
                       "Peri-urban vs Urban",
                       "Rural vs Urban",
                       "Sex (F vs M)",
                       "Age 5-14 vs 15+",
                       "Age <5 vs 15+", 
           ))



TotalPop_PD <-ggplot(Total_GEE_Output_PD, (aes(x=estimate, y=term, col=Ifxn, fill=Ifxn))) + 
  geom_errorbar(aes(xmin=conf.low, xmax=conf.high, col=Ifxn), size=0.7, width=0.6, position=position_dodge(width = 0.7))+
  geom_point(size=4, shape=21, colour="black", stroke = 0.8, position=position_dodge(width = 0.7)) +
  geom_vline(xintercept=0.0, lty=2)+
  scale_fill_manual(values=c("blue", "red", "orange"))+
  scale_color_manual(values=c("black", "black", "black"))+
  #          ggtitle("Survey-based population (n=1,565 participants across 5,682 visits)")+
  xlab("Prevalence Difference")+
  ylab("")+
  #labs(title = "Total Visits",
  #     subtitle = "n=1,565 participants across 9,089 visits")+
  guides(fill = guide_legend(title = "Species"), col=FALSE)+
  theme(plot.title =element_text(size=16, color="black"), axis.title = element_text(size =14), axis.text.x=element_text(size=11, color="black"), axis.text.y.left=element_text(size=11,  color="black"), legend.text=element_text(size=14, color="black"), legend.title = element_text(size=14, color="black"))
plot(TotalPop_PD) 


##Now without Pf also for a zoomed version on just Pm and Po differences
Total_GEE_Output_PD_NoPf<-Total_GEE_Output_PD%>%filter(Ifxn!= "Pf")
TotalPop_PD_noPf<-ggplot(Total_GEE_Output_PD_NoPf, (aes(x=estimate, y=term, col=Ifxn, fill=Ifxn))) + 
  geom_errorbar(aes(xmin=conf.low, xmax=conf.high, col=Ifxn), size=0.5,  width=0.4, position=position_dodge(width = 0.3))+
  geom_point(size=3, shape=21, colour="black", stroke = 0.8, position=position_dodge(width = 0.3)) +
  geom_vline(xintercept=0.0, lty=2)+
  scale_fill_manual(values=c("blue", "red", "orange"))+
  scale_color_manual(values=c("black", "black", "black"))+
  guides(fill = guide_legend(title = "Species"), col=FALSE)+
  # ggtitle("Survey-based Pop. (N=1,565; 5,682 visits)")+
  xlab("Prevalence Difference")+
  ylab("")+
  #labs(title = "Total Visits",
  #     subtitle = "n=1,565 participants across 9,089 visits")+
  #guides(fill = guide_legend(title = "Species"), col=FALSE)+
  theme(plot.title =element_text(size=11, color="black"), plot.subtitle = element_text(size=9.5), axis.text.x=element_text(size=9, color="black"), axis.text.y.left=element_text(size=10,  color="black"),  legend.text=element_text(size=10, color="black"), legend.title = element_text(size=10, color="black")
        #TotalPop_PD_noPf_2<-TotalPop_PD_noPf + expand_limits(x=c(-0.08, 0.08))
        , legend.position= "none"
  )

TotalPop_PD_noPf_2<-TotalPop_PD_noPf + xlim(-0.05, 0.075)
plot(TotalPop_PD_noPf_2) 



TotalPop_PR <-ggplot(Total_GEE_Output_PR, (aes(x=estimate, y=term, col=Ifxn, fill=Ifxn))) + 
  geom_errorbar(aes(xmin=conf.low, xmax=conf.high, col=Ifxn), size=0.7, width=0.6, position=position_dodge(width = 0.7))+
  geom_point(size=4, shape=21, colour="black", stroke = 0.8, position=position_dodge(width = 0.7)) +
  geom_vline(xintercept=0.0, lty=2)+
  scale_fill_manual(values=c("blue", "red", "orange"))+
  scale_color_manual(values=c("black", "black", "black"))+
  #ggtitle("Survey-based population (n=1,565 participants across 5,682 visits)")+
  xlab("Prevalence Ratio")+
  ylab("")+
  labs(title = "Total Visits",
       subtitle = "n=1,565 participants across 9,089 visits")+
  guides(fill = guide_legend(title = "Species"), col=FALSE)+
  theme(plot.title =element_text(size=16, color="black"), axis.title = element_text(size =14), axis.text.x=element_text(size=11, color="black"), axis.text.y.left=element_text(size=11,  color="black"), legend.text=element_text(size=14, color="black"), legend.title = element_text(size=14, color="black"))
plot(TotalPop_PR) 


##Now without Pf also for a zoomed version on just Pm and Po differences
Total_GEE_Output_PR_NoPf<-Total_GEE_Output_PR%>%filter(Ifxn!= "Pf")
TotalPop_PR_noPf<-ggplot(Total_GEE_Output_PR_NoPf, (aes(x=estimate, y=term, col=Ifxn, fill=Ifxn))) + 
  geom_errorbar(aes(xmin=conf.low, xmax=conf.high, col=Ifxn), size=0.5,  width=0.4, position=position_dodge(width = 0.3))+
  geom_point(size=3, shape=21, colour="black", stroke = 0.8, position=position_dodge(width = 0.3)) +
  geom_vline(xintercept=0.0, lty=2)+
  scale_fill_manual(values=c("blue", "red", "orange"))+
  scale_color_manual(values=c("black", "black", "black"))+
  guides(fill = guide_legend(title = "Species"), col=FALSE)+
  # ggtitle("Survey-based Pop. (N=1,565; 5,682 visits)")+
  xlab("Prevalence Ratio")+
  ylab("")+
  labs(title = "Total Visits",
       subtitle = "n=1,565 participants across 9,089 visits")+
  #guides(fill = guide_legend(title = "Species"), col=FALSE)+
  theme(plot.title =element_text(size=11, color="black"), plot.subtitle = element_text(size=9.5), axis.text.x=element_text(size=9, color="black"), axis.text.y.left=element_text(size=10,  color="black"),  legend.text=element_text(size=10, color="black"), legend.title = element_text(size=10, color="black"))
TotalPop_PR_noPf_2<-TotalPop_PR_noPf + expand_limits(x=c(-3, 6))
plot(TotalPop_PR_noPf_2) 



plot(TotalPop_PD_noPf_2)
plot(ActivePop_PD_noPf_2)
plot(PassivePop_PD_noPf_2)

###Aggregating the non-pf Risk Factor Plots as 1 figure 
gridExtra::grid.arrange(TotalPop_PD_noPf_2, ActivePop_PD_noPf_2, PassivePop_PD_noPf_2, ncol=3, nrow=1) 


###Aggregating the non-pf Risk Factor Plots as 1 figure 
gridExtra::grid.arrange(TotalPop_PD, ActivePop_PD, PassivePop_PD, ncol=3, nrow=1) 











    
    
    