#---------------------------------SETTING UP------------------------------------------------------------------------------------
#Load libraries
library(mosaic)
library(readxl)
library(nlme)

#Load data
All_Data <- read.csv("C:/Users/maddy/Box/Autism_CSF/Utah_Longitudinal_Results/Raw_EACSF_Output/All_Data.csv", fill=TRUE)

#Add the QC file
QC <- read_excel("C:/Users/maddy/Box/Autism_CSF/Utah_Longitudinal_Results/QC/QC_All.xlsx")
All_Data_QC <- merge(All_Data, QC, by ="SUBJID", all=TRUE)

#Transform data from wide to long
data_long_QC <- reshape(All_Data_QC, direction='long', 
                        varying=c('Time1_QC_Maddy_MID02', 'Time1_raw_CSF', 'AgeYearsT1', 'Time2_QC_Maddy_MID02', 'Time2_raw_CSF', 'AgeYearsT2', 'Time3_QC_Maddy_MID02', 'Time3_raw_CSF', 'AgeYearsT3', 'Time4_QC_Maddy_MID02', 'Time4_raw_CSF', 'AgeYrsT4', 'Time5_QC_Maddy_MID02', 'Time5_raw_CSF', 'AgeYrsT5'), 
                        timevar='timepoint',
                        times=c('Time1', 'Time2', 'Time3', 'Time4', 'Time5'),
                        v.names=c('raw_CSF', 'Age', 'QC_Maddy_MID02'),
                        idvar='SUBJID')

#Drop time points with 2 or 3 on MID02 (EA-CSF) QC
data_long_QC_dropped<-data_long_QC[!(data_long_QC$QC_Maddy_MID02=="2" | data_long_QC$QC_Maddy_MID02=="3"),]
data_long_QC_dropped<-subset(data_long_QC_dropped, AutismControl!="NA")

#Create binary time variable, CSF in cm3
data_long_QC_dropped$time_bin[data_long_QC_dropped$timepoint=="Time1"] <- "0"
data_long_QC_dropped$time_bin[data_long_QC_dropped$timepoint=="Time2"] <- "1"
data_long_QC_dropped$time_bin[data_long_QC_dropped$timepoint=="Time3"] <- "1"
data_long_QC_dropped$time_bin[data_long_QC_dropped$timepoint=="Time4"] <- "1"
data_long_QC_dropped$time_bin[data_long_QC_dropped$timepoint=="Time5"] <- "1"
data_long_QC_dropped$CSF_cm <- data_long_QC_dropped$raw_CSF/1000

#Exclusion criteria 
data_long_QC_dropped <- subset(data_long_QC_dropped, Age<=45)
data_long_QC_dropped<-subset(data_long_QC_dropped, AutismControl!="NA")
data_long_QC_dropped<-subset(data_long_QC_dropped, Age!="NA")
data_long_QC_dropped<-subset(data_long_QC_dropped, CSF_cm!="NA")

#Create age_squared
data_long_QC_dropped$age_squared <- data_long_QC_dropped$Age*data_long_QC_dropped$Age

#---------------------------------ANALYSIS------------------------------------------------------------

#Run fitc model
fitC = lme(CSF_cm~AutismControl + Age + time_bin+ age_squared + AutismControl*Age + age_squared*AutismControl, 
           random=~1+Age|SUBJID,data= data_long_QC_dropped, method="ML")
summary(fitC)

#Run final best fitting model using REML
fitC_REML=lme(CSF_cm~AutismControl*Age+sex+timepoint+age_squared*AutismControl,random=~1+Age|SUBJID,
              data=data_long_QC_dropped,method="REML")
summary(fitc_REML)

#Extract random effects 
random.effects(fitc_REML)


#---------------------------------CENTERED MODELS-----------------------------------------------------------

#This model has age recentered at 10
data_long_QC_dropped$Age10<-data_long_QC_dropped$Age-10

fit10 = lme(CSF_cm~AutismControl + Age10 + time_bin+ age_squared + AutismControl*Age10 + age_squared*AutismControl, 
            random=~1+Age10|SUBJID,data= data_long_QC_dropped, method="REML")
summary(fit10) 


#This model has age recentered at 20
data_long_QC_dropped$Age20<-data_long_QC_dropped$Age-20

fit20 = lme(CSF_cm~AutismControl + Age20 + time_bin+ age_squared + AutismControl*Age20 + age_squared*AutismControl, 
            random=~1+Age20|SUBJID,data= data_long_QC_dropped, method="REML")
summary(fit20) 


#This model has age recentered at 30
data_long_QC_dropped$Age30<-data_long_QC_dropped$Age-30

fit30 = lme(CSF_cm~AutismControl + Age30 + time_bin+ age_squared + AutismControl*Age30 + age_squared*AutismControl, 
            random=~1+Age30|SUBJID,data= data_long_QC_dropped, method="REML")
summary(fit30)
