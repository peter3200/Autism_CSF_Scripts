#-----------------------------------------SETUP-----------------------------------------------

#Load packages
library(mosaic)
library(dplyr)
library(reshape2)
library(readxl)
library(tidyverse)
library(car)
library(MatchIt)
library(nlme)
library(readxl)

All_Data <- read.csv("C:/Users/maddy/Box/Autism_CSF/Utah_Longitudinal_Results/Raw_EACSF_Output/All_Data.csv", fill=TRUE)

All_Data$int_5_4 <- (All_Data$AgeYrsT5 - All_Data$AgeYrsT4)
All_Data$int_4_3 <- (All_Data$AgeYrsT4 - All_Data$AgeYearsT3)
All_Data$int_3_2 <- (All_Data$AgeYearsT3 - All_Data$AgeYearsT2)
All_Data$int_2_1 <- (All_Data$AgeYearsT2 - All_Data$AgeYearsT1)

All_Data$avg_int <- rowMeans(All_Data[,36:39], na.rm=TRUE)

#add the QC file
QC <- read_excel("C:/Users/maddy/Box/Autism_CSF/Utah_Longitudinal_Results/QC/QC_All.xlsx")
All_Data_QC <- merge(All_Data, QC, by ="SUBJID", all=TRUE)

#Merge in ADOS scores with wide dataset
#ADOS_long <- read_excel("C:/Users/maddy/Box/Autism_CSF/data/ADOSADI.xlsx")
#names(ADOS_long)[1] <- "SUBJID"
#merge with other wide data
#All_Data_QC <- merge(All_Data_QC, ADOS_long, by =c("SUBJID"), all=TRUE)

#Merge in complete ADOS scores
ADOS2 <- read.csv("C:/Users/maddy/Box/Autism_CSF/data/UofU_Longitudinal_ADOS_CSS_210428.csv")
names(ADOS2)[1] <- "SUBJID"
All_Data_QC <- merge(All_Data_QC, ADOS2, by =c("SUBJID"), all=TRUE)


#merge in IQ data 
IQ_all <- read_excel("C:/Users/maddy/Box/Autism_CSF/data/IQ_Times15_210506.xlsx")
IQ_all <- subset(IQ_all, TimePoint!="No Time 5 IQ")

#Subset the data
IQ_all.wide <- subset(IQ_all, select = c("LabID","PIQ", "VIQ", "FIQ", "TimePoint"))
#grab the IQ averages
#piq_avg_data<-aggregate(x = IQ_all.wide$PIQ,  # Specify  data column
#                        by = list(IQ_all.wide$LabID),              # Specify group indicator
#                        FUN = mean, na.rm=TRUE)  
#viq_avg_data<-aggregate(x = IQ_all.wide$VIQ,  # Specify  data column
#                        by = list(IQ_all.wide$LabID),              # Specify group indicator
#                        FUN = mean, na.rm=TRUE)
IQ_all.wide$FIQ<-as.numeric(IQ_all.wide$FIQ)
fiq_avg_data<-aggregate(x = IQ_all.wide$FIQ,  # Specify  data column
                        by = list(IQ_all.wide$LabID),              # Specify group indicator
                        FUN = mean, na.rm=TRUE)
#rename Group.1 to SUBJID, merge with All_Data
#names(piq_avg_data)[1] <- "SUBJID"
#names(viq_avg_data)[1] <- "SUBJID"
names(fiq_avg_data)[1] <- "SUBJID"
#names(piq_avg_data)[2] <- "piq_avg"
#names(viq_avg_data)[2] <- "viq_avg"
names(fiq_avg_data)[2] <- "fiq_avg"
#All_Data_QC <- merge(All_Data_QC, piq_avg_data, by ="SUBJID", all=TRUE)
All_Data_QC <- merge(All_Data_QC, fiq_avg_data, by ="SUBJID", all=TRUE)
#All_Data_QC <- merge(All_Data_QC, viq_avg_data, by ="SUBJID", all=TRUE)

All_Data_QC <- transform(All_Data_QC,scan1=ifelse(!is.na(ScanDateT1),1, 0 ))
All_Data_QC <- transform(All_Data_QC,scan2=ifelse(!is.na(ScanDateT2),1, 0 ))
All_Data_QC <- transform(All_Data_QC,scan3=ifelse(!is.na(ScanDateT3),1, 0 ))
All_Data_QC <- transform(All_Data_QC,scan4=ifelse(!is.na(ScanDateT4),1, 0 ))
All_Data_QC <- transform(All_Data_QC,scan5=ifelse(!is.na(ScanDateT5),1, 0 ))

All_Data_QC$num_scans <- (All_Data_QC$scan1 + All_Data_QC$scan2 + All_Data_QC$scan3 + All_Data_QC$scan4 + All_Data_QC$scan5)


#transform data from wide to long
data_long_QC <- reshape(All_Data_QC, direction='long', 
                        varying=c('Time1_QC_Maddy_MID02', 'Time1_raw_CSF', 'AgeYearsT1', 'Time2_QC_Maddy_MID02', 'Time2_raw_CSF', 'AgeYearsT2', 'Time3_QC_Maddy_MID02', 'Time3_raw_CSF', 'AgeYearsT3', 'Time4_QC_Maddy_MID02', 'Time4_raw_CSF', 'AgeYrsT4', 'Time5_QC_Maddy_MID02', 'Time5_raw_CSF', 'AgeYrsT5'), 
                        timevar='timepoint',
                        times=c('Time1', 'Time2', 'Time3', 'Time4', 'Time5'),
                        v.names=c('raw_CSF', 'Age', 'QC_Maddy_MID02'),
                        idvar='SUBJID')


#Merge in IQ long
IQ_time15 <- subset(IQ_all, TimePoint!="Pre-Time 1")
trim <- function (x) gsub("\\s", "", x)  
IQ_time15$TimePoint <- trim(IQ_time15$TimePoint)
names(IQ_time15)[1] <- "SUBJID"
names(IQ_time15)[4] <- "timepoint"
data_long_QC <- merge(data_long_QC, IQ_time15, by=c("SUBJID", "timepoint"), all=TRUE)


#drop time points with 2 or 3 on MID02 QC
data_long_QC_dropped<-data_long_QC[!(data_long_QC$QC_Maddy_MID02=="2" | data_long_QC$QC_Maddy_MID02=="3"),]
data_long_QC_dropped<-subset(data_long_QC_dropped, AutismControl!="NA")


#Drop participants with incomplete or questionable information 
data_long_QC_dropped<-subset(data_long_QC_dropped, SUBJID!=39031) #Subject has very large ventricles, listed as a control
data_long_QC_dropped<-subset(data_long_QC_dropped, SUBJID!=59507) #No record of a scan
data_long_QC_dropped<-subset(data_long_QC_dropped, SUBJID!=65548) #No record of a scan
data_long_QC_dropped<-subset(data_long_QC_dropped, SUBJID!=91541) #No record of a scan
data_long_QC_dropped<-subset(data_long_QC_dropped, SUBJID!=99995) #No record of a scan
data_long_QC_dropped<-subset(data_long_QC_dropped, SUBJID!=63305) #Same person as 59544
data_long_QC_dropped<-subset(data_long_QC_dropped, SUBJID!=88711) #Case study, not longitudinal scans.
data_long_QC_dropped<-subset(data_long_QC_dropped, SUBJID!=89340) #Case study, not longitudinal scans

##Merge in UofU Cross-sectional Freesurfer Values
  #FreesurferData <- read.csv("C:/Users/maddy/Box/Autism_CSF/FreesurferData_AllOutput_Ventricles.csv")
  #create uniform variable names in FS data
    #FreesurferData$SUBJID <- gsub("^.{0,3}", "", FreesurferData$LabID)
    #names(FreesurferData)[4] <- "timepoint"
    #FreesurferData_C <- read.csv("C:/Users/maddy/Box/Autism_CSF/data/Cross_Sectional_FS_Data.csv")
    #FreesurferData <- merge(FreesurferData_C, FreesurferData, by =c("SUBJID", "ScanDate"), all=TRUE)
    #data_long_QC_dropped <- merge(FreesurferData, data_long_QC_dropped, by =c("SUBJID", "timepoint"), all=TRUE)

#Merge in BYU Cross-sectional Freesurfer values
    FreesurferData <- read.csv("C:/Users/maddy/Box/Autism_CSF/data/freesurfer_cross/aseg_stats_concatenated_long.csv")
    data_long_QC_dropped <- merge(FreesurferData, data_long_QC_dropped, by=c("SUBJID", "timepoint"), all=TRUE)

#variables
data_long_QC_dropped$ADOS_comb <- ifelse(is.na(data_long_QC_dropped$TOTAL.CSS) & data_long_QC_dropped$Time5ADOS.TOTAL.CSS!="NA", data_long_QC_dropped$Time5ADOS.TOTAL.CSS, data_long_QC_dropped$TOTAL.CSS)
data_long_QC_dropped$time_bin[data_long_QC_dropped$timepoint=="Time1"] <- "0"
data_long_QC_dropped$time_bin[data_long_QC_dropped$timepoint=="Time2"] <- "1"
data_long_QC_dropped$time_bin[data_long_QC_dropped$timepoint=="Time3"] <- "1"
data_long_QC_dropped$time_bin[data_long_QC_dropped$timepoint=="Time4"] <- "1"
data_long_QC_dropped$time_bin[data_long_QC_dropped$timepoint=="Time5"] <- "1"
data_long_QC_dropped$CSF_cm <- data_long_QC_dropped$raw_CSF/1000
data_long_QC_dropped$ETIV_cm <- data_long_QC_dropped$EstimatedTotalIntraCranialVol/1000
data_long_QC_dropped$TBV_cm <- data_long_QC_dropped$BrainSegVol/1000
mean(data_long_QC_dropped$TBV_cm) #1289.437
TBV_mean=1289.437
data_long_QC_dropped$TBV_mc <- data_long_QC_dropped$TBV_cm - TBV_mean
data_long_QC_dropped$TBV_squared_mc <- data_long_QC_dropped$TBV_mc * data_long_QC_dropped$TBV_mc

#Exclusion criteria
data_long_QC_dropped <- subset(data_long_QC_dropped, Age<=43)
data_long_QC_dropped<-subset(data_long_QC_dropped, AutismControl!="NA")
data_long_QC_dropped<-subset(data_long_QC_dropped, Age!="NA")
data_long_QC_dropped<-subset(data_long_QC_dropped, CSF_cm!="NA")
data_long_QC_dropped<-subset(data_long_QC_dropped, TBV_cm!="NA")
meanAge = 20.28034
data_long_QC_dropped$age_mc <- data_long_QC_dropped$Age - meanAge
data_long_QC_dropped$age_squared_mc <- data_long_QC_dropped$age_mc*data_long_QC_dropped$age_mc
data_long_QC_dropped$age_cubed_mc <- data_long_QC_dropped$age_mc*data_long_QC_dropped$age_squared_mc
#data_long_QC_dropped$TotalBrain_Volume_cm <- data_long_QC_dropped$TotalBrain_Volume/1000

data_long_QC_dropped<-subset(data_long_QC_dropped, sex!="2") #drop females

#remove duplicates
data_long_QC_dropped <- data_long_QC_dropped[!duplicated(data_long_QC_dropped$TBV_cm), ]

#reshape back to wide from long! 
data_wide_all <- reshape(data_long_QC_dropped, 
                         timevar = "timepoint",
                         idvar = c("SUBJID", "sex", "AutismControl", "Group", "Mean_Age", "Participant", "avg_int"),
                         direction = "wide")
mean(data_wide_all$Mean_Age, na.rm = TRUE) #20.28034





#create dataset but include all QC values
data_long_QC <- merge(FreesurferData, data_long_QC, by =c("SUBJID", "timepoint"), all=TRUE)
data_long_QC$QC_fail <- ifelse(data_long_QC$QC_Maddy_MID02=="2" | data_long_QC$QC_Maddy_MID02=="3", 1, 0)
data_long_QC<-subset(data_long_QC, AutismControl!="NA")

#Drop participants with incomplete or questionable information 
data_long_QC<-subset(data_long_QC, SUBJID!=39031) #Subject has very large ventricles, listed as a control
data_long_QC<-subset(data_long_QC, SUBJID!=59507) #No record of a scan
data_long_QC<-subset(data_long_QC, SUBJID!=65548) #No record of a scan
data_long_QC<-subset(data_long_QC, SUBJID!=91541) #No record of a scan
data_long_QC<-subset(data_long_QC, SUBJID!=99995) #No record of a scan
data_long_QC<-subset(data_long_QC, SUBJID!=63305) #Same person as 59544
data_long_QC<-subset(data_long_QC, SUBJID!=88711) #Case study, not longitudinal scans.
data_long_QC<-subset(data_long_QC, SUBJID!=89340) #Case study, not longitudinal scans
#variables
data_long_QC$time_bin[data_long_QC$timepoint=="Time1"] <- "0"
data_long_QC$time_bin[data_long_QC$timepoint=="Time2"] <- "1"
data_long_QC$time_bin[data_long_QC$timepoint=="Time3"] <- "1"
data_long_QC$time_bin[data_long_QC$timepoint=="Time4"] <- "1"
data_long_QC$time_bin[data_long_QC$timepoint=="Time5"] <- "1"
data_long_QC$CSF_cm <- data_long_QC$raw_CSF/1000
#Exclusion criteria
data_long_QC <- subset(data_long_QC, Age<=43)
data_long_QC<-subset(data_long_QC, AutismControl!="NA")
data_long_QC<-subset(data_long_QC, Age!="NA")
data_long_QC<-subset(data_long_QC, CSF_cm!="NA")
data_long_QC$age_squared <- data_long_QC$Age*data_long_QC$Age
#data_long_QC$TotalBrain_Volume_cm <- data_long_QC$TotalBrain_Volume/1000
data_long_QC<-subset(data_long_QC, sex!="2") #drop females

#remove duplicates
data_long_QC <- data_long_QC[!duplicated(data_long_QC$raw_CSF), ]

#reshape back to wide from long! 
data_wide_all_QC <- reshape(data_long_QC, 
                         timevar = "timepoint",
                         idvar = c("SUBJID", "sex", "AutismControl", "QC_Maddy_MID02", "Group", "Mean_Age", "Participant", "avg_int", "QC_fail"),
                         direction = "wide")  



#----------------------------------REVIEWER 1: ROUND ONE---------------------------------------

#The mean age of the groups were adults, is this actually a developmental study or a study of adults?
  # Summary statistics
favstats(Age ~ AutismControl, data=data_long_QC_dropped)
  # Find proportion of participants under 20
  table(data_long_QC_dropped$AutismControl, data_long_QC_dropped$Age<=20)
 

#Sub-analysis of age-matched groups 
  #make autismcontrol 0/1 values
  data_long_QC_dropped$Dx_bin <- ifelse(data_long_QC_dropped$AutismControl == "autism", 0, 1)
  m.out=matchit(Dx_bin~age_mc, method="nearest", data=data_long_QC_dropped, ratio=1)
  summary(m.out)
  #verify good match with qq-plots
    plot(m.out, type = "qq", interactive = FALSE, which.xs = c("age_mc"))
  #extract matched dataset
    m.data1 <- match.data(m.out, drop.unmatched = TRUE)

  #run multilevel model 
    #REML
    fitmatch2 = lme(CSF_cm~AutismControl + age_mc + QC_Maddy_MID02 + TBV_mc +TBV_squared_mc+ time_bin+ age_squared_mc +age_cubed_mc+ AutismControl*age_cubed_mc+ AutismControl*age_mc + age_squared_mc*AutismControl, 
                   random=~1+age_mc|SUBJID,data= m.data1, method="REML")
    summary(fitmatch2)
    
    unique(m.data1$SUBJID)

  #Plot using matched data
      Palette <- c("#0072B2", "#E69F00")

      #plot age by CSF
      ggplot(m.data1, aes(x=Age, y=CSF_cm, color=AutismControl))+
        geom_line(aes(group=Participant))+
        labs(x = "Age (Years)", y = 'Extra-axial CSF Volume (' ~cm^3*')')+
        labs(fill = " ")+
        labs(color = " ")+
        scale_colour_manual(values=Palette, labels = c("Autism", "Control"))+
        scale_fill_manual(values=Palette, labels = c("Autism", "Control"))+
        geom_point(aes(fill=AutismControl), colour="black", pch=21, cex=1)+
        geom_smooth(method=loess, aes(fill=AutismControl), se=TRUE) +
        scale_y_continuous(limits=c(30,150))+
        scale_x_continuous(limits=c(0,42))+
        guides(color=guide_legend(override.aes=list(fill=NA)))+
        theme_bw()+
        theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
        theme(axis.title = element_text(colour = "black", size=9), axis.text.y =element_text(colour = "black", angle = 90, hjust = 0.6), 
          axis.text.x =element_text(colour = "black"), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
          legend.position = c(.9, .15), legend.title=element_text(colour = "black", size = 9), legend.text=element_text(colour = "black", size = 8), 
          legend.background = element_rect(fill="white", size=0.5) , axis.line = element_line(colour = "black", size = 1, linetype = "solid"), 
          axis.ticks = element_line(colour = "black", size =1, linetype ="solid"), panel.border = element_blank(), panel.background = element_blank())

      #ggsave(filename = paste("Rev1.1_210521.svg"), width = 3.4, height = 3.4,
       #  path = "C:/Users/maddy/Box/Autism_CSF/figures", dpi = 300)
      
      
  # Did the matching reduce the age gap between groups?
      favstats(Age~AutismControl, data= m.data1)
      
  # MLM with Age as outcome and group as predictor
      fitage = lme(age_mc~AutismControl, random=~1 |SUBJID,data= m.data1, method="REML")
      summary(fitage)
      
      
      
#Analysis with males only (N= 439 scans)
  fitC = lme(CSF_cm~AutismControl + age_mc + QC_Maddy_MID02 + TBV_mc +TBV_squared_mc+ time_bin+ age_squared_mc +age_cubed_mc+ AutismControl*age_cubed_mc+ AutismControl*age_mc + age_squared_mc*AutismControl, 
           random=~1+age_mc|SUBJID,data= data_long_QC_dropped, method="REML")
  summary(fitC)

  


#Attrition analysis: did the clinical severity of the autistic individual preclude their ability to return?
  #Suggestion from Molly: compare ADOS scores between the ASD participants with a single timepoint vs those w/ more than 1 timepoint.
    #Dummy variable for single scan versus multiple scans
    data_long_QC_dropped$single_scan <- ifelse(data_long_QC_dropped$num_scans == "1", "1", "0")
    #subset to participants with ADOS
    data_long_QC_ADOS <- subset(data_long_QC_dropped, ADOS_comb!="NA")
    #MLM
    fitsingle = lme(ADOS_comb~single_scan, random=~1 |SUBJID,data= data_long_QC_ADOS, method="REML")
    summary(fitsingle)
  

  #Corr. of clinical severity by number of scans available

  #data_wide_all_QC <- transform(data_wide_all_QC,scan1=ifelse(!is.na(ScanDateT1.Time1),1, 0 ))
  #data_wide_all_QC <- transform(data_wide_all_QC,scan2=ifelse(!is.na(ScanDateT2.Time2),1, 0 ))
  #data_wide_all_QC <- transform(data_wide_all_QC,scan3=ifelse(!is.na(ScanDateT3.Time3),1, 0 ))
  #data_wide_all_QC <- transform(data_wide_all_QC,scan4=ifelse(!is.na(ScanDateT4.Time4),1, 0 ))
  #data_wide_all_QC <- transform(data_wide_all_QC,scan5=ifelse(!is.na(ScanDateT5.Time5),1, 0 ))

  #data_wide_all_QC$num_scans <- (data_wide_all_QC$scan1 + data_wide_all_QC$scan2 + data_wide_all_QC$scan3 + data_wide_all_QC$scan4 + data_wide_all_QC$scan5)
  #table(data_wide_all_QC$num_scans, data_wide_all_QC$AutismControl)


    
  #MLM QC scores ~ AutismControl
    fitQC = lme(QC_Maddy_MID02~AutismControl, 
               random=~1|SUBJID,data= data_long_QC_dropped, method="REML")
    summary(fitQC)
    
  # MLM num_scans ~ ADOS CSS  
    data_long_QC_dropped_ADOS<-subset(data_long_QC_dropped, ADOS_comb!="NA")
    fitnum = lme(num_scans~ADOS_comb, 
                random=~1|SUBJID,data= data_long_QC_dropped_ADOS, method="REML")
    summary(fitnum)
    
    boxplot(num_scans~ADOS_comb,
            data=data_long_QC_dropped_ADOS,
            xlab="ADOS CSS at Entry",
            ylab="Number of Available Scans",
            col="seagreen",
            border="black"
    )
    
    
  #MLM with ADOS Predictor
    fitADOS = lme(CSF_cm~ADOS_comb + age_mc + TBV_mc + TBV_squared_mc + QC_Maddy_MID02 + time_bin + age_squared_mc + age_cubed_mc, 
                  random=~1+age_mc|SUBJID,data= data_long_QC_dropped_ADOS, method="REML")
    summary(fitADOS) 

    
    
#Is this is a study of ASD individuals who are merely 'high functioning'? 
    #Subset dataset to only include participants with avg FIQ score
    which( colnames(data_wide_all)=="FIQ.Time1" )
    which( colnames(data_wide_all)=="FIQ.Time2" )
    which( colnames(data_wide_all)=="FIQ.Time3" )
    which( colnames(data_wide_all)=="FIQ.Time4" )
    which( colnames(data_wide_all)=="FIQ.Time5" )
    data_wide_all$FIQ.Time1 <- as.numeric(data_wide_all$FIQ.Time1)
    data_wide_all$FIQ.Time2 <- as.numeric(data_wide_all$FIQ.Time2)
    data_wide_all$FIQ.Time3 <- as.numeric(data_wide_all$FIQ.Time3)
    data_wide_all$FIQ.Time4 <- as.numeric(data_wide_all$FIQ.Time4)
    data_wide_all$FIQ.Time5 <- as.numeric(data_wide_all$FIQ.Time5)
    data_wide_all$fiq_avg <- rowMeans(data_wide_all[,c(261,663,395,529,127)], na.rm=TRUE)
    
    data_wide_all_FIQ <- subset(data_wide_all, fiq_avg!="NA")
    data_wide_all_FIQ$fiq_avg <- as.numeric(data_wide_all_FIQ$fiq_avg)
    data_wide_all_FIQ$LVCP <- ifelse(data_wide_all_FIQ$fiq_avg <= 79, "1", "0")
    #Number of participants with LVCP/HVCP
    table(data_wide_all_FIQ$LVCP, data_wide_all_FIQ$AutismControl)
    #Number of scans for LVCP and HVCP individuals
    data_wide_all_FIQ <- transform(data_wide_all_FIQ,scan1=ifelse(!is.na(ScanDateT1.Time1),1, 0 ))
    data_wide_all_FIQ <- transform(data_wide_all_FIQ,scan2=ifelse(!is.na(ScanDateT2.Time2),1, 0 ))
    data_wide_all_FIQ <- transform(data_wide_all_FIQ,scan3=ifelse(!is.na(ScanDateT3.Time3),1, 0 ))
    data_wide_all_FIQ <- transform(data_wide_all_FIQ,scan4=ifelse(!is.na(ScanDateT4.Time4),1, 0 ))
    data_wide_all_FIQ <- transform(data_wide_all_FIQ,scan5=ifelse(!is.na(ScanDateT5.Time5),1, 0 ))
    data_wide_all_FIQ$num_scans <- (data_wide_all_FIQ$scan1 + data_wide_all_FIQ$scan2 + data_wide_all_FIQ$scan3 + data_wide_all_FIQ$scan4 + data_wide_all_FIQ$scan5)
    data_wide_all_LVCP <- subset(data_wide_all_FIQ, LVCP=="1")
    data_wide_all_LVCP_ASD <- subset(data_wide_all_LVCP, AutismControl=="autism")
    data_wide_all_LVCP_TD <- subset(data_wide_all_LVCP, AutismControl=="control")
    #LVCP #s
    sum(data_wide_all_LVCP_ASD$num_scans)  
    sum(data_wide_all_LVCP_TD$num_scans)
    
    data_wide_all_HVCP <- subset(data_wide_all_FIQ, LVCP=="0")
    data_wide_all_HVCP_ASD <- subset(data_wide_all_HVCP, AutismControl=="autism")
    data_wide_all_HVCP_TD <- subset(data_wide_all_HVCP, AutismControl=="control")
    #HVCP #s
    sum(data_wide_all_HVCP_ASD$num_scans)  
    sum(data_wide_all_HVCP_TD$num_scans)
    
    
    
    #Distributions of FIQ-avg scores   
    which( colnames(data_wide_all)=="FIQ.Time1" )
    which( colnames(data_wide_all)=="FIQ.Time2" )
    which( colnames(data_wide_all)=="FIQ.Time3" )
    which( colnames(data_wide_all)=="FIQ.Time4" )
    which( colnames(data_wide_all)=="FIQ.Time5" )
    data_wide_all$FIQ.Time1 <- as.numeric(data_wide_all$FIQ.Time1)
    data_wide_all$FIQ.Time2 <- as.numeric(data_wide_all$FIQ.Time2)
    data_wide_all$FIQ.Time3 <- as.numeric(data_wide_all$FIQ.Time3)
    data_wide_all$FIQ.Time4 <- as.numeric(data_wide_all$FIQ.Time4)
    data_wide_all$FIQ.Time5 <- as.numeric(data_wide_all$FIQ.Time5)
    data_wide_all$fiq_avg <- rowMeans(data_wide_all[,c(259,658,392,525,126)], na.rm=TRUE)
    
    fiq <- ggplot(data_wide_all, aes(x=AutismControl, y=fiq_avg, fill=AutismControl)) +
      geom_violin(trim=FALSE) + labs(x = "Group", y = 'Average Full-Scale IQ Score')
    fiq + geom_boxplot(width=0.1) + theme_classic()
        
    #ggsave(filename = paste("Rev1.2_210513.svg"), width = 3.6, height = 3.6,
    #       path = "C:/Users/maddy/Box/Autism_CSF/figures", dpi = 300)
    
    
#Is extra-axial CSF volume related to ASD severity?
    #Subset to only include participants with an ADOS CSS score
    data_long_QC_dropped_ADOS<-subset(data_long_QC_dropped, ADOS_comb!="NA")
    
    #Multilevel model 
    fitADOS = lme(CSF_cm~ADOS_comb + age_mc + TBV_mc + TBV_squared_mc + QC_Maddy_MID02 + time_bin + age_squared_mc + age_cubed_mc, 
               random=~1+age_mc|SUBJID,data= data_long_QC_dropped_ADOS, method="REML")
    summary(fitADOS) 
    
    
    #boxplots 
    boxplot(CSF_cm~ADOS_comb,
            data=data_long_QC_dropped_ADOS,
            main="EACSF Volume by ADOS CSS Entry Score",
            xlab="ADOS CSS at Entry",
            ylab="EACSF (in cm3)",
            border="black"
    )
    
    
    
    #Plot EACSF by ADOS CSS at entry score
    Palette <- c("#0072B2", "#E69F00")
    
    #plot ADOS by CSF
    ggplot(data_long_QC_dropped_ADOS, aes(x=ADOS_comb, y=CSF_cm))+
      labs(x = "ADOS CSS Scores at Entry", y = 'EA-CSF Volume ('~cm^3*')')+
      geom_point(colour="black", pch=21, cex=1)+
      labs(fill = " ")+
      labs(color = " ")+
      geom_smooth(method=lm, se=TRUE) +
      guides(color=guide_legend(override.aes=list(fill=NA)))+
      scale_y_continuous(limits=c(30,150))+
      scale_x_continuous(limits=c(0,15), expand = c(0,0))+
      theme_bw()+
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
      theme(axis.title = element_text(colour = "black", size=9), axis.text.y =element_text(colour = "black", angle = 90, hjust = 0.6), 
            axis.text.x =element_text(colour = "black"), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
            legend.position = c(.1, .9), legend.title=element_text(colour = "black", size = 9), legend.text=element_text(colour = "black", size = 8), 
            legend.background = element_rect(fill="white", size=0.5) , axis.line = element_line(colour = "black", size = 1, linetype = "solid"), 
            axis.ticks = element_line(colour = "black", size =1, linetype ="solid"), panel.border = element_blank(), panel.background = element_blank())
    
    
   
    
    
#How does IQ relate to EA-CSF volumes?
    #FIQ *long* as a predictor of EA-CSF 
    data_long_QC_dropped$FIQ <- as.numeric(data_long_QC_dropped$FIQ)
    data_long_QC_dropped_FIQ<-subset(data_long_QC_dropped, FIQ!="NA") 
    data_long_QC_dropped_FIQ<-subset(data_long_QC_dropped_FIQ, FIQ!="NaN")
    data_long_QC_dropped_FIQ<-subset(data_long_QC_dropped_FIQ, AutismControl!="Na")
    data_long_QC_dropped_FIQ<-subset(data_long_QC_dropped_FIQ, sex!="2")
    #Multilevel model (N=297 scans), 188 ASD, 109 TD
    fitFIQ2 = lme(CSF_cm~FIQ + AutismControl + QC_Maddy_MID02 + TBV_mc + age_mc +TBV_squared_mc+ time_bin + age_squared_mc + age_cubed_mc + age_cubed_mc*AutismControl +  AutismControl*age_mc + age_squared_mc*AutismControl, 
                    random=~1+age_mc|SUBJID,data=data_long_QC_dropped_FIQ, method="REML")
    summary(fitFIQ2)
   
    table(data_long_QC_dropped_FIQ$AutismControl)
    table(data_long_QC_dropped_FIQ$timepoint)
    
    #Plot EACSF by FIQ long
    Palette <- c("#0072B2", "#E69F00", "#E69F00")
    ggplot(data_long_QC_dropped_FIQ, aes(x=FIQ, y=CSF_cm, color=AutismControl))+
      geom_line(aes(color = AutismControl, group = SUBJID))+
      labs(x = "Full-scale IQ Score", y = 'EA-CSF Volume ('~cm^3*')')+
      geom_point(aes(fill=AutismControl), colour="black", pch=21, cex=1)+
      labs(fill = " ")+
      labs(color = " ")+
      scale_colour_manual(values=Palette, labels = c("Autism", "Control", "NA"))+
      scale_fill_manual(values=Palette, labels = c("Autism", "Control", "NA"))+
      geom_smooth(method=loess, aes(fill=AutismControl), se=TRUE) +
      guides(color=guide_legend(override.aes=list(fill=NA)))+
      scale_y_continuous(limits=c(30,170))+
      scale_x_continuous(limits=c(45,165), expand = c(0,0))+
      theme_bw()+
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
      theme(axis.title = element_text(colour = "black", size=9), axis.text.y =element_text(colour = "black", angle = 90, hjust = 0.6), 
            axis.text.x =element_text(colour = "black"), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
            legend.position = c(.15, .9), legend.title=element_text(colour = "black", size = 9), legend.text=element_text(colour = "black", size = 8), 
            legend.background = element_rect(fill="white", size=0.2) , axis.line = element_line(colour = "black", size = 1, linetype = "solid"), 
            axis.ticks = element_line(colour = "black", size =1, linetype ="solid"), panel.border = element_blank(), panel.background = element_blank())
    
    
    #VIQ *long* as a predictor of EA-CSF
    data_long_QC_dropped$VIQ <- as.numeric(data_long_QC_dropped$VIQ)
    data_long_QC_dropped_VIQ<-subset(data_long_QC_dropped, VIQ!="NA") 
    data_long_QC_dropped_VIQ<-subset(data_long_QC_dropped_VIQ, FIQ!="NaN")
    data_long_QC_dropped_VIQ<-subset(data_long_QC_dropped_VIQ, AutismControl!="Na")
    data_long_QC_dropped_VIQ<-subset(data_long_QC_dropped_VIQ, sex!="2")
    #Multilevel model (N=218 scans)
    fitVIQ2 = lme(CSF_cm~VIQ + AutismControl + age_mc + TBV_mc +TBV_squared_mc+QC_Maddy_MID02 + time_bin + age_squared_mc +age_cubed_mc + age_cubed_mc*AutismControl + AutismControl*age_mc + age_squared_mc*AutismControl, 
                  random=~1+age_mc|SUBJID,data=data_long_QC_dropped_VIQ, method="REML")
    summary(fitVIQ2)
    
    table(data_long_QC_dropped_VIQ$AutismControl)
    table(data_long_QC_dropped_VIQ$timepoint)
    
    #Plot EACSF by VIQ long
    Palette <- c("#0072B2", "#E69F00", "#E69F00")
    ggplot(data_long_QC_dropped_VIQ, aes(x=FIQ, y=CSF_cm, color=AutismControl))+
      geom_line(aes(color = AutismControl, group = SUBJID))+
      labs(x = "Verbal IQ Score", y = 'EA-CSF Volume ('~cm^3*')')+
      geom_point(aes(fill=AutismControl), colour="black", pch=21, cex=1)+
      labs(fill = " ")+
      labs(color = " ")+
      scale_colour_manual(values=Palette, labels = c("Autism", "Control", "NA"))+
      scale_fill_manual(values=Palette, labels = c("Autism", "Control", "NA"))+
      geom_smooth(method=loess, aes(fill=AutismControl), se=TRUE) +
      guides(color=guide_legend(override.aes=list(fill=NA)))+
      scale_y_continuous(limits=c(30,150))+
      scale_x_continuous(limits=c(45,170), expand = c(0,0))+
      theme_bw()+
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
      theme(axis.title = element_text(colour = "black", size=9), axis.text.y =element_text(colour = "black", angle = 90, hjust = 0.6), 
            axis.text.x =element_text(colour = "black"), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
            legend.position = c(.15, .9), legend.title=element_text(colour = "black", size = 9), legend.text=element_text(colour = "black", size = 8), 
            legend.background = element_rect(fill="white", size=0.5) , axis.line = element_line(colour = "black", size = 1, linetype = "solid"), 
            axis.ticks = element_line(colour = "black", size =1, linetype ="solid"), panel.border = element_blank(), panel.background = element_blank())
    
    
    #PIQ *long* as a predictor of EA-CSF
    data_long_QC_dropped$PIQ <- as.numeric(data_long_QC_dropped$PIQ)
    data_long_QC_dropped_PIQ<-subset(data_long_QC_dropped, PIQ!="NA") 
    data_long_QC_dropped_PIQ<-subset(data_long_QC_dropped_PIQ, PIQ!="NaN")
    data_long_QC_dropped_PIQ<-subset(data_long_QC_dropped_PIQ, AutismControl!="Na")
    data_long_QC_dropped_PIQ<-subset(data_long_QC_dropped_PIQ, sex!="2")
    #Multilevel model 
    fitPIQ2 = lme(CSF_cm~PIQ + AutismControl + age_mc + TBV_mc +TBV_squared_mc+QC_Maddy_MID02 + time_bin + age_squared_mc +age_cubed_mc + age_cubed_mc*AutismControl + AutismControl*age_mc + age_squared_mc*AutismControl, 
                  random=~1+age_mc|SUBJID,data=data_long_QC_dropped_PIQ, method="REML")
    summary(fitPIQ2)
    
    table(data_long_QC_dropped_PIQ$AutismControl)
    table(data_long_QC_dropped_PIQ$timepoint)
    
    #Plot EACSF by PIQ long
    Palette <- c("#0072B2", "#E69F00", "#E69F00")
    ggplot(data_long_QC_dropped_PIQ, aes(x=PIQ, y=CSF_cm, color=AutismControl))+
      geom_line(aes(color = AutismControl, group = SUBJID))+
      labs(x = "Performance IQ Score", y = 'EA-CSF Volume ('~cm^3*')')+
      geom_point(aes(fill=AutismControl), colour="black", pch=21, cex=1)+
      labs(fill = " ")+
      labs(color = " ")+
      scale_colour_manual(values=Palette, labels = c("Autism", "Control", "NA"))+
      scale_fill_manual(values=Palette, labels = c("Autism", "Control", "NA"))+
      geom_smooth(method=loess, aes(fill=AutismControl), se=TRUE) +
      guides(color=guide_legend(override.aes=list(fill=NA)))+
      scale_y_continuous(limits=c(30,150))+
      scale_x_continuous(limits=c(45,170), expand = c(0,0))+
      theme_bw()+
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
      theme(axis.title = element_text(colour = "black", size=9), axis.text.y =element_text(colour = "black", angle = 90, hjust = 0.6), 
            axis.text.x =element_text(colour = "black"), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
            legend.position = c(.15, .9), legend.title=element_text(colour = "black", size = 9), legend.text=element_text(colour = "black", size = 8), 
            legend.background = element_rect(fill="white", size=0.5) , axis.line = element_line(colour = "black", size = 1, linetype = "solid"), 
            axis.ticks = element_line(colour = "black", size =1, linetype ="solid"), panel.border = element_blank(), panel.background = element_blank())
    
    
#Is there a difference in IQ between those who did or did not have ADOS?
    #Create vars
    #data_wide_all$ados_dummy <- ifelse(is.na(data_wide_all$ADOS_comb.Time1) & is.na(data_wide_all$ADOS_comb.Time2) & is.na(data_wide_all$ADOS_comb.Time3) & is.na(data_wide_all$ADOS_comb.Time4) & is.na(data_wide_all$ADOS_comb.Time5), "0", "1")
    #table(data_wide_all$ados_dummy)
    
    #which( colnames(data_wide_all)=="FIQ.Time1" )
    #which( colnames(data_wide_all)=="FIQ.Time2" )
    #which( colnames(data_wide_all)=="FIQ.Time3" )
    #which( colnames(data_wide_all)=="FIQ.Time4" )
    #which( colnames(data_wide_all)=="FIQ.Time5" )
    #data_wide_all$FIQ.Time1 <- as.numeric(data_wide_all$FIQ.Time1)
    #data_wide_all$FIQ.Time2 <- as.numeric(data_wide_all$FIQ.Time2)
    #data_wide_all$FIQ.Time3 <- as.numeric(data_wide_all$FIQ.Time3)
    #data_wide_all$FIQ.Time4 <- as.numeric(data_wide_all$FIQ.Time4)
    #data_wide_all$FIQ.Time5 <- as.numeric(data_wide_all$FIQ.Time5)
    #data_wide_all$fiq_avg <- rowMeans(data_wide_all[,c(259,658,392,525,126)], na.rm=TRUE)
    #favstats(data_wide_all$fiq_avg)  
      
    #Compare avg. FIQ with two-sided t-test
    #t.test(fiq_avg ~ ados_dummy, data = data_wide_all, mu = 0, alternative = "two.sided", conf.level = 0.95)
    
    
    
# Was the proportion of ASD and TD acquired by each sequence/head coil the same?
    #Table of timebin and autismcontrol
    table(data_long_QC_dropped$time_bin, data_long_QC_dropped$AutismControl)


    
# There is a large difference between the number of ASD (n=31) and TD (n=53) who had only 1 scan. Were these ~20 TD individuals scanned only once the same individuals the authors mentioned who entered the study later?    
    data_wide_all <- transform(data_wide_all,scan1=ifelse(!is.na(ScanDateT1.Time1),1, 0 ))
    data_wide_all <- transform(data_wide_all,scan2=ifelse(!is.na(ScanDateT2.Time2),1, 0 ))
    data_wide_all <- transform(data_wide_all,scan3=ifelse(!is.na(ScanDateT3.Time3),1, 0 ))
    data_wide_all <- transform(data_wide_all,scan4=ifelse(!is.na(ScanDateT4.Time4),1, 0 ))
    data_wide_all <- transform(data_wide_all,scan5=ifelse(!is.na(ScanDateT5.Time5),1, 0 ))
    
    data_wide_all$num_scans <- (data_wide_all$scan1 + data_wide_all$scan2 + data_wide_all$scan3 + data_wide_all$scan4 + data_wide_all$scan5)
    table(data_wide_all$num_scans, data_wide_all$AutismControl)
    
    #Set up if/else function
    data_wide_all$new_time1 <- ifelse(data_wide_all$scan1=="1", 1, 0)
    data_wide_all$new_time2 <- ifelse(data_wide_all$scan1=="0" & data_wide_all$scan2=="1", 1, 0)
    data_wide_all$new_time3 <- ifelse(data_wide_all$scan1=="0" & data_wide_all$scan2=="0" & data_wide_all$scan3=="1", 1, 0)        
    data_wide_all$new_time4 <- ifelse(data_wide_all$scan1=="0" & data_wide_all$scan2=="0" & data_wide_all$scan3=="0" & data_wide_all$scan4=="1", 1, 0)
    data_wide_all$new_time5 <- ifelse(data_wide_all$scan1=="0" & data_wide_all$scan2=="0" & data_wide_all$scan3=="0" & data_wide_all$scan4=="0" & data_wide_all$scan5=="1", 1, 0)
    
    table(data_wide_all$new_time1=="1", data_wide_all$SUBJID)
    
    #Number of new subjects at each timepoint
    table(data_wide_all$new_time1=="1", data_wide_all$AutismControl)
    table(data_wide_all$new_time2=="1", data_wide_all$AutismControl)
    table(data_wide_all$new_time3=="1", data_wide_all$AutismControl)
    table(data_wide_all$new_time4=="1", data_wide_all$AutismControl)
    table(data_wide_all$new_time5=="1", data_wide_all$AutismControl)
    
    
    data_wide_20 <- subset(data_wide_all, num_scans ==1)
    data_wide_20 <- subset(data_wide_20, AutismControl == "control")
    #Due to attrition or simply joined later? 34 TD joined at time 5 
    table(data_wide_20$new_time1=="1") #3 joined at time1 with only 1 scan
    table(data_wide_20$new_time2=="1") #2 joined at time2 with only 1 scan
    table(data_wide_20$new_time3=="1") #7 joined at time3 with only 1 scan
    table(data_wide_20$new_time4=="1") #3 joined at time4 with only 1 scan
    table(data_wide_20$new_time5=="1") #34 joined at time5 with only 1 scan
    
    

    

    
#Are the results the same if there were two separate sub-analyses (i.e., if scans from a single head coil/sequence are analyzed separately from the other head coil/sequence)? 
    #Subset time_bin = 1
    data_long_QC_dropped_bin1 <- subset(data_long_QC_dropped, time_bin==1)
    #MLM
    fit_bin1 = lme(CSF_cm~AutismControl + age_mc + TBV_mc +TBV_squared_mc+ QC_Maddy_MID02 + age_squared_mc +age_cubed_mc + age_cubed_mc*AutismControl + AutismControl*age_mc + age_squared_mc*AutismControl, 
                   random=~1+age_mc|SUBJID,data= data_long_QC_dropped_bin1, method="REML")
    summary(fit_bin1)
 
    
    #plot age by CSF
    ggplot(data_long_QC_dropped_bin1, aes(x=Age, y=CSF_cm, color=AutismControl))+
      geom_line(aes(color = AutismControl, group = SUBJID))+
      labs(x = "Age (Years)", y = 'Extra-axial CSF Volume ( '~cm^3*') for Times 2-5')+
      geom_point(aes(fill=AutismControl), colour="black", pch=21, cex=1)+
      labs(fill = " ")+
      labs(color = " ")+
      scale_colour_manual(values=Palette, labels = c("Autism", "Control", "NA"))+
      scale_fill_manual(values=Palette, labels = c("Autism", "Control", "NA"))+
      geom_smooth(method=loess, aes(fill=AutismControl), se=TRUE) +
      guides(color=guide_legend(override.aes=list(fill=NA)))+
      scale_y_continuous(limits=c(0,150))+
      scale_x_continuous(limits=c(0,43), expand = c(0,0))+
      theme_bw()+
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
      theme(axis.title = element_text(colour = "black", size=9), axis.text.y =element_text(colour = "black", angle = 90, hjust = 0.6), 
            axis.text.x =element_text(colour = "black"), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
            legend.position = c(.9, .2), legend.title=element_text(colour = "black", size = 9), legend.text=element_text(colour = "black", size = 8), 
            legend.background = element_rect(fill="white", size=0.5) , axis.line = element_line(colour = "black", size = 1, linetype = "solid"), 
            axis.ticks = element_line(colour = "black", size =1, linetype ="solid"), panel.border = element_blank(), panel.background = element_blank())
    
    #ggsave(filename = paste("Rev1.3_210521.svg"), width = 3.4, height = 3.4,
    #       path = "C:/Users/maddy/Box/Autism_CSF/figures", dpi = 300)
    
    
    
#Was TBV x group interaction tested? (does the effect of brain volume on extra-axial CSF differ between groups?)       
    #MLM with TCVxAutismControl (N = 412 scans, 184 participants)
    fit_TBV1 = lme(CSF_cm~AutismControl + age_mc + QC_Maddy_MID02 + age_squared_mc +age_cubed_mc + age_cubed_mc*AutismControl +TBV_mc +TBV_squared_mc+ AutismControl*age_mc + age_squared_mc*AutismControl, 
                  random=~1+age_mc|SUBJID,data= data_long_QC_dropped, method="REML")
    summary(fit_TBV1)
    
    #interaction
    fit_TBV2 = lme(CSF_cm~AutismControl + age_mc + QC_Maddy_MID02 +TBV_squared_mc+ age_squared_mc +age_cubed_mc + age_cubed_mc*AutismControl + TBV_mc*AutismControl + AutismControl*age_mc + age_squared_mc*AutismControl, 
                   random=~1+age_mc|SUBJID,data= data_long_QC_dropped, method="REML")
    summary(fit_TBV2)
    
    #No TBV covariate
    fit_noTBV = lme(CSF_cm~AutismControl + age_mc + QC_Maddy_MID02 + time_bin+ age_squared_mc +age_cubed_mc + age_cubed_mc*AutismControl + AutismControl*age_mc + age_squared_mc*AutismControl, 
               random=~1+age_mc|SUBJID,data= data_long_QC_dropped, method="REML")
    summary(fit_noTBV)
    
    
    
    
#What was the effect of sex? (comment out exclusion of females in SETUP and re-load data)
    fit_s = lme(CSF_cm~AutismControl + age_mc + sex +TBV_mc +TBV_squared_mc+ QC_Maddy_MID02 + time_bin+ age_squared_mc +age_cubed_mc + age_cubed_mc*AutismControl + AutismControl*age_mc + age_squared_mc*AutismControl, 
               random=~1+age_mc|SUBJID,data= data_long_QC_dropped, method="REML")
    summary(fit_s)
    

#What was the relationship to IQ scores or ADOS scores, given the previous associations with nonverbal, motor, and ADOS scores?     
  #ADOS Predictor model
    #Subset to only include participants with an ADOS CSS score
    data_long_QC_dropped_ADOS<-subset(data_long_QC_dropped, ADOS_comb!="NA")
    
    #Multilevel model 
    fitADOS = lme(CSF_cm~ADOS_comb + age_mc + TBV_mc + TBV_squared_mc + QC_Maddy_MID02 + time_bin + age_squared_mc + age_cubed_mc, 
                  random=~1+age_mc|SUBJID,data= data_long_QC_dropped_ADOS, method="REML")
    summary(fitADOS) 
    
  # EA-CSF by ADOS CSS Corr using scans from the time when CSS scores were collected
    #data_wide_all <- transform(data_wide_all,scan1=ifelse(!is.na(ScanDateT1.Time1),1, 0 ))
    #data_wide_all <- transform(data_wide_all,scan2=ifelse(!is.na(ScanDateT2.Time2),1, 0 ))
    #data_wide_all <- transform(data_wide_all,scan3=ifelse(!is.na(ScanDateT3.Time3),1, 0 ))
    #data_wide_all <- transform(data_wide_all,scan4=ifelse(!is.na(ScanDateT4.Time4),1, 0 ))
    #data_wide_all <- transform(data_wide_all,scan5=ifelse(!is.na(ScanDateT5.Time5),1, 0 ))
    
    #data_wide_all$num_scans <- (data_wide_all$scan1 + data_wide_all$scan2 + data_wide_all$scan3 + data_wide_all$scan4 + data_wide_all$scan5)
    #table(data_wide_all$num_scans, data_wide_all$AutismControl)
    
    #Set up if/else function
    #data_wide_all$new_time1 <- ifelse(data_wide_all$scan1=="1", 1, "")
    #data_wide_all$new_time2 <- ifelse(data_wide_all$scan1=="0" & data_wide_all$scan2=="1", 2, "")
    #data_wide_all$new_time3 <- ifelse(data_wide_all$scan1=="0" & data_wide_all$scan2=="0" & data_wide_all$scan3=="1", 3, "")        
    #data_wide_all$new_time4 <- ifelse(data_wide_all$scan1=="0" & data_wide_all$scan2=="0" & data_wide_all$scan3=="0" & data_wide_all$scan4=="1", 4, "")
    #data_wide_all$new_time5 <- ifelse(data_wide_all$scan1=="0" & data_wide_all$scan2=="0" & data_wide_all$scan3=="0" & data_wide_all$scan4=="0" & data_wide_all$scan5=="1", 5, "")
   
    #concatenate new_time vars
    #data_wide_all$time_entry <- (paste0(data_wide_all$new_time1, data_wide_all$new_time2, data_wide_all$new_time3, data_wide_all$new_time4, data_wide_all$new_time5))
    #table(data_wide_all$time_entry)
    
    #set entry scan to time5 if time5 ADOS was used
    #ADOS_long <- read.csv("C:/Users/maddy/Box/Autism_CSF/data/UofU_Longitudinal_ADOS_CSS_210428.csv") 
    #names(ADOS_long)[1] <- "SUBJID"
    #merge with other wide data
    #data_wide_all_ADOS <- merge(ADOS_long, data_wide_all, by =c("SUBJID"), all=FALSE)
    #data_wide_all_ADOS <- subset(data_wide_all_ADOS, sex!=2)
    #data_wide_all_ADOS$ADOS_comb <- ifelse(is.na(data_wide_all_ADOS$TOTAL.CSS) & data_wide_all_ADOS$Time5ADOS.TOTAL.CSS!="NA", data_wide_all_ADOS$Time5ADOS.TOTAL.CSS, data_wide_all_ADOS$TOTAL.CSS)
    
    #data_wide_all_ADOS$time_entry <- ifelse(is.na(data_wide_all_ADOS$TOTAL.CSS), "5", data_wide_all_ADOS$time_entry)
    #table(data_wide_all_ADOS$time_entry)
    
    #Entry_CSF
    #data_wide_all_ADOS$entry_CSF <- NA
    #data_wide_all_ADOS$entry_CSF <- ifelse(data_wide_all_ADOS$time_entry=="1", data_wide_all_ADOS$CSF_cm.Time1, data_wide_all_ADOS$entry_CSF)
    #data_wide_all_ADOS$entry_CSF <- ifelse(data_wide_all_ADOS$time_entry=="2", data_wide_all_ADOS$CSF_cm.Time2, data_wide_all_ADOS$entry_CSF)
    #data_wide_all_ADOS$entry_CSF <- ifelse(data_wide_all_ADOS$time_entry=="3", data_wide_all_ADOS$CSF_cm.Time3, data_wide_all_ADOS$entry_CSF)
    #data_wide_all_ADOS$entry_CSF <- ifelse(data_wide_all_ADOS$time_entry=="4", data_wide_all_ADOS$CSF_cm.Time4, data_wide_all_ADOS$entry_CSF)
    #data_wide_all_ADOS$entry_CSF <- ifelse(data_wide_all_ADOS$time_entry=="5", data_wide_all_ADOS$CSF_cm.Time5, data_wide_all_ADOS$entry_CSF)
    #data_wide_all_ADOS <- subset(data_wide_all_ADOS, entry_CSF!="NA")
    #data_wide_all_ADOS <- subset(data_wide_all_ADOS, ADOS_comb!="NA")
    
    #Correlation
    #cor.test(data_wide_all_ADOS$entry_CSF, data_wide_all_ADOS$ADOS_comb)
        
    
    
    
#Subgroups -Attempt
  #Run model and generate predicted values
    fitC <- lme(CSF_cm~AutismControl + age_mc + TBV_mc +TBV_squared_mc+ QC_Maddy_MID02 + time_bin+ age_squared_mc +age_cubed_mc + age_cubed_mc*AutismControl + AutismControl*age_mc + age_squared_mc*AutismControl, 
                random=~1+age_mc|SUBJID,data= data_long_QC_dropped, method="ML")
    summary(fitC)
    data_long_QC_dropped$predlme = predict(fitC)
    
    #Subset to ASD only
    data_long_QC_dropped_ASD <- subset(data_long_QC_dropped, AutismControl=="autism")  
    data_wide_all_ASD <- subset(data_wide_all, AutismControl=="autism")  
    data_wide_all_ASD <- subset(data_long_QC_dropped_ASD, Age!="NA")
  #1. Create age bins (LONG)
    data_long_QC_dropped_ASD$Age <- as.numeric(data_long_QC_dropped_ASD$Age)
    data_long_QC_dropped_ASD$Age_bins <- data_long_QC_dropped_ASD$Age
   
    data_long_QC_dropped_ASD$Age_bins <- ifelse(data_long_QC_dropped_ASD$Age >= "5" & data_long_QC_dropped_ASD$Age < "6", "05", data_long_QC_dropped_ASD$Age_bins)
    data_long_QC_dropped_ASD$Age_bins <- ifelse(data_long_QC_dropped_ASD$Age >= "6" & data_long_QC_dropped_ASD$Age < "7", "06", data_long_QC_dropped_ASD$Age_bins)
    data_long_QC_dropped_ASD$Age_bins <- ifelse(data_long_QC_dropped_ASD$Age >= "7" & data_long_QC_dropped_ASD$Age < "8", "07", data_long_QC_dropped_ASD$Age_bins)
    data_long_QC_dropped_ASD$Age_bins <- ifelse(data_long_QC_dropped_ASD$Age >= "8" & data_long_QC_dropped_ASD$Age < "9", "08", data_long_QC_dropped_ASD$Age_bins)
    data_long_QC_dropped_ASD$Age_bins <- ifelse(data_long_QC_dropped_ASD$Age >= "9" & data_long_QC_dropped_ASD$Age < "10", "09", data_long_QC_dropped_ASD$Age_bins)
    data_long_QC_dropped_ASD$Age_bins <- ifelse(data_long_QC_dropped_ASD$Age == "9.5" | data_long_QC_dropped_ASD$Age == "9.75" | data_long_QC_dropped_ASD$Age == "9.41666666666667" | data_long_QC_dropped_ASD$Age == "9.08333333333333" | data_long_QC_dropped_ASD$Age == "9.33333333333333", "09", data_long_QC_dropped_ASD$Age_bins)
    data_long_QC_dropped_ASD$Age_bins <- ifelse(data_long_QC_dropped_ASD$Age >= "10" & data_long_QC_dropped_ASD$Age < "11", "10", data_long_QC_dropped_ASD$Age_bins)
    data_long_QC_dropped_ASD$Age_bins <- ifelse(data_long_QC_dropped_ASD$Age_bins == "05", "5", data_long_QC_dropped_ASD$Age_bins)
    data_long_QC_dropped_ASD$Age_bins <- ifelse(data_long_QC_dropped_ASD$Age_bins == "06" | data_long_QC_dropped_ASD$Age_bins == "07" | data_long_QC_dropped_ASD$Age_bins == "08" |data_long_QC_dropped_ASD$Age_bins == "09", "10", data_long_QC_dropped_ASD$Age_bins)
    data_long_QC_dropped_ASD$Age_bins <- ifelse(data_long_QC_dropped_ASD$Age >= "3" & data_long_QC_dropped_ASD$Age < "6", "5", data_long_QC_dropped_ASD$Age_bins)
    data_long_QC_dropped_ASD$Age_bins <- ifelse(data_long_QC_dropped_ASD$Age >= "6" & data_long_QC_dropped_ASD$Age < "11", "10", data_long_QC_dropped_ASD$Age_bins)
    data_long_QC_dropped_ASD$Age_bins <- ifelse(data_long_QC_dropped_ASD$Age >= "11" & data_long_QC_dropped_ASD$Age < "16", "15", data_long_QC_dropped_ASD$Age_bins)
    data_long_QC_dropped_ASD$Age_bins <- ifelse(data_long_QC_dropped_ASD$Age >= "16" & data_long_QC_dropped_ASD$Age < "21", "20", data_long_QC_dropped_ASD$Age_bins)
    data_long_QC_dropped_ASD$Age_bins <- ifelse(data_long_QC_dropped_ASD$Age >= "21" & data_long_QC_dropped_ASD$Age < "26", "25", data_long_QC_dropped_ASD$Age_bins)
    data_long_QC_dropped_ASD$Age_bins <- ifelse(data_long_QC_dropped_ASD$Age >= "26" & data_long_QC_dropped_ASD$Age < "31", "30", data_long_QC_dropped_ASD$Age_bins)
    data_long_QC_dropped_ASD$Age_bins <- ifelse(data_long_QC_dropped_ASD$Age >= "31" & data_long_QC_dropped_ASD$Age < "36", "35", data_long_QC_dropped_ASD$Age_bins)
    data_long_QC_dropped_ASD$Age_bins <- ifelse(data_long_QC_dropped_ASD$Age >= "36" & data_long_QC_dropped_ASD$Age < "41", "40", data_long_QC_dropped_ASD$Age_bins)
    data_long_QC_dropped_ASD$Age_bins <- ifelse(data_long_QC_dropped_ASD$Age >= "41" & data_long_QC_dropped_ASD$Age < "46", "45", data_long_QC_dropped_ASD$Age_bins)

    #Table to verify that Age_bins worked
    table(data_long_QC_dropped_ASD$Age_bins)
    
  #2. Create subgroup boundaries (LONG). Lowerfence = mean - 1.5sd, Upperfence = mean + 1.5sd
    favstats(predlme ~ Age_bins, data=data_long_QC_dropped_ASD)
    
    #Create lowerfence for each age bin using expected EA-CSF values
    lowerfence5=48.74183 - (1*(2.790334))
    lowerfence10=61.69400 - (1*(13.199403))
    lowerfence15=68.22173 - (1*(11.516379))
    lowerfence20=86.56519 - (1*(14.514601))
    lowerfence25=102.93354 - (1*(14.838154))
    lowerfence30=92.03349 - (1*(28.147246))
    lowerfence35=103.19913 - (1*(20.510061))
    lowerfence40=91.28344 - (1*(29.921691))
    lowerfence45=91.66802 - (1*(26.349854))
    
    #Create upperfence for each age bin using expected EA-CSF values
    upperfence5=48.74183 + (1*(2.790334))
    upperfence10=61.69400 + (1*(13.199403))
    upperfence15=68.22173 + (1*(11.516379))
    upperfence20=86.56519 + (1*(14.514601))
    upperfence25=102.93354 + (1*(14.838154))
    upperfence30=92.03349 + (1*(28.147246))
    upperfence35=103.19913 + (1*(20.510061))
    upperfence40=91.28344 + (1*(29.921691))
    upperfence45=91.66802 + (1*(26.349854))
    
    #Create group var
    #normal
    data_long_QC_dropped_ASD$subgroup <- "normal"
    
    #upper
    data_long_QC_dropped_ASD$subgroup <- ifelse(data_long_QC_dropped_ASD$predlme >= upperfence5 & data_long_QC_dropped_ASD$Age_bins=="5", "upper", data_long_QC_dropped_ASD$subgroup)
    data_long_QC_dropped_ASD$subgroup <- ifelse(data_long_QC_dropped_ASD$predlme >= upperfence10 & data_long_QC_dropped_ASD$Age_bins=="10", "upper", data_long_QC_dropped_ASD$subgroup)
    data_long_QC_dropped_ASD$subgroup <- ifelse(data_long_QC_dropped_ASD$predlme >= upperfence15 & data_long_QC_dropped_ASD$Age_bins=="15", "upper", data_long_QC_dropped_ASD$subgroup)
    data_long_QC_dropped_ASD$subgroup <- ifelse(data_long_QC_dropped_ASD$predlme >= upperfence20 & data_long_QC_dropped_ASD$Age_bins=="20", "upper", data_long_QC_dropped_ASD$subgroup)
    data_long_QC_dropped_ASD$subgroup <- ifelse(data_long_QC_dropped_ASD$predlme >= upperfence25 & data_long_QC_dropped_ASD$Age_bins=="25", "upper", data_long_QC_dropped_ASD$subgroup)
    data_long_QC_dropped_ASD$subgroup <- ifelse(data_long_QC_dropped_ASD$predlme >= upperfence30 & data_long_QC_dropped_ASD$Age_bins=="30", "upper", data_long_QC_dropped_ASD$subgroup)
    data_long_QC_dropped_ASD$subgroup <- ifelse(data_long_QC_dropped_ASD$predlme >= upperfence35 & data_long_QC_dropped_ASD$Age_bins=="35", "upper", data_long_QC_dropped_ASD$subgroup)
    data_long_QC_dropped_ASD$subgroup <- ifelse(data_long_QC_dropped_ASD$predlme >= upperfence40 & data_long_QC_dropped_ASD$Age_bins=="40", "upper", data_long_QC_dropped_ASD$subgroup)
    data_long_QC_dropped_ASD$subgroup <- ifelse(data_long_QC_dropped_ASD$predlme >= upperfence45 & data_long_QC_dropped_ASD$Age_bins=="45", "upper", data_long_QC_dropped_ASD$subgroup)
    
    #lower
    data_long_QC_dropped_ASD$subgroup <- ifelse(data_long_QC_dropped_ASD$predlme <= lowerfence5 & data_long_QC_dropped_ASD$Age_bins=="5", "lower", data_long_QC_dropped_ASD$subgroup)
    data_long_QC_dropped_ASD$subgroup <- ifelse(data_long_QC_dropped_ASD$predlme <= lowerfence10 & data_long_QC_dropped_ASD$Age_bins=="10", "lower", data_long_QC_dropped_ASD$subgroup)
    data_long_QC_dropped_ASD$subgroup <- ifelse(data_long_QC_dropped_ASD$predlme <= lowerfence15 & data_long_QC_dropped_ASD$Age_bins=="15", "lower", data_long_QC_dropped_ASD$subgroup)
    data_long_QC_dropped_ASD$subgroup <- ifelse(data_long_QC_dropped_ASD$predlme <= lowerfence20 & data_long_QC_dropped_ASD$Age_bins=="20", "lower", data_long_QC_dropped_ASD$subgroup)
    data_long_QC_dropped_ASD$subgroup <- ifelse(data_long_QC_dropped_ASD$predlme <= lowerfence25 & data_long_QC_dropped_ASD$Age_bins=="25", "lower", data_long_QC_dropped_ASD$subgroup)
    data_long_QC_dropped_ASD$subgroup <- ifelse(data_long_QC_dropped_ASD$predlme <= lowerfence30 & data_long_QC_dropped_ASD$Age_bins=="30", "lower", data_long_QC_dropped_ASD$subgroup)
    data_long_QC_dropped_ASD$subgroup <- ifelse(data_long_QC_dropped_ASD$predlme <= lowerfence35 & data_long_QC_dropped_ASD$Age_bins=="35", "lower", data_long_QC_dropped_ASD$subgroup)
    data_long_QC_dropped_ASD$subgroup <- ifelse(data_long_QC_dropped_ASD$predlme <= lowerfence40 & data_long_QC_dropped_ASD$Age_bins=="40", "lower", data_long_QC_dropped_ASD$subgroup)
    data_long_QC_dropped_ASD$subgroup <- ifelse(data_long_QC_dropped_ASD$predlme <= lowerfence45 & data_long_QC_dropped_ASD$Age_bins=="45", "lower", data_long_QC_dropped_ASD$subgroup)
    
    #Plot with normal curve to verify subgroups worked
    table(data_long_QC_dropped_ASD$subgroup)
    
    Palette <- c("#0072B2", "#E69F00", "#00BCB8")
    ggplot(data_long_QC_dropped_ASD, aes(x=predlme, color=subgroup)) +        
      labs(x = "Model-Predicted EA-CSF Volume", y = 'Density')+
      labs(fill = " ")+
      labs(color = " ")+
      scale_colour_manual(values=Palette, labels = c("Lower", "Normal", "Upper"))+
      scale_fill_manual(values=Palette, labels = c("Lower", "Normal", "Upper"))+
      geom_histogram(aes(y = ..density..)) +
      stat_function(fun = dnorm,
                    args = list(mean = mean(data_long_QC_dropped_ASD$predlme),
                                sd = sd(data_long_QC_dropped_ASD$predlme)),
                    col = "skyblue",
                    size = 1.5)+
      theme_bw()

  #4. Compare IQ based on group (LONG)
    data_long_QC_dropped_ASD$FIQ <- as.numeric(data_long_QC_dropped_ASD$FIQ)
    data_long_QC_dropped_AFIQ<-subset(data_long_QC_dropped_ASD, FIQ!="NA") 
    data_long_QC_dropped_AFIQ<-subset(data_long_QC_dropped_AFIQ, FIQ!="NaN")
    data_long_QC_dropped_AFIQ<-subset(data_long_QC_dropped_AFIQ, sex!="2")
    
    fit_sub <- lme(FIQ~subgroup, 
                random=~1|SUBJID,data= data_long_QC_dropped_AFIQ, method="ML")
    summary(fit_sub)
    
    
    #Plot FIQ in ASD by subgroup
    #Plot EACSF by FIQ long
    Palette <- c("#0072B2", "#E69F00", "#00BCB8")
    ggplot(data_long_QC_dropped_AFIQ, aes(x=FIQ, y=predlme, color=subgroup))+
      geom_line(aes(color = subgroup, group = SUBJID))+
      labs(x = "Full-scale IQ Score (time points 1-4)", y = 'Model-Predicted EA-CSF Volume ('~cm^3*')')+
      geom_point(aes(fill=subgroup), colour="black", pch=21, cex=1)+
      labs(fill = " ")+
      labs(color = " ")+
      scale_colour_manual(values=Palette, labels = c("Lower", "Normal", "Upper"))+
      scale_fill_manual(values=Palette, labels = c("Lower", "Normal", "Upper"))+
      geom_smooth(method=loess, aes(fill=subgroup), se=TRUE) +
      guides(color=guide_legend(override.aes=list(fill=NA)))+
      scale_y_continuous(limits=c(30,150))+
      scale_x_continuous(limits=c(50,165), expand = c(0,0))+
      theme_bw()+
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
      theme(axis.title = element_text(colour = "black", size=9), axis.text.y =element_text(colour = "black", angle = 90, hjust = 0.6), 
            axis.text.x =element_text(colour = "black"), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
            legend.position = c(.8, .2), legend.title=element_text(colour = "black", size = 9), legend.text=element_text(colour = "black", size = 8), 
            legend.background = element_rect(fill="white", size=0.5) , axis.line = element_line(colour = "black", size = 1, linetype = "solid"), 
            axis.ticks = element_line(colour = "black", size =1, linetype ="solid"), panel.border = element_blank(), panel.background = element_blank())
    
    

# Given the wide age range, were the results the same across different age bins  (late childhood, adolescence, young adulthood, middle adulthood)?
    
    #Models with age re-centered  
    #This model has age recentered at 10
    data_long_QC_dropped$Age10<- data_long_QC_dropped$Age - 10 
    data_long_QC_dropped$age_squared10<-data_long_QC_dropped$Age10 * data_long_QC_dropped$Age10
    data_long_QC_dropped$age_cubed10<-data_long_QC_dropped$age_squared10 * data_long_QC_dropped$Age10
    
    fit_age10=lme(CSF_cm~AutismControl + Age10 + QC_Maddy_MID02 + TBV_mc +TBV_squared_mc+ time_bin + age_squared10 + age_cubed10 + AutismControl*age_cubed10 +AutismControl*Age10 + AutismControl*age_squared10,random=~1+Age10|SUBJID,
                  data=data_long_QC_dropped,method="REML", na.action=na.omit)
    summary(fit_age10)
    
    #This model has age recentered at 20
    data_long_QC_dropped$Age20<- data_long_QC_dropped$Age - 20
    data_long_QC_dropped$age_squared20<-data_long_QC_dropped$Age20 * data_long_QC_dropped$Age20
    data_long_QC_dropped$age_cubed20<-data_long_QC_dropped$age_squared20 * data_long_QC_dropped$Age20
    
    fit_age20=lme(CSF_cm~AutismControl + Age20 + QC_Maddy_MID02 + TBV_mc +TBV_squared_mc+ time_bin + age_squared20 +age_cubed20 + AutismControl*age_cubed20+ AutismControl*Age20 + AutismControl*age_squared20,random=~1+Age20|SUBJID,
                  data=data_long_QC_dropped,method="REML", na.action=na.omit)
    summary(fit_age20)
    
    
    #This model has age recentered at 30
    data_long_QC_dropped$Age30<- data_long_QC_dropped$Age - 30
    data_long_QC_dropped$age_squared30<-data_long_QC_dropped$Age30 * data_long_QC_dropped$Age30
    data_long_QC_dropped$age_cubed30<-data_long_QC_dropped$age_squared30 * data_long_QC_dropped$Age30
    
    fit_age30=lme(CSF_cm~AutismControl + Age30 + QC_Maddy_MID02 + TBV_mc +TBV_squared_mc+ time_bin + age_squared30 + age_cubed30 + age_cubed30*AutismControl + AutismControl*Age30 + AutismControl*age_squared30,random=~1+Age30|SUBJID,
                  data=data_long_QC_dropped,method="REML", na.action=na.omit)
    summary(fit_age30)
    
  
    
    
# IQ Matching
    #Sub-analysis of FIQ-matched groups -- uncomment out FIQ merger in SETUP first
    #make autismcontrol 0/1 values
    data_long_QC_dropped$Dx_bin <- ifelse(data_long_QC_dropped$AutismControl == "autism", 0, 1)
    #only participants with FIQ score included
    data_long_QC_dropped$FIQ <- as.numeric(data_long_QC_dropped$FIQ)
    data_long_QC_dropped_FIQ<-subset(data_long_QC_dropped, FIQ!="NA") 
    data_long_QC_dropped_FIQ<-subset(data_long_QC_dropped_FIQ, FIQ!="NaN")
    m.out2=matchit(Dx_bin~FIQ, method="nearest", data=data_long_QC_dropped_FIQ, ratio=1)
    summary(m.out2)
    #verify good match with qq-plots
    plot(m.out2, type = "qq", interactive = FALSE, which.xs = c("FIQ"))
    #extract matched dataset
    m.data2 <- match.data(m.out2, drop.unmatched = TRUE)
    
    #run multilevel model (N = 218 scans)
    fitmatch2 = lme(CSF_cm~AutismControl + age_mc +TBV_mc +TBV_squared_mc+ QC_Maddy_MID02 + time_bin + age_squared_mc + age_cubed_mc + age_cubed_mc*AutismControl + AutismControl*age_mc + age_squared_mc*AutismControl, 
                   random=~1+age_mc|SUBJID,data= m.data2, method="REML")
    summary(fitmatch2)
    
    table(m.data2$AutismControl)
    unique(m.data2$SUBJID)
    table(m.data2$AutismControl, unique(m.data2$SUBJID))
    
    #Plot using matched data
    Palette <- c("#0072B2", "#E69F00")
    
    #plot age by CSF
    ggplot(m.data2, aes(x=Age, y=CSF_cm, color=AutismControl))+
      geom_line(aes(group=Participant))+
      labs(x = "Age (Years)", y = 'EA-CSF Volume ('~cm^3*')')+
      labs(fill = " ")+
      labs(color = " ")+
      scale_colour_manual(values=Palette, labels = c("Autism", "Control"))+
      scale_fill_manual(values=Palette, labels = c("Autism", "Control"))+
      geom_point(aes(fill=AutismControl), colour="black", pch=21, cex=1)+
      geom_smooth(method=loess, aes(fill=AutismControl), se=TRUE) +
      scale_y_continuous(limits=c(30,150))+
      scale_x_continuous(limits=c(0,42))+
      guides(color=guide_legend(override.aes=list(fill=NA)))+
      theme_bw()+
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
      theme(axis.title = element_text(colour = "black", size=9), axis.text.y =element_text(colour = "black", angle = 90, hjust = 0.6), 
            axis.text.x =element_text(colour = "black"), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
            legend.position = c(.9, .2), legend.title=element_text(colour = "black", size = 9), legend.text=element_text(colour = "black", size = 8), 
            legend.background = element_rect(fill="white", size=0.5) , axis.line = element_line(colour = "black", size = 1, linetype = "solid"), 
            axis.ticks = element_line(colour = "black", size =1, linetype ="solid"), panel.border = element_blank(), panel.background = element_blank())
    
    

    
#-----------------------------REVIEWER 2: ROUND ONE---------------------------------------------

#1. The final sample comprised of 101 individuals with ASD (4 females) and 98 TD individuals (5 females). How was this imbalance in sex distribution controlled? Can the authors perform the analysis using only male sample? 
    #Sub-analysis with males only 
    #drop females
    data_long_QC_MALES<-subset(data_long_QC_dropped, sex=="1")
    
    #run multilevel model 
    fitmales = lme(CSF_cm~AutismControl + age_mc +TBV_mc +TBV_squared_mc+ QC_Maddy_MID02 + time_bin+ age_squared_mc + age_cubed_mc + age_cubed_mc*AutismControl + AutismControl*age_mc + age_squared_mc*AutismControl, 
                   random=~1+age_mc|SUBJID,data= data_long_QC_MALES, method="REML")
    summary(fitmales)
    
    #original analysis for comparison (N= 439 scans)
    fitC = lme(CSF_cm~AutismControl + age_mc +TBV_mc +TBV_squared_mc+ QC_Maddy_MID02 + time_bin+ age_squared_mc + age_cubed_mc + age_cubed_mc*AutismControl + AutismControl*age_mc + age_squared_mc*AutismControl, 
               random=~1+age_mc|SUBJID,data= data_long_QC_dropped, method="REML")
    summary(fitC)

    
    
    #Multilevel model with sex covariate
    fits = lme(CSF_cm~AutismControl + age_mc +TBV_mc +TBV_squared_mc+ QC_Maddy_MID02 + sex+ time_bin+ age_squared_mc +age_cubed_mc + age_cubed_mc*AutismControl + AutismControl*age_mc + age_squared_mc*AutismControl, 
               random=~1+age_mc|SUBJID,data= data_long_QC_dropped, method="REML")
    summary(fits)
    #Compare fit with original analysis
    anova(fits,fitC)

    
#2. A mixed-effects linear model comprising of the random effects, age at scan as a fixed effect model, diagnostic group, diagnostic group by age, sex, and age2. Previous studies have shown linear, quadratic as well as cubic age trajectories (e.g. Shaw et al. JNSc 2008). I would like to see the AIC comparisons of linear, quadratic and cubic age interaction with diagnostic group.         
    
    #linear interaction of age with group(not age_squared*group interaction)
    fit_lin = lme(CSF_cm~AutismControl + age_mc +TBV_mc +TBV_squared_mc+ QC_Maddy_MID02 + time_bin + AutismControl*age_mc, 
               random=~1+age_mc|SUBJID,data= data_long_QC_dropped, method="ML")
    summary(fit_lin)
    
    #quadratic interaction of age with group
    fit_quad = lme(CSF_cm~AutismControl + age_mc +TBV_mc +TBV_squared_mc+ QC_Maddy_MID02 + time_bin+ age_squared_mc + AutismControl*age_mc + AutismControl*age_squared_mc, 
                   random=~1+age_mc|SUBJID,data= data_long_QC_dropped, method="ML")
    
    summary(fit_quad)
    
    #cubic interaction of age with group
    data_long_QC_dropped$age_cubed_mc <- (data_long_QC_dropped$age_mc)^3
    fit_cube =lme(CSF_cm~AutismControl + age_mc +TBV_mc +TBV_squared_mc+ QC_Maddy_MID02 + time_bin + age_squared_mc + age_cubed_mc + AutismControl*age_mc + AutismControl*age_squared_mc + AutismControl*age_cubed_mc, 
                  random=~1+age_mc|SUBJID,data= data_long_QC_dropped, method="ML")
    summary(fit_cube)
    
    anova(fit_lin, fit_quad)
    anova(fit_quad, fit_cube)
    anova(fit_lin, fit_cube)
    
#3. Please provide more details on criteria used for imaging QC, as well as information on failed images.    
    #Participant #s
      #Started out with...
      table(data_long_QC$AutismControl)
      #Failed scans... (fail=1)
      table(data_long_QC$QC_fail, data_long_QC$AutismControl)
    
    
    #Corr. of clinical severity by failed QC rating
    #Setup
      ADOS_long <- read_excel("C:/Users/maddy/Box/Autism_CSF/data/ADOSADI.xlsx")
      names(ADOS_long)[1] <- "SUBJID"
      #merge with other wide data
      data_wide_all_QC <- merge(ADOS_long, data_wide_all_QC, by =c("SUBJID"), all=TRUE)
      #Create QC_fail var (score of 2 or 3 on visual QC)
      data_wide_all_QC$QC_fail <- ifelse(data_wide_all_QC$QC_Maddy_MID02=="2" | data_wide_all_QC$QC_Maddy_MID02=="3", 1, 0)
    
    #Correlation of ADOS severity by failed QC
    cor.test(ADOS_comb ~ QC_fail, data = data_wide_all_QC)
    
    #Is QC_fail significantly different between groups? No.
    t.test(QC_fail ~ AutismControl, data = data_long_QC, mu = 0, alternative = "two.sided", conf.level = 0.95)
    t.test(QC_Maddy_MID02 ~ AutismControl, data = data_long_QC, mu = 0, alternative = "two.sided", conf.level = 0.95)
    
    
    
    #MLM QC scores ~ AutismControl
    fitQC = lme(QC_Maddy_MID02~AutismControl, 
                random=~1|SUBJID,data= data_long_QC_dropped, method="REML")
    summary(fitQC)
    
    
    #Add QC scores to MLM
    fitCQC = lme(CSF_cm~AutismControl + TBV_mc +TBV_squared_mc+QC_Maddy_MID02 + age_mc + time_bin+ age_squared_mc +age_cubed_mc + age_cubed_mc*AutismControl + AutismControl*age_mc + age_squared_mc*AutismControl, 
                random=~1+age_mc|SUBJID,data= data_long_QC_dropped, method="REML")
    summary(fitCQC)
    
    
    # MLM QC fail ~ ADOS CSS  
    data_long_QC_dropped_ADOS<-subset(data_long_QC_dropped, ADOS_comb!="NA")
    #Create QC_fail var (score of 2 or 3 on visual QC)
    data_long_QC_dropped_ADOS$QC_fail <- ifelse(data_long_QC_dropped_ADOS$QC_Maddy_MID02=="2" | data_long_QC_dropped_ADOS$QC_Maddy_MID02=="3", 1, 0)
    fitnum = lme(num_scans~ADOS_comb, 
                 random=~1|SUBJID,data= data_long_QC_dropped_ADOS, method="REML")
    summary(fitnum)
    
    
    
    
    
#-----------------------------------REVIEWER 3: ROUND ONE---------------------------------------       
    
    
#1.1 In addition, some manual segmentation in a subset of the scans should be included to help evaluate the quality of the analysis, by comparing pipeline analysis values to manual segmentation values in both autistic group and typical developing group.    
    
    #Manual editing of 20 HCP images (MID02 file)
    edits <- read_excel("C:/Users/maddy/Box/Autism_CSF/Training_set_stats/Training_set_difference.xlsx")
    
    #Plot
    Palette <- c("#0072B2", "#E69F00")
    
    ggplot(edits, aes(x=Unedited_Vol, y=Maddy_edits_Volume))+
      labs(x = 'Unedited MID02 Volume ('~cm^3*')', y = 'Manually-Edited MID02 Volume ('~cm^3*')')+
      scale_colour_manual(values=Palette)+
      scale_fill_manual(values=Palette)+
      geom_smooth(aes(colour="007282"), method = "lm", size=1, se = FALSE)+ 
      geom_abline(slope=1, intercept = 0, size=1, linetype= "dashed")+
      geom_point(aes(fill="999999"), colour="black", pch=21)+
      theme_bw()+
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
      theme(axis.title = element_text(colour = "black", size=9), axis.text.y =element_text(colour = "black", angle = 90, hjust = 0.6), 
            axis.text.x =element_text(colour = "black"), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
            legend.position ="none", legend.title=element_blank(), legend.text=element_blank(), 
            legend.background = element_rect(fill="white", size=0.5) , axis.line = element_line(colour = "black", size = 1, linetype = "solid"), 
            axis.ticks = element_line(colour = "black", size =1, linetype ="solid"), panel.border = element_blank(), panel.background = element_blank())
    
    #Correlation of original versus edited
    cor(edits$Unedited_Vol, edits$Maddy_edits_Volume) 
      #0.637
    
    #t-test -- is there a significant difference between groups?
    names(edits)[5] <- "Edited_Vol"
    edits$ID <- gsub(".{29}$", "", edits$ID)
    edits_long <- reshape(as.data.frame(edits), direction='long', 
                               varying=c('Unedited_Vol', 'Edited_Vol'),
                               timevar='file',
                               times=c('Unedited', 'Edited'),
                               v.names=c('eacsf'),
                               idvar='ID')
    
    t.test(eacsf~file, data=edits_long, mu = 0, alternative = "less", conf.level = 0.95)
      #sig. difference between groups: p=0.0058, diff between group means = 12.457 cm3
    
    #boxplot
    Palette <- c("#0072B2", "#E69F00")
    
    boxplot(eacsf~file,
            data=edits_long,
            main="EACSF Volume by File",
            xlab="Edited vs. Unedited",
            ylab="EACSF (in cm3)",
            col=Palette,
            border="black"
    )
    
    
    
    
#1.2 The authors described only analysis output with a visual score of 0 or 1 are included. How many out of the 452 scans made the cut? If the 452 is the total number that passed the quality test, how many didn't? Do images in the autistic group and the control group have comparable quality scores? Is the score age-related? Does the pipeline work better in young children than older adults?      
    
    data_wide_all_QC <- merge(ADOS_long, data_wide_all_QC, by =c("SUBJID"), all=TRUE)
    #Create QC_fail var (score of 2 or 3 on visual QC)
    data_wide_all_QC$QC_fail <- ifelse(data_wide_all_QC$QC_Maddy_MID02=="2" | data_wide_all_QC$QC_Maddy_MID02=="3", 1, 0)
    
    #Correlation of ADOS severity by failed QC
    cor.test(ADOS_CSS.Entry ~ QC_fail, data = data_wide_all_QC)
    
    #Is QC_fail significantly different between groups? No.
    t.test(QC_fail ~ AutismControl, data = data_long_QC, mu = 0, alternative = "two.sided", conf.level = 0.95)
    t.test(QC_Maddy_MID02 ~ AutismControl, data = data_long_QC, mu = 0, alternative = "two.sided", conf.level = 0.95)
    
        #Number of failed images
    table(data_long_QC$QC_fail, data_long_QC$AutismControl)
    
       
    #MLM EA-CSF QC and Group
    fitQC = lme(QC_Maddy_MID02~AutismControl, 
                 random=~1|SUBJID,data= data_long_QC_dropped, method="REML")
    summary(fitQC)
    
    
    #MLM EA-CSF segmentation and age
    fitage = lme(QC_Maddy_MID02~age_mc, 
                 random=~1|SUBJID,data= data_long_QC_dropped, method="REML")
    summary(fitage)
    
    
    #Violin plot
    #data_long_QC$QC_fail <- ifelse(data_long_QC$QC_Maddy_MID02=="2" | data_long_QC$QC_Maddy_MID02=="3", "Fail", "Pass")
    data_long_QC$QC_fail <- ifelse(data_long_QC$QC_fail=="1", "Fail", "Pass")
    data_long_QC <- subset(data_long_QC, QC_fail!= "NA")
    
    ggplot(data_long_QC, aes(x = QC_fail, y = Age, fill = AutismControl)) +
      labs(x = "QC Rating", y = 'Age')+
      geom_violin() +
      geom_boxplot(width=0.1, color="black", position=position_dodge(.91)) +
      scale_colour_manual(values=Palette)+
      scale_fill_manual(values=Palette)+
      theme_bw()
    
    #ggsave(filename = paste("Rev3.1_210514.svg"), width = 4, height = 3.4,
    #   path = "C:/Users/maddy/Box/Autism_CSF/figures", dpi = 300)
    
# 2.1 Do the authors observe any differences in EA-CSF volume from children whose first scans were taken before 3 years old?        
    
    #Plot Children and EA-CSF specifically
    Palette <- c("#0072B2", "#E69F00")
    ggplot(data_long_QC_dropped, aes(x=Age, y=CSF_cm, color=AutismControl))+
      geom_line(aes(group=Participant))+
      labs(x = 'Age', y = 'Extra-axial CSF Volume ( '~cm^3*')')+
      labs(fill = " ")+
      labs(color = " ")+
      scale_colour_manual(values=Palette, labels = c("Autism", "Control"))+
      scale_fill_manual(values=Palette, labels = c("Autism", "Control"))+
      geom_point(aes(fill=AutismControl), colour="black", pch=21, cex=3)+
      scale_y_continuous(limits=c(0, 150))+
      scale_x_continuous(limits=c(3,7), expand = c(0,0))+
      guides(color=guide_legend(override.aes=list(fill=NA)))+
      theme_bw()+
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
      theme(axis.title = element_text(colour = "black", size=9), axis.text.y =element_text(colour = "black", angle = 90, hjust = 0.6), 
            axis.text.x =element_text(colour = "black"), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
            legend.position = c(.15, .8), legend.title=element_text(colour = "black", size = 9), legend.text=element_text(colour = "black", size = 8), 
            legend.background = element_rect(fill="white", size=0.5) , axis.line = element_line(colour = "black", size = 1, linetype = "solid"), 
            axis.ticks = element_line(colour = "black", size =1, linetype ="solid"), panel.border = element_blank(), panel.background = element_blank())
    
    #ggsave(filename = paste("Rev3.2_210521.svg"), width = 4, height = 4,
     #      path = "C:/Users/maddy/Box/Autism_CSF/figures", dpi = 300)
    
    
#2.2 While the authors did not observe differences in the trajectory of EA-CSF volume change as a whole to control subjects, is there any correlation between EA-CSF and the severity of autism at the time of each scan?    
    # EA-CSF by ADOS CSS corr using scans from the time when CSS scores were collected
    #data_wide_all <- transform(data_wide_all,scan1=ifelse(!is.na(ScanDateT1.Time1),1, 0 ))
    #data_wide_all <- transform(data_wide_all,scan2=ifelse(!is.na(ScanDateT2.Time2),1, 0 ))
    #data_wide_all <- transform(data_wide_all,scan3=ifelse(!is.na(ScanDateT3.Time3),1, 0 ))
    #data_wide_all <- transform(data_wide_all,scan4=ifelse(!is.na(ScanDateT4.Time4),1, 0 ))
    #data_wide_all <- transform(data_wide_all,scan5=ifelse(!is.na(ScanDateT5.Time5),1, 0 ))
    
    #data_wide_all$num_scans <- (data_wide_all$scan1 + data_wide_all$scan2 + data_wide_all$scan3 + data_wide_all$scan4 + data_wide_all$scan5)
    #table(data_wide_all$num_scans, data_wide_all$AutismControl)
    
    #Set up if/else function
    #data_wide_all$new_time1 <- ifelse(data_wide_all$scan1=="1", 1, "")
    #data_wide_all$new_time2 <- ifelse(data_wide_all$scan1=="0" & data_wide_all$scan2=="1", 2, "")
    #data_wide_all$new_time3 <- ifelse(data_wide_all$scan1=="0" & data_wide_all$scan2=="0" & data_wide_all$scan3=="1", 3, "")        
    #data_wide_all$new_time4 <- ifelse(data_wide_all$scan1=="0" & data_wide_all$scan2=="0" & data_wide_all$scan3=="0" & data_wide_all$scan4=="1", 4, "")
    #data_wide_all$new_time5 <- ifelse(data_wide_all$scan1=="0" & data_wide_all$scan2=="0" & data_wide_all$scan3=="0" & data_wide_all$scan4=="0" & data_wide_all$scan5=="1", 5, "")
 
    #concatenate new_time vars
    #data_wide_all$time_entry <- (paste0(data_wide_all$new_time1, data_wide_all$new_time2, data_wide_all$new_time3, data_wide_all$new_time4, data_wide_all$new_time5))
    #table(data_wide_all$time_entry)
    
    #set entry scan to time5 if time5 ADOS was used
    #ADOS_long <- read.csv("C:/Users/maddy/Box/Autism_CSF/data/UofU_Longitudinal_ADOS_CSS_210428.csv") 
    #names(ADOS_long)[1] <- "SUBJID"
    #merge with other wide data
    #data_wide_all_ADOS <- merge(ADOS_long, data_wide_all, by =c("SUBJID"), all=FALSE)
    #data_wide_all_ADOS <- subset(data_wide_all_ADOS, sex!=2)
    #data_wide_all_ADOS$ADOS_comb <- ifelse(is.na(data_wide_all_ADOS$TOTAL.CSS) & data_wide_all_ADOS$Time5ADOS.TOTAL.CSS!="NA", data_wide_all_ADOS$Time5ADOS.TOTAL.CSS, data_wide_all_ADOS$TOTAL.CSS)

    #data_wide_all_ADOS$time_entry <- ifelse(is.na(data_wide_all_ADOS$TOTAL.CSS), "5", data_wide_all_ADOS$time_entry)
    #table(data_wide_all_ADOS$time_entry)
   
    #Entry_CSF
    #data_wide_all_ADOS$entry_CSF <- NA
    #data_wide_all_ADOS$entry_CSF <- ifelse(data_wide_all_ADOS$time_entry=="1", data_wide_all_ADOS$CSF_cm.Time1, data_wide_all_ADOS$entry_CSF)
    #data_wide_all_ADOS$entry_CSF <- ifelse(data_wide_all_ADOS$time_entry=="2", data_wide_all_ADOS$CSF_cm.Time2, data_wide_all_ADOS$entry_CSF)
    #data_wide_all_ADOS$entry_CSF <- ifelse(data_wide_all_ADOS$time_entry=="3", data_wide_all_ADOS$CSF_cm.Time3, data_wide_all_ADOS$entry_CSF)
    #data_wide_all_ADOS$entry_CSF <- ifelse(data_wide_all_ADOS$time_entry=="4", data_wide_all_ADOS$CSF_cm.Time4, data_wide_all_ADOS$entry_CSF)
    #data_wide_all_ADOS$entry_CSF <- ifelse(data_wide_all_ADOS$time_entry=="5", data_wide_all_ADOS$CSF_cm.Time5, data_wide_all_ADOS$entry_CSF)
    #data_wide_all_ADOS <- subset(data_wide_all_ADOS, entry_CSF!="NA")
    #data_wide_all_ADOS <- subset(data_wide_all_ADOS, ADOS_comb!="NA")
    
    #Correlation
    #cor.test(data_wide_all_ADOS$entry_CSF, data_wide_all_ADOS$ADOS_comb)
    
    
    
    
    #ADOS predictor in overall model
    #Subset to only include participants with an ADOS CSS score
    data_long_QC_dropped_ADOS<-subset(data_long_QC_dropped, ADOS_comb!="NA")
    #Multilevel model
    fitADOS = lme(CSF_cm~ADOS_comb + TBV_mc +TBV_squared_mc + age_mc + QC_Maddy_MID02 + time_bin + age_squared_mc + age_cubed_mc, 
                  random=~1+age_mc|SUBJID,data= data_long_QC_dropped_ADOS, method="REML")
    summary(fitADOS) 
    
   
#2.3 Perhaps a separate analysis with patients that have 3 or more scans, if possible, will provide additional information to support the negative data. 
    data_wide_all <- transform(data_wide_all,scan1=ifelse(!is.na(ScanDateT1.Time1),1, 0 ))
    data_wide_all <- transform(data_wide_all,scan2=ifelse(!is.na(ScanDateT2.Time2),1, 0 ))
    data_wide_all <- transform(data_wide_all,scan3=ifelse(!is.na(ScanDateT3.Time3),1, 0 ))
    data_wide_all <- transform(data_wide_all,scan4=ifelse(!is.na(ScanDateT4.Time4),1, 0 ))
    data_wide_all <- transform(data_wide_all,scan5=ifelse(!is.na(ScanDateT5.Time5),1, 0 ))
    
    data_wide_all$num_scans <- (data_wide_all$scan1 + data_wide_all$scan2 + data_wide_all$scan3 + data_wide_all$scan4 + data_wide_all$scan5)
    table(data_wide_all$num_scans, data_wide_all$AutismControl)

    #Drop participants who only have 1 or 2 scans
    data_wide_all$num_scans3 <- ifelse(data_wide_all$num_scans >= "3", 1, 0) 
    data_wide_3scans <- subset(data_wide_all, num_scans3!="0")
    table(data_wide_3scans$SUBJID, data_wide_3scans$sex)
    table(data_wide_3scans$AutismControl)
    
    #Data frame with only 2 or 1 scans
    data_wide_2scans <- subset(data_wide_all, num_scans < "3")

    table(data_wide_2scans$SUBJID)
    records_to_rm <- c(23714, 28774, 28868, 31021, 33114, 33890, 35793, 36591, 36913, 37060, 38386, 39017, 41704, 42225, 42518, 44375, 46833, 48151, 49408, 50784, 50843, 51837, 52048, 52098, 53352, 57690, 57797, 57827, 57828, 57833, 57836, 57849, 59477, 59480, 59486, 59500, 59504, 59511, 59513, 59524, 59528, 59551, 62445, 62587, 63264, 63399, 63451, 63537, 63561, 63694, 63833, 63962, 63988, 64061, 64124, 64139, 64141, 64482, 64486, 65182, 65791, 65802, 65914, 66533, 66737, 66830, 66909, 67089, 67235, 67531, 67651, 67896, 67920, 68413, 70012, 70282, 70361, 70610, 70704, 71443, 72052, 72495, 73400, 73674, 74106, 74808, 75079, 75232, 78509, 83855, 84537, 84898, 86189, 86535, 88135, 88636, 89748, 90300, 90320, 90430, 90482, 90490, 90503, 91135, 91158, 91246, 95321, 95576, 1926001, 1941001)
    data_long_QC_3scans <- subset(data_long_QC_dropped,!(SUBJID %in% records_to_rm))
    table(data_long_QC_3scans$AutismControl)
 
    #MLM with participants with 3+ scans
    fitC3 = lme(CSF_cm~AutismControl + age_mc + TBV_mc +TBV_squared_mc+ QC_Maddy_MID02 + time_bin+ age_squared_mc +age_cubed_mc + age_cubed_mc*AutismControl + AutismControl*age_mc + age_squared_mc*AutismControl, 
               random=~1+age_mc|SUBJID,data= data_long_QC_3scans, method="REML")
    summary(fitC3)
    
    
    #Plot age by CSF using this subset (3+ scans per participant)
    #plot age by CSF
    ggplot(data_long_QC_3scans, aes(x=Age, y=CSF_cm, color=AutismControl))+
      geom_line(aes(group=Participant))+
      labs(x = "Age (Years)", y = 'Extra-axial CSF Volume ( '~cm^3*')')+
      labs(fill = " ")+
      labs(color = " ")+
      scale_colour_manual(values=Palette, labels = c("Autism", "Control"))+
      scale_fill_manual(values=Palette, labels = c("Autism", "Control"))+
      geom_point(aes(fill=AutismControl), colour="black", pch=21, cex=1)+
      geom_smooth(method=loess, aes(fill=AutismControl), se=TRUE) +
      scale_y_continuous(limits=c(30,150))+
      scale_x_continuous(limits=c(0,42))+
      guides(color=guide_legend(override.aes=list(fill=NA)))+
      theme_bw()+
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
      theme(axis.title = element_text(colour = "black", size=9), axis.text.y =element_text(colour = "black", angle = 90, hjust = 0.6), 
            axis.text.x =element_text(colour = "black"), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
            legend.position = c(.9, .15), legend.title=element_text(colour = "black", size = 9), legend.text=element_text(colour = "black", size = 8), 
            legend.background = element_rect(fill="white", size=0.5) , axis.line = element_line(colour = "black", size = 1, linetype = "solid"), 
            axis.ticks = element_line(colour = "black", size =1, linetype ="solid"), panel.border = element_blank(), panel.background = element_blank())
    
    #ggsave(filename = paste("Rev3.3_210521.svg"), width = 4, height = 4,
    #       path = "C:/Users/maddy/Box/Autism_CSF/figures", dpi = 300)
    
    
    
    
#2.4 Since the study focuses on age-related changes, it is curious to see if the age differences are more prominent in certain age-groups (i.e., are the autistic patients and controls more different in young children or more different in middle-aged adults?), and if the differences may influence the conclusion.     
    
    #Sub-analysis of age-matched groups 
    #make autismcontrol 0/1 values
    data_long_QC_dropped$Dx_bin <- ifelse(data_long_QC_dropped$AutismControl == "autism", 0, 1)
    m.out=matchit(Dx_bin~Age, method="nearest", data=data_long_QC_dropped, ratio=1)
    summary(m.out)
    #verify good match with qq-plots
    plot(m.out, type = "qq", interactive = FALSE, which.xs = c("Age"))
    #extract matched dataset
    m.data1 <- match.data(m.out, drop.unmatched = TRUE)
    
    #run multilevel model (N = 368 scans)
    fitmatch = lme(CSF_cm~AutismControl + age_mc + TBV_mc +TBV_squared_mc+ QC_Maddy_MID02 + time_bin+ age_squared_mc +age_cubed_mc + age_cubed_mc*AutismControl + AutismControl*age_mc + age_squared_mc*AutismControl, 
                   random=~1+age_mc|SUBJID,data= m.data1, method="REML")
    summary(fitmatch)
    
    unique(m.data1$SUBJID)
    
    #Plot using matched data
    Palette <- c("#0072B2", "#E69F00")
    
    #plot age by CSF
    ggplot(m.data1, aes(x=Age, y=CSF_cm, color=AutismControl))+
      geom_line(aes(group=Participant))+
      labs(x = "Age (Years)", y = 'Extra-axial CSF Volume ( '~cm^3*')')+
      labs(fill = " ")+
      labs(color = " ")+
      scale_colour_manual(values=Palette, labels = c("Autism", "Control"))+
      scale_fill_manual(values=Palette, labels = c("Autism", "Control"))+
      geom_point(aes(fill=AutismControl), colour="black", pch=21, cex=1)+
      geom_smooth(method=loess, aes(fill=AutismControl), se=TRUE) +
      scale_y_continuous(limits=c(30,150))+
      scale_x_continuous(limits=c(0,42))+
      guides(color=guide_legend(override.aes=list(fill=NA)))+
      theme_bw()+
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
      theme(axis.title = element_text(colour = "black", size=9), axis.text.y =element_text(colour = "black", angle = 90, hjust = 0.6), 
            axis.text.x =element_text(colour = "black"), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
            legend.position = c(.9, .15), legend.title=element_text(colour = "black", size = 9), legend.text=element_text(colour = "black", size = 8), 
            legend.background = element_rect(fill="white", size=0.5) , axis.line = element_line(colour = "black", size = 1, linetype = "solid"), 
            axis.ticks = element_line(colour = "black", size =1, linetype ="solid"), panel.border = element_blank(), panel.background = element_blank())
    
    #ggsave(filename = paste("Rev3.4_210521.svg"), width = 4, height = 4,
    #       path = "C:/Users/maddy/Box/Autism_CSF/figures", dpi = 300)
    
    # Did the matching reduce the age gap between groups?
    favstats(Age~AutismControl, data= m.data1)
    
    # MLM with Age as outcome and group as predictor
    fitage = lme(age_mc~AutismControl, random=~1 |SUBJID,data= m.data1, method="REML")
    summary(fitage)
    
    
    
    #Models with age re-centered  
    #This model has age recentered at 10
    data_long_QC_dropped$Age10<- data_long_QC_dropped$Age - 10 
    data_long_QC_dropped$age_squared10<-data_long_QC_dropped$Age10 * data_long_QC_dropped$Age10
    data_long_QC_dropped$age_cubed10<-data_long_QC_dropped$age_squared10 * data_long_QC_dropped$Age10
    
    fit_age10=lme(CSF_cm~AutismControl + Age10 + QC_Maddy_MID02 + TBV_mc +TBV_squared_mc+ time_bin + age_squared10 + age_cubed10 + AutismControl*age_cubed10 +AutismControl*Age10 + AutismControl*age_squared10,random=~1+Age10|SUBJID,
                  data=data_long_QC_dropped,method="REML", na.action=na.omit)
    summary(fit_age10)
    
    #This model has age recentered at 20
    data_long_QC_dropped$Age20<- data_long_QC_dropped$Age - 20
    data_long_QC_dropped$age_squared20<-data_long_QC_dropped$Age20 * data_long_QC_dropped$Age20
    data_long_QC_dropped$age_cubed20<-data_long_QC_dropped$age_squared20 * data_long_QC_dropped$Age20
    
    fit_age20=lme(CSF_cm~AutismControl + Age20 + QC_Maddy_MID02 + TBV_mc +TBV_squared_mc+ time_bin + age_squared20 +age_cubed20 + AutismControl*age_cubed20+ AutismControl*Age20 + AutismControl*age_squared20,random=~1+Age20|SUBJID,
                  data=data_long_QC_dropped,method="REML", na.action=na.omit)
    summary(fit_age20)
    
    
    #This model has age recentered at 30
    data_long_QC_dropped$Age30<- data_long_QC_dropped$Age - 30
    data_long_QC_dropped$age_squared30<-data_long_QC_dropped$Age30 * data_long_QC_dropped$Age30
    data_long_QC_dropped$age_cubed30<-data_long_QC_dropped$age_squared30 * data_long_QC_dropped$Age30
    
    fit_age30=lme(CSF_cm~AutismControl + Age30 + QC_Maddy_MID02 + TBV_mc +TBV_squared_mc+ time_bin + age_squared30 + age_cubed30 + age_cubed30*AutismControl + AutismControl*Age30 + AutismControl*age_squared30,random=~1+Age30|SUBJID,
                  data=data_long_QC_dropped,method="REML", na.action=na.omit)
    summary(fit_age30)
    
    
    
    
#2.5 Do the authors find any association between brain sizes and EA-CSF volume? Do brain sizes differ in young autistic vs. control children? Do they normalize over aging, and if so, does it happen around the same time EA-CSF volume normalizes?
    #Model with TBV covariate 
    #data_long_QC_droppedTBV <- subset(data_long_QC_dropped, TotalBrain_Volume_cm!="NA")
    #favstats(data_long_QC_droppedTBV$TotalBrain_Volume_cm)
    #TBVmean = 1132.435
    #data_long_QC_droppedTBV$TBV_mc <- data_long_QC_droppedTBV$TotalBrain_Volume_cm - TBVmean
    
    #MLM with TCVxAutismControl 
    fit_TBV1 = lme(CSF_cm~AutismControl + age_mc + TBV_mc +TBV_squared_mc+ QC_Maddy_MID02 + age_squared_mc + age_cubed_mc + age_cubed_mc*AutismControl + TBV_mc + AutismControl*age_mc + age_squared_mc*AutismControl, 
                   random=~1+age_mc|SUBJID,data= data_long_QC_dropped, method="REML")
    summary(fit_TBV1)

    
    #TBV by Age plot
    ggplot(data_long_QC_dropped, aes(x=Age, y=TBV_cm, color=AutismControl))+
      geom_line(aes(group=Participant))+
      labs(x = 'Age', y = 'Total Brain Volume ('~cm^3*')')+
      labs(fill = " ")+
      labs(color = " ")+
      scale_colour_manual(values=Palette, labels = c("Autism", "Control"))+
      scale_fill_manual(values=Palette, labels = c("Autism", "Control"))+
      geom_point(aes(fill=AutismControl), colour="black", pch=21, cex=1)+
      geom_smooth(method=loess, aes(fill=AutismControl), se=TRUE) +
      scale_y_continuous(limits=c(1000,1600))+
      scale_x_continuous(limits=c(0,43), expand = c(0,0))+
      guides(color=guide_legend(override.aes=list(fill=NA)))+
      theme_bw()+
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
      theme(axis.title = element_text(colour = "black", size=9), axis.text.y =element_text(colour = "black", angle = 90, hjust = 0.6), 
            axis.text.x =element_text(colour = "black"), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
            legend.position = c(.9, .9), legend.title=element_text(colour = "black", size = 9), legend.text=element_text(colour = "black", size = 8), 
            legend.background = element_rect(fill="white", size=0.5) , axis.line = element_line(colour = "black", size = 1, linetype = "solid"), 
            axis.ticks = element_line(colour = "black", size =1, linetype ="solid"), panel.border = element_blank(), panel.background = element_blank())
    
    #ggsave(filename = paste("Rev3.5_210514.svg"), width = 6, height = 4,
    #       path = "C:/Users/maddy/Box/Autism_CSF/figures", dpi = 300)
    
    
    #TBV by CSF plot
    ggplot(data_long_QC_dropped, aes(x=CSF_cm, y=TBV_cm, color=AutismControl))+
      geom_line(aes(group=Participant))+
      labs(x = 'Extra-axial CSF ( '~cm^3*')', y = 'Total Brain Volume ('~cm^3*')')+
      labs(fill = " ")+
      labs(color = " ")+
      scale_colour_manual(values=Palette, labels = c("Autism", "Control"))+
      scale_fill_manual(values=Palette, labels = c("Autism", "Control"))+
      geom_point(aes(fill=AutismControl), colour="black", pch=21, cex=1)+
      geom_smooth(method=loess, aes(fill=AutismControl), se=TRUE) +
      scale_y_continuous(limits=c(1000,1600))+
      scale_x_continuous(limits=c(30,170), expand = c(0,0))+
      guides(color=guide_legend(override.aes=list(fill=NA)))+
      theme_bw()+
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
      theme(axis.title = element_text(colour = "black", size=9), axis.text.y =element_text(colour = "black", angle = 90, hjust = 0.6), 
            axis.text.x =element_text(colour = "black"), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
            legend.position = c(.9, .2), legend.title=element_text(colour = "black", size = 9), legend.text=element_text(colour = "black", size = 8), 
            legend.background = element_rect(fill="white", size=0.5) , axis.line = element_line(colour = "black", size = 1, linetype = "solid"), 
            axis.ticks = element_line(colour = "black", size =1, linetype ="solid"), panel.border = element_blank(), panel.background = element_blank())
    
    #ggsave(filename = paste("Rev3.6_210521.svg"), width = 4, height = 4,
    #       path = "C:/Users/maddy/Box/Autism_CSF/figures", dpi = 300)
    

#--------------------------------------REVIEWER 1: ROUND TWO------------------------------

#1. FROM RESPONSE. Sub-analysis with subjects who have a QC rating of zero?
    # Number of scans in both groups with QC rating of 0
    data_long_QC_zero <- subset(data_long_QC_dropped, QC_Maddy_MID02=="0")
    table(data_long_QC_zero$AutismControl) #ASD = 18, TD = 4
    
    # Number of scans in both groups with QC rating of 1
    data_long_QC_one <- subset(data_long_QC_dropped, QC_Maddy_MID02=="1")
    table(data_long_QC_one$AutismControl)
    
    # Run the main model using subset with QC rating of 1 scans
    fit_one <- lme(CSF_cm~AutismControl + age_mc + TBV_mc + TBV_squared_mc + time_bin+ age_squared_mc + age_cubed_mc + age_cubed_mc*AutismControl + AutismControl*age_mc + age_squared_mc*AutismControl, 
                random=~1+age_mc|SUBJID,data= data_long_QC_one, method="ML")
    summary(fit_one)
    
    
    
#2. FROM RESPONSE. Sub-analysis with subjects 3-8 years old
    # Number of subjects in this subset
    data_long_QC_child <- subset(data_long_QC_dropped, Age<9) #46 scans
    favstats(data_long_QC_child$Age)
    table(data_long_QC_child$AutismControl)
    
    # Re-center age at 4
    data_long_QC_dropped$Age4<- data_long_QC_dropped$Age - 4 
    data_long_QC_dropped$age_squared4<-data_long_QC_dropped$Age4 * data_long_QC_dropped$Age4
    data_long_QC_dropped$age_cubed4<-data_long_QC_dropped$age_squared4 * data_long_QC_dropped$Age4
    
    fit_age4=lme(CSF_cm~AutismControl + Age4 + QC_Maddy_MID02 + TBV_mc +TBV_squared_mc+ time_bin + age_squared4 + age_cubed4 + age_cubed4*AutismControl + AutismControl*Age4 + AutismControl*age_squared4,random=~1+Age4|SUBJID,
                  data=data_long_QC_dropped,method="REML", na.action=na.omit)
    summary(fit_age4)
        
    
#--------------------------------------OTHER----------------------------------------------

#Plot EA-CSF by eTIV (eTIV values come from cross-sectional FreeSurfer pipeline)
    #plot 4.2 EACSF by ETIV
    ggplot(data_long_QC_dropped, aes(x=CSF_cm, y=ETIV_cm, color=AutismControl))+
      geom_line(aes(group=Participant))+
      labs(x = 'EACSF ('~cm^3*')', y = 'Estimated Total Intracranial Volume ('~cm^3*')')+
      labs(fill = " ")+
      labs(color = " ")+
      scale_colour_manual(values=Palette, labels = c("Autism", "Control"))+
      scale_fill_manual(values=Palette, labels = c("Autism", "Control"))+
      geom_point(aes(fill=AutismControl), colour="black", pch=21, cex=1)+
      geom_smooth(method=loess, aes(fill=AutismControl), se=TRUE) +
      scale_y_continuous(limits=c(1000,2400))+
      scale_x_continuous(limits=c(0,180), expand = c(0,0))+
      guides(color=guide_legend(override.aes=list(fill=NA)))+
      theme_bw()+
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
      theme(axis.title = element_text(colour = "black", size=9), axis.text.y =element_text(colour = "black", angle = 90, hjust = 0.6), 
            axis.text.x =element_text(colour = "black"), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
            legend.position = c(.1, .9), legend.title=element_text(colour = "black", size = 9), legend.text=element_text(colour = "black", size = 8), 
            legend.background = element_rect(fill="white", size=0.5) , axis.line = element_line(colour = "black", size = 1, linetype = "solid"), 
            axis.ticks = element_line(colour = "black", size =1, linetype ="solid"), panel.border = element_blank(), panel.background = element_blank())
    
    #Plot eTIV by Age
    ggplot(data_long_QC_dropped, aes(x=Age, y=ETIV_cm, color=AutismControl))+
      geom_line(aes(group=Participant))+
      labs(x = 'Age', y = 'Estimated Total Intracranial Volume ('~cm^3*')')+
      labs(fill = " ")+
      labs(color = " ")+
      scale_colour_manual(values=Palette, labels = c("Autism", "Control"))+
      scale_fill_manual(values=Palette, labels = c("Autism", "Control"))+
      geom_point(aes(fill=AutismControl), colour="black", pch=21, cex=1)+
      geom_smooth(method=loess, aes(fill=AutismControl), se=TRUE) +
      scale_y_continuous(limits=c(1000,2400))+
      scale_x_continuous(limits=c(0,42), expand = c(0,0))+
      guides(color=guide_legend(override.aes=list(fill=NA)))+
      theme_bw()+
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
      theme(axis.title = element_text(colour = "black", size=9), axis.text.y =element_text(colour = "black", angle = 90, hjust = 0.6), 
            axis.text.x =element_text(colour = "black"), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
            legend.position = c(.1, .9), legend.title=element_text(colour = "black", size = 9), legend.text=element_text(colour = "black", size = 8), 
            legend.background = element_rect(fill="white", size=0.5) , axis.line = element_line(colour = "black", size = 1, linetype = "solid"), 
            axis.ticks = element_line(colour = "black", size =1, linetype ="solid"), panel.border = element_blank(), panel.background = element_blank())
    
    
#Model with eTIV (sig.)
    data_long_QC_eTIV <- subset(data_long_QC_dropped, ETIV_cm!="NA")
    fiteTIV = lme(CSF_cm~AutismControl + age_mc + QC_Maddy_MID02 + time_bin + age_squared_mc +age_cubed_mc + age_cubed_mc*AutismControl +  ETIV_cm + AutismControl*age_mc + age_squared_mc*AutismControl, 
               random=~1+age_mc|SUBJID,data= data_long_QC_eTIV, method="REML")
    summary(fiteTIV)
    
    
    #eTIV-squared (not sig.)
    data_long_QC_eTIV$ETIV_squared <- data_long_QC_eTIV$ETIV_cm * data_long_QC_eTIV$ETIV_cm
    fiteTIV2 = lme(CSF_cm~AutismControl + age_mc + QC_Maddy_MID02 + time_bin + age_squared_mc +age_cubed_mc + age_cubed_mc*AutismControl +  ETIV_cm + ETIV_squared + AutismControl*age_mc + age_squared_mc*AutismControl, 
                                 random=~1+age_mc|SUBJID,data= data_long_QC_eTIV, method="REML")
    summary(fiteTIV2)
    
    
#Model with mean-centered eTIV and eTIV2
    data_long_QC_eTIV <- subset(data_long_QC_dropped, ETIV_cm!="NA")
    favstats(data_long_QC_eTIV$ETIV_cm)
    eTIV_mean = 1646.078
    data_long_QC_eTIV$ETIV_mc <- data_long_QC_eTIV$ETIV_cm - eTIV_mean
    data_long_QC_eTIV$ETIV_squared_mc <- data_long_QC_eTIV$ETIV_mc * data_long_QC_eTIV$ETIV_mc
    
    fiteTIV3 = lme(CSF_cm~AutismControl + age_mc + QC_Maddy_MID02 + time_bin + age_squared_mc +age_cubed_mc + age_cubed_mc*AutismControl + ETIV_mc + ETIV_squared_mc + AutismControl*age_mc + age_squared_mc*AutismControl, 
                   random=~1+age_mc|SUBJID,data= data_long_QC_eTIV, method="REML")
    summary(fiteTIV3)
    
    
  
#Model with mean-centered TBV and TBV2 covariate 
    data_long_QC_droppedTBV <- subset(data_long_QC_dropped, TotalBrain_Volume_cm!="NA")
    favstats(data_long_QC_droppedTBV$TotalBrain_Volume_cm)
    TBVmean = 1132.435
    data_long_QC_droppedTBV$TBV_mc <- data_long_QC_droppedTBV$TotalBrain_Volume_cm - TBVmean
    data_long_QC_droppedTBV$TBV_squared_mc <- data_long_QC_droppedTBV$TBV_mc*data_long_QC_droppedTBV$TBV_mc
    
    #MLM with TBVxAutismControl (N = 402scans, 175 participants)
    fit_TBV1 = lme(CSF_cm~AutismControl + age_mc + QC_Maddy_MID02 + age_squared_mc +age_cubed_mc + age_cubed_mc*AutismControl + TBV_mc + TBV_squared_mc + AutismControl*age_mc + age_squared_mc*AutismControl, 
                   random=~1+age_mc|SUBJID,data= data_long_QC_droppedTBV, method="REML")
    summary(fit_TBV1)    
      
    
    
# Plot Figure 3 using Molly's code
    
    # Run the model before creating this plot!
    fitC <- lme(CSF_cm~AutismControl + age_mc + QC_Maddy_MID02 +TBV_mc + TBV_squared_mc +age_cubed_mc + age_cubed_mc*AutismControl + time_bin+ age_squared_mc + AutismControl*age_mc + age_squared_mc*AutismControl, 
               random=~1+age_mc|SUBJID,data= data_long_QC_dropped, method="ML")
    summary(fitC)
    data_long_QC_dropped$predlme = predict(fitC)
    
    Palette <- c("#0072B2", "#E69F00")
    #plot age by CSF
    ggplot(data_long_QC_dropped, aes(x=Age, y=predlme, color=AutismControl))+
      geom_line(aes(group=Participant))+
      labs(x = "Age (Years)", y = 'Model-Fitted EA-CSF Volume ('~cm^3*')')+
      labs(fill = " ")+
      labs(color = " ")+
      scale_colour_manual(values=Palette, labels = c("Autism", "Control"))+
      scale_fill_manual(values=Palette, labels = c("Autism", "Control"))+
      geom_point(aes(fill=AutismControl), colour="black", pch=21, cex=1)+
      geom_line(data=data_long_QC_dropped, aes(y=predict(fitC,level=0), group=AutismControl, colour=AutismControl), size = 1.5)+
      scale_y_continuous(limits=c(0,150))+
      scale_x_continuous(limits=c(0,43), expand = c(0,0))+
      guides(color=guide_legend(override.aes=list(fill=NA)))+
      theme_bw()+
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
      theme(axis.title = element_text(colour = "black", size=12), axis.text.y =element_text(colour = "black", angle = 90, hjust = 0.6), 
            axis.text.x =element_text(colour = "black"), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
            legend.position = c(.9, .2), legend.title=element_text(colour = "black", size = 12), legend.text=element_text(colour = "black", size = 12), 
            legend.background = element_rect(fill="white", size=0.5) , axis.line = element_line(colour = "black", size = 1, linetype = "solid"), 
            axis.ticks = element_line(colour = "black", size =1, linetype ="solid"), panel.border = element_blank(), panel.background = element_blank())
    
    #plot mean-centered age by CSF
    ggplot(data_long_QC_dropped, aes(x=age_mc, y=predlme, color=AutismControl))+
      geom_line(aes(group=Participant))+
      labs(x = "Mean-centered Age (Years)", y = 'Model-Fitted EA-CSF Volume ('~cm^3*')')+
      labs(fill = " ")+
      labs(color = " ")+
      scale_colour_manual(values=Palette, labels = c("Autism", "Control"))+
      scale_fill_manual(values=Palette, labels = c("Autism", "Control"))+
      geom_point(aes(fill=AutismControl), colour="black", pch=21, cex=1)+
      geom_line(data=data_long_QC_dropped, aes(y=predict(fitC,level=0), group=AutismControl, colour=AutismControl), size = 1.5)+
      scale_y_continuous(limits=c(0,150))+
      scale_x_continuous(limits=c(-23,23), expand = c(0,0))+
      guides(color=guide_legend(override.aes=list(fill=NA)))+
      theme_bw()+
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
      theme(axis.title = element_text(colour = "black", size=12), axis.text.y =element_text(colour = "black", angle = 90, hjust = 0.6), 
            axis.text.x =element_text(colour = "black"), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
            legend.position = c(.9, .2), legend.title=element_text(colour = "black", size = 12), legend.text=element_text(colour = "black", size = 12), 
            legend.background = element_rect(fill="white", size=0.5) , axis.line = element_line(colour = "black", size = 1, linetype = "solid"), 
            axis.ticks = element_line(colour = "black", size =1, linetype ="solid"), panel.border = element_blank(), panel.background = element_blank())
    
    
#Model-adjusted plots 2.0
    #1. Run model
    fitC <- lme(CSF_cm~AutismControl + age_mc + QC_Maddy_MID02 +TBV_mc + TBV_squared_mc + time_bin+ age_squared_mc +age_cubed_mc + age_cubed_mc*AutismControl + AutismControl*age_mc + age_squared_mc*AutismControl, 
                random=~1+age_mc|SUBJID,data= data_long_QC_dropped, method="REML")
    summary(fitC)
    
    #2. Manually adjust for headcoil, TBV, TBV2, and QC MID02 
    data_long_QC_dropped$time_bin <- as.numeric(data_long_QC_dropped$time_bin)
    data_long_QC_dropped$CSF_adjusted <- data_long_QC_dropped$CSF_cm - ((1.86112*(data_long_QC_dropped$time_bin - 0)) + (0.47722*(data_long_QC_dropped$QC_Maddy_MID02 - mean(data_long_QC_dropped$QC_Maddy_MID02))) + (-0.00053*(data_long_QC_dropped$TBV_mc - mean(data_long_QC_dropped$TBV_mc))) + (0.00010*(data_long_QC_dropped$TBV_squared_mc - mean(data_long_QC_dropped$TBV_squared_mc))))
      #mean shifted check
      mean(data_long_QC_dropped$CSF_adjusted)
      mean(data_long_QC_dropped$CSF_cm)
    
    #3. Plot manually adjusted CSF values (sanity check)
    Palette <- c("#0072B2", "#E69F00")
    ggplot(data_long_QC_dropped, aes(x=Age, y=CSF_adjusted, color=AutismControl))+
      geom_line(aes(group=Participant))+
      labs(x = "Age (Years)", y = 'Adjusted EA-CSF Volume ('~cm^3*')')+
      labs(fill = " ")+
      labs(color = " ")+
      scale_colour_manual(values=Palette, labels = c("Autism", "Control"))+
      scale_fill_manual(values=Palette, labels = c("Autism", "Control"))+
      geom_point(aes(fill=AutismControl), colour="black", pch=21, cex=1)+
      geom_smooth(method=loess, aes(fill=AutismControl), se=TRUE) +
      scale_y_continuous(limits=c(0,150))+
      scale_x_continuous(limits=c(0,43), expand = c(0,0))+
      guides(color=guide_legend(override.aes=list(fill=NA)))+
      theme(panel.grid.major = element_line(color = "black"))+
      theme(axis.title = element_text(colour = "black", size=9), axis.text.y =element_text(colour = "black", angle = 90, hjust = 0.6), 
            axis.text.x =element_text(colour = "black"), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
            legend.position = c(.9, .2), legend.title=element_text(colour = "black", size = 9), legend.text=element_text(colour = "black", size = 8), 
            legend.background = element_rect(fill="white", size=0.5) , axis.line = element_line(colour = "black", size = 1, linetype = "solid"), 
            axis.ticks = element_line(colour = "black", size =1, linetype ="solid"), panel.border = element_blank(), panel.background = element_blank())

    #4. Re-run model based on headcoil-adjusted values
    fitC2 <- lme(CSF_adjusted~AutismControl +QC_Maddy_MID02 +TBV_mc + TBV_squared_mc + time_bin+ age_mc + age_squared_mc +age_cubed_mc + age_cubed_mc*AutismControl + AutismControl*age_mc + age_squared_mc*AutismControl, 
                 random=~1+age_mc|SUBJID,data= data_long_QC_dropped, method="REML")
    summary(fitC2)
  
    #5. Plot head-coil adjusted values and model fit lines
    ggplot(data_long_QC_dropped, aes(x=Age, y=CSF_adjusted, color=AutismControl))+
      geom_line(aes(group=Participant))+
      labs(x = "Age (Years)", y = 'Adjusted EA-CSF Volume ('~cm^3*')')+
      labs(fill = " ")+
      labs(color = " ")+
      scale_colour_manual(values=Palette, labels = c("Autism", "Control"))+
      scale_fill_manual(values=Palette, labels = c("Autism", "Control"))+
      geom_point(aes(fill=AutismControl), colour="black", pch=21, cex=1)+
      geom_line(data=data_long_QC_dropped, aes(y=predict(fitC2,level=0), group=AutismControl, colour=AutismControl), size = 1.5)+
      scale_y_continuous(limits=c(0,150))+
      scale_x_continuous(limits=c(0,43), expand = c(0,0))+
      guides(color=guide_legend(override.aes=list(fill=NA)))+
      theme_bw()+
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
      theme(axis.title = element_text(colour = "black", size=9), axis.text.y =element_text(colour = "black", angle = 90, hjust = 0.6), 
            axis.text.x =element_text(colour = "black"), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
            legend.position = c(.9, .2), legend.title=element_text(colour = "black", size = 9), legend.text=element_text(colour = "black", size = 9), 
            legend.background = element_rect(fill="white", size=0.5) , axis.line = element_line(colour = "black", size = 1, linetype = "solid"), 
            axis.ticks = element_line(colour = "black", size =1, linetype ="solid"), panel.border = element_blank(), panel.background = element_blank())
    

   
#Model with TBV mean-centered covariate 
    data_long_QC_droppedTBV <- subset(data_long_QC_dropped, TotalBrain_Volume_cm!="NA")
    #MLM with TBV covariate (N = 412 scans, 184 participants)
    fit_TBV1 = lme(CSF_cm~AutismControl + age_mc + QC_Maddy_MID02 + age_squared_mc +age_cubed_mc + age_cubed_mc*AutismControl + TotalBrain_Volume_cm + AutismControl*age_mc + age_squared_mc*AutismControl, 
                   random=~1+age_mc|SUBJID,data= data_long_QC_droppedTBV, method="REML")
    summary(fit_TBV1)
    
    #Mean-center
    favstats(data_long_QC_droppedTBV$TotalBrain_Volume_cm)
    TBV_mean = 1132.435
    data_long_QC_droppedTBV$TBV_mc <- data_long_QC_droppedTBV$TotalBrain_Volume_cm - TBV_mean
    data_long_QC_droppedTBV$TBV_squared_mc <- data_long_QC_droppedTBV$TBV_mc * data_long_QC_droppedTBV$TBV_mc
    
    #MLM with TBV mean-centered
    fit_TBV2 = lme(CSF_cm~AutismControl + age_mc + QC_Maddy_MID02 + age_squared_mc + TBV_mc + TBV_squared_mc +age_cubed_mc + age_cubed_mc*AutismControl + AutismControl*age_mc + age_squared_mc*AutismControl, 
                   random=~1+age_mc|SUBJID,data= data_long_QC_droppedTBV, method="REML")
    summary(fit_TBV2)
    
    
    
# Model with QCistern CSF (and plot)
    #Load QCistern long form
    QCistern <- read_excel("C:/Users/maddy/Box/Autism_CSF/data/QCistern/Concatenated_QCistern.xlsx")
    QCistern$SUBJID <- strsplit(QCistern$SUBJID,"_")
    #Merge with data_long_QC_dropped
    data_long_QCistern <- merge(data_long_QC_dropped, QCistern, by =c("SUBJID", "timepoint"), all=FALSE)
    
    #transform QCistern raw to cm3
    data_long_QCistern$QCistern_cm <- data_long_QCistern$Raw_QCistern/1000
    
    #MLM arrives at the same results!
    fit_QCistern = lme(QCistern_cm~AutismControl + age_mc + QC_Maddy_MID02 + age_squared_mc +age_cubed_mc + age_cubed_mc*AutismControl + AutismControl*age_mc + age_squared_mc*AutismControl, 
                   random=~1+age_mc|SUBJID,data= data_long_QCistern, method="REML")
    summary(fit_QCistern)
    
    
    #Plot of QCistern CSF by Age
    ggplot(data_long_QCistern, aes(x=Age, y=QCistern_cm, color=AutismControl))+
      geom_line(aes(group=Participant))+
      labs(x = 'Age', y = 'EA-CSF Volume: QCistern File ('~cm^3*')')+
      labs(fill = " ")+
      labs(color = " ")+
      scale_colour_manual(values=Palette, labels = c("Autism", "Control"))+
      scale_fill_manual(values=Palette, labels = c("Autism", "Control"))+
      geom_point(aes(fill=AutismControl), colour="black", pch=21, cex=1)+
      geom_smooth(method=loess, aes(fill=AutismControl), se=TRUE) +
      scale_y_continuous(limits=c(0, 150))+
      scale_x_continuous(limits=c(0,42), expand = c(0,0))+
      guides(color=guide_legend(override.aes=list(fill=NA)))+
      theme_bw()+
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
      theme(axis.title = element_text(colour = "black", size=9), axis.text.y =element_text(colour = "black", angle = 90, hjust = 0.6), 
            axis.text.x =element_text(colour = "black"), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
            legend.position = c(.1, .9), legend.title=element_text(colour = "black", size = 9), legend.text=element_text(colour = "black", size = 8), 
            legend.background = element_rect(fill="white", size=0.5) , axis.line = element_line(colour = "black", size = 1, linetype = "solid"), 
            axis.ticks = element_line(colour = "black", size =1, linetype ="solid"), panel.border = element_blank(), panel.background = element_blank())
    
    
    #Plot of QCistern CSF by EA-CSF
    ggplot(data_long_QCistern, aes(x=CSF_cm, y=QCistern_cm, color=AutismControl))+
      geom_line(aes(group=Participant))+
      labs(x = 'EA-CSF Volume: MID02 File ('~cm^3*')', y = 'EA-CSF Volume: QCistern File ('~cm^3*')')+
      labs(fill = " ")+
      labs(color = " ")+
      scale_colour_manual(values=Palette, labels = c("Autism", "Control"))+
      scale_fill_manual(values=Palette, labels = c("Autism", "Control"))+
      geom_point(aes(fill=AutismControl), colour="black", pch=21, cex=1)+
      geom_smooth(method=loess, aes(fill=AutismControl), se=TRUE) +
      scale_y_continuous(limits=c(0, 150))+
      scale_x_continuous(limits=c(0, 155), expand = c(0,0))+
      guides(color=guide_legend(override.aes=list(fill=NA)))+
      theme_bw()+
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
      theme(axis.title = element_text(colour = "black", size=9), axis.text.y =element_text(colour = "black", angle = 90, hjust = 0.6), 
            axis.text.x =element_text(colour = "black"), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
            legend.position = c(.1, .9), legend.title=element_text(colour = "black", size = 9), legend.text=element_text(colour = "black", size = 8), 
            legend.background = element_rect(fill="white", size=0.5) , axis.line = element_line(colour = "black", size = 1, linetype = "solid"), 
            axis.ticks = element_line(colour = "black", size =1, linetype ="solid"), panel.border = element_blank(), panel.background = element_blank())
 

    
#Mean Cortical Thickness and EA-CSF
    #Setup
    rh_aparc <- read.csv("C:/Users/maddy/Box/Autism_CSF/data/freesurfer_cross/aparc_RH_stats_concatenated_long.csv")
    lh_aparc <- read.csv("C:/Users/maddy/Box/Autism_CSF/data/freesurfer_cross/aparc_LH_stats_concatenated_long.csv")
    
    rh_aparc <- rh_aparc[,c("SUBJID", "timepoint", "rh_MeanThickness_thickness")]
    lh_aparc <- lh_aparc[,c("SUBJID", "timepoint", "lh_MeanThickness_thickness")]
    rh_aparc$SUBJID <- as.factor(rh_aparc$SUBJID)
    rh_aparc$timepoint <- as.factor(rh_aparc$timepoint)
    lh_aparc$SUBJID <- as.factor(lh_aparc$SUBJID)
    lh_aparc$timepoint <- as.factor(lh_aparc$timepoint)
    data_long_QC_dropped$SUBJID <- as.factor(data_long_QC_dropped$SUBJID)
    data_long_QC_dropped$timepoint <- as.factor(data_long_QC_dropped$timepoint)
    
    #Remove spaces before and after timepoint var and SUBJID var
    trim <- function (x) gsub("^\\s+|\\s+$", "", x)
    lh_aparc$SUBJID <- trim(lh_aparc$SUBJID)
    rh_aparc$SUBJID <- trim(rh_aparc$SUBJID)
    lh_aparc$timepoint <- trim(lh_aparc$timepoint)
    rh_aparc$timepoint <- trim(rh_aparc$timepoint)
    lh_aparc <- distinct(lh_aparc)
    rh_aparc <- distinct(rh_aparc)
   
    #Merge
    data_aparc <- merge(rh_aparc, lh_aparc, by=c("SUBJID", "timepoint"), all=FALSE)
    data_long_QC_aparc <- merge(data_aparc, data_long_QC_dropped, by=c("SUBJID", "timepoint"), all=FALSE)
    
    # Variables
    data_long_QC_aparc$totalmean_CT <- (data_long_QC_aparc$rh_MeanThickness_thickness + data_long_QC_aparc$lh_MeanThickness_thickness)/2
    data_long_QC_aparc2 <- subset(data_long_QC_aparc, age_mc!="NA")
    data_long_QC_aparc2 <- subset(data_long_QC_aparc2, totalmean_CT!="NA")
    
    # 1. Model 
    fitC.1 <- lme(totalmean_CT~time_bin+QC_Maddy_MID02+AutismControl+TBV_mc+TBV_squared_mc, 
                  random=~1|SUBJID,data= data_long_QC_aparc2, method="REML")
    summary(fitC.1)
    
    # 2. Adjust CT for headcoil
    data_long_QC_aparc2$time_bin <- as.numeric(data_long_QC_aparc2$time_bin)
    data_long_QC_aparc2$CT_adjusted <- data_long_QC_aparc2$totalmean_CT - ((-0.1348748*(data_long_QC_aparc2$time_bin - 0)) + (-0.0359074*(data_long_QC_aparc2$QC_Maddy_MID02 - mean(data_long_QC_dropped$QC_Maddy_MID02))) + (0.0003482*(data_long_QC_dropped$TBV_mc - mean(data_long_QC_dropped$TBV_mc))) + (-0.0000001*(data_long_QC_dropped$TBV_squared_mc - mean(data_long_QC_dropped$TBV_squared_mc))))
    #mean shifted check
    mean(data_long_QC_aparc2$CT_adjusted)
    mean(data_long_QC_aparc2$totalmean_CT)
       # Adjust EA-CSF for heacoil, qc
    fitC.2 <- lme(CSF_cm~CT_adjusted + QC_Maddy_MID02 + time_bin + AutismControl + TBV_mc + TBV_squared_mc, 
                  random=~1|SUBJID,data= data_long_QC_aparc2, method="REML")
    summary(fitC.2)
    data_long_QC_aparc2$time_bin <- as.numeric(data_long_QC_aparc2$time_bin)
    data_long_QC_aparc2$CSF_adjusted <- data_long_QC_aparc2$CSF_cm - ((21.92174*(data_long_QC_aparc2$time_bin - 0)) + (5.64254*(data_long_QC_aparc2$QC_Maddy_MID02 - mean(data_long_QC_dropped$QC_Maddy_MID02))) + (0.03056*(data_long_QC_dropped$TBV_mc - mean(data_long_QC_dropped$TBV_mc))) + (0.00011*(data_long_QC_dropped$TBV_squared_mc - mean(data_long_QC_dropped$TBV_squared_mc))))

    #mean shifted check
    mean(data_long_QC_aparc2$CSF_adjusted)
    mean(data_long_QC_aparc2$CSF_cm)
    
    # 3. Model with adjusted CT values
    fitC.3 <- lme(CSF_adjusted~CT_adjusted + QC_Maddy_MID02 + time_bin + AutismControl + TBV_mc + TBV_squared_mc, 
                  random=~1|SUBJID,data= data_long_QC_aparc2, method="REML")
    summary(fitC.3)
    
    # 4. Plots using adjusted CT values

    #Plot Total Mean Cortical Thickness by Age
    Palette <- c("#0072B2", "#E69F00")
    ggplot(data_long_QC_aparc2, aes(x=Age, y=CT_adjusted, color=AutismControl))+
      geom_line(aes(group=Participant))+
      labs(x = "Age (Years)", y = 'Adjusted Total Mean Cortical Thickness')+
      labs(fill = " ")+
      labs(color = " ")+
      scale_colour_manual(values=Palette, labels = c("Autism", "Control"))+
      scale_fill_manual(values=Palette, labels = c("Autism", "Control"))+
      geom_point(aes(fill=AutismControl), colour="black", pch=21, cex=1)+
      geom_smooth(method=loess, aes(fill=AutismControl), se=TRUE) +
      scale_y_continuous(limits=c(2.25,3.25))+
      scale_x_continuous(limits=c(0,43), expand = c(0,0))+
      guides(color=guide_legend(override.aes=list(fill=NA)))+
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
      theme(axis.title = element_text(colour = "black", size=9), axis.text.y =element_text(colour = "black", angle = 90, hjust = 0.6), 
            axis.text.x =element_text(colour = "black"), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
            legend.position = c(.9, .9), legend.title=element_text(colour = "black", size = 9), legend.text=element_text(colour = "black", size = 8), 
            legend.background = element_rect(fill="white", size=0.4) , axis.line = element_line(colour = "black", size = 1, linetype = "solid"), 
            axis.ticks = element_line(colour = "black", size =1, linetype ="solid"), panel.border = element_blank(), panel.background = element_blank())
    
    #ggsave(filename = paste("Figure4.A_CT_age_small_210510.png"), width = 3.4, height = 3.4,
     #  path = "C:/Users/maddy/Box/Autism_CSF/figures", dpi = 300)
    
    #Plot Total Mean Cortical Thickness by EA-CSF
    ggplot(data_long_QC_aparc2, aes(x=CT_adjusted, y=CSF_adjusted, color=AutismControl))+
      geom_line(aes(group=Participant))+
      labs(x = 'Adjusted Total Mean Cortical Thickness', y = 'Adjusted Extra-axial CSF ('~cm^3*')')+
      labs(fill = " ")+
      labs(color = " ")+
      scale_colour_manual(values=Palette, labels = c("Autism", "Control"))+
      scale_fill_manual(values=Palette, labels = c("Autism", "Control"))+
      geom_point(aes(fill=AutismControl), colour="black", pch=21, cex=1)+
      geom_smooth(method=loess, aes(fill=AutismControl), se=TRUE) +
      scale_y_continuous(limits=c(0,140))+
      scale_x_continuous(limits=c(2.25,3.25), expand = c(0,0))+
      guides(color=guide_legend(override.aes=list(fill=NA)))+
      theme_bw()+
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
      theme(axis.title = element_text(colour = "black", size=9), axis.text.y =element_text(colour = "black", angle = 90, hjust = 0.6), 
            axis.text.x =element_text(colour = "black"), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
            legend.position = c(.9, .9), legend.title=element_text(colour = "black", size = 9), legend.text=element_text(colour = "black", size = 8), 
            legend.background = element_rect(fill="white", size=0.4) , axis.line = element_line(colour = "black", size = 1, linetype = "solid"), 
            axis.ticks = element_line(colour = "black", size =1, linetype ="solid"), panel.border = element_blank(), panel.background = element_blank())
    
    #ggsave(filename = paste("Figure4.B_CT_small_210510.png"), width = 3.4, height = 3.4,
     #   path = "C:/Users/maddy/Box/Autism_CSF/figures", dpi = 300)
    
 
           
# Are there participants with ADOS at Time5 without ADOS at Entry?  
    #Discrepancies?
    data_long_QC_dropped$ADOS_discrep <- ifelse(is.na(data_long_QC_dropped$TOTAL.CSS) & data_long_QC_dropped$Time5ADOS.TOTAL.CSS!="NA", "1", "0")
    table(data_long_QC_dropped$ADOS_discrep)
    
    #Add in time 5 if no ADOS at Entry
    data_long_QC_dropped$ADOS_comb <- ifelse(is.na(data_long_QC_dropped$TOTAL.CSS) & data_long_QC_dropped$Time5ADOS.TOTAL.CSS!="NA", data_long_QC_dropped$Time5ADOS.TOTAL.CSS, data_long_QC_dropped$TOTAL.CSS)
    table(data_long_QC_dropped$ADOS_comb)  
    
    
#-----------------------------------MANUSCRIPT------------------------------------------------        
    
#Abstract
    #Number of scans and ASD/TD participants
    table(data_long_QC_dropped$AutismControl)
    table(data_wide_all$AutismControl)
    
    #Single versus multiple scan for ADOS score (ASD severity)
    #Dummy variable for single scan versus multiple scans
    data_long_QC_dropped$single_scan <- ifelse(data_long_QC_dropped$num_scans == "1", "1", "0")
    #subset to participants with ADOS
    data_long_QC_ADOS <- subset(data_long_QC_dropped, ADOS_comb!="NA")
    #MLM
    fitsingle = lme(ADOS_comb~single_scan, random=~1 |SUBJID,data= data_long_QC_ADOS, method="REML")
    summary(fitsingle)
    
#Method    
    #Original number of scans
    table(data_long_QC$AutismControl)
    table(data_long_QC$AutismControl, data_long_QC$sex)
    
    #Current number of scans
    table(data_long_QC_dropped$AutismControl)
    table(data_wide_all$AutismControl)

    
  #LVCP numbers
    #Subset dataset to only include participants with avg FIQ score
    which( colnames(data_wide_all)=="FIQ.Time1" )
    which( colnames(data_wide_all)=="FIQ.Time2" )
    which( colnames(data_wide_all)=="FIQ.Time3" )
    which( colnames(data_wide_all)=="FIQ.Time4" )
    which( colnames(data_wide_all)=="FIQ.Time5" )
    data_wide_all$FIQ.Time1 <- as.numeric(data_wide_all$FIQ.Time1)
    data_wide_all$FIQ.Time2 <- as.numeric(data_wide_all$FIQ.Time2)
    data_wide_all$FIQ.Time3 <- as.numeric(data_wide_all$FIQ.Time3)
    data_wide_all$FIQ.Time4 <- as.numeric(data_wide_all$FIQ.Time4)
    data_wide_all$FIQ.Time5 <- as.numeric(data_wide_all$FIQ.Time5)
    data_wide_all$fiq_avg <- rowMeans(data_wide_all[,c(261,663,395,529,127)], na.rm=TRUE)
    
    data_wide_all_FIQ <- subset(data_wide_all, fiq_avg!="NA")
    data_wide_all_FIQ$fiq_avg <- as.numeric(data_wide_all_FIQ$fiq_avg)
    data_wide_all_FIQ$LVCP <- ifelse(data_wide_all_FIQ$fiq_avg <= 79, "1", "0")
    #Number of participants with LVCP/HVCP
    table(data_wide_all_FIQ$LVCP, data_wide_all_FIQ$AutismControl)
    #Number of scans for LVCP and HVCP individuals
    data_wide_all_FIQ <- transform(data_wide_all_FIQ,scan1=ifelse(!is.na(ScanDateT1.Time1),1, 0 ))
    data_wide_all_FIQ <- transform(data_wide_all_FIQ,scan2=ifelse(!is.na(ScanDateT2.Time2),1, 0 ))
    data_wide_all_FIQ <- transform(data_wide_all_FIQ,scan3=ifelse(!is.na(ScanDateT3.Time3),1, 0 ))
    data_wide_all_FIQ <- transform(data_wide_all_FIQ,scan4=ifelse(!is.na(ScanDateT4.Time4),1, 0 ))
    data_wide_all_FIQ <- transform(data_wide_all_FIQ,scan5=ifelse(!is.na(ScanDateT5.Time5),1, 0 ))
    data_wide_all_FIQ$num_scans <- (data_wide_all_FIQ$scan1 + data_wide_all_FIQ$scan2 + data_wide_all_FIQ$scan3 + data_wide_all_FIQ$scan4 + data_wide_all_FIQ$scan5)
    data_wide_all_LVCP <- subset(data_wide_all_FIQ, LVCP=="1")
    data_wide_all_LVCP_ASD <- subset(data_wide_all_LVCP, AutismControl=="autism")
    data_wide_all_LVCP_TD <- subset(data_wide_all_LVCP, AutismControl=="control")
      #LVCP #s
      sum(data_wide_all_LVCP_ASD$num_scans)  
      sum(data_wide_all_LVCP_TD$num_scans)
    
    data_wide_all_HVCP <- subset(data_wide_all_FIQ, LVCP=="0")
    data_wide_all_HVCP_ASD <- subset(data_wide_all_HVCP, AutismControl=="autism")
    data_wide_all_HVCP_TD <- subset(data_wide_all_HVCP, AutismControl=="control")
      #HVCP #s
    sum(data_wide_all_HVCP_ASD$num_scans)  
    sum(data_wide_all_HVCP_TD$num_scans)
    
    
    
    #Number of individuals with each number of scan
    data_wide_all <- transform(data_wide_all,scan1=ifelse(!is.na(ScanDateT1.Time1),1, 0 ))
    data_wide_all <- transform(data_wide_all,scan2=ifelse(!is.na(ScanDateT2.Time2),1, 0 ))
    data_wide_all <- transform(data_wide_all,scan3=ifelse(!is.na(ScanDateT3.Time3),1, 0 ))
    data_wide_all <- transform(data_wide_all,scan4=ifelse(!is.na(ScanDateT4.Time4),1, 0 ))
    data_wide_all <- transform(data_wide_all,scan5=ifelse(!is.na(ScanDateT5.Time5),1, 0 ))
    
    data_wide_all$num_scans <- (data_wide_all$scan1 + data_wide_all$scan2 + data_wide_all$scan3 + data_wide_all$scan4 + data_wide_all$scan5)
    table(data_wide_all$num_scans, data_wide_all$AutismControl)
    
    
  #Table1  
    #Comparison 1: Age 
      #Independent Samples t-test
      t.test(Age ~ AutismControl, data = data_long_QC_dropped, mu = 0, alternative = "two.sided", conf.level = 0.95)
    
      #Five number summary
      favstats(Age ~ AutismControl, data=data_long_QC_dropped)
      #Overall mean age
      mean(data_long_QC_dropped$Age, na.rm=TRUE)
      
    #Comparison 2: Interscan interval
      #Independent t test
      t.test(avg_int ~ AutismControl, data = data_long_QC_dropped, mu = 0, alternative = "two.sided", conf.level = 0.95)
      
      #Five number summary
      favstats(avg_int ~ AutismControl, data=data_long_QC_dropped)
      #Overall mean interscan interval
      favstats(data_long_QC_dropped$avg_int)  
      
    #Comparison 3-5: IQ
      which( colnames(data_wide_all)=="FIQ.Time1" )
      which( colnames(data_wide_all)=="FIQ.Time2" )
      which( colnames(data_wide_all)=="FIQ.Time3" )
      which( colnames(data_wide_all)=="FIQ.Time4" )
      which( colnames(data_wide_all)=="FIQ.Time5" )
      data_wide_all$FIQ.Time1 <- as.numeric(data_wide_all$FIQ.Time1)
      data_wide_all$FIQ.Time2 <- as.numeric(data_wide_all$FIQ.Time2)
      data_wide_all$FIQ.Time3 <- as.numeric(data_wide_all$FIQ.Time3)
      data_wide_all$FIQ.Time4 <- as.numeric(data_wide_all$FIQ.Time4)
      data_wide_all$FIQ.Time5 <- as.numeric(data_wide_all$FIQ.Time5)
      data_wide_all$fiq_avg <- rowMeans(data_wide_all[,c(261,663,395,529,127)], na.rm=TRUE)
      
      which( colnames(data_wide_all)=="PIQ.Time1" )
      which( colnames(data_wide_all)=="PIQ.Time2" )
      which( colnames(data_wide_all)=="PIQ.Time3" )
      which( colnames(data_wide_all)=="PIQ.Time4" )
      which( colnames(data_wide_all)=="PIQ.Time5" )
      data_wide_all$PIQ.Time1 <- as.numeric(data_wide_all$PIQ.Time1)
      data_wide_all$PIQ.Time2 <- as.numeric(data_wide_all$PIQ.Time2)
      data_wide_all$PIQ.Time3 <- as.numeric(data_wide_all$PIQ.Time3)
      data_wide_all$PIQ.Time4 <- as.numeric(data_wide_all$PIQ.Time4)
      data_wide_all$PIQ.Time5 <- as.numeric(data_wide_all$PIQ.Time5)
      data_wide_all$piq_avg <- rowMeans(data_wide_all[,c(262,664,396,530,128)], na.rm=TRUE)
      
      which( colnames(data_wide_all)=="VIQ.Time1" )
      which( colnames(data_wide_all)=="VIQ.Time2" )
      which( colnames(data_wide_all)=="VIQ.Time3" )
      which( colnames(data_wide_all)=="VIQ.Time4" )
      which( colnames(data_wide_all)=="VIQ.Time5" )
      data_wide_all$VIQ.Time1 <- as.numeric(data_wide_all$VIQ.Time1)
      data_wide_all$VIQ.Time2 <- as.numeric(data_wide_all$VIQ.Time2)
      data_wide_all$VIQ.Time3 <- as.numeric(data_wide_all$VIQ.Time3)
      data_wide_all$VIQ.Time4 <- as.numeric(data_wide_all$VIQ.Time4)
      data_wide_all$VIQ.Time5 <- as.numeric(data_wide_all$VIQ.Time5)
      data_wide_all$viq_avg <- rowMeans(data_wide_all[,c(261,663,395,529,127)], na.rm=TRUE)
      
      #PIQ
      #Independent t test
      t.test(piq_avg ~ AutismControl, data = data_wide_all, mu = 0, alternative = "less", conf.level = 0.95, na.rm=TRUE)
      #Five number summary
      favstats(piq_avg ~ AutismControl, data = data_wide_all)
      
      #VIQ
      #Independent t test
      t.test(viq_avg ~ AutismControl, data = data_wide_all, mu = 0, alternative = "less", conf.level = 0.95, na.rm=TRUE)
      #Five number summary
      favstats(viq_avg ~ AutismControl, data = data_wide_all)
      
      #FIQ
      #Independent t test
      t.test(fiq_avg ~ AutismControl, data = data_wide_all, mu = 0, alternative = "less", conf.level = 0.95, na.rm=TRUE)
      #Five number summary
      favstats(fiq_avg ~ AutismControl, data = data_wide_all)
      

    #Comparison 6-8: ADOS
      #load data
      ADOS_long <- read.csv("C:/Users/maddy/Box/Autism_CSF/data/UofU_Longitudinal_ADOS_CSS_210428.csv") 
      names(ADOS_long)[1] <- "SUBJID"
      #merge with other wide data
      data_wide_all_ADOS <- merge(ADOS_long, data_wide_all, by =c("SUBJID"), all=FALSE)
      data_wide_all_ADOS <- subset(data_wide_all_ADOS, sex!=2)
      
      #Five number summary _ ADOS at Entry
      #Add in time 5 if no ADOS at Entry
      data_wide_all_ADOS$ADOS_comb <- ifelse(is.na(data_wide_all_ADOS$TOTAL.CSS) & data_wide_all_ADOS$Time5ADOS.TOTAL.CSS!="NA", data_wide_all_ADOS$Time5ADOS.TOTAL.CSS, data_wide_all_ADOS$TOTAL.CSS)
      table(data_long_QC_dropped$ADOS_comb)
      
      favstats(ADOS_comb ~ AutismControl, data = data_wide_all_ADOS)
      
      #Five number summary _ ADOS at Time5
      #favstats(Time5ADOS.TOTAL.CSS ~ AutismControl, data = data_wide_all_ADOS)
      
      #Five number summary _ ADI-R
      ADI <- read_excel("C:/Users/maddy/Box/Autism_CSF/data/ADOSADI.xlsx")
      names(ADI)[1] <- "SUBJID"
      ADI <- ADI[,c("SUBJID", "ADI_revised")]
      data_wide_all_ADOS <- merge(ADI, data_wide_all_ADOS, by =c("SUBJID"), all=FALSE)
      data_wide_all_ADOS <- subset(data_wide_all_ADOS, sex!=2)
      favstats(ADI_revised ~ AutismControl, data = data_wide_all_ADOS)
     
  #Figure 1: Participant Age at Scan
      Palette <- c("#0072B2", "#E69F00")
      ggplot(data_long_QC_dropped, aes(x=Age, y=Participant, color=AutismControl))+
        geom_line(aes(group = Participant))+
        labs(x = "Age (Years)", y = "Participant")+
        geom_point(aes(fill=AutismControl), colour="black", pch=21)+
        labs(fill = " ")+
        labs(color = " ")+
        scale_colour_manual(values=Palette, labels = c("Autism", "Control"))+
        scale_fill_manual(values=Palette, labels = c("Autism", "Control"))+
        scale_y_continuous(limits=c(0,300))+
        scale_x_continuous(limits=c(0,43))+
        theme_bw()+
        theme(panel.grid.major = element_line(color = "grey"))+
        theme(axis.title = element_text(colour = "black", size=9), axis.text.y =element_text(colour = "black", angle = 90, hjust = 0.6), 
              axis.text.x =element_text(colour = "black"), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
              legend.position = c(0.8, 0.2), legend.title=element_text(colour = "black", size = 9), legend.text=element_text(colour = "black", size = 9), 
              legend.background = element_rect(fill="white", size=0.5) , axis.line = element_line(colour = "black", size = 1, linetype = "solid"), 
              axis.ticks = element_line(colour = "black", size =1, linetype ="solid"), panel.border = element_blank(), panel.background = element_blank())
      
      #ggsave(filename = paste("Figure1_210713.pdf"), width = 3.35, height = 5,
      #       path = "C:/Users/maddy/Box/Autism_CSF/figures/pdf_format_210713", dpi = 300)
      
      
  # FIGURE 2 HCP TEST-RETEST 
      #Figure 2.1 Comparison of 1.7.7 MPR1, MPR2 test-retest 28 subjects
      #import data
      MPR1_MPR2 <- read_excel("C:/Users/maddy/Box/Autism_CSF/Auto_EACSF_Pipeline/test_retest_1.7.7_studyparams/Comparison_MPR1_MPR2.xlsx")
      #rename vars
      names(MPR1_MPR2) <- c("SUBJID", "MPR1", "MPR2")
      MPR1_MPR2$MPR1_cm <- MPR1_MPR2$MPR1/1000
      MPR1_MPR2$MPR2_cm <- MPR1_MPR2$MPR2/1000
      
      Palette <- c("#0072B2", "#E69F00")
      ggplot(MPR1_MPR2, aes(x=MPR1_cm, y=MPR2_cm))+
        labs(x = 'Session 1 Extra-axial CSF Volume ('~cm^3*')', y = 'Session 2 Extra-axial CSF Volume ('~cm^3*')')+
        scale_colour_manual(values=Palette)+
        scale_fill_manual(values=Palette)+
        geom_smooth(aes(colour="007282"), method = "lm", size=1, se = FALSE)+ 
        geom_abline(slope=1, intercept = 0, size=1, linetype= "dashed")+
        geom_point(aes(fill="999999"), colour="black", pch=21)+
        theme_bw()+
        theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
        theme(axis.title = element_text(colour = "black", size=9), axis.text.y =element_text(colour = "black", angle = 90, hjust = 0.6), 
              axis.text.x =element_text(colour = "black"), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
              legend.position ="none", legend.title=element_blank(), legend.text=element_blank(), 
              legend.background = element_rect(fill="white", size=0.5) , axis.line = element_line(colour = "black", size = 1, linetype = "solid"), 
              axis.ticks = element_line(colour = "black", size =1, linetype ="solid"), panel.border = element_blank(), panel.background = element_blank())
      
      #ggsave(filename = paste("Figure2_210713.pdf"), width = 3.4, height = 3.4,
      #       path = "C:/Users/maddy/Box/Autism_CSF/figures/pdf_format_210713", dpi = 300)
      
      
      #Calculate ICC on HCP test-retest data
      library(psych) 
      #import data
      MPR1_MPR2 <- read_excel("C:/Users/maddy/Box/Autism_CSF/Auto_EACSF_Pipeline/test_retest_1.7.7_studyparams/Comparison_MPR1_MPR2.xlsx")
      #rename vars
      names(MPR1_MPR2) <- c("SUBJID", "MPR1", "MPR2")
      MPR1_MPR2$MPR1_cm <- MPR1_MPR2$MPR1/1000
      MPR1_MPR2$MPR2_cm <- MPR1_MPR2$MPR2/1000
      MPR1_MPR2$SUBJID <- NULL #Remove this column so it can calculate the ICC between MPR1 and MPR2 sessions
      
      ICC(MPR1_MPR2) #ICC3 was reported in publication
      
      
      
  #Visual QC
      #Original number of scans
      table(data_long_QC$AutismControl)
      table(data_long_QC$AutismControl, data_long_QC$sex)
      
      #Failed scans
      table(data_long_QC$QC_fail, data_long_QC$AutismControl)
      
      #MLM EA-CSF QC and Group
      fitQC = lme(QC_Maddy_MID02~AutismControl, 
                  random=~1|SUBJID,data= data_long_QC_dropped, method="REML")
      summary(fitQC)
      
      
      
#Results
    #Main Results
      fitC <- lme(CSF_cm~AutismControl + age_mc + QC_Maddy_MID02 +TBV_mc + TBV_squared_mc + time_bin+ age_squared_mc + age_cubed_mc + age_cubed_mc*AutismControl + AutismControl*age_mc + age_squared_mc*AutismControl, 
                  random=~1+age_mc|SUBJID,data= data_long_QC_dropped, method="ML")
      summary(fitC)
      
      
      
      #This model has age recentered at 10
      data_long_QC_dropped$Age10<- data_long_QC_dropped$Age - 10 
      data_long_QC_dropped$age_squared10<-data_long_QC_dropped$Age10 * data_long_QC_dropped$Age10
      data_long_QC_dropped$age_cubed10<-data_long_QC_dropped$age_squared10 * data_long_QC_dropped$Age10
      
      fit_age10=lme(CSF_cm~AutismControl + Age10 + QC_Maddy_MID02 + TBV_mc +TBV_squared_mc+ time_bin + age_squared10 + age_cubed10 + age_cubed10*AutismControl + AutismControl*Age10 + AutismControl*age_squared10,random=~1+Age10|SUBJID,
                    data=data_long_QC_dropped,method="REML", na.action=na.omit)
      summary(fit_age10)
      
      #This model has age recentered at 20
      data_long_QC_dropped$Age20<- data_long_QC_dropped$Age - 20
      data_long_QC_dropped$age_squared20<-data_long_QC_dropped$Age20 * data_long_QC_dropped$Age20
      data_long_QC_dropped$age_cubed20<-data_long_QC_dropped$age_squared20 * data_long_QC_dropped$Age20
      
      fit_age20=lme(CSF_cm~AutismControl + Age20 + QC_Maddy_MID02 + TBV_mc +TBV_squared_mc+ time_bin + age_squared20 + age_cubed20 + age_cubed20*AutismControl + AutismControl*Age20 + AutismControl*age_squared20,random=~1+Age20|SUBJID,
                    data=data_long_QC_dropped,method="REML", na.action=na.omit)
      summary(fit_age20)
      
      
      #This model has age recentered at 30
      data_long_QC_dropped$Age30<- data_long_QC_dropped$Age - 30
      data_long_QC_dropped$age_squared30<-data_long_QC_dropped$Age30 * data_long_QC_dropped$Age30
      data_long_QC_dropped$age_cubed30<-data_long_QC_dropped$age_squared30 * data_long_QC_dropped$Age30
      
      fit_age30=lme(CSF_cm~AutismControl + Age30 + QC_Maddy_MID02 + TBV_mc +TBV_squared_mc+ time_bin + age_squared30 + age_cubed30 + age_cubed30*AutismControl + AutismControl*Age30 + AutismControl*age_squared30,random=~1+Age30|SUBJID,
                    data=data_long_QC_dropped,method="REML", na.action=na.omit)
      summary(fit_age30)
      
      
    #Figure 3: EA-CSF by Age (RAW values)
      Palette <- c("#0072B2", "#E69F00")
      #plot age by CSF
      ggplot(data_long_QC_dropped, aes(x=Age, y=CSF_cm, color=AutismControl))+
        geom_line(aes(group=Participant))+
        labs(x = "Age (Years)", y = 'Extra-axial CSF Volume ('~cm^3*')')+
        labs(fill = " ")+
        labs(color = " ")+
        scale_colour_manual(values=Palette, labels = c("Autism", "Control"))+
        scale_fill_manual(values=Palette, labels = c("Autism", "Control"))+
        geom_point(aes(fill=AutismControl), colour="black", pch=21, cex=1)+
        geom_smooth(method=loess, aes(fill=AutismControl), se=TRUE) +
        scale_y_continuous(limits=c(0,150))+
        scale_x_continuous(limits=c(0,43), expand = c(0,0))+
        guides(color=guide_legend(override.aes=list(fill=NA)))+
        theme(panel.grid.major = element_line(color = "black"))+
        theme(axis.title = element_text(colour = "black", size=9), axis.text.y =element_text(colour = "black", angle = 90, hjust = 0.6), 
              axis.text.x =element_text(colour = "black"), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
              legend.position = c(.9, .2), legend.title=element_text(colour = "black", size = 9), legend.text=element_text(colour = "black", size = 8), 
              legend.background = element_rect(fill="white", size=0.5) , axis.line = element_line(colour = "black", size = 1, linetype = "solid"), 
              axis.ticks = element_line(colour = "black", size =1, linetype ="solid"), panel.border = element_blank(), panel.background = element_blank())
      
      #ggsave(filename = paste("Figure3_QC_small_210428.png"), width = 3.4, height = 3.4,
      #       path = "C:/Users/maddy/Box/Autism_CSF/figures", dpi = 300)
      
      
      #Model-adjusted plots: FIGURE 3
      #1. Run model
      fitC <- lme(CSF_cm~AutismControl + age_mc + QC_Maddy_MID02 +TBV_mc + TBV_squared_mc + time_bin+ age_squared_mc +age_cubed_mc + age_cubed_mc*AutismControl + AutismControl*age_mc + age_squared_mc*AutismControl, 
                  random=~1+age_mc|SUBJID,data= data_long_QC_dropped, method="REML")
      summary(fitC)
      
      #2. Manually adjust for headcoil, TBV, TBV2, and QC MID02 
      data_long_QC_dropped$time_bin <- as.numeric(data_long_QC_dropped$time_bin)
      data_long_QC_dropped$CSF_adjusted <- data_long_QC_dropped$CSF_cm - ((2.83314*(data_long_QC_dropped$time_bin - 0)) + (0.43070*(data_long_QC_dropped$QC_Maddy_MID02 - mean(data_long_QC_dropped$QC_Maddy_MID02))) + (0.00355*(data_long_QC_dropped$TBV_mc - mean(data_long_QC_dropped$TBV_mc))) + (0.00010*(data_long_QC_dropped$TBV_squared_mc - mean(data_long_QC_dropped$TBV_squared_mc))))
      #mean shifted check
      mean(data_long_QC_dropped$CSF_adjusted)
      mean(data_long_QC_dropped$CSF_cm)
      
      #3. Plot manually adjusted CSF values (sanity check)
      Palette <- c("#0072B2", "#E69F00")
      ggplot(data_long_QC_dropped, aes(x=Age, y=CSF_adjusted, color=AutismControl))+
        geom_line(aes(group=Participant))+
        labs(x = "Age (Years)", y = 'Adjusted Extra-axial CSF Volume ('~cm^3*')')+
        labs(fill = " ")+
        labs(color = " ")+
        scale_colour_manual(values=Palette, labels = c("Autism", "Control"))+
        scale_fill_manual(values=Palette, labels = c("Autism", "Control"))+
        geom_point(aes(fill=AutismControl), colour="black", pch=21, cex=1)+
        geom_smooth(method=loess, aes(fill=AutismControl), se=TRUE) +
        scale_y_continuous(limits=c(0,150))+
        scale_x_continuous(limits=c(0,43), expand = c(0,0))+
        guides(color=guide_legend(override.aes=list(fill=NA)))+
        theme(panel.grid.major = element_line(color = "black"))+
        theme(axis.title = element_text(colour = "black", size=9), axis.text.y =element_text(colour = "black", angle = 90, hjust = 0.6), 
              axis.text.x =element_text(colour = "black"), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
              legend.position = c(.9, .2), legend.title=element_text(colour = "black", size = 9), legend.text=element_text(colour = "black", size = 8), 
              legend.background = element_rect(fill="white", size=0.5) , axis.line = element_line(colour = "black", size = 1, linetype = "solid"), 
              axis.ticks = element_line(colour = "black", size =1, linetype ="solid"), panel.border = element_blank(), panel.background = element_blank())
      
      #4. Re-run model based on headcoil-adjusted values
      fitC2 <- lme(CSF_adjusted~AutismControl +QC_Maddy_MID02 +TBV_mc + TBV_squared_mc + time_bin+ age_mc + age_squared_mc +age_cubed_mc + age_cubed_mc*AutismControl + AutismControl*age_mc + age_squared_mc*AutismControl, 
                   random=~1+age_mc|SUBJID,data= data_long_QC_dropped, method="REML")
      summary(fitC2)
      
      #5. Plot head-coil adjusted values and model fit lines
      ggplot(data_long_QC_dropped, aes(x=Age, y=CSF_adjusted, color=AutismControl))+
        geom_line(aes(group=Participant))+
        labs(x = "Age (Years)", y = 'Adjusted Extra-axial CSF Volume ('~cm^3*')')+
        labs(fill = " ")+
        labs(color = " ")+
        scale_colour_manual(values=Palette, labels = c("Autism", "Control"))+
        scale_fill_manual(values=Palette, labels = c("Autism", "Control"))+
        geom_point(aes(fill=AutismControl), colour="black", pch=21, cex=1)+
        geom_line(data=data_long_QC_dropped, aes(y=predict(fitC2,level=0), group=AutismControl, colour=AutismControl), size = 1.5)+
        scale_y_continuous(limits=c(0,150))+
        scale_x_continuous(limits=c(0,43), expand = c(0,0))+
        guides(color=guide_legend(override.aes=list(fill=NA)))+
        theme_bw()+
        theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
        theme(axis.title = element_text(colour = "black", size=9), axis.text.y =element_text(colour = "black", angle = 90, hjust = 0.6), 
              axis.text.x =element_text(colour = "black"), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
              legend.position = c(.9, .2), legend.title=element_text(colour = "black", size = 9), legend.text=element_text(colour = "black", size = 9), 
              legend.background = element_rect(fill="white", size=0.5) , axis.line = element_line(colour = "black", size = 1, linetype = "solid"), 
              axis.ticks = element_line(colour = "black", size =1, linetype ="solid"), panel.border = element_blank(), panel.background = element_blank())
    
      
      #Using loess smoother
      ggplot(data_long_QC_dropped, aes(x=Age, y=CSF_adjusted, color=AutismControl))+
        geom_line(aes(group=Participant))+
        labs(x = "Age (Years)", y = 'Adjusted Extra-axial CSF Volume ('~cm^3*')')+
        labs(fill = " ")+
        labs(color = " ")+
        scale_colour_manual(values=Palette, labels = c("Autism", "Control"))+
        scale_fill_manual(values=Palette, labels = c("Autism", "Control"))+
        geom_point(aes(fill=AutismControl), colour="black", pch=21, cex=1)+
        geom_smooth(method=loess, aes(fill=AutismControl), se=TRUE) +
        scale_y_continuous(limits=c(0,150))+
        scale_x_continuous(limits=c(0,43), expand = c(0,0))+
        guides(color=guide_legend(override.aes=list(fill=NA)))+
        theme_bw()+
        theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
        theme(axis.title = element_text(colour = "black", size=9), axis.text.y =element_text(colour = "black", angle = 90, hjust = 0.6), 
              axis.text.x =element_text(colour = "black"), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
              legend.position = c(.9, .2), legend.title=element_text(colour = "black", size = 9), legend.text=element_text(colour = "black", size = 9), 
              legend.background = element_rect(fill="white", size=0.5) , axis.line = element_line(colour = "black", size = 1, linetype = "solid"), 
              axis.ticks = element_line(colour = "black", size =1, linetype ="solid"), panel.border = element_blank(), panel.background = element_blank())
      #ggsave(filename = paste("Figure3_model_adjusted.pdf"), width = 3.4, height = 3.4,
      #       path = "C:/Users/maddy/Box/Autism_CSF/figures/pdf_format_210713", dpi = 300)
      
      
      
      
      # CT Analysis
      #Setup
      rh_aparc <- read.csv("C:/Users/maddy/Box/Autism_CSF/data/freesurfer_cross/aparc_RH_stats_concatenated_long.csv")
      lh_aparc <- read.csv("C:/Users/maddy/Box/Autism_CSF/data/freesurfer_cross/aparc_LH_stats_concatenated_long.csv")
      
      rh_aparc <- rh_aparc[,c("SUBJID", "timepoint", "rh_MeanThickness_thickness")]
      lh_aparc <- lh_aparc[,c("SUBJID", "timepoint", "lh_MeanThickness_thickness")]
      rh_aparc$SUBJID <- as.factor(rh_aparc$SUBJID)
      rh_aparc$timepoint <- as.factor(rh_aparc$timepoint)
      lh_aparc$SUBJID <- as.factor(lh_aparc$SUBJID)
      lh_aparc$timepoint <- as.factor(lh_aparc$timepoint)
      data_long_QC_dropped$SUBJID <- as.factor(data_long_QC_dropped$SUBJID)
      data_long_QC_dropped$timepoint <- as.factor(data_long_QC_dropped$timepoint)
      
      #Remove spaces before and after timepoint var and SUBJID var
      trim <- function (x) gsub("^\\s+|\\s+$", "", x)
      lh_aparc$SUBJID <- trim(lh_aparc$SUBJID)
      rh_aparc$SUBJID <- trim(rh_aparc$SUBJID)
      lh_aparc$timepoint <- trim(lh_aparc$timepoint)
      rh_aparc$timepoint <- trim(rh_aparc$timepoint)
      lh_aparc <- distinct(lh_aparc)
      rh_aparc <- distinct(rh_aparc)
      
      #Merge
      data_aparc <- merge(rh_aparc, lh_aparc, by=c("SUBJID", "timepoint"), all=FALSE)
      data_long_QC_aparc <- merge(data_aparc, data_long_QC_dropped, by=c("SUBJID", "timepoint"), all=FALSE)
      
      # Variables
      data_long_QC_aparc$totalmean_CT <- (data_long_QC_aparc$rh_MeanThickness_thickness + data_long_QC_aparc$lh_MeanThickness_thickness)/2
      data_long_QC_aparc2 <- subset(data_long_QC_aparc, age_mc!="NA")
      data_long_QC_aparc2 <- subset(data_long_QC_aparc2, totalmean_CT!="NA")
      
      # 1. Model 
      fitC.1 <- lme(totalmean_CT~time_bin+QC_Maddy_MID02+AutismControl+TBV_mc+TBV_squared_mc, 
                    random=~1|SUBJID,data= data_long_QC_aparc2, method="REML")
      summary(fitC.1)
      
      # 2. Adjust CT for headcoil
      data_long_QC_aparc2$time_bin <- as.numeric(data_long_QC_aparc2$time_bin)
      data_long_QC_aparc2$CT_adjusted <- data_long_QC_aparc2$totalmean_CT - ((-0.1348748*(data_long_QC_aparc2$time_bin - 0)) + (-0.0359074*(data_long_QC_aparc2$QC_Maddy_MID02 - mean(data_long_QC_dropped$QC_Maddy_MID02))) + (0.0003482*(data_long_QC_dropped$TBV_mc - mean(data_long_QC_dropped$TBV_mc))) + (-0.0000001*(data_long_QC_dropped$TBV_squared_mc - mean(data_long_QC_dropped$TBV_squared_mc))))
      #mean shifted check
      mean(data_long_QC_aparc2$CT_adjusted)
      mean(data_long_QC_aparc2$totalmean_CT)
      # Adjust EA-CSF for heacoil, qc
      fitC.2 <- lme(CSF_cm~CT_adjusted + QC_Maddy_MID02 + time_bin + AutismControl + TBV_mc + TBV_squared_mc, 
                    random=~1|SUBJID,data= data_long_QC_aparc2, method="REML")
      summary(fitC.2)
      data_long_QC_aparc2$time_bin <- as.numeric(data_long_QC_aparc2$time_bin)
      data_long_QC_aparc2$CSF_adjusted <- data_long_QC_aparc2$CSF_cm - ((21.92174*(data_long_QC_aparc2$time_bin - 0)) + (5.64254*(data_long_QC_aparc2$QC_Maddy_MID02 - mean(data_long_QC_dropped$QC_Maddy_MID02))) + (0.03056*(data_long_QC_dropped$TBV_mc - mean(data_long_QC_dropped$TBV_mc))) + (0.00011*(data_long_QC_dropped$TBV_squared_mc - mean(data_long_QC_dropped$TBV_squared_mc))))
      
      #mean shifted check
      mean(data_long_QC_aparc2$CSF_adjusted)
      mean(data_long_QC_aparc2$CSF_cm)
      
      # 3. Model with adjusted CT values
      fitC.3 <- lme(CSF_adjusted~CT_adjusted + QC_Maddy_MID02 + time_bin + AutismControl + TBV_mc + TBV_squared_mc, 
                    random=~1|SUBJID,data= data_long_QC_aparc2, method="REML")
      summary(fitC.3)
      
      # 4. Plots using adjusted CT values
      #Plot Total Mean Cortical Thickness by Age
      Palette <- c("#0072B2", "#E69F00")
      ggplot(data_long_QC_aparc2, aes(x=Age, y=CT_adjusted, color=AutismControl))+
        geom_line(aes(group=Participant))+
        labs(x = "Age (Years)", y = 'Adjusted Total Mean Cortical Thickness (mm)')+
        labs(fill = " ")+
        labs(color = " ")+
        scale_colour_manual(values=Palette, labels = c("Autism", "Control"))+
        scale_fill_manual(values=Palette, labels = c("Autism", "Control"))+
        geom_point(aes(fill=AutismControl), colour="black", pch=21, cex=1)+
        geom_smooth(method=loess, aes(fill=AutismControl), se=TRUE) +
        scale_y_continuous(limits=c(2.25,3.25))+
        scale_x_continuous(limits=c(0,43), expand = c(0,0))+
        guides(color=guide_legend(override.aes=list(fill=NA)))+
        theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
        theme(axis.title = element_text(colour = "black", size=9), axis.text.y =element_text(colour = "black", angle = 90, hjust = 0.6), 
              axis.text.x =element_text(colour = "black"), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
              legend.position = c(.9, .9), legend.title=element_text(colour = "black", size = 9), legend.text=element_text(colour = "black", size = 8), 
              legend.background = element_rect(fill="white", size=0.4) , axis.line = element_line(colour = "black", size = 1, linetype = "solid"), 
              axis.ticks = element_line(colour = "black", size =1, linetype ="solid"), panel.border = element_blank(), panel.background = element_blank())
      
      #ggsave(filename = paste("Figure4.A_CT_age_210713.pdf"), width = 3.4, height = 3.4,
      #  path = "C:/Users/maddy/Box/Autism_CSF/figures/pdf_format_210713", dpi = 300)
      
      
      #Plot Total Mean Cortical Thickness by EA-CSF
      ggplot(data_long_QC_aparc2, aes(x=CT_adjusted, y=CSF_adjusted, color=AutismControl))+
        geom_line(aes(group=Participant))+
        labs(x = 'Adjusted Total Mean Cortical Thickness (mm)', y = 'Adjusted Extra-axial CSF ('~cm^3*')')+
        labs(fill = " ")+
        labs(color = " ")+
        scale_colour_manual(values=Palette, labels = c("Autism", "Control"))+
        scale_fill_manual(values=Palette, labels = c("Autism", "Control"))+
        geom_point(aes(fill=AutismControl), colour="black", pch=21, cex=1)+
        geom_smooth(method=loess, aes(fill=AutismControl), se=TRUE) +
        scale_y_continuous(limits=c(0,140))+
        scale_x_continuous(limits=c(2.25,3.25), expand = c(0,0))+
        guides(color=guide_legend(override.aes=list(fill=NA)))+
        theme_bw()+
        theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
        theme(axis.title = element_text(colour = "black", size=9), axis.text.y =element_text(colour = "black", angle = 90, hjust = 0.6), 
              axis.text.x =element_text(colour = "black"), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
              legend.position = c(.9, .9), legend.title=element_text(colour = "black", size = 9), legend.text=element_text(colour = "black", size = 8), 
              legend.background = element_rect(fill="white", size=0.4) , axis.line = element_line(colour = "black", size = 1, linetype = "solid"), 
              axis.ticks = element_line(colour = "black", size =1, linetype ="solid"), panel.border = element_blank(), panel.background = element_blank())
      
      #ggsave(filename = paste("Figure4.B_CT_210713.pdf"), width = 3.4, height = 3.4,
      #   path = "C:/Users/maddy/Box/Autism_CSF/figures/pdf_format_210713", dpi = 300)
      
      
      
      
      
    #Exploratory Analyses
      
      #Sub-analysis of age-matched groups 
      #make autismcontrol 0/1 values
      data_long_QC_dropped$Dx_bin <- ifelse(data_long_QC_dropped$AutismControl == "autism", 0, 1)
      m.out=matchit(Dx_bin~age_mc, method="nearest", data=data_long_QC_dropped, ratio=1)
      summary(m.out)
      #verify good match with qq-plots
      plot(m.out, type = "qq", interactive = FALSE, which.xs = c("age_mc"))
      #extract matched dataset
      m.data1 <- match.data(m.out, drop.unmatched = TRUE)
      
      #run multilevel model (N = 366 scans)
      #REML
      fitmatch2 = lme(CSF_cm~AutismControl + age_mc + QC_Maddy_MID02 +age_cubed_mc + age_cubed_mc*AutismControl + TBV_mc +TBV_squared_mc+ time_bin+ age_squared_mc + AutismControl*age_mc + age_squared_mc*AutismControl, 
                      random=~1+age_mc|SUBJID,data= m.data1, method="REML")
      summary(fitmatch2)
      
      unique(m.data1$SUBJID)
      
      
      
      #Sub-analysis of FIQ-matched groups 
      #make autismcontrol 0/1 values
      data_long_QC_dropped$Dx_bin <- ifelse(data_long_QC_dropped$AutismControl == "autism", 0, 1)
      #only participants with FIQ score included
      data_long_QC_dropped$FIQ <- as.numeric(data_long_QC_dropped$FIQ)
      data_long_QC_dropped_FIQ<-subset(data_long_QC_dropped, FIQ!="NA") 
      data_long_QC_dropped_FIQ<-subset(data_long_QC_dropped_FIQ, FIQ!="NaN")
      m.out2=matchit(Dx_bin~FIQ, method="nearest", data=data_long_QC_dropped_FIQ, ratio=1)
      summary(m.out2)
      #verify good match with qq-plots
      plot(m.out2, type = "qq", interactive = FALSE, which.xs = c("FIQ"))
      #extract matched dataset
      m.data2 <- match.data(m.out2, drop.unmatched = TRUE)
      
      #run multilevel model (N = 218 scans)
      fitmatch2 = lme(CSF_cm~AutismControl + age_mc +TBV_mc +TBV_squared_mc+ QC_Maddy_MID02 + time_bin + age_squared_mc +age_cubed_mc + age_cubed_mc*AutismControl + AutismControl*age_mc + age_squared_mc*AutismControl, 
                      random=~1+age_mc|SUBJID,data= m.data2, method="REML")
      summary(fitmatch2)
      
      table(m.data2$AutismControl)
      unique(m.data2$SUBJID)
      table(m.data2$AutismControl, unique(m.data2$SUBJID))
      
      
      
      #FIQ long as a predictor of EA-CSF 
      data_long_QC_dropped$FIQ <- as.numeric(data_long_QC_dropped$FIQ)
      data_long_QC_dropped_FIQ<-subset(data_long_QC_dropped, FIQ!="NA") 
      data_long_QC_dropped_FIQ<-subset(data_long_QC_dropped_FIQ, FIQ!="NaN")
      data_long_QC_dropped_FIQ<-subset(data_long_QC_dropped_FIQ, AutismControl!="Na")
      data_long_QC_dropped_FIQ<-subset(data_long_QC_dropped_FIQ, sex!="2")
      #Multilevel model
      fitFIQ2 = lme(CSF_cm~FIQ + AutismControl + QC_Maddy_MID02 + TBV_mc + age_mc +TBV_squared_mc+ time_bin + age_squared_mc +age_cubed_mc + age_cubed_mc*AutismControl + AutismControl*age_mc + age_squared_mc*AutismControl, 
                    random=~1+age_mc|SUBJID,data=data_long_QC_dropped_FIQ, method="REML")
      summary(fitFIQ2)
      
      table(data_long_QC_dropped_FIQ$AutismControl)
      
      
      #ADOS predictor of EA-CSF sub-analysis
      #Subset to only include participants with an ADOS CSS score
      data_long_QC_dropped_ADOS<-subset(data_long_QC_dropped, ADOS_comb!="NA")
      
      #Multilevel model (N=213 scans, 69 individuals)
      fitADOS = lme(CSF_cm~ADOS_comb + age_mc + TBV_mc + TBV_squared_mc + QC_Maddy_MID02 + time_bin + age_squared_mc + age_cubed_mc, 
                    random=~1+age_mc|SUBJID,data= data_long_QC_dropped_ADOS, method="REML")
      summary(fitADOS) 
      
      
      #Only use subjects from times 2-5 sub-analysis
      #Subset time_bin = 1
      data_long_QC_dropped_bin1 <- subset(data_long_QC_dropped, time_bin==1)
      #MLM
      fit_bin1 = lme(CSF_cm~AutismControl + age_mc + TBV_mc +TBV_squared_mc+ QC_Maddy_MID02 + age_squared_mc +age_cubed_mc + age_cubed_mc*AutismControl + AutismControl*age_mc + age_squared_mc*AutismControl, 
                     random=~1+age_mc|SUBJID,data= data_long_QC_dropped_bin1, method="REML")
      summary(fit_bin1)
      
    
       