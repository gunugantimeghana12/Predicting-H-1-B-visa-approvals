library(plotly)
library(dplyr)
library(tidyr)
library(corrplot)
library(tidyverse)
library(lubridate)
library(ggplot2)
require(scales)

data<- read.csv("C:/Users/gunaganti meghana/Desktop/GMU/COURSES/Spring 2020 OR 568/Assignments/FINAL PROJECT/H-1B_Disclosure_Data_18.csv")

data = subset(data, select = -c(NAICS_CODE))
PREVAILING_WAGE <- as.numeric(gsub(",", "",as.character(data$PREVAILING_WAGE)))
data$PREVAILING_WAGE = ifelse(data$PW_UNIT_OF_PAY=='Bi-Weekly',data$PREVAILING_WAGE <- (PREVAILING_WAGE/2)*52.143,
                               ifelse(data$PW_UNIT_OF_PAY=='Hour',data$PREVAILING_WAGE <- (PREVAILING_WAGE*40)*52.143,
                                      ifelse(data$PW_UNIT_OF_PAY=='Month',data$PREVAILING_WAGE <- PREVAILING_WAGE*12,
                                             ifelse(data$PW_UNIT_OF_PAY=='Week',data$PREVAILING_WAGE <- PREVAILING_WAGE*52.143,
                                                    ifelse(data$PW_UNIT_OF_PAY=='Year',data$PREVAILING_WAGE <- PREVAILING_WAGE,NA
                                                    )))))
data<- data%>%
  filter(VISA_CLASS == 'H-1B')
as.data.frame(data)
head(data)
colnames(data)

#Retrieving Numeric Data

NumericData<-select_if(data, is.numeric)
as.data.frame(NumericData)
drop_na(NumericData)
NumericData[!(is.na(NumericData=="")),] 

#Correlation Plot

ND_CorrelationPlot <- cor(NumericData)
corrplot(ND_CorrelationPlot, method = "circle")

#Group by Case Status and sum of each case status
CASE_STATUS_GROUP <- group_by(data,CASE_STATUS)
CASE_STATUS_GROUP
CASE_STATUS_GROUP_Summary<- data.frame(table(CASE_STATUS_GROUP$CASE_STATUS))
as.data.frame(CASE_STATUS_GROUP_Summary)
#Plot Count of CaseStatus
COUNT_CASE_STATUS_PLOT<-ggplot(CASE_STATUS_GROUP_Summary, aes(Var1, Freq))+ geom_bar(stat='identity') + scale_y_continuous(name="Count", labels = comma)
COUNT_CASE_STATUS_PLOT

COUNT_CASE_STATUS_PLOT_O<-ggplot(CASE_STATUS_GROUP_Summary, aes(x=reorder(Var1, -Freq),y=Freq))+ geom_bar(stat='identity',fill="#FF9999", colour="black") + scale_y_continuous(name="Count", labels = comma)+xlab("Case Status") +
  ggtitle("Count of Case Status")
COUNT_CASE_STATUS_PLOT_O

#Groupby Jobtitle, Prevailing Wage with average of it.
JT_PW_Group <- group_by(data,PREVAILING_WAGE,JOB_TITLE)
JT_PW_Group
as.data.frame(JT_PW_Group)
JT_PW_Group_Summary<-summarize(JT_PW_Group,Average_Prevailing_Wage= mean(as.numeric(PREVAILING_WAGE),na.rm = TRUE))
is.null(JT_PW_Group_Summary)
as.data.frame(JT_PW_Group_Summary)
JT_PW_Group_Summary_Desc<-JT_PW_Group_Summary[order(JT_PW_Group_Summary$Average_Prevailing_Wage, rev(JT_PW_Group_Summary$JOB_TITLE), decreasing = TRUE), ]
JT_PW_Group_Summary_Top20<-head(JT_PW_Group_Summary_Desc,20)
as.data.frame(JT_PW_Group_Summary_Top20)

#Plot Top 20 Job Title with their Average Prevailing Wage

Top20_JT_PW_plot<-ggplot(JT_PW_Group_Summary_Top20, aes(x=JOB_TITLE,y = PREVAILING_WAGE)) +geom_bar(stat = "identity") + scale_y_continuous(name="Top 20 Average Prevailing Wages of Job Title", labels = comma)+theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))+coord_flip()
Top20_JT_PW_plot

Top20_JT_PW_plot_O<-ggplot(JT_PW_Group_Summary_Top20, aes(x=reorder(JOB_TITLE,-PREVAILING_WAGE),y=PREVAILING_WAGE)) +geom_bar(stat = "identity",fill="#E69F00", colour="black") + scale_y_continuous(name="Prevailing Wage", labels = comma)+theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))+coord_flip() +
  xlab("Job Title") +
  ggtitle("Top 20 Average Prevailing Wages of Job Title")
Top20_JT_PW_plot_O
#Groupby Case Status, Employer Name and sum by its each case status group .
EN_PW_Group <- group_by(data,EMPLOYER_NAME)
EN_PW_Group

EN_PW_Group_Summary <- data.frame(table(EN_PW_Group$EMPLOYER_NAME))
EN_PW_Group_Summary_Desc <- EN_PW_Group_Summary[order(EN_PW_Group_Summary $Freq, rev(EN_PW_Group_Summary $Var1),decreasing = TRUE),]
EN_PW_Group_Summary_Top20 <- head(EN_PW_Group_Summary_Desc,20)
as.data.frame(EN_PW_Group_Summary_Top20)

#Plot Top 20 Employers 
Top20_EN<-ggplot(EN_PW_Group_Summary_Top20, aes(Var1,Freq))+ geom_bar(stat='identity') + scale_y_continuous(name="Top 20 Employer Names", labels = comma)+theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))+coord_flip()
Top20_EN

Top20_EN_O<-ggplot(EN_PW_Group_Summary_Top20, aes(x=reorder(Var1,-Freq),y=Freq))+ geom_bar(stat='identity',fill="#D55E00", colour="black") + scale_y_continuous(name="Count", labels = comma)+theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))+coord_flip()+ xlab("Employer Name") + 
  ggtitle("Top 20 Employer Names")
Top20_EN_O


#Plot Top 20 SOC Names with their Average Prevailing Wage
SN_PW_Group <- group_by(data,SOC_NAME,PREVAILING_WAGE)
SN_PW_Group
SN_PW_Group_Summary<-summarize(SN_PW_Group,Average_PrevailingWage = mean(as.numeric(PREVAILING_WAGE),na.rm = TRUE))
as.data.frame(SN_PW_Group_Summary)
SN_PW_Group_Summary_Desc<-SN_PW_Group_Summary[order(SN_PW_Group_Summary$Average_PrevailingWage, rev(SN_PW_Group_Summary$SOC_NAME), decreasing = TRUE), ]
SN_PW_Group_Summary_Top20<-head(SN_PW_Group_Summary_Desc,20)
as.data.frame(SN_PW_Group_Summary_Top20)

#Plot Top 20 SOC Names with their Average Prevailing Wage
Top20_SN<-ggplot(SN_PW_Group_Summary_Top20, aes(SOC_NAME,Average_PrevailingWage ))+ geom_bar(stat='identity') + scale_y_continuous(name="Top 20 Average Prevailing Wages of Soc Name", labels = comma)+theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))+coord_flip()
Top20_SN

Top20_SN_O<-ggplot(SN_PW_Group_Summary_Top20)+ geom_bar(aes(x=reorder(SOC_NAME,Average_PrevailingWage),y=Average_PrevailingWage ),stat='identity',fill="#0072B2", colour="black") +theme_minimal()+ scale_y_continuous(name="Prevailing Wage", labels = comma)+theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))+coord_flip()+xlab(" Soc Names") +
  ggtitle("Top 20 Average Prevailing Wages of Soc Name")
Top20_SN_O

#Filtering only CERTIFIED and DENIED Case Status
data_boxplot <- data%>%
  filter(CASE_STATUS == 'CERTIFIED' | CASE_STATUS == 'DENIED')

# plot boxplot 
ggplot(aes(y = PREVAILING_WAGE, x = CASE_STATUS, fill = CASE_STATUS,
           notch = TRUE, notchwidth = .3), 
       data = data_boxplot) + 
  geom_boxplot(notch = TRUE) + 
  scale_fill_manual(values = c("#29a329", "#ea4b1f"))+
  scale_y_continuous(limits = c(0, 150000), 
                     breaks = seq(0, 150000, 5000)) + 
  ggtitle("Wages for certified & denied H1B cases")+
  theme(
    plot.title = element_text(size = rel(2)),
    panel.background = element_rect(fill = 'light gray'),
    panel.grid.major = element_line(colour = '#f0f0f0'),
    panel.grid.major.x = element_line(linetype = 'blank'),
    panel.grid.minor = element_line(linetype = 'blank')
  )


  
  
  