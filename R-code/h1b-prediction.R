
library(dplyr)
library(ggplot2)
library(readxl)
library(hashmap)
library(data.table)
library(dplyr)
library(ggplot2)
library(readxl)
library(hashmap)
library(data.table)
library(xtable)
library(treemap)
library(d3Tree)
library(plotly)
library(gridExtra)
library(purrr)
library(mice)
library(caret)
library(mlbench)
library(VIM)
library(Boruta)
library(reshape)
library(glmnet)
library(ModelMetrics)
library(data.table)
library(dummies)
library(randomForest)
library(gam)
library(rpart)
library(earth)
library(mda)
library(bartMachine)
library("rJava")
library("e1071")
library(stringr)
library(dplyr)
library(ggplot2)
library(readxl)
library(hashmap)
library(data.table)
library(xtable)
library(treemap)
library(d3Tree)
library(plotly)
library(gridExtra)
library(sm)
library(ggmap)
library(leaflet)
library(UBL)
library(ROSE)
library(DMwR)
library(caret)
library(xgboost)
library(class)
library(FNN)
library(glmnet)
library(heuristica)
library(doParallel)
library(rpart.plot)
library(gbm)
library(plotmo)
library(dplyr)
library(ggplot2)
library(readxl)
library(hashmap)
library(data.table)
library(xtable)
library(treemap)
library(d3Tree)
library(plotly)
library(gridExtra)
library(sm)
library(ggmap)
library(leaflet)
library(magrittr)


data2015=read.csv("H-1B_Disclosure_Data_FY15.csv",stringsAsFactors = FALSE)
data2015$YEAR<-2015
data2016=read.csv("H-1B_Disclosure_Data_FY16.csv",stringsAsFactors = FALSE)
data2016$YEAR<-2016
data2017=read.csv("H-1B_Disclosure_Data_FY17.csv",stringsAsFactors = FALSE)
data2017$YEAR<-2017
data2018=read.csv("H-1B_Disclosure_Data_FY18.csv",stringsAsFactors = FALSE)
data2018$YEAR<-2018

nrow(data2015)
nrow(data2016)
nrow(data2017)
nrow(data2018)

library(base)

data2017$NAIC_CODE=data2017$NAICS_CODE
data2018$NAIC_CODE=data2018$NAICS_CODE
data2016$WAGE_RATE_OF_PAY=data2016$WAGE_RATE_OF_PAY_FROM
data2017$WAGE_RATE_OF_PAY=data2017$WAGE_RATE_OF_PAY_FROM
data2018$WAGE_RATE_OF_PAY=data2018$WAGE_RATE_OF_PAY_FROM


cols_1=intersect(colnames(data2017),colnames(data2016))
cols_1
cols_2=intersect(colnames(data2017),colnames(data2015))
cols_2


cols=intersect(cols_1,cols_2)


data2015=data2015[,cols,drop=FALSE]
data2016=data2016[,cols,drop=FALSE]
data2017=data2017[,cols,drop=FALSE]
data2018=data2018[,cols,drop=FALSE]

h1b_data=rbind(data2015,data2016,data2017,data2018)
names(h1b_data)
unique(h1b_data$WAGE_UNIT_OF_PAY)
h1b_data[1,c("WAGE_UNIT_OF_PAY")]


#Assign h1b data to check_h1b data
check_h1b<-h1b_data


#Null report

nullrep.gen=function(data)
{
  data=data.frame(data)
  names_summary=names(data)
  ischar=unlist(sapply(names_summary,function(n) typeof(data[,n]))=="character")
  nullreport=data.frame(colnames=names_summary,null=sapply(names_summary,function(n) sum(is.na(data[,n])) + 
                                                             ifelse(ischar[n],sum(nchar(trimws(data[,n][!is.na(data[,n])]))==0),0)),
                        nrows=sapply(names_summary,function(n) length(data[,n])))
  nullreport$percent_of_complete_values=((nullreport$nrows-nullreport$null)/nullreport$nrows)*100
  data.table(nullreport)
}

nullrep.gen(check_h1b)


#Removing comma and dots 
check_h1b$PREVAILING_WAGE<-sub(",","",check_h1b$PREVAILING_WAGE)
check_h1b$PREVAILING_WAGE<-sub(",","",check_h1b$PREVAILING_WAGE)
check_h1b$PREVAILING_WAGE<-gsub("*\\..*","",check_h1b$PREVAILING_WAGE)
check_h1b$PREVAILING_WAGE<-gsub(" ..*", '', check_h1b$PREVAILING_WAGE)
table(complete.cases(check_h1b$PREVAILING_WAGE))
check_h1b$total_wage<-check_h1b$PREVAILING_WAGE
#Removing Unit rows with no value
unique(check_h1b$PW_UNIT_OF_PAY)
check_h1b<-check_h1b[-which(check_h1b$PW_UNIT_OF_PAY==""),]
table(complete.cases(check_h1b$PREVAILING_WAGE))
#Converting to Numeric for calculation total wages
check_h1b$PREVAILING_WAGE<-as.numeric(check_h1b$PREVAILING_WAGE)
#Removing NA rows
check_h1b<-check_h1b[-which(!complete.cases(check_h1b$PREVAILING_WAGE)),]
table(complete.cases(check_h1b$PREVAILING_WAGE))

sapply(check_h1b,function(x) sum(is.na(x)))

check_h1b<-check_h1b[,-c(13,15)]
table(complete.cases(check_h1b[check_h1b$YEAR==2016,]))
#check_h1b<-check_h1b[complete.cases(check_h1b),]
table(complete.cases(check_h1b))

names(check_h1b)


###Data wrangling


#Converting wrongly printed PW_Unit of pay from hour to year

a=check_h1b[((check_h1b$PW_UNIT_OF_PAY =="Month") ),c("PREVAILING_WAGE")]

median(unlist(a))
mean(unlist(a))


check_h1b[((check_h1b$PW_UNIT_OF_PAY =="Hour") & (check_h1b$WAGE_UNIT_OF_PAY=="Year")& (check_h1b$PREVAILING_WAGE>=9000)  ),c("PW_UNIT_OF_PAY")]<-"Year"



#Percentage of wage distributions summary

check_h1b%>%group_by(WAGE_UNIT_OF_PAY)%>%summarise(count=n(),percentage.total.data=round(((count/nrow(check_h1b))*100),digits = 2))


#Converting all wages to yearly scale 
check_h1b[check_h1b$PW_UNIT_OF_PAY=="Hour",c("total_wage")]
check_h1b[check_h1b$PW_UNIT_OF_PAY=="Hour",c("total_wage")]<-check_h1b[check_h1b$PW_UNIT_OF_PAY=="Hour",c("PREVAILING_WAGE")]*40*4*12
check_h1b[check_h1b$PW_UNIT_OF_PAY=="Month",c("total_wage")]<-check_h1b[check_h1b$PW_UNIT_OF_PAY=="Month",c("PREVAILING_WAGE")]*12
check_h1b[check_h1b$PW_UNIT_OF_PAY=="Week",c("total_wage")]<-check_h1b[check_h1b$PW_UNIT_OF_PAY=="Week",c("PREVAILING_WAGE")]*4*12
check_h1b[check_h1b$PW_UNIT_OF_PAY=="Bi-Weekly",c("total_wage")]<-check_h1b[check_h1b$PW_UNIT_OF_PAY=="Bi-Weekly",c("PREVAILING_WAGE")]*2*12
names(check_h1b)


check_h1b[check_h1b$PW_UNIT_OF_PAY=="Hour",]
check_h1b[check_h1b$PW_UNIT_OF_PAY=="Month",]
check_h1b[check_h1b$PW_UNIT_OF_PAY=="Week",]
check_h1b[check_h1b$PW_UNIT_OF_PAY=="Bi-Weekly",]


#COnverting total wage to numeric
check_h1b$total_wage<-as.numeric(check_h1b$total_wage)



#Full Time position 

Ft_2015=check_h1b[check_h1b$YEAR==2015,c("total_wage","FULL_TIME_POSITION")]
Ft_2017=check_h1b[check_h1b$YEAR==2017,c("total_wage","FULL_TIME_POSITION")]
Ft_2018=check_h1b[check_h1b$YEAR==2018,c("total_wage","FULL_TIME_POSITION")]


med1=as.data.frame(Ft_2015[Ft_2015$FULL_TIME_POSITION=="Y",c("total_wage")])
colnames(med1)="med"
med2=as.data.frame(Ft_2017[Ft_2017$FULL_TIME_POSITION=="Y",c("total_wage")])
colnames(med2)="med"
med3=as.data.frame(Ft_2018[Ft_2018$FULL_TIME_POSITION=="Y",c("total_wage")])
colnames(med3)="med"

med=rbind(med1,med2,med3)

summary(med)


check_h1b[((check_h1b$total_wage>=73000) & (check_h1b$YEAR==2016)),c("FULL_TIME_POSITION")]='Y'

check_h1b[((check_h1b$total_wage<73000) & (check_h1b$YEAR==2016)),c("FULL_TIME_POSITION")]='N'

sum(is.na(check_h1b$FULL_TIME_POSITION))


#Geting the number of full time and half time positions

check_h1b%>%group_by(FULL_TIME_POSITION)%>%summarise(count=n(),percentage.total.data=round(((count/nrow(check_h1b))*100),digits = 5))

#Removing empty rows in dataframe

check_h1b=check_h1b[!apply(check_h1b == "", 1, all),]


#Trimming NAIC Code to first 4 digits

check_h1b$NAIC_CODE=strtrim(check_h1b$NAIC_CODE,4)


check_h1b$Sector_data=check_h1b$NAIC_CODE

check_h1b$Sector_data=strtrim(check_h1b$Sector_data,2)

summary(check_h1b)

options(scipen = 999)
#Converting character data to factors
check_h1b$CASE_STATUS=as.factor(check_h1b$CASE_STATUS)

check_h1b$VISA_CLASS=as.factor(check_h1b$VISA_CLASS)

#''''check_h1b$EMPLOYER_COUNTRY=as.factor(check_h1b$EMPLOYER_COUNTRY)''''

#''''check_h1b$EMPLOYER_STATE=as.factor(check_h1b$EMPLOYER_STATE)''''


check_h1b<-check_h1b[-which(check_h1b$EMPLOYER_STATE==""),]

check_h1b<-check_h1b[-which(check_h1b$EMPLOYER_COUNTRY==""),]

unique(check_h1b$EMPLOYER_STATE)
unique(check_h1b$EMPLOYER_COUNTRY)



summary(check_h1b)



##Checking NA:

str(check_h1b)

check_h1b=as.data.frame(check_h1b[complete.cases(check_h1b),])

table(complete.cases(check_h1b))

#Changing data class:

check_h1b$YEAR=as.factor(check_h1b$YEAR)
unique(check_h1b$YEAR)

check_h1b$FULL_TIME_POSITION=as.factor(check_h1b$FULL_TIME_POSITION)
unique(check_h1b$FULL_TIME_POSITION)


check_h1b$Sector_data=as.factor(check_h1b$Sector_data)
unique(check_h1b$Sector_data)


#Employer State abbreviaton
split= function(data, split = " ") {
  return(strsplit(data,split= split)[1][1])
}
check_h1b$EMPLOYER_STATE = sapply(check_h1b$EMPLOYER_STATE,split, split=",")

emp_state_abb = c("AK", "AL", "AR", "AZ", "CA", "CO", "CT", "DC", "DE", "FL", "GA",
                  "HI", "IA", "ID", "IL", "IN", "KS", "KY", "LA", "MA", "MD", "ME",
                  "MI", "MN", "MO", "MS",  "MT", "NC", "ND", "NE", "NH", "NJ", "NM",
                  "NV", "NY", "OH", "OK", "OR", "PA", "PR", "RI", "SC", "SD", "TN",
                  "TX", "UT", "VA", "VT", "WA", "WI", "WV", "WY")
emp_state_nonabb = c("Alaska","Alabama","Arkansas","Arizona","California","Colorado",
                     "Connecticut","Columbia","Delaware","Florida","Georgia",
                     "Hawai","Iowa","Idaho","Illinois","Indiana","Kansas","Kentucky",
                     "Louisiana","Massachusetts","Maryland","Maine","Michigan","Minnesota",
                     "Missouri","Mississippi","Montana","North carolina","North dakota",
                     "Nebraska","New Hampshire","New Jersey","New Mexico","Nevada",
                     "New York","Ohio","Oklahoma","Oregon","Pennsylvania","Puerto rico",
                     "Rhode Island","South Carolina","South Dakota","Tennessee","Texas",
                     "Utah","Virginia","Vermont","Washington","Wisconsin",
                     "West Virginia","Wyoming")

emp_hash_table_of_state=hashmap(emp_state_abb,emp_state_nonabb)

check_h1b$EMP_STATE_full = sapply(check_h1b$EMPLOYER_STATE, function(x,y) {return(y[[x]])}, y = emp_hash_table_of_state)


emp_site_merge <- function(x,y) {
  return(paste0(x,", ",y))
}

check_h1b %>%
  dplyr::rename(EMP_STATE_abb = EMPLOYER_STATE) -> check_h1b
check_h1b$EMP_State_and_city = mapply(emp_site_merge,check_h1b$EMPLOYER_CITY,check_h1b$EMP_STATE_full)

#Worksite State abbreviaton

check_h1b$WORKSITE_STATE = sapply(check_h1b$WORKSITE_STATE,split, split=",")

Worksite_state_abb = c("AK", "AL", "AR", "AZ", "CA", "CO", "CT", "DC", "DE", "FL", "GA",
                       "HI", "IA", "ID", "IL", "IN", "KS", "KY", "LA", "MA", "MD", "ME",
                       "MI", "MN", "MO", "MS",  "MT", "NC", "ND", "NE", "NH", "NJ", "NM",
                       "NV", "NY", "OH", "OK", "OR", "PA", "PR", "RI", "SC", "SD", "TN",
                       "TX", "UT", "VA", "VT", "WA", "WI", "WV", "WY")
Worksite_state_nonabb = c("Alaska","Alabama","Arkansas","Arizona","California","Colorado",
                          "Connecticut","Columbia","Delaware","Florida","Georgia",
                          "Hawai","Iowa","Idaho","Illinois","Indiana","Kansas","Kentucky",
                          "Louisiana","Massachusetts","Maryland","Maine","Michigan","Minnesota",
                          "Missouri","Mississippi","Montana","North carolina","North dakota",
                          "Nebraska","New Hampshire","New Jersey","New Mexico","Nevada",
                          "New York","Ohio","Oklahoma","Oregon","Pennsylvania","Puerto rico",
                          "Rhode Island","South Carolina","South Dakota","Tennessee","Texas",
                          "Utah","Virginia","Vermont","Washington","Wisconsin",
                          "West Virginia","Wyoming")

Worksite_hash_table_of_state=hashmap(Worksite_state_abb,Worksite_state_nonabb)

check_h1b$Worksite_STATE_full = sapply(check_h1b$WORKSITE_STATE, function(x,y) {return(y[[x]])}, y = Worksite_hash_table_of_state)


Worksite_site_merge <- function(x,y) {
  return(paste0(x,", ",y))
}

check_h1b %>%
  dplyr::rename(Worksite_STATE_abb = WORKSITE_STATE) -> check_h1b
check_h1b$Worksite_State_and_city = mapply(Worksite_site_merge,check_h1b$WORKSITE_CITY,check_h1b$Worksite_STATE_full)


#Saving data frame into csv

#fwrite(check_h1b, file ="H1b_EDA_data.csv")





#########################################################################################################

############################################# EDA #######################################################







my_data=read.csv("H1b_EDA_data.csv",stringsAsFactors = FALSE)
#nullrep.gen(my_data)


#remove.packages(c("sm", "ggmap"))

#install.packages('leaflet', dependencies = TRUE)
#install.packages('sm', dependencies = TRUE)
#require(devtools)
#devtools::install_github("dkahle/ggmap", ref = "tidyup")


#Checking NA's
my_data=as.data.frame(my_data[complete.cases(my_data),])

sapply(my_data,function(x) sum(is.na(x)))

#Tree map
RColorBrewer::display.brewer.all()



##Grouped based on the number of applications

my_data %>% filter(Worksite_STATE_full != '') %>% group_by(Worksite_STATE_full) %>% summarise(n = n()) %>% ungroup() -> grp_by_state
colnames(grp_by_state)=c("State","Count")


treemap(grp_by_state, 
        index=c("State"), 
        type="value",
        vSize = "Count",  
        vColor = "Count",
        palette = "Reds",  
        title="TOTAL NUMBER OF APPLICATIONS PER STATE", 
        title.legend = "Number of Applications",
        fontsize.title = 14 
)

##Grouped based on total wage


my_data %>% filter(total_wage != '') %>% filter(total_wage != 130982400) %>%  filter(total_wage != 88817280)  %>% group_by(total_wage) %>% group_by(Worksite_STATE_full)%>%filter(Worksite_STATE_full != '') %>% summarise(n = mean(total_wage)) %>% ungroup() -> grp_by_wage
colnames(grp_by_wage)=c("State","Wage")


treemap(grp_by_wage, 
        index=c("State"), 
        type="value",
        vSize = "Wage",  
        vColor = "Wage",
        palette = "RdBu",  
        title="MEAN ANNUAL WAGE DISTRIBUTION PER STATE", 
        title.legend = "Mean Annual Wage",
        fontsize.title = 14 
)

#Grouped applications based on year and state 
my_data %>% filter(Worksite_STATE_full != '') %>% filter(Worksite_STATE_full != '') %>% group_by(Worksite_STATE_full,YEAR) %>% summarise(n = n()) %>% ungroup() -> grp_by_app_and_state
colnames(grp_by_app_and_state)=c("State","Year","Count")


#2015-2018

year=c(2015,2016,2017,2018)

for (i in unlist(year)){
  
  
  s=subset(grp_by_app_and_state[,c(1,3)],grp_by_app_and_state$Year==i)
  
  
  treemap(s, 
          index=c("State"), 
          type="value",
          vSize = "Count",  
          vColor = "Count",
          palette = "Greys",  
          title=sprintf("TOTAL NUMBER OF APPLICATIONS PER STATE DURING %d",i),
          title.legend = "Mean Annual Wage",
          fontsize.title = 14 
  )
  
}


#Top 10 Employers with higest applications irrespective of Case status


my_data %>% filter(EMPLOYER_NAME!='') %>% group_by(EMPLOYER_NAME) %>% summarise(n=n())%>%arrange(desc(n))%>%head(10)->Employers_count

treemap(Employers_count, 
        index=c("EMPLOYER_NAME"), 
        type="value",
        vSize = "n",  
        vColor = "n",
        palette = "RdBu",  
        title="TOP TEM EMPLOYERS WITH MOST APPLICATIONS FILED", 
        title.legend = "Number of Applications",
        fontsize.title = 14 
)


plot=ggplot(Employers_count, aes(x=reorder(EMPLOYER_NAME,+n),y=n))+
  geom_bar(stat="identity",col="white",fill="darkorange")+ggtitle("TOP TEM EMPLOYERS WITH MOST APPLICATIONS FILED")+
  labs(x="Employer Name",y="Total No of Applications")+ geom_text(aes(x = EMPLOYER_NAME, y = 1, label = paste0("",n,"",sep="")),
                                                                  hjust=0, vjust=.5, size = 4, colour = 'black',
                                                                  fontface = 'bold')+theme_bw() +coord_flip()

#Top 10 Employers with higest applications with Case status certified


my_data %>% filter(EMPLOYER_NAME!='') %>% group_by(EMPLOYER_NAME)%>% filter(CASE_STATUS=="CERTIFIED")%>% summarise(n=n())%>%arrange(desc(n))%>%head(10)->Employers_count_certified


plot1=ggplot(Employers_count_certified, aes(x=reorder(EMPLOYER_NAME,+n),y=n))+
  geom_bar(stat="identity",col="white",fill="darkorange")+ggtitle("TOP TEM EMPLOYERS WITH MOST APPLICATIONS FILED CERTIFIED")+
  labs(x="Employer Name",y="Total No of Applications")+ geom_text(aes(x = EMPLOYER_NAME, y = 1, label = paste0("",n,"",sep="")),
                                                                  hjust=0, vjust=.5, size = 4, colour = 'black',
                                                                  fontface = 'bold')+theme_bw() +coord_flip()
grid.arrange(plot,plot1)


#Year wise increase in application

my_data %>% filter(!is.na(YEAR)) %>% group_by(YEAR) %>% summarise(nr = n()) %>% ungroup() -> year_application

ggplot(data = year_application, aes(x = YEAR, y = nr)) +  
  geom_bar(stat="identity", fill="orangered1", colour="black") +
  labs(title="YEAR WISE COMPARISION OF APPLICATIONS", x ="Year", y = "Number of applications ")+coord_flip()

###
my_data %>% filter(EMPLOYER_NAME!='') %>% group_by(EMPLOYER_NAME) %>% summarise(n=n())%>%arrange(desc(n))%>%head(10)->perc_data_1

my_data[,c("CASE_STATUS","EMPLOYER_NAME")] %>% filter(!is.na(CASE_STATUS)) %>% filter(CASE_STATUS != "WITHDRAWN") %>%filter(!is.na(EMPLOYER_NAME)) %>% filter(EMPLOYER_NAME %in% perc_data_1$EMPLOYER_NAME)->perc_data_2

perc_data_2$CASE_STATUS[perc_data_2$CASE_STATUS=="CERTIFIED-WITHDRAWN"]<-"CERTIFIED"


perc_data_2 %>% group_by(EMPLOYER_NAME,CASE_STATUS)%>% summarise(n=n())%>%arrange(desc(n))->perc_data_3

perc_data_3=merge(perc_data_3,perc_data_1,by="EMPLOYER_NAME")


perc_data_3 = mutate(perc_data_3, perc =   round((perc_data_3$n.x / perc_data_3$n.y)*100,digits=3))


perc_plot =
  ggplot() + 
  coord_flip() +
  geom_bar(aes(y = perc, x = EMPLOYER_NAME, fill = CASE_STATUS), data = perc_data_3, stat="identity")+
  geom_text(data=perc_data_3, 
            aes(x = EMPLOYER_NAME, y = perc, label = paste0(perc,"%")),colour="black", size=4) +
  theme(legend.position="bottom", legend.direction="horizontal")+
  ggtitle("CASE STATUS DECISION FOR TOP 10 EMPLOYERS")+
  labs(x="Employer Name", y="Percentage of Applications") 

perc_plot

#Occupation_field plot

my_data %>% filter(!is.na(SOC_NAME)) %>% group_by(SOC_NAME) %>% summarise(number=n()) %>% 
  top_n(n=25) %>%arrange(desc(number)) %>% ungroup() -> occupation_field

ggplot(data = occupation_field, aes(x = reorder(SOC_NAME,number), y = number)) +  
  geom_bar(stat="identity", fill="green", colour="black") +
  coord_flip() + theme_bw(base_size = 10)  +
  labs(title="TOP 25 OCCUPATIONS BASED ON NUMBER OF APPLICATIONS", x ="OOCUPATION", y = "Number of applications")


#JOB_TITLE plot

my_data %>% filter(!is.na(SOC_NAME)) %>% group_by(SOC_NAME) %>% summarise(wage=mean(total_wage)) %>% 
  top_n(n=30) %>%arrange(desc(wage)) %>% ungroup() -> job_title

job_title[5,1]="NATURAL SCIENCE MANAGER"
job_title[8,1]="PHYSICIANS AND SURGEONS, OTHERS"


job_title%>%filter(SOC_NAME!="PHYSICIANS AND SURGEONS, ALL OTHER - HOSPITALISTS")%>%
  filter(SOC_NAME!="PHYSICIANS AND SURGEONS, ALL OTHTER")%>%filter(SOC_NAME!="PHYSICIANS AND SURGEONS, OTHERS")->job_title

job_title[3,1]="FINANCIAL MANAGERS"
job_title[2,1]="PHYSICIANS AND SURGEONS"


ggplot(data = job_title[1:15,], aes(x = reorder(SOC_NAME,wage), y = wage/1000)) +  
  geom_bar(stat="identity", fill="yellow", colour="black") +
  coord_flip() + theme_bw(base_size = 10)  +
  labs(title="TOP 15 OCCUPATIONS BASED ON MEAN ANNUAL WAGE", x ="OCCUPATION", y = "Mean Annual wage(in thousands)")

#Density plot of top jobs and their wages

my_data %>% filter(!is.na(JOB_TITLE)) %>% group_by(JOB_TITLE) %>% summarise(number=n()) %>% 
  top_n(n=25) %>%arrange(desc(number)) %>% ungroup() -> job_title



my_data %>% filter(!is.na(total_wage))%>%filter(CASE_STATUS=="CERTIFIED")%>%
  filter(JOB_TITLE%in%job_title$JOB_TITLE)%>%group_by(JOB_TITLE,YEAR)%>%summarise(avg_wage=mean(total_wage))->dnplot_jobs_vs_wage

ggplot(data = dnplot_jobs_vs_wage, aes(x = YEAR, y = avg_wage, colour = JOB_TITLE)) +       
  geom_line() + geom_point() + theme_bw() +
  labs(x="Year", y="Mean annual wage", colour="Job title", 
       title="MEAN ANNUAL WAGE OF JOBS OVER THE YEARS")+theme(legend.position="right")

#Density plot of data scientists based on years

my_data  %>% 
  filter(!is.na(JOB_TITLE)) %>% filter(CASE_STATUS == "CERTIFIED") %>%
  filter(JOB_TITLE %in% c("DATA SCIENTIST","DATA ANALYST")) %>% ungroup() -> data_scientists

a = data_scientists$total_wage
b = as.factor(data_scientists$YEAR)

sm.density.compare(a,b, xlab="Mean Annual wage", ylab="Density")
title(main="MEAN ANNUAL WAGE FOR DATA SCIENTISTS & DATA ANALYSTS OVER THE YEARS")
grid()
colfill<-c(2:(2+length(levels(b)))) 
legend(x = "topright", levels(b), fill=colfill)


#Density plot of data scientists based on state

# Not including missisipi, puerto rico, rhode island , vermont because of their singularity

my_data  %>% 
  filter(!is.na(JOB_TITLE)) %>% filter(EMP_STATE_full!="")%>%filter(CASE_STATUS == "CERTIFIED") %>%
  filter(JOB_TITLE%in% c("DATA SCIENTIST","DATA ANALYST")) %>%group_by(EMP_STATE_full)%>% summarise(no=n())%>%
  top_n(n=10)%>%arrange(desc(no)) %>% ungroup() -> data_scientists_state

emp_states=data_scientists_state$EMP_STATE_full


my_data  %>% 
  filter(!is.na(JOB_TITLE)) %>% filter(EMP_STATE_full!="")%>%filter(CASE_STATUS == "CERTIFIED") %>%
  filter(JOB_TITLE%in% c("DATA SCIENTIST","DATA ANALYST")) %>%filter(EMP_STATE_full%in%emp_states) %>%group_by(EMP_STATE_full) %>% ungroup() -> data_scientists_state


a = data_scientists_state$total_wage
b = as.factor(data_scientists_state$EMP_STATE_full)


sm.density.compare(a,b, xlab="Mean Annual wage", ylab="Density")
title(main="MEAN ANNUAL WAGE FOR DATA SCIENTISTS & DATA ANALYSTS BASED ON STATES")
grid()
colfill<-c(2:(2+length(levels(b)))) 
legend(x = "right", levels(b), fill=colfill)


unique(my_data$EMPLOYER_CITY)


#Geting geo codes of employer city

#register_google(key = "AIzaSyDvz44Hy0dKX4opHPuqL-Bqpp9QTGg5cdI")

my_data%>%filter(!is.na(EMPLOYER_CITY))%>%filter(EMPLOYER_CITY!="")%>%group_by(EMPLOYER_CITY)%>%
  summarise(n=n())%>%arrange(desc(n))%>%ungroup()->emp_city_info

emp_city_top200 <- (emp_city_info$EMPLOYER_CITY)[1:200]

#emp_city_geocodes <- cbind(geocode(emp_city_top200),emp_city_top200)

emp_city_geocodes = read.csv("emp_city_top200.csv",stringsAsFactors = FALSE)


emp_city_geocodes%>%dplyr::rename(emp_city_geocodes=EMPLOYER_CITY ) -> emp_city_geocodes

colnames(emp_city_geocodes)=c("lon","lat","EMPLOYER_CITY")

################################################################## 201 to 1000 ##############################

emp_city_201_to_1000 <- (emp_city_info$EMPLOYER_CITY)[201:1000]

#emp_city_geocodes_01 <- cbind(geocode(emp_city_201_to_1000),emp_city_201_to_1000)

#fwrite(emp_city_geocodes_01, file ="emp_city_201_to_1000.csv")

emp_city_geocodes_01 = read.csv("emp_city_201_to_1000.csv",stringsAsFactors = FALSE)


emp_city_geocodes_01 %>%dplyr::rename(emp_city_201_to_1000=EMPLOYER_CITY  ) -> emp_city_geocodes_01

colnames(emp_city_geocodes_01)=c("lon","lat","EMPLOYER_CITY")



emp_city_1001_to_2500 <- (emp_city_info$EMPLOYER_CITY)[1001:2500]

#emp_city_geocodes_02 <- cbind(geocode(emp_city_1001_to_2500),emp_city_1001_to_2500)

#fwrite(emp_city_geocodes_02, file ="emp_city_1001_to_2500.csv")

emp_city_geocodes_02 = read.csv("emp_city_1001_to_2500.csv",stringsAsFactors = FALSE)


emp_city_geocodes_02 %>%
  dplyr::rename(EMPLOYER_CITY = emp_city_1001_to_2500) -> emp_city_geocodes_02

colnames(emp_city_geocodes_02)=c("lon","lat","EMPLOYER_CITY")




#######




emp_city_2501_to_4000 <- (emp_city_info$EMPLOYER_CITY)[2501:4000]

#emp_city_geocodes_03 <- cbind(geocode(emp_city_2501_to_4000),emp_city_2501_to_4000)

#fwrite(emp_city_geocodes_03, file ="emp_city_2501_to_4000.csv")

emp_city_geocodes_03 = read.csv("emp_city_2501_to_4000.csv",stringsAsFactors = FALSE)


emp_city_geocodes_03 %>%
  dplyr::rename( emp_city_2501_to_4000=EMPLOYER_CITY ) -> emp_city_geocodes_03

colnames(emp_city_geocodes_03)=c("lon","lat","EMPLOYER_CITY")


########    4001   to 6000######



emp_city_4001_to_7944 <- (emp_city_info$EMPLOYER_CITY)[4001:7944]

#emp_city_geocodes_04 <- cbind(geocode(emp_city_4001_to_7944),emp_city_4001_to_7944)

#fwrite(emp_city_geocodes_04, file ="emp_city_4001_to_7944.csv")

emp_city_geocodes_04 = read.csv("emp_city_4001_to_7944.csv",stringsAsFactors = FALSE)


emp_city_geocodes_04 %>%
  dplyr::rename(emp_city_4001_to_7944=EMPLOYER_CITY  ) -> emp_city_geocodes_04

colnames(emp_city_geocodes_04)=c("lon","lat","EMPLOYER_CITY")









#row binding 

emp_city_geocodes=rbind(emp_city_geocodes,emp_city_geocodes_01,emp_city_geocodes_02,emp_city_geocodes_03,emp_city_geocodes_04)

#fwrite(emp_city_geocodes, file ="emp_city_geocodes.csv")







#binding with data frame


my_data <- full_join(my_data,emp_city_geocodes,by= "EMPLOYER_CITY")

#Checking NA's
my_data=as.data.frame(my_data[complete.cases(my_data),])

sapply(my_data,function(x) sum(is.na(x)))



RColorBrewer::display.brewer.all()

############################################################################################



#Map of cities based on application number

my_data  %>% group_by(lat,lon) %>%
  summarise(n = length(lat)) %>% ungroup() -> map_application_no
colnames(map_application_no) <- c("lat","lon","n")
bins <- c(max(map_application_no$n),150000,100000,50000,min(map_application_no$n))
pallete <- colorBin("RdYlBu", domain = map_application_no$n, bins = bins)

leaflet(data = map_application_no) %>%
  addTiles() %>% setView(-99, 35, zoom = 4) %>%addLegend("bottomright", pal = pallete, values = ~n,
                                                         title = "TOTAL NUMBER OF APPLICATIONS",
                                                         labFormat = labelFormat(prefix = ""),
                                                         opacity = 1)%>%
  addCircleMarkers(lat=map_application_no$lat, lng=map_application_no$lon, radius=sqrt(map_application_no$n)/10, color = ~pallete(map_application_no$n), weight=1.5, opacity=0.8,
                   popup= paste("<br><strong>Applications: </strong>", map_application_no$n
                   ))




#Map of prevailing wage

my_data %>%  
  filter(CASE_STATUS == "CERTIFIED") %>% group_by(lat,lon) %>%
  summarise(avg = mean(total_wage)) %>% ungroup() -> map_pw_wage
colnames(map_pw_wage) <- c("lat","lon","n")
bins <- c(min(map_pw_wage$n),50000, 100000, 150000, 200000 ,max(map_pw_wage$n))
pallete <- colorBin("RdYlGn", domain = map_pw_wage$n, bins = bins)
leaflet(data = map_pw_wage) %>%
  addTiles() %>% setView(-99, 35, zoom = 4) %>% addLegend("bottomright", pal = pallete, values = ~n,
                                                          title = "MEAN ANNUAL WAGE",
                                                          labFormat = labelFormat(prefix = "$"),
                                                          opacity = 1)%>%
  addCircleMarkers(
    lat=map_pw_wage$lat, lng=map_pw_wage$lon, radius=sqrt(map_pw_wage$n)/30, color = ~pallete(map_pw_wage$n), weight=1.5, opacity=0.8,
    popup= paste("<br><strong>Average wage: </strong>", round(map_pw_wage$n/1000,0), "k$"
    ))

######################

my_data %>%  
  filter(CASE_STATUS == "CERTIFIED")%>%filter(JOB_TITLE==c("DATA SCIENTIST","DATA ANALYST")) %>% group_by(lat,lon,JOB_TITLE,EMPLOYER_NAME) %>%
  summarise(avg = mean(total_wage)) %>% ungroup() -> map_pw_wage
colnames(map_pw_wage) <- c("lat","lon","JOB_TITLE","EMPLOYER_NAME","n")
bins <- c(min(map_pw_wage$n),50000, 100000, 150000, 200000 ,max(map_pw_wage$n))
pallete <- colorBin("Spectral", domain = map_pw_wage$n, bins = bins)
leaflet(data = map_pw_wage) %>%
  addTiles() %>% setView(-99, 35, zoom = 4) %>% addLegend("bottomright", pal = pallete, values = ~n,
                                                          title = "MEAN ANNUAL WAGE OF DATA SCIENTISTS & DATA ANALYSTS",
                                                          labFormat = labelFormat(prefix = "$"),
                                                          opacity = 1)%>%
  addCircleMarkers(
    lat=map_pw_wage$lat, lng=map_pw_wage$lon, radius=sqrt(map_pw_wage$n)/30, color = ~pallete(map_pw_wage$n), weight=1.5, opacity=0.8,
    popup= paste("<br><strong>Average wage: </strong>", round(map_pw_wage$n/1000,0), "k$","<br>","<strong>JOB TITLE:</strong>", map_pw_wage$JOB_TITLE, "<br>","<strong>EMPLOYER:</strong>", map_pw_wage$EMPLOYER_NAME
    ))




##################################################################################################################

##################################Modeling#######################################################################


modeling_data=read.csv("H1b_EDA_data.csv",stringsAsFactors = FALSE)
names(modeling_data)

modeling_data=modeling_data[!apply(modeling_data=="",1,all),]


#Null report

nullrep.gen=function(data)
{
  data=data.frame(data)
  names_summary=names(data)
  ischar=unlist(sapply(names_summary,function(n) typeof(data[,n]))=="character")
  nullreport=data.frame(colnames=names_summary,null=sapply(names_summary,function(n) sum(is.na(data[,n])) + 
                                                             ifelse(ischar[n],sum(nchar(trimws(data[,n][!is.na(data[,n])]))==0),0)),
                        nrows=sapply(names_summary,function(n) length(data[,n])))
  nullreport$percent_of_complete_values=((nullreport$nrows-nullreport$null)/nullreport$nrows)*100
  data.table(nullreport)
}

nullrep.gen(modeling_data)

#Trimming modeling data's start and end employement date:

modeling_data$start=str_sub(modeling_data$EMPLOYMENT_START_DATE,-4,-1)

modeling_data%>%filter(modeling_data$start!="")->modeling_data

modeling_data$end=str_sub(modeling_data$EMPLOYMENT_END_DATE,-4,-1)

modeling_data%>%filter(modeling_data$end!="")->modeling_data

#Converting start and end date to numeric

modeling_data$start=as.numeric(modeling_data$start)

modeling_data$end=as.numeric(modeling_data$end)

#Extracting duration of employement 

modeling_data$EMP_PERIOD=(modeling_data$end)-(modeling_data$start)

names(modeling_data)

#Assigning modeling columns

model_data=modeling_data[-c(1,3,4,5,6,7,8,9,10,11,12,13,14,15,16,19,21,22,23,24,25,26,27,29,30,33,34,36,37,38)]

names(model_data)

nullrep.gen(model_data)
missing_data=nullrep.gen(modeling_data)
missing_data=missing_data[missing_data$null>0]


ggplot(data = missing_data, aes(x = colnames, y = null/1000)) +  
  geom_bar(stat="identity", fill="orangered1", colour="black") +
  labs(title="Plot of variables with missing values", x ="Year", y = "Number of missing values in thousands ")+coord_flip()

#Checking NA's
model_data=as.data.frame(model_data[complete.cases(modeling_data),])

sapply(model_data,function(x) sum(is.na(x)))




#Feature selection

unique(model_data$SOC_CODE)

model_data$SOC_CODE<-sub(",","",model_data$SOC_CODE)
model_data$SOC_CODE<-gsub("*\\..*","",model_data$SOC_CODE)
model_data$SOC_CODE<-gsub(" ..*", '', model_data$SOC_CODE)
model_data$SOC_CODE<-sub('-*', '', model_data$SOC_CODE)


#Trimming SOC Code to first 2 digits

model_data$SOC_CODE=strtrim(model_data$SOC_CODE,2)

#Removing rows with soc code entries ""

model_data%>%filter(SOC_CODE!="")%>%filter(SOC_CODE!="<F")->model_data


#Converting wrongly entered soc code to their respective field soc code


model_data[((model_data$SOC_CODE=="69")|(model_data$SOC_CODE=="76")|(model_data$SOC_CODE=="73")|(model_data$SOC_CODE=="78")
            |(model_data$SOC_CODE=="75")|(model_data$SOC_CODE=="79")|(model_data$SOC_CODE=="SO")|(model_data$SOC_CODE=="CO")
            |(model_data$SOC_CODE=="5-")|(model_data$SOC_CODE=="1-")|(model_data$SOC_CODE=="N/")
            |(model_data$SOC_CODE=="16")|(model_data$SOC_CODE=="18")|(model_data$SOC_CODE=="38")
            |(model_data$SOC_CODE=="36")|(model_data$SOC_CODE=="12")|(model_data$SOC_CODE=="46")
            |(model_data$SOC_CODE=="48")|(model_data$SOC_CODE=="22")|(model_data$SOC_CODE=="34")
            |(model_data$SOC_CODE=="28")|(model_data$SOC_CODE=="32")|(model_data$SOC_CODE=="74")
            |(model_data$SOC_CODE=="03")),c("SOC_CODE")]='15'

model_data[((model_data$SOC_CODE=="ME")|(model_data$SOC_CODE=="EL")|(model_data$SOC_CODE=="EN")),c("SOC_CODE")]="17"
model_data[(model_data$SOC_CODE=="AC"),c("SOC_CODE")]="13"
model_data[(model_data$SOC_CODE=="AI"),c("SOC_CODE")]="53"
model_data[((model_data$SOC_CODE=="10")&(model_data$JOB_TITLE=="POSTDOCTORAL RESEARCH FELLOW")),c("SOC_CODE")]="25"
model_data[(model_data$SOC_CODE=="10"),c("SOC_CODE")]="15"
model_data[(model_data$SOC_CODE=="A10"),c("SOC_CODE")]="15"
model_data[(model_data$SOC_CODE=="54"),c("SOC_CODE")]="25"
model_data[((model_data$SOC_CODE=="26") & (model_data$JOB_TITLE=="PHYSICIAN")),c("SOC_CODE")]="29"
model_data[((model_data$SOC_CODE=="26") & (model_data$JOB_TITLE=="MANAGER JC50")),c("SOC_CODE")]="13"
model_data[((model_data$SOC_CODE=="26") & (model_data$JOB_TITLE=="ATTENDING, DEVELOPMENTAL PEDIATRICS")),c("SOC_CODE")]=29
model_data[(model_data$SOC_CODE=="26"),c("SOC_CODE")]="15"
model_data[((model_data$SOC_CODE=="71") & (model_data$JOB_TITLE=="MANAGER JC50")),c("SOC_CODE")]="13"
model_data[((model_data$SOC_CODE=="71") & (model_data$JOB_TITLE=="PRODUCT ENGINEER")),c("SOC_CODE")]="17"
model_data[(model_data$SOC_CODE=="71"),c("SOC_CODE")]="15"
model_data[((model_data$SOC_CODE=="50") & (model_data$JOB_TITLE=="MANAGER JC50")),c("SOC_CODE")]="13"
model_data[(model_data$SOC_CODE=="50"),c("SOC_CODE")]="15"
model_data[((model_data$SOC_CODE=="40") & (model_data$JOB_TITLE=="MANAGER JC50")),c("SOC_CODE")]="13"
model_data[(model_data$SOC_CODE=="40"),c("SOC_CODE")]="15"
model_data[((model_data$SOC_CODE=="24") & (model_data$JOB_TITLE=="MANAGER JC50")),c("SOC_CODE")]="13"
model_data[(model_data$SOC_CODE=="24"),c("SOC_CODE")]="15"
model_data[((model_data$SOC_CODE=="42") & (model_data$JOB_TITLE=="MANAGER JC50")),c("SOC_CODE")]="13"
model_data[(model_data$SOC_CODE=="42"),c("SOC_CODE")]="15"
model_data[((model_data$SOC_CODE=="20") & (model_data$JOB_TITLE=="LAW CLERK (PARALEGALS AND LEGAL ASSISTANTS)")),c("SOC_CODE")]="23"
model_data[(model_data$SOC_CODE=="20"),c("SOC_CODE")]="15"
unique(model_data$SOC_CODE)


#Converting soc code to factors

model_data$SOC_CODE=as.factor(model_data$SOC_CODE)


#Converting full time position tinto factors

model_data[(model_data$FULL_TIME_POSITION=="Y") ,c("FULL_TIME_POSITION")]="1"
model_data[(model_data$FULL_TIME_POSITION=="N") ,c("FULL_TIME_POSITION")]="2"
model_data$FULL_TIME_POSITION=as.factor(model_data$FULL_TIME_POSITION)
unique(model_data$FULL_TIME_POSITION)

#Converting case status into factors

unique(model_data$CASE_STATUS)
model_data%>%filter(CASE_STATUS!="WITHDRAWN")->model_data
model_data[(model_data$CASE_STATUS=="CERTIFIED-WITHDRAWN") ,c("CASE_STATUS")]="CERTIFIED"
model_data[(model_data$CASE_STATUS=="CERTIFIED") ,c("CASE_STATUS")]="1"
model_data[(model_data$CASE_STATUS=="DENIED") ,c("CASE_STATUS")]="2"



#Converting wrongly entered soc code to their respective field sector code

model_data%>%filter(Sector_data!=46)->model_data

model_data[(model_data$Sector_data==60),c("Sector_data")]=51
model_data[(model_data$Sector_data==35),c("Sector_data")]=51
model_data[((model_data$Sector_data==13)& (model_data$total_wage==123032)|(model_data$total_wage==49920)),c("Sector_data")]=52
model_data[(model_data$Sector_data==13),c("Sector_data")]=51
model_data[(model_data$Sector_data==74),c("Sector_data")]=62
model_data[(model_data$Sector_data==86),c("Sector_data")]=55
model_data[((model_data$Sector_data==64)& (model_data$JOB_TITLE=="BIOCHEMIST")|(model_data$JOB_TITLE=="BIOINFORMATICS SCIENTIST")),c("Sector_data")]=62
model_data[(model_data$Sector_data==64),c("Sector_data")]=51
model_data[(model_data$Sector_data==65),c("Sector_data")]=54
model_data[(model_data$Sector_data==73),c("Sector_data")]=54
model_data[(model_data$Sector_data==43),c("Sector_data")]=61
model_data[(model_data$Sector_data==82),c("Sector_data")]=61
model_data[((model_data$Sector_data==59)& (model_data$JOB_TITLE=="SALES TRAINING MANAGER")),c("Sector_data")]=55
model_data[(model_data$Sector_data==59),c("Sector_data")]=51
model_data[(model_data$Sector_data==0),c("Sector_data")]=54
model_data[(model_data$Sector_data==69),c("Sector_data")]=54
model_data[(model_data$Sector_data==99),c("Sector_data")]=54
model_data[((model_data$Sector_data==15)& (model_data$JOB_TITLE=="PLASTERER")),c("Sector_data")]=23
model_data[(model_data$Sector_data==15),c("Sector_data")]=54
model_data[(model_data$Sector_data==87),c("Sector_data")]=51
model_data[(model_data$Sector_data==91),c("Sector_data")]=52
model_data[(model_data$Sector_data==83),c("Sector_data")]=55
model_data[(model_data$Sector_data==20),c("Sector_data")]=54
model_data[(model_data$Sector_data==34),c("Sector_data")]=53
model_data[(model_data$Sector_data==25) & model_data$JOB_TITLE== "ACCOUNT EXECUTIVE, TALENT SALES" ,c("Sector_data")]=55
model_data[(model_data$Sector_data==25),c("Sector_data")]=54
model_data[(model_data$Sector_data==6),c("Sector_data")]=52
model_data[(model_data$Sector_data==14),c("Sector_data")]=55
model_data[(model_data$Sector_data==67),c("Sector_data")]=56
model_data[(model_data$Sector_data==5),c("Sector_data")]=56
model_data[(model_data$Sector_data==27),c("Sector_data")]=56
model_data[(model_data$Sector_data==16),c("Sector_data")]=56
model_data[(model_data$Sector_data==2),c("Sector_data")]=33
model_data[(model_data$Sector_data==10),c("Sector_data")]=52
model_data[(model_data$Sector_data==12),c("Sector_data")]=54
model_data[(model_data$Sector_data==19),c("Sector_data")]=54
model_data[(model_data$Sector_data==58),c("Sector_data")]=51
model_data[(model_data$Sector_data==29),c("Sector_data")]=62
model_data[(model_data$Sector_data==24),c("Sector_data")]=51
model_data[(model_data$Sector_data==38),c("Sector_data")]=33
model_data[(model_data$Sector_data==50),c("Sector_data")]=54
model_data[(model_data$Sector_data==84),c("Sector_data")]=54
model_data[(model_data$Sector_data==78),c("Sector_data")]=55
model_data[(model_data$Sector_data==36),c("Sector_data")]=51
model_data[(model_data$Sector_data==30),c("Sector_data")]=55
model_data[(model_data$Sector_data==57),c("Sector_data")]=52
model_data[(model_data$Sector_data==80),c("Sector_data")]=52
model_data[(model_data$Sector_data==47),c("Sector_data")]=54

model_data$Sector_data=as.factor(model_data$Sector_data)
unique(model_data$Sector_data)


#fwrite(model_data, file ="H1b_model_data.csv")

#cleaning worksite state

model_data$Worksite_STATE_full<-sub(",","",model_data$Worksite_STATE_full)

#Converting datatypes

model_data$FULL_TIME_POSITION=as.factor(model_data$FULL_TIME_POSITION)
model_data$SOC_CODE=as.factor(model_data$SOC_CODE)
model_data$Sector_data=as.factor(model_data$Sector_data)
model_data$YEAR=as.factor(model_data$YEAR)
model_data$CASE_STATUS=as.factor(model_data$CASE_STATUS)
model_data$EMP_PERIOD=as.numeric(model_data$EMP_PERIOD)
model_data$Worksite_STATE_full=as.factor(model_data$Worksite_STATE_full)
str(model_data)
unique(model_data$Worksite_STATE_full)
model_data=model_data[-c(2)]

#Removing withdrawn application

model_data%>%filter(CASE_STATUS!=4)->model_data
model_data[(model_data$CASE_STATUS=="3") ,c("CASE_STATUS")]=1
model_data$CASE_STATUS=as.factor(model_data$CASE_STATUS)


#fwrite(model_data, file ="Preprocessed_DATA.csv")


#sampling

perc=table(model_data$CASE_STATUS)
original_proportion=(perc[2]/perc[1])*100
original_proportion


#Under sampling data and then random sampling from it 

data_balanced_under <- ovun.sample(CASE_STATUS ~ ., data = model_data, method = "under",N = 72000)$data
table(data_balanced_under$CASE_STATUS)
data_undersampled=sample_n(data_balanced_under, 10000)
table(data_undersampled$CASE_STATUS)
#fwrite(data_undersampled, file ="data_undersampled.csv")


#Both under and over sampling

data_balanced_both <- ovun.sample(CASE_STATUS ~ ., data = model_data, method = "both", p=0.5,N=10000)$data
table(data_balanced_both$CASE_STATUS)
#fwrite(data_balanced_both, file ="data_balanced_both.csv")


#Synthetic data generation

data.rose <- ROSE(CASE_STATUS ~ ., data = model_data, seed = 1,N=10000)$data
table(data.rose$CASE_STATUS)
#fwrite(data.rose, file ="data_rose.csv")


#ROC curves for different sampling methods to find best accuracy

data_undersampled=read.csv("data_undersampled.csv",stringsAsFactors = FALSE)
data_balanced_both=read.csv("data_balanced_both.csv",stringsAsFactors = FALSE)
data.rose=read.csv("data_rose.csv",stringsAsFactors = FALSE)
data_undersampled$FULL_TIME_POSITION=as.factor(data_undersampled$FULL_TIME_POSITION)
data_undersampled$SOC_CODE=as.factor(data_undersampled$SOC_CODE)
data_undersampled$Sector_data=as.factor(data_undersampled$Sector_data)
data_undersampled$YEAR=as.factor(data_undersampled$YEAR)
data_undersampled$CASE_STATUS=as.factor(data_undersampled$CASE_STATUS)
data_undersampled$EMP_PERIOD=as.numeric(data_undersampled$EMP_PERIOD)
data_undersampled$Worksite_STATE_full=as.factor(data_undersampled$Worksite_STATE_full)
data_balanced_both$FULL_TIME_POSITION=as.factor(data_balanced_both$FULL_TIME_POSITION)
data_balanced_both$SOC_CODE=as.factor(data_balanced_both$SOC_CODE)
data_balanced_both$Sector_data=as.factor(data_balanced_both$Sector_data)
data_balanced_both$YEAR=as.factor(data_balanced_both$YEAR)
data_balanced_both$CASE_STATUS=as.factor(data_balanced_both$CASE_STATUS)
data_balanced_both$EMP_PERIOD=as.numeric(data_balanced_both$EMP_PERIOD)
data_balanced_both$Worksite_STATE_full=as.factor(data_balanced_both$Worksite_STATE_full)
data.rose$FULL_TIME_POSITION=as.factor(data.rose$FULL_TIME_POSITION)
data.rose$SOC_CODE=as.factor(data.rose$SOC_CODE)
data.rose$Sector_data=as.factor(data.rose$Sector_data)
data.rose$YEAR=as.factor(data.rose$YEAR)
data.rose$CASE_STATUS=as.factor(data.rose$CASE_STATUS)
data.rose$EMP_PERIOD=as.numeric(data.rose$EMP_PERIOD)
data.rose$Worksite_STATE_full=as.factor(data.rose$Worksite_STATE_full)

tree.rose <- rpart(CASE_STATUS ~ ., data = data.rose)
tree.both <- rpart(CASE_STATUS ~ ., data = data_balanced_both)
tree.under <- rpart(CASE_STATUS ~ ., data = data_undersampled)


Accuracy=function(tree.rose,tree.both,tree.under,model_data){
  
  AUC_rose=c()
  AUC_both=c()
  AUC_under=c()
  
  for (i in 1:10){
    
    accuracy_test=sample_n(model_data, 10000)
    
    pred.tree.rose <- predict(tree.rose, newdata = accuracy_test,type='class')
    pred.tree.both <- predict(tree.both, newdata = accuracy_test,type='class')
    pred.tree.under <- predict(tree.under, newdata = accuracy_test,type='class')
    
    AUC_rose[i]=roc.curve(accuracy_test$CASE_STATUS, pred.tree.rose)$auc
    AUC_both[i]=roc.curve(accuracy_test$CASE_STATUS, pred.tree.both)$auc
    AUC_under[i]=roc.curve(accuracy_test$CASE_STATUS, pred.tree.under)$auc
    
  }
  Accuracy=rbind(mean(AUC_rose)*100,mean(AUC_both)*100,mean(AUC_under)*100)
  return(Accuracy)
}
Accuracy(tree.rose,tree.both,tree.under,model_data)


#Choosing data_undersampled as best sample data

#fwrite(data_undersampled, file ="Sample_data.csv")

data_model=read.csv("Sample_data.csv",stringsAsFactors = FALSE)

library(caret)
#Converting datatypes

data_model$FULL_TIME_POSITION=as.factor(data_model$FULL_TIME_POSITION)
data_model$SOC_CODE=as.factor(data_model$SOC_CODE)
data_model$Sector_data=as.factor(data_model$Sector_data)
data_model$YEAR=as.factor(data_model$YEAR)
data_model$CASE_STATUS=as.factor(data_model$CASE_STATUS)
data_model$EMP_PERIOD=as.numeric(data_model$EMP_PERIOD)
data_model$Worksite_STATE_full=as.factor(data_model$Worksite_STATE_full)
data_model=as.data.frame(data_model[complete.cases(data_model),])

#One hot encoding
data_model_response=data_model$CASE_STATUS
dmy <- dummyVars(" ~ .", data=data_model[-c(1)])
data_model <- data.frame(predict(dmy, newdata=data_model))
data_model$CASE_STATUS=data_model_response

normalize = function(x){
  new_x = (x - min(x)) / max(x)
  return(new_x)
}
data_model[,names(data_model) != "CASE_STATUS"] <- apply(data_model[,names(data_model) != "CASE_STATUS"], 2, normalize)


# train/val/test split

set.seed(100)
train_comb_idx = sample(nrow(data_model), 0.8*nrow(data_model), replace=FALSE)
test_idx = (1:nrow(data_model))[-train_comb_idx]
val_idx = sample(train_comb_idx, 0.3*nrow(data_model), replace=FALSE)
train_idx = train_comb_idx[-val_idx]
train_comb = data_model[train_comb_idx, ]
train = data_model[train_idx, ]
val = data_model[val_idx, ]
test = data_model[test_idx, ]


#Function for manipulating accuracy, precision, error rate

result=function(conf_matrix){
  
  result=data.frame()
  accuracy=((conf_matrix[1,1]+conf_matrix[2,2])/sum(conf_matrix))*100
  precision_positive=(conf_matrix[1,1]/(conf_matrix[1,1]+conf_matrix[2,1]))*100
  precision_negative=(conf_matrix[2,2]/(conf_matrix[2,1]+conf_matrix[2,2]))*100
  Error_rate=100-accuracy
  sensitivity=(conf_matrix[1,1]/(conf_matrix[1,1]+conf_matrix[1,2]))*100
  specificity=(conf_matrix[2,2]/(conf_matrix[2,2]+conf_matrix[2,1]))*100
  result=rbind(accuracy,precision_positive,precision_negative,Error_rate,sensitivity,specificity)
  rownames(result)=c("accuracy","precision_positive","precision_negative","Error_rate","sensitivity","specificity")
  colnames(result)="Values"
  return(result)
  
}


#Logistic regression

logit_model <- glm(CASE_STATUS ~ ., data=train_comb, family='binomial')


summary(logit_model)

par(mfrow=c(1,1))
plot(logit_model)
logit_model$coefficients


train.probs <-predict(logit_model, train_comb, type='response')
pred.logit_train <- rep('2',length(train.probs))
pred.logit_train[train.probs>=0.5] <- '1'
conf_mat_log_train=table(pred.logit_train, train_comb$CASE_STATUS)
result(conf_mat_log_train)


test.probs <-predict(logit_model, test, type='response')
pred.logit_test <- rep('2',length(test.probs))
pred.logit_test[test.probs>=0.5] <- '1'
conf_mat_log_test=table(pred.logit_test, test$CASE_STATUS)
result(conf_mat_log_test)

#K nearest neighbour algorithm

trctrl <- trainControl(method = "repeatedcv", number = 5, repeats =1)
set.seed(100)
knn_fit <- train(CASE_STATUS ~., data = train_comb, method = "knn",
                 trControl=trctrl,
                 tuneLength = 10)
knn_fit
knn_fit$bestTune
#Best model
knn_model_train <- knn(train_comb[,-108],train_comb[,-108],cl=train_comb[,108],k=knn_fit$bestTune[1,1])
tab_train <- table(knn_model_train,train_comb[,108])
tab_train
result(tab_train)
knn_model_test <- knn(train_comb[,-108],test[,-108],cl=train_comb[,108],k=knn_fit$bestTune[1,1])
tab_test <- table(knn_model_test,test[,108])
tab_test
result(tab_test)

#Decision Tree model


tree=rpart(CASE_STATUS ~ .,data=train_comb,method="class")
tree_hyp=train(CASE_STATUS ~ ., data = train_comb, 
               method = "rpart",
               tuneGrid = data.frame(cp = c(0.0001,0.001,0.01, 0.05,0.1)),
               control = rpart.control(xval = 5))
tree_hyp$bestTune
tree.pruned <- prune(tree, cp =tree_hyp$bestTune[1,1] )
#tree=rpart(CASE_STATUS ~ .,data=train_comb,method="class",control=rpart.control(cp=0.1))

summary(tree.pruned)
plotcp(tree)
pred_train_tree <- predict(tree, newdata=train_comb, type='class')
conf_matrix_train_tree=table(pred_train_tree, train_comb$CASE_STATUS)
conf_matrix_train_tree
result(conf_matrix_train_tree)
pred_test_tree <- predict(tree, newdata=test, type='class')
conf_matrix_test_tree=table(pred_test_tree, test$CASE_STATUS)
conf_matrix_test_tree
result(conf_matrix_test_tree)
pred_test_tree_pruned_rpart <- predict(tree.pruned, newdata=test, type='class')
conf_matrix_test_pruned_rpart=table(pred_test_tree_pruned_rpart, test$CASE_STATUS)
conf_matrix_test_pruned_rpart
result(conf_matrix_test_pruned_rpart)


#Random forests

#No.of trees tuning
metric <- "Accuracy"
control <- trainControl(method="repeatedcv", number=5, repeats=1, search="grid")
tunegrid <- expand.grid(.mtry=c(sqrt(ncol(train_comb))))
modellist <- list()
for (ntree in c(1000, 1500, 2000, 2500)) {
  set.seed(100)
  fit <- train(CASE_STATUS~., data=train_comb, method="rf", metric=metric, tuneGrid=tunegrid, trControl=control, ntree=ntree)
  key <- toString(ntree)
  modellist[[key]] <- fit
}
modellist
# compare results
rf_results <- resamples(modellist)
summary(rf_results)
dotplot(rf_results)
#mtry tuning
set.seed(100)
bestmtry <- tuneRF(train_comb[,1:107], train_comb[,108], stepFactor=1.5, improve=0.00001, ntree=1500,plot=TRUE)
plot(bestmtry,main="PLot for mtry tuning")
print(bestmtry)
#Best model
rf_model=randomForest(CASE_STATUS ~ ., data = train_comb, ntree = 1500,      mtry = 7, importance = TRUE) 
rf_model
pred_rf_train = predict(rf_model, newdata=train_comb, type='class')
conf_matrix_rf_train=table(pred_rf_train, train_comb$CASE_STATUS)
conf_matrix_rf_train
result(conf_matrix_rf_train)
pred_rf_test = predict(rf_model, newdata=test, type='class')
conf_matrix_rf_test=table(pred_rf_test, test$CASE_STATUS)
conf_matrix_rf_test
result(conf_matrix_rf_test)

varImpPlot(rf_model)





#Gradient Boosting

grid <- expand.grid(n.trees = c(1000,1500), interaction.depth=c(1,2,3), shrinkage=c(0.01,0.05,0.1), n.minobsinnode=c(1,3,5))
ctrl <- trainControl(method = "repeatedcv",number = 5, repeats = 1, allowParallel = T)
#registerDoParallel(detectCores()-1)
GBMModel <- train(CASE_STATUS~.,data = train_comb,
                  method = "gbm", trControl = ctrl, tuneGrid = grid)
print(GBMModel)
#best_model
gbm_train_comb=train_comb
gbm_train_comb$CASE_STATUS=ifelse(gbm_train_comb$CASE_STATUS==1,0,1)
unique(gbm_train_comb$CASE_STATUS)
gbm_test_comb=test
gbm_test_comb$CASE_STATUS=ifelse(gbm_test_comb$CASE_STATUS==1,0,1)
unique(gbm_test_comb$CASE_STATUS)
gbm_model<-gbm(CASE_STATUS~., distribution = 'bernoulli',data=gbm_train_comb,n.trees = 1500,interaction.depth = 3,shrinkage=.01,n.minobsinnode = 5)
gbm_model
#prediction
gbm_train<-predict(gbm_model,newdata = gbm_train_comb,type = 'response', n.trees = 1500)
gbm_class<-ifelse(gbm_train<0.5,1,2)
conf_matrix_gbm_train=table(gbm_class, gbm_train_comb$CASE_STATUS)
conf_matrix_gbm_train
result(conf_matrix_gbm_train)
gbm_test<-predict(gbm_model,newdata = gbm_test_comb,type = 'response', n.trees = 1500)
gbm_class<-ifelse(gbm_test<0.5,1,2)
conf_matrix_gbm_test=table(gbm_class, gbm_test_comb$CASE_STATUS)
conf_matrix_gbm_test
result(conf_matrix_gbm_test)




#Support vector machines

#Linear kernel

tune.linear <- tune.svm(CASE_STATUS ~., data=train_comb, kernel='linear',
                        cost=c(0.01,0.1,1,5))
summary(tune.linear)
plot(tune.linear)
tune.linear$best.model


pred_train_lin <- predict(tune.linear$best.model, train_comb)
conf_mat_tr_ln=table(pred_train_lin,train_comb$CASE_STATUS)
result(conf_mat_tr_ln)


pred_test_lin <- predict(tune.linear$best.model, test)
conf_mat_test_ln=table(pred_test_lin,test$CASE_STATUS)
result(conf_mat_test_ln)


#Radial Kernel

tune.radial <- tune.svm(CASE_STATUS ~., data=train_comb, kernel='radial',
                        gamma=c(0.1,1,2,3))
summary(tune.radial)
plot(tune.radial)
tune.radial$best.model


pred_train_rad <- predict(tune.radial$best.model, train_comb)
conf_mat_tr_rad=table(pred_train_rad,train_comb$CASE_STATUS)
result(conf_mat_tr_rad)


pred_test_rad <- predict(tune.radial$best.model, test)
conf_mat_test_rad=table(pred_test_rad,test$CASE_STATUS)
result(conf_mat_test_rad)



#Polynomial kernel
tune.poly <- tune.svm(CASE_STATUS ~., data=train_comb, kernel='polynomial',degree=c(3,4,5), coef0=c(0.1,1,2))

summary(tune.poly)
plot(tune.poly)
tune.poly$best.model


pred_train_poly <- predict(tune.poly$best.model, train_comb)
conf_mat_tr_poly=table(pred_train_poly,train_comb$CASE_STATUS)
result(conf_mat_tr_poly)


pred_test_poly <- predict(tune.poly$best.model, test)
conf_mat_test_poly=table(pred_test_poly,test$CASE_STATUS)
result(conf_mat_test_poly)


#Sigmoid kernel

tune.sig <- tune.svm(CASE_STATUS ~., data=train_comb, kernel='sigmoid',gamma=c(3,4,5), coef0=c(0.1,1,2))

summary(tune.sig)
plot(tune.sig)
tune.sig$best.model


pred_train_sig <- predict(tune.sig$best.model, train_comb)
conf_mat_tr_sig=table(pred_train_sig,train_comb$CASE_STATUS)
result(conf_mat_tr_sig)


pred_test_sig <- predict(tune.sig$best.model, test)
conf_mat_test_sig=table(pred_test_sig,test$CASE_STATUS)
result(conf_mat_test_sig)



save(list=ls(all=T), file='GENALYTICA.RData')





