my_data=read.csv("H1b_EDA_data.csv",stringsAsFactors = FALSE)
#nullrep.gen(my_data)

###EXPLORATORY DATA ANALYSIS###
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


emp_city_geocodes %>%rename(EMPLOYER_CITY = emp_city_geocodes) -> emp_city_geocodes

colnames(emp_city_geocodes)=c("lon","lat","EMPLOYER_CITY")

################################################################## 201 to 1000 ##############################

emp_city_201_to_1000 <- (emp_city_info$EMPLOYER_CITY)[201:1000]

#emp_city_geocodes_01 <- cbind(geocode(emp_city_201_to_1000),emp_city_201_to_1000)

#fwrite(emp_city_geocodes_01, file ="emp_city_201_to_1000.csv")

emp_city_geocodes_01 = read.csv("emp_city_201_to_1000.csv",stringsAsFactors = FALSE)


emp_city_geocodes_01 %>%rename(EMPLOYER_CITY = emp_city_201_to_1000) = emp_city_geocodes_01

colnames(emp_city_geocodes_01)=c("lon","lat","EMPLOYER_CITY")



emp_city_1001_to_2500 <- (emp_city_info$EMPLOYER_CITY)[1001:2500]

#emp_city_geocodes_02 <- cbind(geocode(emp_city_1001_to_2500),emp_city_1001_to_2500)

#fwrite(emp_city_geocodes_02, file ="emp_city_1001_to_2500.csv")

emp_city_geocodes_02 = read.csv("emp_city_1001_to_2500.csv",stringsAsFactors = FALSE)


emp_city_geocodes_02 %>%
  rename(EMPLOYER_CITY = emp_city_1001_to_2500) -> emp_city_geocodes_02

colnames(emp_city_geocodes_02)=c("lon","lat","EMPLOYER_CITY")




#######




emp_city_2501_to_4000 <- (emp_city_info$EMPLOYER_CITY)[2501:4000]

#emp_city_geocodes_03 <- cbind(geocode(emp_city_2501_to_4000),emp_city_2501_to_4000)

#fwrite(emp_city_geocodes_03, file ="emp_city_2501_to_4000.csv")

emp_city_geocodes_03 = read.csv("emp_city_2501_to_4000.csv",stringsAsFactors = FALSE)


emp_city_geocodes_03 %>%
  rename(EMPLOYER_CITY = emp_city_2501_to_4000) -> emp_city_geocodes_03

colnames(emp_city_geocodes_03)=c("lon","lat","EMPLOYER_CITY")


########    4001   to 6000######



emp_city_4001_to_7944 <- (emp_city_info$EMPLOYER_CITY)[4001:7944]

#emp_city_geocodes_04 <- cbind(geocode(emp_city_4001_to_7944),emp_city_4001_to_7944)

#fwrite(emp_city_geocodes_04, file ="emp_city_4001_to_7944.csv")

emp_city_geocodes_04 = read.csv("emp_city_4001_to_7944.csv",stringsAsFactors = FALSE)


emp_city_geocodes_04 %>%
  rename(EMPLOYER_CITY = emp_city_4001_to_7944) -> emp_city_geocodes_04

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






#Top 7500 Cities certified

#my_data %>% 
 # filter(!is.na(EMPLOYER_CITY)) %>% 
 #  filter(CASE_STATUS == "CERTIFIED") %>% filter(!is.na(lon))%>%filter(!is.na(lat))%>%
 #  group_by(EMPLOYER_CITY) %>%
 #  summarise(n = n()) %>%
 #  arrange(desc(n))%>%head(7500)->map_top500_wage


#geocoding bsed on city and state

register_google(key = "AIzaSyDvz44Hy0dKX4opHPuqL-Bqpp9QTGg5cdI")

my_data%>%filter(!is.na(Worksite_State_and_city))%>%filter(Worksite_State_and_city!="")%>%group_by(Worksite_State_and_city)%>%
  summarise(n=n())%>%arrange(desc(n))%>%ungroup()->Worksite_State_and_city_info

Worksite_State_and_city_info_geo <- (Worksite_State_and_city_info$Worksite_State_and_city)


#worksite_city_geocodes <- cbind(geocode(Worksite_State_and_city_info_geo),Worksite_State_and_city_info_geo)

#fwrite(worksite_city_geocodes, file ="worksite_city_geocodes.csv")

worksite_city_geocodes = read.csv("worksite_city_geocodes",stringsAsFactors = FALSE)


