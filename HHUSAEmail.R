library(ggplot2)
library(dplyr)
dfSM <- read.csv('Downloads/Group Project Option 2 and 3 HHUSA/vr__VR_Email_History_Contact__c.csv')
summary(dfSM)
str(dfSM)

dfSM <- subset(dfSM, select = c(Name,
                                vr__Mail_Date__c,
                                vr__Opened__c,
                                vr__Clicked__c,
                                vr__Contact__c))
dfSM$Name <- factor(dfSM$Name)
dfSM$vr__Opened__c <- factor(dfSM$vr__Opened__c)
dfSM$vr__Clicked__c <- factor(dfSM$vr__Clicked__c)
dfSM$vr__Contact__c <- factor(dfSM$vr__Contact__c)
summary(dfSM)
str(dfSM)

table(dfSM$Name)

dfVCF <- dfSM[grep("Virtual Career Fair",dfSM$Name),]
dfTJ <- dfSM[grep("Top Jobs",dfSM$Name),]

table(dfVCF$Name)
summary(dfVCF)
table(dfTJ$Name)
summary(dfTJ)

dfEmail <- rbind(dfVCF,dfTJ)
summary(dfEmail)
str(dfEmail)
table(dfEmail$Name)

dfEmail$row_num <- seq.int(nrow(dfEmail)) 
dfEmail2 <- dfEmail[-c(15479:18626),]

summary(dfEmail2)
str(dfEmail2)
table(dfEmail2$Name)


# ONLY USE TOB JOBS, since NO RESPONSE RECORDED for VIRTUAL CAREER FAIR
summary(dfTJ)
str(dfTJ)

dfTJunique <- subset(dfTJ, !duplicated(vr__Contact__c))

summary(dfTJunique)
str(dfTJ)

dfTJduplicates <- subset(dfTJ, duplicated(vr__Contact__c))
summary(dfTJduplicates)

dfTJfindallduplicates <- subset(dfTJ, vr__Contact__c %in% c('0033800002cxoPpAAI',
                   '0033800002exIKiAAM',
                   '0033800002g3IFlAAM',
                   '0033800002jGSHxAAO',
                   '0033800002n6wVvAAI',
                   '0033800002n4oDeAAI',
                   '0033800002sIKqXAAW',
                   '0033800002oyZf7AAE',
                   '0033800002p06DeAAI',
                   '0033800002eHPTuAAO',
                   '0033800002p27WeAAI',
                   '0033800002q3jLgAAI',
                   '0033800002q64E7AAI',
                   '0033800002rgXBiAAM',
                   '0033800002oDkDEAA0',
                   '0033800002qphsjAAA',
                   '0033800002sIKozAAG',
                   '0033800002rgKABAA2',
                   '0033800002qrJmvAAE',
                   '0033800002sIEjEAAW',
                   '0033800002sHcqDAAS',
                   '0033800002sJYkjAAG'))

bar <- data.frame(matrix(c(715,683,117),
                           ncol = 2, 
                           nrow = 3))
x <- c("Emails","Frequency")
colnames(bar) <- x

bar[1,"Emails"] = "Sent"
bar[2,"Emails"] = "Opened"
bar[3,"Emails"] = "Clicked Job Board link"

ggplot(data=bar, aes(x=Emails, y=Frequency)) + 
  geom_bar(stat="identity") +
  scale_x_discrete(limits=c("Sent","Opened","Clicked Job Board link")) +
  geom_text(aes(label=c('715','683 (95.5%)','117 (16.4%)'),vjust=-.5)) +
  ggtitle("Hire Heroes USA Email Campaign") +
  theme(plot.title = element_text(hjust = 0.5,face="bold"))