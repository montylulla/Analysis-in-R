install.packages("ggplot2")
install.packages("fasttime")
library(fasttime)
library(ggplot2)

#Data of USA
data_subset_usa <-subset(Merged,Merged$Country=="United States")
data_subset_usa <- data.frame(data_subset_usa)

USA_data <- data.frame(subset(data_subset_usa, data_subset_usa$NPS_type!="NA"))
#Promoters in USA (174,329)
dataUSA_subset_promoters <-data.frame(subset(USA_data ,USA_data$NPS_type=="Promoter"))

for(i in 4:13){
  dataUSA_subset_promoters[is.na(dataUSA_subset_promoters[,i]), i] <- floor(mean(dataUSA_subset_promoters[,i], na.rm = TRUE))
}

dataUSA_subset_promoters <- data.frame(na.omit(dataUSA_subset_promoters))
#Promoters in USA(126,351)

#Detractors in USA (30,508)
dataUSA_subset_detractor <-data.frame(subset(USA_data ,USA_data$NPS_type=="Detractor"))

for(i in 4:13){
  dataUSA_subset_detractor[is.na(dataUSA_subset_detractor[,i]), i] <- floor(mean(dataUSA_subset_detractor[,i], na.rm = TRUE))
}

dataUSA_subset_detractor <- data.frame(na.omit(dataUSA_subset_detractor))
#Detractors in USA(23,470)

#Passives in USA(46,531)
dataUSA_subset_passive <-data.frame(subset(USA_data ,USA_data$NPS_type=="Passive"))

for(i in 4:13){
  dataUSA_subset_passive[is.na(dataUSA_subset_passive[,i]), i] <- floor(mean(dataUSA_subset_passive[,i], na.rm = TRUE))
}

dataUSA_subset_passive <- data.frame(na.omit(dataUSA_subset_passive))

#Passives in USA(35,330)


USA_data <- rbind(dataUSA_subset_promoters,dataUSA_subset_detractor,dataUSA_subset_passive)
View(USA_data)
#############################################################################################################################






########################DATA CLEANING FOR THE ROW#####################################################################

data_subset_ROW <- data.frame(subset (Merged, Merged$Country != "United States"))

Merged1 <- data.frame(subset(data_subset_ROW, data_subset_ROW$NPS_type!="NA"))
Merged1 <-data.frame(Merged1[c(-16,-17)])
#Number of PromotersROW(41,269)
dataROW_NPSType_subset_promoters <-data.frame(subset(Merged1 ,Merged1$NPS_type=="Promoter"))

for(i in 4:13){
  dataROW_NPSType_subset_promoters[is.na(dataROW_NPSType_subset_promoters[,i]), i] <- floor(mean(dataROW_NPSType_subset_promoters[,i], na.rm = TRUE))
}


dataROW_NPSType_subset_promoters <- data.frame(na.omit(dataROW_NPSType_subset_promoters))
#Number of PromotersROW(31,224)

#Detractors in ROW (6,656)
dataROW_NPSType_subset_detractor <-data.frame(subset(Merged1 ,Merged1$NPS_type=="Detractor"))

for(i in 4:13){
  dataROW_NPSType_subset_detractor[is.na(dataROW_NPSType_subset_detractor[,i]), i] <- floor(mean(dataROW_NPSType_subset_detractor[,i], na.rm = TRUE))
}

dataROW_NPSType_subset_detractor <- data.frame(na.omit(dataROW_NPSType_subset_detractor))
#Detractors in USA(4,617)

#Passives in ROW(13,891)
dataROW_NPSType_subset_passive <-data.frame(subset(Merged1 ,Merged1$NPS_type=="Passive"))

for(i in 4:13){
  dataROW_NPSType_subset_passive[is.na(dataROW_NPSType_subset_passive[,i]), i] <- floor(mean(dataROW_NPSType_subset_passive[,i], na.rm = TRUE))
}

dataROW_NPSType_subset_passive <- data.frame(na.omit(dataROW_NPSType_subset_passive))

#Passives in ROW(10,293)

Merged1<- rbind (dataROW_NPSType_subset_promoters,dataROW_NPSType_subset_detractor,dataROW_NPSType_subset_passive)
View(Merged1)
#######################################################################################################################






#sample dataset

#USA_data <- out_201402[,-c(1,17,14,50,34,44,40,36,21,64,65,71,90:97,88,87,98,103,104,110,112,115,150:157,175,176,196,53)]
#USA_data <- read.csv("\\hd.ad.syr.edu/01/d36348/Documents/Downloads/out-201402")


#Reduce data and assign variable
#February data
Feb_data <- out_201402[,c(19,23,24,137:145,147,163,167,168,169,171,175,176,179,187,201:205,208,210,213,218,232)]
View(Feb_data)
#April data
April_data <- out_201404[,c(19,23,24,137:145,147,163,167,168,169,171,175,176,179,187,201:205,208,210,213,218,232)]
#June_data <- out_201406[,c(19,23,24,137:145,147,163,167,168,169,171,175,176,179,187,201:205,208,210,213,218,232)]
August_data <- out_201408[,c(19,23,24,137:145,147,163,167,168,169,171,175,176,179,187,201:205,208,210,213,218,232)]
#October_data <- out_201410[,c(19,23,24,137:145,147,163,167,168,169,171,175,176,179,187,201:205,208,210,213,218,232)]
December_data <- out_201412[,c(19,23,24,137:145,147,163,167,168,169,171,175,176,179,187,201:205,208,210,213,218,232)]

memory.limit(56000)
#Merge data
Merged <- rbind(Feb_data, April_data, August_data, December_data)
colnames(Merged) <- c("Duration","Purpose","Rate","Recommendation","Satisfaction","Guest_satisfaction","Tranquility","Hotel_condition","Customer_service","Customer_care","Internet","Quality_Checkin","Overall_F&B","Hotel_Name","City","State","Region","Country","Latitude","Longitude","NPS_goal","Meeting_space","Bell_staff","Business_center","Casino","Conference","Convention","Golf","Limo","Fitness","Resort","NPS_type")

#USA_data <- subset(USA_data, USA_data$Country_PL=="United States")
#Merged1 <- Merged[,-c(16,17)]

#unique(USA_data$Country_PL)
#Remove NA's
#USA_data <- na.omit(USA_data)

#Merged1 <- na.omit(Merged1)
#unique(Merged1$Country_PL)
#unique(USA_data$Country_PL)
#Change column names
colnames(USA_data) <- c("Duration","Purpose","Rate","Recommendation","Satisfaction","Guest_satisfaction","Tranquility","Hotel_condition","Customer_service","Customer_care","Internet","Quality_Checkin","Overall_F&B","Hotel_Name","City","State","Region","Country","Latitude","Longitude","NPS_goal","Meeting_space","Bell_staff","Business_center","Casino","Conference","Convention","Golf","Limo","Fitness","Resort","NPS_type")
colnames(Merged1) <- c("Duration","Purpose","Rate","Recommendation","Satisfaction","Guest_satisfaction","Tranquility","Hotel_condition","Customer_service","Customer_care","Internet","Quality_Checkin","Overall_F&B","Hotel_Name","City","Country","Latitude","Longitude","NPS_goal","Meeting_space","Bell_staff","Business_center","Casino","Conference","Convention","Golf","Limo","Fitness","Resort","NPS_type")

View(Merged1)
#columns <- c(19,23,24,67,70,108,137:145,147,163,167,168,169,171,179,187,191,200:208,218,232)
#fread(file="D:/Rahul/Documents/Syracuse University/1st Semester/IST 687 - Applied Data Science/Project/Dataset/out-201402.csv", header=TRUE, select=coltoread, verbose=TRUE)
#fread(file=out_201402, header=TRUE, select=columns, verbose=TRUE)
#modified dataset
#USA_data <- SurveyDataExample[,-c(1,17,14,50,34,44,40,36,21,64,65,71,90:97,88,87)]
#View(USA_data)


#Replacing NA's with 0.5, since NA's are for the same day checkout 
#USA_data$LENGTH_OF_STAY_C[is.na(USA_data$LENGTH_OF_STAY_C)] <- 0.5

#replacing Na's with 0, since NA's are if there are no children with adults
#USA_data$CHILDREN_NUM_C[is.na(USA_data$CHILDREN_NUM_C)] <- 0


#replacing Y with 1, which acts as a flag and indicates there is a child with adults
#USA_data$NO_SHOW_FLAG_C[gsub("Y", 1, USA_data$NO_SHOW_FLAG_C)]
#View(USA_data)

#Replacing NA's with 0, for the booked rooms but no show
#USA_data$NO_SHOW_FLAG_C[is.na(USA_data$NO_SHOW_FLAG_C)] <- 0



#count Promoters
countPromoters <- length(as.numeric(which(USA_data$Recommendation==10 | USA_data$Recommendation==9)))

#count detactors
countDetractors <- length(as.numeric(which(USA_data$Recommendation <= 6)))

#Total number of responses

Total <- length(USA_data$Recommendation)
str(Total)

#Calculate NPS using one column and basic formula
NPS <- (countPromoters - countDetractors)*100/Total
NPS


#Creating a data frame of the columns that impact NPS


#to get the column number
which(colnames(USA_data) == "Recommendation")

#plot a graph to see the relationship between room occupancy and average daily rate
#plot(USA_data$average_daily_rate_CC, USA_data$occ_room_cnt_CC)

#Regression model to see the effect of daily rate on room occupancy
#samplemodel <- lm(formula = occ_room_cnt_CC ~ average_daily_rate_CC  , data = USA_data)
#summary(samplemodel)
#predict(samplemodel,newdata = USA_data)


#FUnction of NPS
PredictNPS <- function(promoters)
{
  countPromoters <- length(as.numeric(which(promoters==10 | USA_data$Recommendation==9)))
  
  countDetractors <- length(as.numeric(which(promoters <= 6)))
  nps <- (countPromoters - countDetractors)*100/Total
  nps
  
}
PredictNPS(USA_data$Recommendation)




#Plot likelihood to recommend with room type 
#plot(factor(plot_df1$sample_frame.ROOM_TYPE_DESCRIPTION_C),plot_df1$sample_frame.Likelihood_Recommend)
#unique(sample_frame$ROOM_TYPE_DESCRIPTION_C)

#######################################################################
#linear modeling function######################################
linear_model <- function(x,y)
{
samplemodel <- lm(formula = x ~ y  , data = USA_data)
s <- summary(samplemodel)
p <- plot(x=x, y=y)
}

linear_model1 <- lm(USA_data$Recommendation ~ USA_data$Satisfaction, data= USA_data )
summary(linear_model1)
ggplot(USA_data, aes(USA_data$Recommendation,USA_data$Satisfaction))+ geom_smooth("linear_model2")

plot(USA_data$Recommendation, USA_data$Satisfaction)
abline(linear_model1)

linear_model2 <- lm(USA_data$Recommendation ~ USA_data$Guest_satisfaction, data= USA_data )
summary(linear_model2)
library(ggplot2)
abline(linear_model2)


linear_model3 <- lm(USA_data$Recommendation ~ USA_data$Tranquility, data=USA_data )
summary(linear_model3)
plot(USA_data$Recommendation, USA_data$Tranquility)
abline(linear_model3)


linear_model4 <- lm(USA_data$Recommendation ~ USA_data$Hotel_condition, data = USA_data )
summary(linear_model4)
plot(USA_data$Recommendation, USA_data$Hotel_condition)
abline(linear_model4)

linear_model5 <- lm(USA_data$Recommendation ~ USA_data$Customer_service, data = USA_data )
summary(linear_model5)
plot(USA_data$Recommendation, USA_data$Customer_service)
abline(linear_model5)

linear_model6 <- lm(USA_data$Recommendation ~ USA_data$Customer_care, data= USA_data )
summary(linear_model6)
plot(USA_data$Recommendation, USA_data$Customer_care)
abline(linear_model6)

linear_model7 <- lm(USA_data$Recommendation ~ +USA_data$Fitness+USA_data$Limo+USA_data$Resort, data= USA_data )
summary(linear_model7)
plot(USA_data$Recommendation, USA_data$Customer_care)
abline(linear_model6)


#########Subset for linear model##########
linear_data <- subset(USA_data, select= c(4,6,8,9,10,12,13,27,30,31))
linear_final <- lm(Recommendation ~., data=linear_data) 
#plot(linear_final)
summary(linear_final)
ggplot(USA_data, aes(x=Guest_satisfaction, y=Recommendation, color=NPS_type)) +
  geom_smooth(method = "lm") + ylab("Recommended Rating") +  xlab(" Gues_Room ") +  ggtitle(" Effect of Overall F&B on Net Promoter Score")
ggplot(USA_data, aes(x=Overall_F.B, y=Recommendation, color=NPS_type)) +
  geom_smooth(method = "lm") + ylab("Recommended Rating") +  xlab(" Overall_F.B ") +  ggtitle(" Effect of Overall F&B on Net Promoter Score")


ggplot(linear_final, aes(Recommendation, Overall_F.B)) + geom_point()


library(ggplot2)
GG <-ggplot(data=USA_data,aes(x=Guest_satisfaction,y=Recommendation))
ScatterPLot<- GG +geom_point(aes(colour=NPS_type),shape=19,alpha=0.5,position = position_jitter(w=0.5,h=0.5))
ScatterPLot
GG <-ggplot(data=USA_data,aes(x=Customer_service,y=Recommendation))
ScatterPLot<- GG +geom_point(aes(colour=NPS_type),shape=19,alpha=0.5,position = position_jitter(w=0.5,h=0.5))
ScatterPLot
GG <-ggplot(data=USA_data,aes(x=Overall_F.B,y=Recommendation))
ScatterPLot<- GG +geom_point(aes(colour=NPS_type),shape=19,alpha=0.5,position = position_jitter(w=0.5,h=0.5))
ScatterPLot

###################################COuntry wise respondents######################################

library(sqldf)
library(ggplot2)
Temp_P <- sqldf('select count(*) as Total_Promoters ,Country
from Merged1 where NPS_type = "Promoter" Group By
                    Country')
Temp_P
Temp_D <- sqldf('select count(*) as Total_Detractors ,Country
                    from Merged1 where NPS_type = "Detractor" Group By
                    Country')
Temp_D

Temp_P_D <- sqldf('select a.Total_Promoters, b.Total_Detractors, a.Country from 
                      Temp_P a LEFT JOIN Temp_D b ON a.Country = b.Country')
Temp_P_D

Temp_Pa <- sqldf('select count(*) as Total_Passives ,Country from
Merged1 where NPS_Type = "Passive" Group By
                    Country')
Temp_Pa

Temp_P_D_Pa <- sqldf('select a.Total_Promoters, a.Total_Detractors, b.Total_Passives,
                          a.Country from Temp_P_D a LEFT JOIN Temp_Pa b ON a.Country=b.Country')
Respondents <- Temp_P_D_Pa$Total_Promoters+Temp_P_D_Pa$Total_Detractors+Temp_P_D_Pa$Total_Passives
Percentage_P <- Temp_P_D_Pa$Total_Promoters *100/Respondents
Percentage_D <- Temp_P_D_Pa$Total_Detractors *100/Respondents
Percentage_Pa <- Temp_P_D_Pa$Total_Passives *100/ Respondents
NPS_Country <- ((Temp_P_D_Pa$Total_Promoters-Temp_P_D_Pa$Total_Detractors)/ Respondents)*100
Temp_P_D_Pa$Percentage_P <- Percentage_P
Temp_P_D_Pa$Percentage_Pa <- Percentage_Pa
Temp_P_D_Pa$Percentage_D <- Percentage_D
Temp_P_D_Pa$Total_R <- Respondents
Temp_P_D_Pa$Total_Detractors[is.na(Temp_P_D_Pa$Total_Detractors)] <- 0
Temp_P_D_Pa$Percentage_P[is.na(Temp_P_D_Pa$Percentage_P)] <- 94
Temp_P_D_Pa$Percentage_D[is.na(Temp_P_D_Pa$Percentage_D)] <- 0
Temp_P_D_Pa$Percentage_Pa[is.na(Temp_P_D_Pa$Percentage_Pa)] <- 6
Temp_P_D_Pa$Total_R[is.na(Temp_P_D_Pa$Total_R)] <- 33
Temp_P_D_Pa$NPS <- NPS_Country
Temp_P_D_Pa$NPS[is.na(Temp_P_D_Pa$NPS)] <- 94

View(Temp_P_D_Pa)

ggplot(subset(Merged1,NPS_type=="Promoter"),aes(x=Country)) + 
  geom_bar() + ggtitle("Promoter Count") + 
  labs(x = "State", y = "Promoter Count") + coord_flip()

###Detractors####
ggplot(subset(Merged1, NPS_type=="Detractor"),aes(x=Country)) + 
  geom_bar() + ggtitle("Detractor Count") + 
  labs(x = "State", y = "Detractor Count") + coord_flip()

###############Distribution of NPS over country#######################################
ggplot(Temp_P_D_Pa, aes(x= Country, y = NPS)) + geom_point() +
  theme(axis.text.x = element_text(angle = 90,hjust = 1))

unique(Merged1$Country)
############################################################################################################

###########ARULES###############################################################################
install.packages("arules")
install.packages("arulesViz")
library(arulesViz)
library(arules)

USA_data$Guest_satisfaction_Category <- USA_data_value_category$Guest_satisfaction_category
USA_data$Hotel_condition_Category <- USA_data_value_category$Hotel_condition_category
USA_data$Overall_F.B_Category <- USA_data_value_category$Overall_F.B_category
USA_data$Customer_care_Category <- USA_data_value_category$Customer_care_category
USA_data$Quality_Checkin_Category <- USA_data_value_category$Quality_Checkin_category
View(USA_data)
Rules_data <- subset(USA_data, select= c(2,23:37))
Rules_data <- replace(Rules_data, TRUE, lapply(Rules_data, factor))

RuleSet <- apriori(Rules_data,parameter =list(support=0.5,confidence=0.8), appearance = list(rhs= c("NPS_type=Promoter", "NPS_type=Detractor")))
inspect(RuleSet)
RuleSet_df <- data.frame(LHS=labels(lhs(RuleSet)),RHS=labels(rhs(RuleSet)), quality(RuleSet))
View(RuleSet_df)
GoodRules <- RuleSet[quality(RuleSet)$lift >1.2]
GoodRulesGraph<- plot(GoodRules,method="graph",measure="support",shading="lift",interactive=TRUE)
inspect(GoodRules)

Rules_df <- data.frame(LHS=labels(lhs(GoodRules)),RHS=labels(rhs(GoodRules)), quality(GoodRules))
View(Rules_df)
Rules_P <- Rules_df[Rules_df$RHS=='{NPS_Type=Promoter}']
View(Rules_P)
##Convert facilities from char to number as###############################
### Y as 1 and N as 0

USA_data$Bell_staff <- ifelse(USA_data$Bell_staff== "Y",1,0)
USA_data$Business_center <- ifelse(USA_data$Business_center=='Y',1,0)
USA_data$Casino <- ifelse(USA_data$Casino=='Y',1,0)
USA_data$Conference <- ifelse(USA_data$Conference=='Y',1,0)
USA_data$Golf <- ifelse(USA_data$Golf=='Y',1,0)
USA_data$Limo <- ifelse(USA_data$Limo=='Y',1,0)
USA_data$Resort <- ifelse(USA_data$Resort=='Y',1,0)
USA_data$Fitness <- ifelse(USA_data$Fitness=='Y',1,0)
USA_data$Convention <- ifelse(USA_data$Convention=='Y',1,0)
View(USA_data)
facilities <- subset(USA_data, select = c(4,21:29))
lm(Recommendation ~ ., data =facilities)
#*******************************************************************************
install.packages("maps")
install.packages("ggmap")
library(maps)
library(ggmap)
library(ggplot2)
USmap <- ggplot2::map_data("state")

LikelihoodtoRecommendHeatMapYearly <-
  ggplot(USA_data,aes(map_id=region))+geom_map(
  map=USMap,aes(fill=AverageLikelihoodtoRecommend),col="white")+expand_li
mits(x=USMap$long,y=USMap$lat)+coord_map()+ggtitle("State wise
Likelihood of Recommendation")
LikelihoodtoRecommendHeatMapYearly




###################ksvm###############################################################
library(kernlab)
library(e1071)
dim(USA_data)
USA_data_value_category <-USA_data[c(6:13)]

USA_data_value_category$Overall_F.B <-as.numeric(USA_data_value_category$Overall_F.B)

values <- c("High","Medium", "Low")

USA_data_value_category$Guest_satisfaction_category <-(ifelse(USA_data_value_category$Guest_satisfaction>=8,"HIGH",
                                                              ifelse(USA_data_value_category$Guest_satisfaction>=5 & USA_data_value_category$Guest_satisfaction<8,"MEDIUM",
                                                                     ifelse(USA_data_value_category$Guest_satisfaction<5,"LOW",NA))))


USA_data_value_category$Tranquility_category <-(ifelse(USA_data_value_category$Tranquility>=8,"HIGH",
                                                       ifelse(USA_data_value_category$Tranquility>=5 & USA_data_value_category$Tranquility<8,"MEDIUM",
                                                              ifelse(USA_data_value_category$Tranquility<5,"LOW",NA))))

USA_data_value_category$Hotel_condition_category <-(ifelse(USA_data_value_category$Hotel_condition>=8,"HIGH",
                                                           ifelse(USA_data_value_category$Hotel_condition>=5 & USA_data_value_category$Hotel_condition<8,"MEDIUM",
                                                                  ifelse(USA_data_value_category$Hotel_condition<5,"LOW",NA))))

USA_data_value_category$Customer_service_category <-(ifelse(USA_data_value_category$Customer_service>=8,"HIGH",
                                                            ifelse(USA_data_value_category$Customer_service>=5 & USA_data_value_category$Customer_service<8,"MEDIUM",
                                                                   ifelse(USA_data_value_category$Customer_service<5,"LOW",NA))))

USA_data_value_category$Customer_care_category <-(ifelse(USA_data_value_category$Customer_care>=8,"HIGH",
                                                         ifelse(USA_data_value_category$Customer_care>=5 & USA_data_value_category$Customer_care<8,"MEDIUM",
                                                                ifelse(USA_data_value_category$Customer_care<5,"LOW",NA))))

USA_data_value_category $Interne_categoryt<-(ifelse(USA_data_value_category$Internet>=8,"HIGH",
                                                    ifelse(USA_data_value_category$Internet>=5 & USA_data_value_category$Internet<8,"MEDIUM",
                                                           ifelse(USA_data_value_category$Internet<5,"LOW",NA))))

USA_data_value_category$Quality_Checkin_category <-(ifelse(USA_data_value_category$Quality_Checkin>=8,"HIGH",
                                                           ifelse(USA_data_value_category$Quality_Checkin>=5 & USA_data_value_category$Quality_Checkin<8,"MEDIUM",
                                                                  ifelse(USA_data_value_category$Quality_Checkin<5,"LOW",NA))))

USA_data_value_category$Overall_F.B_category <-(ifelse(USA_data_value_category$Overall_F.B>=8,"HIGH",
                                                       ifelse(USA_data_value_category$Overall_F.B>=5 & USA_data_value_category$Overall_F.B<8,"MEDIUM",
                                                              ifelse(USA_data_value_category$Overall_F.B<5,"LOW",NA))))
USA_data_value_category$state <- USA_data$State
USA_data_model <- subset(USA_data_value_category)
View(USA_data_value_category)
NPS_type_number<- ifelse(USA_data$NPS_type=="Promoter",10,ifelse(USA_data$NPS_type=="Passive",8,ifelse(USA_data$NPS_type=="Detractor",6,NA)))
USA_data$NPS_type_no <- NPS_type_number

ksvm_data <- subset(USA_data, select=c(2,23:37))
random.indexes <- sample(1:nrow(ksvm_data))
cutPoint2_3 <- floor(nrow(ksvm_data)/3*2)

#Creating test and train datasets for future computation
SVM_data.train <- ksvm_data[random.indexes[1:cutPoint2_3],]
SVM_data.test <- ksvm_data[random.indexes[(cutPoint2_3+1):nrow(ksvm_data)],]
ksvm_model <- ksvm(NPS_type ~ .,data=SVM_data.train, kernel = "rbfdot",kpar="automatic", C=5, cross=10,prob.model=T)
 pred <- predict(ksvm_model, SVM_data.test)
 
 pred_NPS_type <- data.frame(pred, SVM_data.test$NPS_type)
 result <- table(pred,SVM_data.test$NPS_type)
 View(result)
 Accuracy <- (result[1,1]+result[2,2])/(result[1,1]+result[1,2]+result[2,1]+result[2,2])*100
 Accuracy
##########################################################################



length(which(USA_data$NPS_type== "Promoter")),



Country1 <- subset(USA_data$State, USA_data$Country=="United States")
library(sqldf)
sqldf("Select NPS_type from USA_data where State='Texas'&& NPS_type='Promoter'" )

length(which(USA_data$NPS_type=="Promoter" | USA_data$State== "Texas"))
################################################
tapply(Texas, Type, length)
Texas <- which(USA_data$State=="Texas")
Type <- subset(USA_data, USA_data$State=="Texas")
#################################################
###COnverting allthe amenities value: Y as 1 and N as 0




#StateCloudFrame <-  SpecifiedFreqMatrix Merged1$Country

UserStateTreeMap<- treemap(StateCloudFrame,index =
                             c("Name"),vSize="freq",type="index",palette = "Dark2",title = "State
                           wise Customer distribution",fontsize.title = 14,fontsize.labels =
                             12,border.col = "white")

install.packages("tm")
library(tm)
Hotel_distribution <- tapply(USA_data$Country, USA_data$Hotel_Name, length)

#frequency <- function(f){
#Source <- VectorSource(f)
#wordcorpus <- Corpus(Source)
#temp_matrix <- TermDocumentMatrix(wordcorpus)
#Matrix <- as.matrix(temp_matrix)
#Sum <- rowSums(Matrix)
#Count <- sort(Sum, decreasing = T)
#df<- data.frame(name=names(Count), freq = Count)
#return(df)
#}

##State in which most hotels are booked
#frequency(USA_data$State)

#install.packages("treemap")
#library(treemap)
#UserStateTreeMap<- treemap(df,index =
 #                            c(name),vSize="freq",type="index",palette = "Dark2",title = "State
#                           wise Customer distribution",fontsize.title = 14,fontsize.labels =
 #                            12,border.col = "white")


Stat <- unique(USA_data$State)
tapply(USA_data$NPS_type, USA_data$State, unique)
tapply(Stat, USA_data$NPS_type, length)


library(sqldf)
State_respondents <- sqldf("Select Count(NPS_type),State from USA_data GROUP BY State")
State_respondents
percentage <- State_respondents$`Count(NPS_type)`*100/ sum(State_respondents$`Count(NPS_type)`)







#############################Separation of NPS-type in USA_data###############################################

Temp_NPS_P <- sqldf('select count(*) as Total_Promoters ,State
from USA_data where NPS_type = "Promoter" Group By
            State')
Temp_NPS_P
Temp_NPS_D <- sqldf('select count(*) as Total_Detractors ,State
                  from USA_data where NPS_type = "Detractor" Group By
                  State')
Temp_NPS_D

Texas <- which(USA_data$State=="Texas")
Type <- subset(USA_data$NPS_type, USA_data$State=="Texas")
tapply(Texas, Type, length)


Temp_NPS_P_D <- sqldf('select a.Total_Promoters, b.Total_Detractors, a.State from 
Temp_NPS_P a LEFT JOIN Temp_NPS_D b ON a.State = b.State')




Temp_NPS_Pa <- sqldf('select count(*) as Total_Passives ,State from
USA_data where NPS_Type = "Passive" Group By
                    State')
Temp_NPS_Pa

Temp_NPS_P_D_Pa <- sqldf('select a.Total_Promoters, a.Total_Detractors, b.Total_Passives,
                          a.State from Temp_NPS_P_D a LEFT JOIN Temp_NPS_Pa b ON a.State=b.State')
Total_Respondents <- Temp_NPS_P_D_Pa$Total_Promoters+Temp_NPS_P_D_Pa$Total_Detractors+Temp_NPS_P_D_Pa$Total_Passives
Percentage_Promoter <- Temp_NPS_P_D_Pa$Total_Promoters *100/Total_Respondents
Percentage_Detractor <- Temp_NPS_P_D_Pa$Total_Detractors *100/Total_Respondents
Percentage_Passive <- Temp_NPS_P_D_Pa$Total_Passives *100/ Total_Respondents

##Promoters###
ggplot(subset(USA_data,NPS_type=="Promoter"),aes(x=State)) + 
  geom_bar() + ggtitle("Promoter Count") + 
  labs(x = "State", y = "Promoter Count") + coord_flip()

###Detractors####
ggplot(subset(USA_data, NPS_type=="Detractor"),aes(x=State)) + 
  geom_bar() + ggtitle("Detractor Count") + 
  labs(x = "State", y = "Detractor Count") + coord_flip()


NPS_State <- ((Temp_NPS_P_D_Pa$Total_Promoters+Temp_NPS_P_D_Pa$Total_Detractors)/ Total_Respondents)*100
Temp_NPS_P_D_Pa$Percentage_Promoters <- Percentage_Promoter
Temp_NPS_P_D_Pa$Percentage_Detractors <- Percentage_Detractor
Temp_NPS_P_D_Pa$Percentage_Passives <- Percentage_Passive
Temp_NPS_P_D_Pa$Total <- Total_Respondents
Temp_NPS_P_D_Pa$NPS <- NPS_State
View(Temp_NPS_P_D_Pa)

#######Scatter- Distribution of NPS Over State########################################
ggplot(Temp_NPS_P_D_Pa, aes(x= State, y = NPS)) + geom_point() +
  theme(axis.text.x = element_text(angle = 90,hjust = 1))
#######################################################################


## COmparison of Overall Satisfaction and Recommendation
GG <-ggplot(data=USA_data,aes(x=Hotel_condition,y=Recommendation))
ScatterPLot<- GG +geom_point(aes(colour=NPS_type),shape=19,alpha=0.5,position = position_jitter(w=0.5,h=0.5))
ScatterPLot

########### Texas data as per purpose of visit################
TexasDataSet <- sqldf("select count(Purpose) Total_People,
                    Purpose from USA_data where
                    State='Texas' group by Purpose")
TexasDataSet

gplot <- ggplot(TexasDataSet, aes(x = Purpose))+geom_bar(aes(y=Total_People),stat="identity",fill="green")
gplot <- gplot+ggtitle("Purpose of Visit to Texas")
gplot
###################################################################################3


