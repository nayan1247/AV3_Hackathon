#Load Libraries
library(caret)
library(class)
library(ggplot2)
library(sqldf)
library(Metrics)

getwd()
rm(list=ls())
setwd('C:/Users/Nayan Dharamshi/Google Drive/AV3')

#Read Training and Testing File
training_read<-read.csv('Train.csv')
testing_read<-read.csv('Test.csv')

#Outlier Treatment - Remove the values >21000 which are very high and biasing the linear model
training_read<-training_read[-which(training_read$Project_Valuation>21000),]


#Vector for Insitute using Lat,Long and County
#Create a probability vector for each college which will be used in the Linear Model
col_vec<-sqldf("select distinct institute_latitude,institute_longitude,institute_country,sum(Project_Valuation)/count(*) sm from training_read group by 1,2,3 order by sm desc")
col_vec$sm_scaled<-(max(col_vec$sm)-col_vec$sm)/(max(col_vec$sm)-min(col_vec$sm))
col_vec$sm_scaled<-1-col_vec$sm_scaled

training_read<-sqldf("select A.*,B.sm_scaled from training_read as A left join col_vec as B on A.institute_latitude=B.institute_latitude and A.institute_longitude=B.institute_longitude and A.institute_country=B.institute_country ")
testing_read<-sqldf("select A.*,B.sm_scaled from testing_read as A left join col_vec as B on A.institute_latitude=B.institute_latitude and A.institute_longitude=B.institute_longitude and A.institute_country=B.institute_country ")

# Vector Creation for Categorical variables on training_read
# ----------------------Create Vectors from Categorical Columns
#Create a Subject Area Index Vector
sub_vct_calc<-sqldf("select subject_area,sum(Project_Valuation) /count(*) as vct from training_read group by 1 order by vct desc")
sub_vct_calc$subject_vector<-sub_vct_calc$vct/sum(sub_vct_calc$vct)


#Create a Secondary Area Index Vector
sec_area_vct_calc<-sqldf("select secondary_area,sum(Project_Valuation) /count(*) as vct from training_read group by 1 order by vct desc")
sec_area_vct_calc$sec_area_vector<-sec_area_vct_calc$vct/sum(sub_vct_calc$vct)
#Create a project_subject Index Vector
project_subject_vct_calc<-sqldf("select project_subject,sum(Project_Valuation) /count(*) as vct from training_read group by 1 order by vct desc")
project_subject_vct_calc$project_subject_vector<-project_subject_vct_calc$vct/sum(project_subject_vct_calc$vct)


#Create a Secondary Subject  Index Vector
sec_sub_vct_calc<-sqldf("select secondary_subject,sum(Project_Valuation) /count(*) as vct from training_read group by 1 order by vct desc")
sec_sub_vct_calc$sec_subject_vector<-sec_sub_vct_calc$vct/sum(sub_vct_calc$vct)


#Create a County Vector
county_vct_calc<-sqldf("select institute_country,sum(Project_Valuation) /count(*) as vct from training_read group by 1 order by vct desc")
county_vct_calc$county_vector<-county_vct_calc$vct/sum(county_vct_calc$vct)


#Create a City Vector
city_vct_calc<-sqldf("select institute_city,sum(Project_Valuation)/count(*) as vct from training_read group by 1 order by vct desc")
city_vct_calc$city_vector<-city_vct_calc$vct/sum(city_vct_calc$vct)

#Create a Resource Category
rc_vct_calc<-sqldf("select Resource_Category,sum(Project_Valuation)/count(*) as vct from training_read group by 1 order by vct desc")
rc_vct_calc$rc_vector<-rc_vct_calc$vct/sum(rc_vct_calc$vct)

#Create a Resource sub Category
rsc_vct_calc<-sqldf("select Resource_Sub_Category,sum(Project_Valuation)/count(*) as vct from training_read group by 1 order by vct desc")
rsc_vct_calc$rsc_vector<-rsc_vct_calc$vct/sum(rsc_vct_calc$vct)

#Create a State vector
state_vct_calc<-sqldf("select institute_state,sum(Project_Valuation)/count(*) as vct from training_read group by 1 order by vct desc")
state_vct_calc$state_vector<-state_vct_calc$vct/sum(state_vct_calc$vct)

#Create a Var8 Vector
var8_vct_calc<-sqldf("select Var8,sum(Project_Valuation)/count(*) as vct from training_read group by 1 order by vct desc")
var8_vct_calc$var8_vector<-var8_vct_calc$vct/sum(state_vct_calc$vct)

#Create a Var15 Vector
var15_vct_calc<-sqldf("select Var15,sum(Project_Valuation)/count(*) as vct from training_read group by 1 order by vct desc")
var15_vct_calc$var15_vector<-var15_vct_calc$vct/sum(state_vct_calc$vct)

#Create a Var23 Vector
var23_vct_calc<-sqldf("select Var23,sum(Project_Valuation)/count(*) as vct from training_read group by 1 order by vct desc")
var23_vct_calc$var23_vector<-var23_vct_calc$vct/sum(state_vct_calc$vct)

#Create a Var24 Vector
var24_vct_calc<-sqldf("select Var24,sum(Project_Valuation)/count(*) as vct from training_read group by 1 order by vct desc")
var24_vct_calc$var24_vector<-var24_vct_calc$vct/sum(state_vct_calc$vct)


# Creating vectors in training_read and testing read using above
# For training_read and testing_read
# -----------------------------
# Subject
training_read<-sqldf("select A.*,B.subject_vector from training_read as A left join sub_vct_calc as B on A.subject_area=B.subject_area;")
testing_read<-sqldf("select A.*,B.subject_vector from testing_read as A left join sub_vct_calc as B on A.subject_area=B.subject_area;")
#County
training_read<-sqldf("select A.*,B.county_vector from training_read as A left join county_vct_calc as B on A.institute_country=B.institute_country;")
testing_read<-sqldf("select A.*,B.county_vector from testing_read as A left join county_vct_calc as B on A.institute_country=B.institute_country;")
# City
training_read<-sqldf("select A.*,B.city_vector from training_read as A left join city_vct_calc as B on A.institute_city=B.institute_city;")
testing_read<-sqldf("select A.*,B.city_vector from testing_read as A left join city_vct_calc as B on A.institute_city=B.institute_city;")
#RC
training_read<-sqldf("select A.*,B.rc_vector from training_read as A left join rc_vct_calc as B on A.Resource_Category=B.Resource_Category;")
testing_read<-sqldf("select A.*,B.rc_vector from testing_read as A left join rc_vct_calc as B on A.Resource_Category=B.Resource_Category;")

#Project_subject - project_subject_vct_calc$project_subject_vector
training_read<-sqldf("select A.*,B.project_subject_vector from training_read as A left join project_subject_vct_calc as B on A.project_subject=B.project_subject;")
testing_read<-sqldf("select A.*,B.project_subject_vector from testing_read as A left join project_subject_vct_calc as B on A.project_subject=B.project_subject;")

training_read$v1<-training_read$Similar_Project_Valuation_other_institute^2
training_read$v2<-training_read$Similar_Project_Valuation_other_institute^3
training_read$v3<-(log(training_read$Similar_Project_Valuation_other_institute))^2

testing_read$v1<-testing_read$Similar_Project_Valuation_other_institute^2
testing_read$v2<-testing_read$Similar_Project_Valuation_other_institute^3
testing_read$v3<-(log(testing_read$Similar_Project_Valuation_other_institute))^2


#Training and Validate sample
training_read<-training_read[,-1]
set.seed(1234)
training_readIndex <- caret::createDataPartition(training_read$Project_Valuation, p = .7,list = FALSE,times = 1)
train <- training_read[ training_readIndex,]
validate  <- training_read[-training_readIndex,]

#Model 1 
# -----------------------------------------
# -----------------------------------------

#Run a Linear Model on Train
names(train)

lm_model<-lm(Project_Valuation~ v3*v1*v2+project_subject_vector+rc_vector+sm_scaled,data=train)
summary(lm_model)

#validate on Validation set and check error metrics
pred_cal<-predict(lm_model,newdata=validate)
summary(pred_cal)
rmse(pred_cal,validate$Project_Valuation)


#Train a linear model on entire data
lm_model<-lm(Project_Valuation~ v3*v1 * v2 +subject_vector+county_vector+rc_vector,data=training_read)
summary(lm_model)
#Score on the Testing data 
testing_read$Project_Valuation_LM1<-predict(lm_model,newdata=testing_read)

#Some fixing - All neg values to zero and all NA's to 0
testing_read$Project_Valuation_LM1[testing_read$Project_Valuation_LM1<0]<-0
testing_read$Project_Valuation_LM1[which(is.na(testing_read$Project_Valuation_LM1))]<-0
summary(testing_read$Project_Valuation_LM1)


# Model 2
# ------------------------------------
# ------------------------------------
#Get the Linear Model 2 - Squeeze information from Linear Variable Similar Project
# in areas where it is highly correlated with Project Valuation
# -------------------------------------
train3<-train[train$Similar_Project_Valuation_other_institute<=118 | train$Similar_Project_Valuation_other_institute>=300,]
validate3<-validate[validate$Similar_Project_Valuation_other_institute<=118 | validate$Similar_Project_Valuation_other_institute>=300,]

train3$v1<-train3$Similar_Project_Valuation_other_institute^3
validate3$v1<-validate3$Similar_Project_Valuation_other_institute^3
train3$v2<-train3$Similar_Project_Valuation_other_institute^2
validate3$v2<-validate3$Similar_Project_Valuation_other_institute^2



lm_when_similar_gr_300_lt_118<-lm(Project_Valuation~Similar_Project_Valuation_other_institute*v1+v2,data=train3)
summary(lm_when_similar_gr_300_lt_118)
Project_Valuation_300_118<-predict(lm_when_similar_gr_300_lt_118,newdata=validate3)
summary(Project_Valuation_300_118)
rmse(validate3$Project_Valuation,Project_Valuation_300_118)

# On training_read data

training_read3<-training_read[training_read$Similar_Project_Valuation_other_institute<=118 | training_read$Similar_Project_Valuation_other_institute>=300,]

training_read3$v1<-training_read3$Similar_Project_Valuation_other_institute^3
testing_read$v1<-testing_read$Similar_Project_Valuation_other_institute^3
training_read3$v2<-training_read3$Similar_Project_Valuation_other_institute^2
testing_read$v2<-testing_read$Similar_Project_Valuation_other_institute^2


lm_when_similar_gr_300_lt_118<-lm(Project_Valuation~Similar_Project_Valuation_other_institute*v1+v2,data=training_read3)
# summary(lm_when_similar_gr_300_lt_118)
testing_read$Project_Valuation_300_118<-predict(lm_when_similar_gr_300_lt_118,newdata=testing_read)
# summary(testing_read$Project_Valuation_300_118)



# Ensemble and Prepare for Submission
# -------------------------------------
# -------------------------------------

#Prepare file for submission
outfile_linear<-testing_read[,c("ID","Project_Valuation_LM1","Project_Valuation_300_118","Similar_Project_Valuation_other_institute")]

#Create Ensemble
outfile_linear$Project_Valuation<-rep(0,nrow(outfile_linear))
outfile_linear$Ave<-(outfile_linear$Project_Valuation_300_118+outfile_linear$Project_Valuation_LM1)/2
outfile_linear$Project_Valuation[outfile_linear$Similar_Project_Valuation_other_institute>300 | outfile_linear$Similar_Project_Valuation_other_institute< 118]<-outfile_linear$Project_Valuation_300_118[outfile_linear$Similar_Project_Valuation_other_institute>300 | outfile_linear$Similar_Project_Valuation_other_institute< 118]
outfile_linear$Project_Valuation[outfile_linear$Similar_Project_Valuation_other_institute<=300 & outfile_linear$Similar_Project_Valuation_other_institute>= 118]<-outfile_linear$Project_Valuation_LM1[outfile_linear$Similar_Project_Valuation_other_institute<=300 & outfile_linear$Similar_Project_Valuation_other_institute>= 118]


outfile_linear$Project_Valuation[outfile_linear$Project_Valuation<0]<-0
summary(outfile_linear$Project_Valuation)

outfile<-outfile_linear[,c("ID","Project_Valuation")]
write.csv(outfile,"./Sunday/Final_check5.csv",row.names=FALSE)
