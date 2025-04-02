library(readxl)
library(dplyr)
library(ggplot2)
library(Information)
install.packages("fastDummies")
library(fastDummies)
library(car)
library(data.table)
library(caret)
# install.packages("pROC")
library(pROC)
#########################
## step 1: loading data
#########################
term_data= read.csv("Term_Loan _sub.csv", stringsAsFactors = TRUE)
str(term_data)
table(term_data$Term_Dep,useNA = "always")
summary(term_data)

for (i in colnames(term_data)) {
  
  print(paste("Sum of NA values for attribute",i,"is:",sum(is.na(term_data[,i]))))
  print(paste("Sum of NULL values for attribute",i,"is:",sum(is.null(term_data[,i]))))
}

### no null values
########################################
## step 2: test n control data creation
########################################

set.seed(123)  # For reproducibility
sample_index <- sample(1:nrow(term_data), size = 0.7 * nrow(term_data))
train_data <- term_data[sample_index, ]
test_data <- term_data[-sample_index, ]

##########################################
##step 3: Categorical variable selection
##########################################
categorical_attribute= c("job","marital","education","default","housing","loan","contact","month","day_of_week","poutcome")

for (i in categorical_attribute) {
  table_cat=table(term_data[,i], term_data$Term_Dep)
  print(paste("Chi-sq for ",i,"is:",round(chisq.test(table_cat)$p.value,10)))
  
}

## only loan and housing are not statistically significant, which should not be the case. Term loan should rely heavily on housing and loan

train_data2 <- train_data[, names(train_data) %in% categorical_attribute]
train_data2 <- cbind(train_data2,train_data$Term_Dep)
colnames(train_data2) <- c(categorical_attribute,"Term_Dep")
train_data2 <- train_data2[, !names(train_data2) %in% c("housing", "loan")]
str(train_data2)

table(train_data$education,train_data$Term_Dep) ## since in chi square we saw education and default had few values for some categories, so we will reduce the categories in these two categorical variables

## education
table(train_data2$education)
train_data2$education=ifelse(train_data2$education=="illiterate","unknown",as.character.factor(train_data2$education))
table(train_data2$education)

## default

table(train_data2$default)
train_data2$default=ifelse(train_data2$default=="yes","unknown",as.character.factor(train_data2$default))
table(train_data2$default)


##########################################
##step 4: Numerical variable selection
##########################################

numerical_attributes <- colnames(train_data)[!names(train_data) %in% categorical_attribute]
## check
categorical_attribute==numerical_attributes

ggplot(train_data,aes(x=age, fill = Term_Dep))+geom_histogram()+stat_bin(bins = 30)+labs(title = "Age with term_dep")
ggplot(train_data,aes(x=duration, fill = Term_Dep))+geom_histogram()+stat_bin(bins = 30)+labs(title = "Duration with term_dep")


## information value for numerical variables
train_data3= train_data[, names(train_data) %in% numerical_attributes]
train_data3$Term_Dep_binary <- ifelse(train_data3$Term_Dep == "yes", 1, 0) # Creating binary outcome
#train_data2 <- cbind(train_data2,train_data$Term_Dep)
train_data2$Term_Dep_binary <- ifelse(train_data2$Term_Dep == "yes", 1, 0) # Creating binary outcome
#train_data2$Term_Dep_binary <- as.factor(train_data2$Term_Dep_binary)
str(train_data3)
str(train_data2)
# Calculate IV
IV <- create_infotables(data = train_data2, y = "Term_Dep_binary", bins = 10, parallel = FALSE)
IV_3 <- create_infotables(data = train_data3, y = "Term_Dep_binary", bins = 10, parallel = FALSE)
# View results
print(IV$Summary)
print(IV_3$Summary)

## since campaign is a variable which has IV , 0.1 so we will drop that variable
train_data3 <- train_data3[, !(names(train_data3) %in% c("campaign"))]
train_data3 <- train_data3[, !(names(train_data3) %in% c("Term_Dep","Term_Dep_binary"))]
str(train_data3)

all_attributes <- c(colnames(train_data2),colnames(train_data3)) ## three variables have dropped loan, housing and campaign
all_attributes

train_data_new = cbind(train_data2,train_data3)
final_attributes = colnames(train_data_new)
#########################################################################
## step 5: creating indicator variable for important categorical variable
#########################################################################

categorical_attribute_new= c("job","marital","education","default","contact","month","day_of_week","poutcome")

for (i in categorical_attribute_new) {
  print(paste("Table for categorical variables:",i))
  print(table(train_data_new[,i],train_data_new$Term_Dep_binary))
  train_data_new <- dummy_cols(train_data_new, select_columns = i, remove_first_dummy = TRUE)
}

train_data_new <- train_data_new[,!names(train_data_new) %in% categorical_attribute_new]
str(train_data_new)

#########################################################################
## step 6: Step wise regression
#########################################################################

full_model <- glm(Term_Dep_binary~., data = train_data_new, family = binomial(link = "logit"))
summary(full_model)

no_model <- glm(Term_Dep_binary~1, data = train_data_new, family = binomial(link = "logit"))
summary(no_model)

logistic_steps = step(no_model,
                      scope=list(lower=formula(no_model),
                                 upper=formula(full_model)),direction="both",
                      family=binomial(link="logit"))
summary(logistic_steps)
logistic_steps$anova 



Select_model<-glm(Term_Dep_binary ~ duration + month_may + poutcome_success + month_mar + 
                    emp_var_rate + poutcome_nonexistent + `job_blue-collar` + 
                    month_nov + pdays + default_unknown + month_jul + month_aug + 
                    cons_price_idx + contact_telephone + euribor3m + job_retired + 
                    day_of_week_mon + job_student + education_university.degree + 
                    day_of_week_wed + education_unknown + month_jun + cons_conf_idx + 
                    job_unemployed + marital_married,
                  data=train_data_new,
                  family=binomial(link = "logit"))
summary(Select_model)


#########################################################################
## step 7: Multicollinearity
#########################################################################

sort(vif(Select_model))

attribute_cor= c("Term_Dep_binary","emp_var_rate","month_nov","cons_price_idx","day_of_week_mon","day_of_week_wed","job_unemployed","duration","month_may","poutcome_nonexistent","pdays","contact_telephone","job_student","education_unknown","marital_married","poutcome_success","`job_blue-collar`","default_unknown","euribor3m","education_university.degree","month_jun","month_mar","month_jul","job_retired","cons_conf_idx","month_aug")

train_data_new_cor = train_data_new[,names(train_data_new) %in% attribute_cor]

test_cor = train_data_new[,names(train_data_new) %in% c("Term_Dep_binary","emp_var_rate","month_nov","cons_price_idx")]
cor(test_cor)
pairs(test_cor)
# par(mar = c(bottom, left, top, right))  # Default is c(5, 4, 4, 2) + 0.1
# 
# # Example: More space at bottom, less at top
# par(mar = c(6, 4, 2, 2))
# plot(x, y)
#pairs(train_data_new_cor)


final_model_select=glm(formula = Term_Dep_binary ~ duration + nr_employed + month_may + pdays  + `job_blue-collar`  + job_retired +  default_unknown, data = train_data_new,family = binomial(link="logit"))											
summary(final_model_select)
vif(final_model_select)

###################################################
## step 8: Generate score using logistic regression
##################################################
train_data_new$predicted_val = predict(final_model_select)
train_data_new$prob= exp(train_data_new$predicted_val)/(1+exp(train_data_new$predicted_val))
range(train_data_new$prob)

ggplot(train_data_new, aes(x=predicted_val))+geom_histogram()+stat_bin(bins=30)
ggplot(train_data_new,aes(x=prob))+geom_histogram()+stat_bin(bins=30)

train_data_new$Term_Dep_binary= ifelse(train_data_new$Term_Dep_binary==1,"Y","N")

ggplot(train_data_new,aes(x=prob,fill = Term_Dep_binary))+geom_histogram()+stat_bin(bins=30)

###############################################
## step 9: KS calculation
###############################################

train_data_new$p_rank_tmp=rank(x = train_data_new$prob,na.last =FALSE,
                        ties.method = c("average"))


summary(train_data_new$p_rank_tmp)
#ggplot(train_data_new, aes(x=p_rank_tmp))+geom_histogram()+stat_bin(bins=100)
## testing our model
train_data_new$resp_predict = ifelse(train_data_new$prob>0.6,"yes","no") ## setting the cutoff prob at 60%, if above then yes and below then no

predicted_tr=as.factor(train_data_new$resp_predict)
actual_tr=as.factor(train_data_new$Term_Dep)

conf_matrix_caret <- confusionMatrix(predicted_tr,actual_tr, positive = "yes")
print(conf_matrix_caret)

conf_matrix <- table(actual_tr, predicted_tr)
print(conf_matrix)
conf_df <- as.data.frame(conf_matrix)

ggplot(conf_df, aes(x = actual_tr, y = predicted_tr, fill = Freq)) +
  geom_tile() +
  geom_text(aes(label = Freq), color = "white") +
  scale_fill_gradient(low = "blue", high = "red") +
  labs(title = "Confusion Matrix for train dataset", x = "Actual", y = "Predicted")

## AUC
# Create ROC curve
roc_obj_tr <- roc(actual_tr, train_data_new$prob)

# Plot ROC curve
plot(roc_obj_tr, main = "ROC Curve for Test dataset", print.auc = TRUE, auc.polygon = TRUE)


###################################
## step 10: Stability check
##################################
#categorical_attribute_new= c("job","marital","education","default","contact","month","day_of_week","poutcome")
test_data$Term_Dep_binary =ifelse(test_data$Term_Dep=="yes",1,0)

test_data_new <- test_data[,names(test_data)%in% final_attributes]
## education
table(test_data_new$education)
test_data_new$education=ifelse(test_data_new$education=="illiterate","unknown",as.character.factor(test_data_new$education))
table(test_data_new$education)

## default

table(test_data_new$default)
test_data_new$default=ifelse(test_data_new$default=="yes","unknown",as.character.factor(test_data_new$default))
table(test_data_new$default)


for (i in categorical_attribute_new) {
  print(paste("Table for categorical variables:",i))
  print(table(test_data[,i],test_data$Term_Dep_binary))
  test_data_new <- dummy_cols(test_data_new, select_columns = i, remove_first_dummy = TRUE)
  #test_data_new <- cbind(test_data,dummy_cols(test_data, select_columns = i, remove_first_dummy = TRUE))
  
}


test_data_new <- test_data_new[,!names(test_data_new) %in% categorical_attribute_new]
colnames(test_data_new)

model_test_set=glm(formula = Term_Dep_binary ~ duration + nr_employed + month_may + pdays  + `job_blue-collar`  + job_retired +  default_unknown, data = test_data_new,family = binomial(link="logit"))	
summary(model_test_set)


test_data_new$predicted_val = predict(model_test_set)
test_data_new$prob= exp(test_data_new$predicted_val)/(1+exp(test_data_new$predicted_val))
range(test_data_new$prob)

test_data_new$resp_predict = ifelse(test_data_new$prob>0.6,"yes","no") ## setting the cutoff prob at 60%, if above then yes and below then no

############## testing the output

## confusion matrix
predicted=as.factor(test_data_new$resp_predict)
actual=as.factor(test_data_new$Term_Dep)

conf_matrix_caret <- confusionMatrix(predicted,actual, positive = "yes")
print(conf_matrix_caret)

conf_matrix <- table(actual, predicted)
print(conf_matrix)
conf_df <- as.data.frame(conf_matrix)

ggplot(conf_df, aes(x = actual, y = predicted, fill = Freq)) +
  geom_tile() +
  geom_text(aes(label = Freq), color = "white") +
  scale_fill_gradient(low = "blue", high = "red") +
  labs(title = "Confusion Matrix for test dataset", x = "Actual", y = "Predicted")

## AUC
# Create ROC curve
roc_obj <- roc(actual, test_data_new$prob)

# Plot ROC curve
plot(roc_obj, main = "ROC Curve for Test dataset", print.auc = TRUE, auc.polygon = TRUE)




