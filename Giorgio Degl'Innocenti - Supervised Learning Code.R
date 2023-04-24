#### Libraries ####
library(tidyverse)
library(dplyr)
library(caret)
library(ggplot2)
library(pROC)
library(MASS)
library(ggcorrplot)
library(car)
library(tidymodels)
library(rpart.plot)
library(vip)
library(baguette)

#### Importing Dataset ####

data <- read.table("C:\\Users\\giorg\\Desktop\\College.csv", sep  = ',', header = TRUE)

summary(data)

data %>% 
  DataExplorer::create_report()

data %>% glimpse()

### Preprocessing ####
#transform ordinal variable from character to numeric

unique(data[c("interest")])

data <- data %>% 
  mutate(
    interest = as.factor(factor(interest, order = TRUE, level = c('Not Interested','Less Interested','Uncertain','Quiet Interested','Very Interested'))),
    school_accreditation = as.factor(factor(school_accreditation, ordered = TRUE, levels = c('B','A'))),
    type_school = as.factor(factor(type_school, level = c('Vocational', 'Academic'))),
    gender = as.factor(factor(gender)),
    residence = as.factor(factor(residence)),
    parent_was_in_college = as.factor(factor(parent_was_in_college)),
    in_college = as.factor(factor(in_college))
  )
# checking normality
shapiro.test(data$parent_age) #reject h0 (is not normally distributed)
shapiro.test(data$parent_salary) #accept h0 ( normally distributed)
shapiro.test(data$house_area) #reject h0 (is not normally distributed)
shapiro.test(data$average_grades) #reject h0 (is not normally distributed)


# checking correlation

model.matrix(in_college~., data=data) %>% 
  cor() %>% 
  ggcorrplot(show.diag = F, type="upper", lab=TRUE, lab_size=2)
# some correlation between av grades and parent salary and house_area (higher lifestyle hard do tell)
# also some importnt between age and salary weird because it's negative

####*Addiotional relation####
#Numerical Variable

ggplot(data, aes(x = parent_age, fill = in_college)) +
  geom_histogram(binwidth = 1) +
  labs(x = "Parent Age",
       title = "Went to college by parent age")


ggplot(data, aes(x = parent_salary, fill = in_college)) +
  geom_histogram(binwidth = 200000) +
  labs(x = "Parent salary",
       title = "Went to college by parent salary")
#notice a positive relationship

ggplot(data, aes(x = house_area, fill = in_college)) +
  geom_histogram(binwidth = 2) +
  labs(x = "House Area",
       title = "Went to college by house area")
#same but is salary correlated with house area

ggplot(data, aes(x = average_grades, fill = in_college)) +
  geom_histogram(binwidth = 1) +
  labs(x = "Average Grade",
       title = "Went to college by average grades")
#still we see a positive relation between av grade and go to college


#Categorical variable
ggplot(data, aes(x = type_school)) +
  geom_bar(aes(fill = in_college)) +
  geom_text(aes(y = ..count.. -200, 
                label = paste0(round(prop.table(..count..),4) * 100, '%')), 
            stat = 'count', 
            position = position_dodge(.1), 
            size = 3)
#higher count on academic (but also more student)

ggplot(data, aes(x = school_accreditation)) +
  geom_bar(aes(fill = in_college)) +
  geom_text(aes(y = ..count.. -200, 
                label = paste0(round(prop.table(..count..),4) * 100, '%')), 
            stat = 'count', 
            position = position_dodge(.1), 
            size = 3)
#no clear division

ggplot(data, aes(x = gender)) +
  geom_bar(aes(fill = in_college)) +
  geom_text(aes(y = ..count.. -200, 
                label = paste0(round(prop.table(..count..),4) * 100, '%')), 
            stat = 'count', 
            position = position_dodge(.1), 
            size = 3)
#no clear division

ggplot(data, aes(x = interest)) +
  geom_bar(aes(fill = in_college)) +
  geom_text(aes(y = ..count.. -200, 
                label = paste0(round(prop.table(..count..),4) * 100, '%')), 
            stat = 'count', 
            position = position_dodge(.1), 
            size = 3)
#quiet interested pretty low value other seems more than half go

ggplot(data, aes(x = residence)) +
  geom_bar(aes(fill = in_college)) +
  geom_text(aes(y = ..count.. -200, 
                label = paste0(round(prop.table(..count..),4) * 100, '%')), 
            stat = 'count', 
            position = position_dodge(.1), 
            size = 3)
#no clear division

ggplot(data, aes(x = parent_was_in_college)) +
  geom_bar(aes(fill = in_college)) +
  geom_text(aes(y = ..count.. -200, 
                label = paste0(round(prop.table(..count..),4) * 100, '%')), 
            stat = 'count', 
            position = position_dodge(.1), 
            size = 3)
#nothing interesting

### Split Data ####

set.seed(42)
split <- initial_split(data, strata=in_college, prop = .7)
folds<-vfold_cv(training(split))
train <- training(split)
test  <- testing(split)

### Logistic regression via tidymodels ####

#model without cv to made the results intelligeble

mlog <- glm(in_college ~.,
                  data = train,
                  family = binomial(link = 'logit'))
summary(mlog)

#train

pred <- predict(mlog, newdata = train, type = 'response')
pred <- ifelse(pred > 0.5, 'True', 'False')
table(predicted = pred, Actual = train$in_college)

#test  (0.008490092 prob who put true negative to 0)

pred <- predict(mlog, newdata = test, type = 'response')
pred <- ifelse(pred > 0.5, 'True', 'False')
table(predicted = pred, Actual = test$in_college)

#train

pred <- predict(mlog, newdata = train, type = 'response')
pred <- ifelse(pred > 0.3, 'True', 'False')
table(predicted = pred, Actual = train$in_college)

#test  (0.008490092 prob who put true negative to 0)

pred <- predict(mlog, newdata = test, type = 'response')
pred <- ifelse(pred > 0.7, 'True', 'False')
table(predicted = pred, Actual = test$in_college)


confusionMatrix(as.factor(pred), test$in_college)
#sensitivity = true positive rate, % of individuals correctly predicted
#specificity = true negative rate, % of individuals model correct predict not go
#total misclassification rate = % of incorrect misclassification

#ROC Curve
pred_roc <- (predict(mlog, newdata = test, type = 'response'))
roc <- roc(test$in_college ~ pred_roc, plot = TRUE, print.auc = TRUE)


#multicollinearity

vif(mlog)
#as a rule of thumb we asses that value < 5 are not a prob so we're good no multicollinearity

#outliers

plot(mlog, which = 4, id.n = 3)

mlog.data <- augment(mlog) %>% 
  mutate(index = 1:n())

mlog.data %>% top_n(3, .cooksd)

###LDA - Linear Discriminant Analysis####

lda_mod = lda(in_college~., data = train)
lda_mod

plot(lda_mod)

lda_pred = predict(lda_mod, test)$class
table(lda_pred, test$in_college)

###Recipe####

rcp <- recipe(in_college ~ ., data = train)
rcp %>% prep() %>% juice() %>% glimpse()

###Tree####

tree_mod <- decision_tree(
  mode = "classification",
  tree_depth = tune(),
  min_n = tune()
) %>% set_engine('rpart')

wrkfl_tree <- workflow() %>%       #tree
  add_model(tree_mod) %>%
  add_recipe(rcp)

set.seed(42)
tree_grid <- grid_regular(
  min_n(),
  tree_depth(),
  levels = 10
)

set.seed(42)
tree_res <- wrkfl_tree %>% 
  tune_grid(
    resamples = folds,
    grid = tree_grid
  )

tree_res %>%  collect_metrics()
tree_best <- tree_res %>% select_best("roc_auc")
tree_best

tree_final_wkfl <- wrkfl_tree %>% 
  finalize_workflow(tree_best)

tree_final_wkfl

final_tree_fit <-
  tree_final_wkfl %>% 
  last_fit(split)


final_tree_fit %>% 
  collect_metrics()

final_tree_fit %>% 
  collect_predictions() %>% 
  roc_curve(in_college,.pred_False) %>% 
  autoplot()

final_tree <- extract_workflow(final_tree_fit)
final_tree

final_tree %>% 
  extract_fit_engine() %>% 
  rpart.plot()

final_tree %>% 
  extract_fit_parsnip() %>% 
  vip()

####Bagging####

bag <- bag_tree(
  mode = "classification",
  tree_depth = tune(),
  min_n = tune()) %>%
  set_engine("rpart")

wrkfl_bag <- workflow() %>%       #tree
  add_model(bag) %>%
  add_recipe(rcp)

bag_grid <- grid_regular(
  tree_depth(),
  min_n(),
  levels = 5
)

set.seed(42)
bag_res <- wrkfl_bag %>% 
  tune_grid(
    resamples = folds,
    grid = bag_grid
  )

bag_res %>%  collect_metrics()
bag_best <- bag_res %>% select_best("roc_auc")
bag_best

bag_final_wkfl <- wrkfl_bag %>% 
  finalize_workflow(bag_best)

bag_final_wkfl

bag_final_fit <-
  bag_final_wkfl %>% 
  last_fit(split)


bag_final_fit %>% 
  collect_metrics()

bag_final_fit %>% 
  collect_predictions() %>% 
  roc_curve(in_college,.pred_False) %>% 
  autoplot()

final_bag <- extract_workflow(bag_final_fit)
final_bag

####Random Forest####

rf <- rand_forest(
  mode = "classification",
  mtry = 3,   #sqrt  features
  trees = tune(),
  min_n = tune()) %>%
  set_engine("ranger")

wrkfl_rf <- workflow() %>%       #tree
  add_model(rf) %>%
  add_recipe(rcp)

rf_grid <- grid_regular(
  min_n(),
  trees(),
  levels = 10
)

set.seed(42)
rf_res <- wrkfl_rf %>% 
  tune_grid(
    resamples = folds,
    grid = rf_grid
  )

rf_res %>%  collect_metrics()
rf_best <- rf_res %>% select_best("roc_auc")
rf_best

rf_final_wkfl <- wrkfl_rf %>% 
  finalize_workflow(rf_best)

rf_final_wkfl

rf_final_fit <-
  rf_final_wkfl %>% 
  last_fit(split)


rf_final_fit %>% 
  collect_metrics()

rf_final_fit %>% 
  collect_predictions() %>% 
  roc_curve(in_college,.pred_False) %>% 
  autoplot()

final_rf <- extract_workflow(rf_final_fit)
final_rf

###Boosting####

data_xgb <- data %>% 
  mutate(
    interest = as.numeric(factor(interest, order = TRUE, level = c('Not Interested','Less Interested','Uncertain','Quiet Interested','Very Interested'))),
    school_accreditation = as.numeric(factor(school_accreditation, ordered = TRUE, levels = c('B','A'))),
    type_school = as.numeric(factor(type_school, level = c('Vocational', 'Academic'))),
    gender = as.numeric(factor(gender)),
    residence = as.numeric(factor(residence)),
    parent_was_in_college = as.numeric(factor(parent_was_in_college)),
    in_college = as.factor(factor(in_college))
  )


set.seed(42)
split_xgb <- initial_split(data_xgb, strata=in_college, prop = .7)
folds_xgb<-vfold_cv(training(split_xgb))
train_xgb <- training(split_xgb)
test_xgb  <- testing(split_xgb)

install.packages('lightgbm')
library(lightgbm)

xgb <- boost_tree(
  mode = "classification",
  mtry = 3,
  trees = 1500,
  min_n = tune(),
  tree_depth = tune()
) %>%
  set_engine("xgboost")

wrkfl_xgb <- workflow() %>%
  add_model(xgb) %>%
  add_recipe(rcp)

xgb_grid <- grid_regular(
  min_n(),
  tree_depth(),
  levels = 5
)

set.seed(42)
xgb_res <- wrkfl_xgb %>% 
  tune_grid(
    resamples = folds_xgb,
    grid = xgb_grid)

xgb_res %>%  collect_metrics()
xgb_best <- xgb_res %>% select_best("roc_auc")
xgb_best

xgb_final_wkfl <- wrkfl_xgb %>% 
  finalize_workflow(xgb_best)

xgb_final_wkfl

xgb_final_fit <-
  xgb_final_wkfl %>% 
  last_fit(split_xgb)

xgb_final_fit %>% 
  collect_metrics()

xgb_final_fit %>% 
  collect_predictions() %>% 
  roc_curve(in_college,.pred_False) %>% 
  autoplot()

final_xgb <- extract_workflow(xgb_final_fit)
final_xgb
