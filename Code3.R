library(readr)
library(dplyr)
set.seed(123) 

# Step 1: Read the data
Quant_Challenge_data_amended <- read_csv("Quant_Challenge_data_amended.csv")

# Step 2: Replace "Not avail.", "<NA>", and "NA" with actual NA values
Quant_Challenge_data_amended <- Quant_Challenge_data_amended %>%
  mutate(across(everything(), ~ replace(.x, .x %in% c("Not avail.", "<NA>", "NA"), NA)))
# Replace 1, 2, and 3 with "Not recognized"
Quant_Challenge_data_amended$occup <- ifelse(
  Quant_Challenge_data_amended$occup %in% c(1, 2, 3, NA),
  "Not recognized",
  Quant_Challenge_data_amended$occup
)
OBS_DATE<-data.frame(Quant_Challenge_data_amended$OBS_DATE)
colnames(OBS_DATE) <- "OBS_DATE"
write.csv(OBS_DATE, file = "OBS_DATE.csv", row.names = FALSE)
OBS_DATE <- read_csv("OBS_DATE.csv", col_types=cols(OBS_DATE = col_datetime(format = "%d%b%Y - %H:%M:%S")))
# Step 3: Change column types
Quant_Challenge_data_amended <- Quant_Challenge_data_amended %>%
  mutate(
    income = as.numeric(income),
    loan_amount = as.numeric(loan_amount),
    term_length = as.numeric(term_length),
    install_to_inc = as.numeric(install_to_inc),
    schufa = as.numeric(schufa),
    num_applic = as.numeric(num_applic),
    target_var = as.logical(as.numeric(target_var)),
    occup = factor(occup, levels = c("Not recognized","Student", "Worker", "Employee")),
    marital = factor(marital, levels = c("Single", "Divorced", "Separated", "Living together", "Married"))
  )
Quant_Challenge_data_amended$OBS_DATE <- NULL
Quant_Challenge_data_amended$OBS_DATE <- OBS_DATE$OBS_DATE
rm(OBS_DATE)
# Step 4: Verify the changes
str(Quant_Challenge_data_amended)  # Check structure of the data


#Check frequencies
sum(is.na(Quant_Challenge_data_amended$...1))
sum(is.na(Quant_Challenge_data_amended$income))
sum(is.na(Quant_Challenge_data_amended$loan_amount))
sum(is.na(Quant_Challenge_data_amended$term_length))
sum(is.na(Quant_Challenge_data_amended$install_to_inc))
sum(is.na(Quant_Challenge_data_amended$schufa))
table(Quant_Challenge_data_amended$occup, useNA = "ifany")
table(Quant_Challenge_data_amended$marital, useNA = "ifany")
table(Quant_Challenge_data_amended$num_applic, useNA = "ifany")
sum(is.na(Quant_Challenge_data_amended$OBS_DATE))
table(Quant_Challenge_data_amended$target_var, useNA = "ifany")

#Replacing NA values:
hist(Quant_Challenge_data_amended$income,
     breaks = 20,                  # Number of bins
     probability = TRUE,           # Scale to probability density
     main = "Histogram of Income with Normal Curve",
     xlab = "Income",
     col = "lightblue",            # Fill color
     border = "black")  
#Looks skewed, then apply log normality:
# Step 1: Log-transform the non-NA values to handle skewness
log_income <- log(Quant_Challenge_data_amended$income[!is.na(Quant_Challenge_data_amended$income)])

# Step 2: Calculate the mean and standard deviation of the log-transformed data
log_mean <- mean(log_income)
log_sd <- sd(log_income)

# Step 3: Generate log-normal random numbers for the NA values
num_na <- sum(is.na(Quant_Challenge_data_amended$income))  # Number of NA values
generated_values <- exp(rnorm(num_na, mean = log_mean, sd = log_sd))  # Back-transform

# Step 4: Replace NA values with the generated skewed random numbers
Quant_Challenge_data_amended$income[is.na(Quant_Challenge_data_amended$income)] <- generated_values

# Step 5: Plot histogram of updated income
hist(Quant_Challenge_data_amended$income,
     breaks = 20,
     probability = TRUE,
     main = "Histogram of Income with Log-Normal Curve",
     xlab = "Income",
     col = "lightblue",
     border = "black")

# Overlay the log-normal curve
x <- seq(min(Quant_Challenge_data_amended$income), max(Quant_Challenge_data_amended$income), length = 100)
y <- dlnorm(x, meanlog = log_mean, sdlog = log_sd)
lines(x, y, col = "red", lwd = 2)


########################################################
hist(Quant_Challenge_data_amended$loan_amount,
     breaks = 20,                  # Number of bins
     probability = TRUE,           # Scale to probability density
     main = "Histogram of loan_amount with Normal Curve",
     xlab = "loan_amount",
     col = "lightblue",            # Fill color
     border = "black")  


# Step 1: Extract non-NA values of loan_amount
loan_amount_non_na <- Quant_Challenge_data_amended$loan_amount[!is.na(Quant_Challenge_data_amended$loan_amount)]

# Step 2: Calculate the mean and standard deviation of the non-NA values
loan_mean <- mean(loan_amount_non_na)
loan_sd <- sd(loan_amount_non_na)

# Step 3: Generate normal random numbers for the NA values
num_na <- sum(is.na(Quant_Challenge_data_amended$loan_amount))  # Number of NA values
generated_values <- rnorm(num_na, mean = loan_mean, sd = loan_sd)  # Generate normal random numbers

# Step 4: Replace NA values with the generated random numbers
Quant_Challenge_data_amended$loan_amount[is.na(Quant_Challenge_data_amended$loan_amount)] <- generated_values

# Step 5: Plot histogram of updated loan_amount
hist(Quant_Challenge_data_amended$loan_amount,
     breaks = 20,
     probability = TRUE,
     main = "Histogram of Loan Amount with Normal Curve",
     xlab = "Loan Amount",
     col = "lightblue",
     border = "black")

# Overlay the normal curve
x <- seq(min(Quant_Challenge_data_amended$loan_amount), 
         max(Quant_Challenge_data_amended$loan_amount), 
         length = 100)

y <- dnorm(x, mean = loan_mean, sd = loan_sd)  # Normal curve density
lines(x, y, col = "red", lwd = 2)  # Add normal curve

############################################################
hist(Quant_Challenge_data_amended$term_length,
     breaks = 20,                  # Number of bins
     probability = TRUE,           # Scale to probability density
     main = "Histogram of term_length with Normal Curve",
     xlab = "term_length",
     col = "lightblue",            # Fill color
     border = "black")  

#Looks skewed, then apply log normality:
log_term_length <- log(Quant_Challenge_data_amended$term_length[!is.na(Quant_Challenge_data_amended$term_length)])

log_mean <- mean(log_term_length)
log_sd <- sd(log_term_length)

num_na <- sum(is.na(Quant_Challenge_data_amended$term_length))  # Number of NA values
generated_values <- exp(rnorm(num_na, mean = log_mean, sd = log_sd))  # Back-transform

Quant_Challenge_data_amended$term_length[is.na(Quant_Challenge_data_amended$term_length)] <- generated_values

hist(Quant_Challenge_data_amended$term_length,
     breaks = 20,
     probability = TRUE,
     main = "Histogram of term_length with Log-Normal Curve",
     xlab = "term_length",
     col = "lightblue",
     border = "black")

# Overlay the log-normal curve
x <- seq(min(Quant_Challenge_data_amended$term_length), max(Quant_Challenge_data_amended$term_length), length = 100)
y <- dlnorm(x, meanlog = log_mean, sdlog = log_sd)
lines(x, y, col = "red", lwd = 2)

##########################################################################

hist(Quant_Challenge_data_amended$install_to_inc,
     breaks = 20,                  # Number of bins
     probability = TRUE,           # Scale to probability density
     main = "Histogram of install_to_inc with Normal Curve",
     xlab = "install_to_inc",
     col = "lightblue",            # Fill color
     border = "black")  
# Step 1: Log-transform the non-NA values to handle skewness
log_install_to_inc <- log(Quant_Challenge_data_amended$install_to_inc[!is.na(Quant_Challenge_data_amended$install_to_inc)])

# Step 2: Calculate the mean and standard deviation of the log-transformed data
log_mean <- mean(log_install_to_inc)
log_sd <- sd(log_install_to_inc)

# Step 3: Generate log-normal random numbers for the NA values
num_na <- sum(is.na(Quant_Challenge_data_amended$install_to_inc))  # Number of NA values
generated_values <- exp(rnorm(num_na, mean = log_mean, sd = log_sd))  # Back-transform

# Step 4: Replace NA values with the generated skewed random numbers
Quant_Challenge_data_amended$install_to_inc[is.na(Quant_Challenge_data_amended$install_to_inc)] <- generated_values

# Step 5: Plot histogram of updated install_to_inc
hist(Quant_Challenge_data_amended$install_to_inc,
     breaks = 20,
     probability = TRUE,
     main = "Histogram of Install to Income with Log-Normal Curve",
     xlab = "Install to Income Ratio",
     col = "lightblue",
     border = "black")

# Overlay the log-normal curve
x <- seq(min(Quant_Challenge_data_amended$install_to_inc), 
         max(Quant_Challenge_data_amended$install_to_inc), 
         length = 100)

y <- dlnorm(x, meanlog = log_mean, sdlog = log_sd)  # Log-normal density
lines(x, y, col = "red", lwd = 2)  # Add log-normal curve

############################################################################

hist(Quant_Challenge_data_amended$schufa,
     breaks = 50,                  # Number of bins
     probability = TRUE,           # Scale to probability density
     main = "Histogram of schufa with Normal Curve",
     xlab = "schufa",
     col = "lightblue",            # Fill color
     border = "black")  
# Step 1: Log-transform the non-NA values to handle skewness
log_schufa <- log(Quant_Challenge_data_amended$schufa[!is.na(Quant_Challenge_data_amended$schufa)])

# Step 2: Calculate the mean and standard deviation of the log-transformed data
log_mean <- mean(log_schufa)
log_sd <- sd(log_schufa)

# Step 3: Generate log-normal random numbers for the NA values
num_na <- sum(is.na(Quant_Challenge_data_amended$schufa))  # Number of NA values
generated_values <- exp(rnorm(num_na, mean = log_mean, sd = log_sd))  # Back-transform

# Step 4: Replace NA values with the generated skewed random numbers
Quant_Challenge_data_amended$schufa[is.na(Quant_Challenge_data_amended$schufa)] <- generated_values

# Step 5: Plot histogram of updated schufa
hist(Quant_Challenge_data_amended$schufa,
     breaks = 20,
     probability = TRUE,
     main = "Histogram of Schufa with Log-Normal Curve",
     xlab = "Schufa Credit Score",
     col = "lightblue",
     border = "black")

# Overlay the log-normal curve
x <- seq(min(Quant_Challenge_data_amended$schufa), 
         max(Quant_Challenge_data_amended$schufa), 
         length = 100)

y <- dlnorm(x, meanlog = log_mean, sdlog = log_sd)  # Log-normal density
lines(x, y, col = "red", lwd = 2)  # Add log-normal curve


###############################################################################
#Decision tree for marital status.
library(rpart)        # For decision tree
library(dplyr)        # For data manipulation
train_data_Marital <- Quant_Challenge_data_amended %>% filter(!is.na(marital))
test_data_Marital <- Quant_Challenge_data_amended %>% filter(is.na(marital))

# Step 2: Train a decision tree to predict 'Marital'
tree_model <- rpart(marital ~ income + loan_amount + occup + schufa + num_applic, data = train_data_Marital, method = "class",control=rpart.control(cp=0.001,maxdepth=4))
# Step 3: Predict 'Marital' for rows with NA values
predicted_marital <- predict(tree_model, newdata = test_data_Marital, type = "class")

# Step 4: Replace NA values in 'Marital' with predictions
Quant_Challenge_data_amended$marital[is.na(Quant_Challenge_data_amended$marital)] <- predicted_marital

# Optional: Plot the decision tree for visualization
library(rpart.plot)
rpart.plot(tree_model)
###############################################################################
# Convert num_applic to a factor variable
Quant_Challenge_data_amended$num_applic <- factor(Quant_Challenge_data_amended$num_applic, 
                                                  levels = c(1, 2), 
                                                  labels = c("One", "Two"))

# Step 1: Create training and test datasets
# Train data: Rows where num_applic is not NA
train_data <- Quant_Challenge_data_amended %>%
  filter(!is.na(num_applic))

# Test data: Rows where num_applic is NA
test_data <- Quant_Challenge_data_amended %>%
  filter(is.na(num_applic))

# Step 2: Fit a logistic regression model
logit_model <- glm(num_applic ~ income + loan_amount + occup + marital, 
                   data = train_data, 
                   family = binomial(link = "logit"))  # Logistic regression

# Step 3: Predict probabilities for the test dataset
test_data$predicted_prob <- predict(logit_model, newdata = test_data, type = "response")
# Step 4: Convert probabilities to predictions
# If predicted probability > 0.5, classify as "Two", else "One"
test_data$predicted_num_applic <- ifelse(test_data$predicted_prob > 0.5, "Two", "One")
# Step 5: Combine predictions back into the original dataset
Quant_Challenge_data_amended$num_applic[is.na(Quant_Challenge_data_amended$num_applic)] <- 
  test_data$predicted_num_applic
Data <-Quant_Challenge_data_amended

########################################MOVE TO ROW 713 TO RUN NON LINEAR MODELS FOR CLASSIFICATION####################
#######################################################################################################################
#Train and test dataset
na_rows <- Quant_Challenge_data_amended[is.na(Quant_Challenge_data_amended$OBS_DATE), ]
nrow(na_rows)
nrow(Quant_Challenge_data_amended)
# Randomly split NA rows: 70% train, 30% test
train_indices <- sample(1:nrow(na_rows), size = floor(0.7 * nrow(na_rows)))
na_train <- na_rows[train_indices, ]  # 70% of NA rows
na_test <- na_rows[-train_indices, ]  # 30% of NA rows

# Define the OOT sample (data from 2016-01-01 to 2018-12-31 23:59:59)
start_date <- as.POSIXct("2016-01-01 00:00:00")
end_date <- as.POSIXct("2018-12-31 23:59:59")

test_data <- subset(Quant_Challenge_data_amended, OBS_DATE >= start_date & OBS_DATE <= end_date & !is.na(OBS_DATE))

# Define the training data (all other dates)
train_data <- subset(Quant_Challenge_data_amended, !(OBS_DATE >= start_date & OBS_DATE <= end_date) & !is.na(OBS_DATE))
# Combine NA splits with respective train and test data
train_data <- rbind(train_data, na_train)
test_data <- rbind(test_data, na_test)
#Choose which outcome to model.
outcome<-"target_var"
#Choose which outcome is considered positive. 
pos<-1

nrow(test_data)
nrow(train_data)
nrow(test_data)/(nrow(test_data)+nrow(train_data))

max(Quant_Challenge_data_amended$OBS_DATE,na.rm = T)
min(Quant_Challenge_data_amended$OBS_DATE,na.rm = T)

#### Methodology Chapter ######

####################################VARIABLE SELECTION ALGORITHM################################

categorical<-c("occup","marital","num_applic")
numerical<-c("income","loan_amount","term_length","install_to_inc","schufa")
# Combine categorical and numerical variables
all_variables <- c(categorical, numerical)

# Generate all possible combinations
# Function to generate combinations of a given size
generate_combinations <- function(vars) {
  lapply(1:length(vars), function(size) combn(vars, size, simplify = FALSE))
}
all_combinations <- generate_combinations(all_variables)
all_combinations <- unlist(all_combinations, recursive = FALSE)
y<-"target_var"
# Convert combinations into a list of formula objects
formula_list <- lapply(all_combinations, function(vars) {
  as.formula(paste("target_var ~ zeros + ", paste(vars, collapse = " + ")))
})

length(all_combinations)
2^8
all_combinations
####################################LOGISTIC REGRESSION RUNNING################################
logit_model_function <- function(y, x) {
#Function for glm() can be "pasted" together
fmla<-paste(y, paste(x, collapse="+"),sep="~")
LogModel <- glm(fmla,family=binomial(link='logit'), data=train_data)

#Making (in-sample and out-of-sample) predictions
#For logit, type="response" needs to be specified to return probabilities
train_data$pred<-predict(LogModel,newdata=train_data,type="response")
test_data$pred<-predict(LogModel,newdata=test_data,type="response")

#################################PERFORMANCE###################################################
#Finding model's precision
#Run for a set of thresholds
results_list <- list()
thresholds <- seq(0, 1, by = 0.001)
# Loop through each threshold
for (threshold in thresholds) {
  
  # Generate confusion matrix at the current threshold
  ctab.test <- table(pred = test_data$pred > threshold, target_var = test_data$target_var)
  
  # Check if there are enough values to calculate precision and recall
  if (nrow(ctab.test) >= 2 && ncol(ctab.test) >= 2) {
    # Precision: The predicted positives are true positives
    precision <- ctab.test[2, 2] / sum(ctab.test[2, ])
    # Recall: How many of the true positives the classifier finds
    recall <- ctab.test[2, 2] / sum(ctab.test[, 2])
  } else {
    # Handle cases where the table is incomplete (e.g., no positives)
    precision <- NA
    recall <- NA
  }
  # Save the results for the current threshold
  results_list[[as.character(threshold)]] <- list(
    threshold = threshold,
    precision = precision,
    recall = recall
  )
}
results_df <- do.call(rbind, lapply(results_list, as.data.frame))
# Calculate the absolute difference between precision and recall
results_df$difference <- abs(results_df$precision - results_df$recall)
best_threshold <- results_df[which.min(results_df$difference), ]$threshold
# Optional: Plot Precision and Recall across thresholds
plot(results_df$threshold, results_df$precision, type = "l", col = "blue", lwd = 2,
     xlab = "Threshold", ylab = "Performance", main = "Precision and Recall vs Threshold")
lines(results_df$threshold, results_df$recall, col = "red", lwd = 2)
legend("bottomleft", legend = c("Precision", "Recall"), col = c("blue", "red"), lwd = 2)

threshold<-best_threshold #We need to set a threshold first, since there are low trues, I won't punish too much and I want to focus on True positives performance.
ctab.test<-table(pred=test_data$pred>threshold,target_var=test_data$target_var)
ctab.test
#Precision: the predicted positives are true positives
precision<-ctab.test[2,2]/sum(ctab.test[2,])
#Recall: how many of the true positives the classifier finds
recall<-ctab.test[2,2]/sum(ctab.test[,2])
#The model is very good at identifying the real true values and that is important if the goal is to avoid giving loans to default people (maximizing positive detection).

#LogModel$null.deviance
#LogModel$deviance
#LogModel$aic
library('ROCR') #Includes ROC and AUC
# Remove rows with NA values
valid_indices <- !is.na(test_data$pred) & !is.na(test_data$target_var)
actual_labels <- test_data$target_var[valid_indices]
predicted_probs <- test_data$pred[valid_indices]
actual_labels <- as.numeric(actual_labels) 
eval <- prediction(predicted_probs,actual_labels) #For the ROCR function, specify the predictions and true outcomes
plot(performance(eval,"tpr","fpr"), col = "blue", lwd = 2, main = "ROC Curve") #ROCR function, specify prediction object and specific measures
abline(a = 0, b = 1, lty = 2, col = "gray")
print(attributes(performance(eval,'auc'))$y.values[[1]]) #AUC, rather non-standard notation
#Information criteria
LL<-function(outcome,prediction,hit=1){
  sum(ifelse(outcome==hit,log(prediction),log(1-prediction)))
}
McFadden_R2<-function(outcome,prediction,hit=1){
  1-LL(outcome,prediction,hit=hit)/LL(outcome,sum(outcome==hit)/length(outcome))
}
McFadden_R2(actual_labels,predicted_probs)



AIC<-function(outcome,prediction,hit=1,k){
  2*k-2*LL(outcome,prediction,hit=hit)}
BIC<-function(outcome,prediction,hit=1,k){
    k*log(length(outcome))-2*LL(outcome,prediction,hit=hit)
  }
AIC(actual_labels,predicted_probs,k=8)
BIC(actual_labels,predicted_probs,k=8)

#precision, recall, AUC, R2, BIC
return(data.frame(
  threshold = best_threshold,
  precision = precision,
  recall = recall,
  AUC = attributes(performance(eval,'auc'))$y.values[[1]],
  R2 = McFadden_R2(actual_labels,predicted_probs),
  BIC = BIC(actual_labels,predicted_probs,k=8)
))}

#end of the function

results_logit <- data.frame(
  id = character(),          # Combination ID or variables used
  threshold = numeric(),
  precision = numeric(),     # Precision metric
  recall = numeric(),        # Recall metric
  AUC = numeric(),           # Area Under Curve metric
  R2 = numeric(),            # R-squared metric
  BIC = numeric(),           # Bayesian Information Criterion
  stringsAsFactors = FALSE   # Prevent factor conversion
)
y <- "target_var"
for (i in all_combinations) {metrics <-logit_model_function(y,i)
new_row <- data.frame(
  id = paste(i, collapse = "+"),  # Combine variable names as ID
  threshold = metrics$threshold,
  precision = metrics$precision,
  recall = metrics$recall,
  AUC = metrics$AUC,
  R2 = metrics$R2,
  BIC = metrics$BIC
)
results_logit <- rbind(results_logit, new_row)
}

#################################LASSO MODEL###################################################
# Load glmnet
library(glmnet)
# Create a clean dataset by removing rows with NA in target_var first
clean_data_train <- train_data[complete.cases(train_data$target_var), ]
clean_data_test <- test_data[complete.cases(test_data$target_var), ]
clean_data_train$zeros <- 0
clean_data_test$zeros <- 0

lasso_model_function <- function(x_lasso) {
X <- model.matrix(x_lasso, data = clean_data_train)[, -1]  # Remove intercept
Y <- as.numeric(clean_data_train$target_var)

# Step 3: Fit the LASSO model
lasso_model <- cv.glmnet(X, Y, family = "binomial", alpha = 1, nfolds = 10)
# Step 4: View optimal lambda
best_lambda <- lasso_model$lambda.min
print(paste("Optimal Lambda:", best_lambda))
# Fit final model
final_model <- glmnet(X, Y, family = "binomial", alpha = 1, lambda = best_lambda)

# View coefficients
summary(final_model)
# Make predictions
X_test <- model.matrix(x_lasso, data = clean_data_test)[, -1]  # Remove intercept
Y_test <- as.numeric(clean_data_test$target_var)

predicted_probs <- predict(final_model, newx = X_test, type = "response")
predicted_classes <- ifelse(predicted_probs > 0.5, 1, 0)

# Evaluate predictions
table(Predicted = predicted_classes, Actual = Y_test)


#################################PERFORMANCE###################################################
#Finding model's precision
#Run for a set of thresholds
results_list <- list()
thresholds <- seq(0, 1, by = 0.001)
# Loop through each threshold
for (threshold in thresholds) {
  
  # Generate confusion matrix at the current threshold
  ctab.test <- table(pred = predicted_probs > threshold, target_var = Y_test)
  
  # Check if there are enough values to calculate precision and recall
  if (nrow(ctab.test) >= 2 && ncol(ctab.test) >= 2) {
    # Precision: The predicted positives are true positives
    precision <- ctab.test[2, 2] / sum(ctab.test[2, ])
    # Recall: How many of the true positives the classifier finds
    recall <- ctab.test[2, 2] / sum(ctab.test[, 2])
  } else {
    # Handle cases where the table is incomplete (e.g., no positives)
    precision <- NA
    recall <- NA
  }
  # Save the results for the current threshold
  results_list[[as.character(threshold)]] <- list(
    threshold = threshold,
    precision = precision,
    recall = recall
  )
}
results_df <- do.call(rbind, lapply(results_list, as.data.frame))
results_df$difference <- abs(results_df$precision - results_df$recall)
best_threshold <- results_df[which.min(results_df$difference), ]$threshold

# Optional: Plot Precision and Recall across thresholds
plot(results_df$threshold, results_df$precision, type = "l", col = "blue", lwd = 2,
     xlab = "Threshold", ylab = "Performance", main = "Precision and Recall vs Threshold")
lines(results_df$threshold, results_df$recall, col = "red", lwd = 2)
legend("bottomleft", legend = c("Precision", "Recall"), col = c("blue", "red"), lwd = 2)

threshold<-best_threshold #We need to set a threshold first, since there are low trues, I won't punish too much and I want to focus on True positives performance.
ctab.test<-table(pred=predicted_probs>threshold,target_var=Y_test)
ctab.test
#Precision: the predicted positives are true positives
precision<-ctab.test[2,2]/sum(ctab.test[2,])
#Recall: how many of the true positives the classifier finds
recall<-ctab.test[2,2]/sum(ctab.test[,2])
#The model is very good at identifying the real true values and that is important if the goal is to avoid giving loans to default people (maximizing positive detection).


library('ROCR') #Includes ROC and AUC
# Remove rows with NA values
eval <- prediction(predicted_probs,Y_test) #For the ROCR function, specify the predictions and true outcomes
plot(performance(eval,"tpr","fpr"), col = "blue", lwd = 2, main = "ROC Curve") #ROCR function, specify prediction object and specific measures
abline(a = 0, b = 1, lty = 2, col = "gray")
print(attributes(performance(eval,'auc'))$y.values[[1]]) #AUC, rather non-standard notation

#Information criteria
LL<-function(outcome,prediction,hit=1){
  sum(ifelse(outcome==hit,log(prediction),log(1-prediction)))
}
McFadden_R2<-function(outcome,prediction,hit=1){
  1-LL(outcome,prediction,hit=hit)/LL(outcome,sum(outcome==hit)/length(outcome))
}
McFadden_R2(Y_test,predicted_probs)

AIC<-function(outcome,prediction,hit=1,k){
  2*k-2*LL(outcome,prediction,hit=hit)}
BIC<-function(outcome,prediction,hit=1,k){
  k*log(length(outcome))-2*LL(outcome,prediction,hit=hit)
}
AIC(Y_test,predicted_probs,k=8)
BIC(Y_test,predicted_probs,k=8)
return(data.frame(
  threshold = best_threshold,
  precision = precision,
  recall = recall,
  AUC = attributes(performance(eval,'auc'))$y.values[[1]],
  R2 = McFadden_R2(Y_test,predicted_probs),
  BIC = BIC(Y_test,predicted_probs,k=8)
))}
results_lasso <- data.frame(
  id = character(),          # Combination ID or variables used
  threshold = numeric(),
  precision = numeric(),     # Precision metric
  recall = numeric(),        # Recall metric
  AUC = numeric(),           # Area Under Curve metric
  R2 = numeric(),            # R-squared metric
  BIC = numeric(),           # Bayesian Information Criterion
  stringsAsFactors = FALSE   # Prevent factor conversion
)
y <- "target_var"
for (i in formula_list) {metrics <-lasso_model_function(i)
new_row <- data.frame(
  id = paste(i, collapse = "+"),  # Combine variable names as ID
  threshold = metrics$threshold,
  precision = metrics$precision,
  recall = metrics$recall,
  AUC = metrics$AUC,
  R2 = metrics$R2,
  BIC = metrics$BIC
)
results_lasso <- rbind(results_lasso, new_row)
}

################################################################RIDGE MODEL##########################################
# Load glmnet
library(glmnet)


clean_data_train <- train_data[complete.cases(train_data$target_var), ]
clean_data_test <- test_data[complete.cases(test_data$target_var), ]
clean_data_train$zeros <- 0
clean_data_test$zeros <- 0

# Step 3: Fit the ridge model
ridge_model_function <- function(x_ridge) {
X <- model.matrix(x_ridge, data = clean_data_train)[, -1]  # Remove intercept
Y <- as.numeric(clean_data_train$target_var)
ridge_model <- cv.glmnet(X, Y, family = "binomial", alpha = 0, nfolds = 10)
# Step 4: View optimal lambda
best_lambda <- ridge_model$lambda.min
print(paste("Optimal Lambda:", best_lambda))
# Fit final model
final_ridge <- glmnet(X, Y, family = "binomial", alpha = 0, lambda = best_lambda)


# Make predictions
X_test <- model.matrix(x_ridge, data = clean_data_test)[, -1]  # Remove intercept
Y_test <- as.numeric(clean_data_test$target_var)
predicted_probs <- predict(final_ridge, newx = X_test, type = "response")
predicted_classes <- ifelse(predicted_probs > 0.5, 1, 0)

# Evaluate predictions
table(Predicted = predicted_classes, Actual = Y_test)

#################################PERFORMANCE###################################################
#Finding model's precision
#Run for a set of thresholds
results_list <- list()
thresholds <- seq(0, 1, by = 0.001)
# Loop through each threshold
for (threshold in thresholds) {
  
  # Generate confusion matrix at the current threshold
  ctab.test <- table(pred = predicted_probs > threshold, target_var = Y_test)
  
  # Check if there are enough values to calculate precision and recall
  if (nrow(ctab.test) >= 2 && ncol(ctab.test) >= 2) {
    # Precision: The predicted positives are true positives
    precision <- ctab.test[2, 2] / sum(ctab.test[2, ])
    # Recall: How many of the true positives the classifier finds
    recall <- ctab.test[2, 2] / sum(ctab.test[, 2])
  } else {
    # Handle cases where the table is incomplete (e.g., no positives)
    precision <- NA
    recall <- NA
  }
  # Save the results for the current threshold
  results_list[[as.character(threshold)]] <- list(
    threshold = threshold,
    precision = precision,
    recall = recall
  )
}
results_df <- do.call(rbind, lapply(results_list, as.data.frame))
results_df$difference <- abs(results_df$precision - results_df$recall)
best_threshold <- results_df[which.min(results_df$difference), ]$threshold

# Optional: Plot Precision and Recall across thresholds
plot(results_df$threshold, results_df$precision, type = "l", col = "blue", lwd = 2,
     xlab = "Threshold", ylab = "Performance", main = "Precision and Recall vs Threshold")
lines(results_df$threshold, results_df$recall, col = "red", lwd = 2)
legend("bottomleft", legend = c("Precision", "Recall"), col = c("blue", "red"), lwd = 2)

threshold<-best_threshold#We need to set a threshold first, since there are low trues, I won't punish too much and I want to focus on True positives performance.
ctab.test<-table(pred=predicted_probs>threshold,target_var=Y_test)
ctab.test
#Precision: the predicted positives are true positives
precision<-ctab.test[2,2]/sum(ctab.test[2,])
#Recall: how many of the true positives the classifier finds
recall<-ctab.test[2,2]/sum(ctab.test[,2])
#The model is very good at identifying the real true values and that is important if the goal is to avoid giving loans to default people (maximizing positive detection).


library('ROCR') #Includes ROC and AUC
# Remove rows with NA values
eval <- prediction(predicted_probs,Y_test) #For the ROCR function, specify the predictions and true outcomes
plot(performance(eval,"tpr","fpr"), col = "blue", lwd = 2, main = "ROC Curve") #ROCR function, specify prediction object and specific measures
abline(a = 0, b = 1, lty = 2, col = "gray")
print(attributes(performance(eval,'auc'))$y.values[[1]]) #AUC, rather non-standard notation

#Information criteria
LL<-function(outcome,prediction,hit=1){
  sum(ifelse(outcome==hit,log(prediction),log(1-prediction)))
}
McFadden_R2<-function(outcome,prediction,hit=1){
  1-LL(outcome,prediction,hit=hit)/LL(outcome,sum(outcome==hit)/length(outcome))
}
McFadden_R2(Y_test,predicted_probs)

AIC<-function(outcome,prediction,hit=1,k){
  2*k-2*LL(outcome,prediction,hit=hit)}
BIC<-function(outcome,prediction,hit=1,k){
  k*log(length(outcome))-2*LL(outcome,prediction,hit=hit)
}
AIC(Y_test,predicted_probs,k=8)
BIC(Y_test,predicted_probs,k=8)
return(data.frame(
  threshold = best_threshold,
  precision = precision,
  recall = recall,
  AUC = attributes(performance(eval,'auc'))$y.values[[1]],
  R2 = McFadden_R2(Y_test,predicted_probs),
  BIC = BIC(Y_test,predicted_probs,k=8)
))}


results_ridge <- data.frame(
  id = character(),          # Combination ID or variables used
  threshold = numeric(),
  precision = numeric(),     # Precision metric
  recall = numeric(),        # Recall metric
  AUC = numeric(),           # Area Under Curve metric
  R2 = numeric(),            # R-squared metric
  BIC = numeric(),           # Bayesian Information Criterion
  stringsAsFactors = FALSE   # Prevent factor conversion
)
formula_list
y <- "target_var"
for (i in formula_list) {
  metrics <-ridge_model_function(i)
  new_row <- data.frame(
    
  id = paste(i, collapse = "+"),  # Combine variable names as ID
  threshold = metrics$threshold,
  precision = metrics$precision,
  recall = metrics$recall,
  AUC = metrics$AUC,
  R2 = metrics$R2,
  BIC = metrics$BIC
)
results_ridge <- rbind(results_ridge, new_row)
}
results_ridge
#################################KNN null values imputing for Non linear models###################################################
# Load required libraries
library(caret)
library(dplyr)
library(tidyr)
# Step 1: Normalize numeric variables
numeric_vars <- c("income", "loan_amount", "term_length", "install_to_inc", "schufa")
train_data <- Data %>% 
  mutate(across(all_of(numeric_vars), ~ scale(.) %>% as.numeric()))
categorical_vars <- c("occup", "marital", "num_applic")
train_data <- train_data %>%
  mutate(across(all_of(categorical_vars), as.factor)) %>%
  fastDummies::dummy_cols(select_columns = categorical_vars, remove_selected_columns = TRUE)
# Step 3: Prepare data for KNN
# Separate rows with and without missing target_var
data_with_target <- train_data %>% filter(!is.na(target_var))
data_without_target <- train_data %>% filter(is.na(target_var))
# Ensure target_var is a factor for classification
data_with_target$target_var <- as.factor(data_with_target$target_var)
# Exclude target_var, OBS_DATE, and the first column
predictor_vars <- colnames(train_data) %>%
  setdiff(c("target_var", "OBS_DATE", colnames(train_data)[1])) # Exclude unnecessary columns

# Add backticks to handle variable names with spaces or special characters
predictor_vars <- paste0("`", predictor_vars, "`")

# Create the formula dynamically
fmla <- as.formula(
  paste("target_var ~", paste(predictor_vars, collapse = " + "))
)
# Step 4: Train the KNN model
knn_model <- train(
  fmla,
  data = data_with_target,
  method = "knn",
  trControl = trainControl(method = "cv", number = 5), # Cross-validation
  tuneGrid = expand.grid(k = 10:100) # Adjust k as needed
)
# Predict missing values for rows without a target_var
predicted_values <- predict(knn_model, newdata = data_without_target)
# Impute missing target_var values
data_without_target <- data_without_target %>%
  mutate(target_var = predicted_values)

# Combine datasets
final_data <- bind_rows(data_with_target, data_without_target)

# Ensure the final dataset is ordered as the original
final_data <- final_data %>%
  arrange(row_number())

#################################Data###################################################
Data <- Data %>%
  left_join(final_data %>% select(...1, target_var), by = "...1")
Data$target_var.x<-NULL
Data <- Data %>%
  rename(target_var = target_var.y)
# Specify categorical and numerical variables
categorical <- c("occup", "marital", "num_applic")
numerical <- c("income", "loan_amount", "term_length", "install_to_inc", "schufa")
all_variables <- c(categorical, numerical)
# Create formula
fmla <- as.formula(paste("target_var ~", paste(all_variables, collapse = "+")))
Data$target_var <- as.factor(Data$target_var)
set.seed(99)
library(parsnip)
library(tidymodels)
library(ranger)
cv_folds <- vfold_cv(Data, v = 5)
#################################Random forests###################################################
# Load required packages
# Specify the random forest model
spec <- rand_forest(
  mtry = 3, #the sqrt(8)
  trees = 500,
  min_n = 10
) %>%
  set_mode("classification") %>%
  set_engine("ranger", importance = "impurity")

# Workflow: Combine preprocessing and model specification
workflow <- workflow() %>%
  add_formula(fmla) %>%
  add_model(spec)
# Perform 4-fold cross-validation
cv_results <- workflow %>%
  fit_resamples(
    resamples = cv_folds,
    metrics = metric_set(accuracy, roc_auc),
    control = control_resamples(save_pred = TRUE)
  )
# View cross-validation results
cv_metrics <- collect_metrics(cv_results)
print(cv_metrics)
# Collect predictions from CV and calculate total AUC
cv_predictions <- collect_predictions(cv_results)

overall_auc <- roc_auc(
  data = cv_predictions,
  truth = target_var,  # Adjust to match your target variable name
  .pred_TRUE, event_level = "second"          # Adjust to match the predicted positive class column
)

print(overall_auc)
#################################Boosting###################################################
library(xgboost)
# Specify the model class
boost_spec <- boost_tree(trees = 500,learn_rate = tune(),tree_depth = tune(),sample_size = tune()) %>%
  # Set the mode
  set_mode("classification") %>%
  # Set the engine
  set_engine("xgboost")

boost_spec

# Create the tuning grid
tunegrid_boost <- grid_regular(parameters(boost_spec), 
                               levels = 3)

tunegrid_boost

# Fit and evaluate models for all folds
tune_results <- tune_grid(boost_spec,
                            fmla, 
                            resamples = cv_folds,
                            grid = tunegrid_boost,
                            metrics = metric_set(roc_auc))
autoplot(tune_results)

# Select the final hyperparameters
best_params <- select_best(tune_results)

# Finalize the specification
final_spec <- finalize_model(boost_spec, best_params)

# Perform cross-validation with the finalized specification
cv_results <- workflow() %>%
  add_formula(fmla) %>%         # Add the formula
  add_model(final_spec) %>%     # Add the finalized model specification
  fit_resamples(
    resamples = cv_folds,
    metrics = metric_set(roc_auc), # Specify evaluation metrics
    control = control_resamples(save_pred = TRUE) # Save predictions for evaluation
  )
# Collect cross-validated metrics
cv_metrics <- collect_metrics(cv_results)
print(cv_metrics) # Display AUC metrics for each fold
                            
# Extract predictions from cross-validation results
cv_predictions <- collect_predictions(cv_results)
                            
# Calculate AUC for the combined predictions
overall_auc <- roc_auc(data = cv_predictions, truth = target_var, .pred_TRUE, event_level = "second") # Adjust based on the name of the predicted probabilities for the positive class)
                            
# Print overall AUC
print(overall_auc)
