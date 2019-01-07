rm(list=ls())

library(readxl)
library(tidyr)
library(dplyr)
library(stringr)
library(lubridate)
library(openxlsx)
library(mice)
library(ROCR)

X_file<- "Q:/DATA/SPRAIMU/4_SysRisk/Data/Output/fulldata_panel.Rda"
Y_file<- "Q:/DATA/SPRAIMU/4_SysRisk/Analysis/SYSTEMIC BANKING CRISES DATABASE_2018.xlsx"

#########========= 1. Load X and Y  ================########

load(X_file)

X<- saveData_Panel

X<- X%>%mutate(Year = year(Quarter))

Y_Raw <- read_xlsx(path = Y_file, sheet = "Crisis Years")

Y_Raw <- Y_Raw%>%filter(!is.na(Country))%>%
    rename('Year' = 'Systemic Banking Crisis (starting date)')%>%
    select(Country, Year)

Y <- Y_Raw%>%separate(Year,into = c("Y1", "Y2", "Y3", "Y4"))%>%
    gather(col = c("Y1", "Y2", "Y3", "Y4"), na.rm = TRUE, value = 'Year')%>%
    select(-'key')%>%mutate(Crisis_Start = 1)

Y$Year <- as.integer(Y$Year)

###====== 2. Merge X and Y into dataframe df========= ###

df <- left_join(X,Y, by =c('Country','Year'))

# Create Crisis Dummy "Crisis_F" if Crisis (Start) Year falls within year+1,2,3
df<- df%>%arrange(Country,Year)%>%
    group_by(Country)%>%
    mutate(Crisis_1= lead(Crisis_Start,4))%>%
    mutate(Crisis_2= lead(Crisis_Start,8))%>%
    mutate(Crisis_3= lead(Crisis_Start,12))

df$Crisis_F = rowSums(df[,c('Crisis_1','Crisis_2','Crisis_3')], na.rm = TRUE )

df<- df%>%select(-c(Crisis_Start,Crisis_1,Crisis_2,Crisis_3))

###======= 3. Data Processing ==================###

# a. Missing value imputation for X
## Keep rows with <=70 variables missing
df$na_count <- apply(df, 1, function(x) sum(is.na(x)))
df <- df%>%filter(na_count <= 70)%>%
    select(-na_count)

## Impute with global mean for the remaining missing values
#md.pattern(df)
mice_imputes = mice(df, method = 'mean', m = 1, maxit =1)
df_balanced = complete(mice_imputes)
# b. Dummary Variable Generation
df_balanced$Country <- factor(df_balanced$Country)

# Alternative approach generate dummies
# country_dummies <-dummy(df_balanced$Country, sep ='_') 
# df_balanced <- cbind(df_balanced,country_dummies)%>%select(-Country)

# remove redundante variables
df_balanced<- df_balanced%>%select(-EMBIG_Spread)

###======= 4. Full Period Regression ==================###

X<- df_balanced%>%select(-c('Code','Quarter','Year','Crisis_F'))
Y<- df_balanced%>%select(Crisis_F)

model <- glm(formula = Crisis_F ~ .- Code - Quarter - Year , family = binomial(link = "probit"), data = df_balanced)

# In-sample evalutaion
p <- predict(model, newdata= df_balanced, type="response")
pr <- prediction(p, df_balanced$Crisis_F)
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf)
auc <- performance(pr, measure = "auc")
auc <- auc@y.values[[1]]
auc

###========== 5. Out-of-sample in 2012 ==============####
training_data <- df_balanced%>%filter(Year<=2012)
test_data <- df_balanced%>%filter(Year>2012)

model_2012 <- glm(formula = Crisis_F ~ .- Code - Quarter - Year , family = binomial(link = "logit"), data = training_data)

### test set evaluation 
p <- predict(model_2012, newdata= test_data, type="response")
pr <- prediction(p, test_data$Crisis_F)

auc <- performance(pr, measure = "auc")
auc <- auc@y.values[[1]]
auc
