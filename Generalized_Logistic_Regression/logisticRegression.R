
library('readxl')
library('fastDummies')

#read the excel file into a dataframe
eBayDF<- read_xls("/Users/vinay/Downloads/GLM Logistic Regression HW Resources-20220211/eBayAuctions.xls")

#data preprocessing
eBayDF$Duration <- as.factor(eBayDF$Duration)
colnames(eBayDF)
colnames(eBayDF)[8] <- "Competitive"

#generating dummy variables
new_eBayDF <- dummy_cols(eBayDF, select_columns = c('Category','currency', 'Duration', 'endDay'))
dim(new_eBayDF)

#remove duplicated columns.
new_eBayDF[!duplicated(as.list(new_eBayDF))]
new_eBayDF <- within(new_eBayDF, rm('Category','currency', 'Duration', 'endDay', 'Category_Toys/Hobbies', 'currency_US', 'Duration_10', 'endDay_Wed'))
dim(new_eBayDF)


###Splitting into train and test data

## set the seed to make your partition reproducible
set.seed(50)

# training data - 60% and test data - 40%
## 60% of the sample size
train_size <- floor(0.6 * nrow(new_eBayDF))
train_ind <- sample(seq_len(nrow(new_eBayDF)), size = train_size)

train_df <- new_eBayDF[train_ind, ]
test_df <- new_eBayDF[-train_ind, ]


#calculate model_all glm model on training data
model_all <- glm(train_df$Competitive ~., data = train_df, family = binomial(link="logit"))
summary(model_all)
max(abs(model_all$coefficients))


#calculate model_all glm model for Category_Coins
model_single<- glm(train_df$Competitive~ train_df$`Category_Coins/Stamps`, family = binomial(link='logit'))
summary(model_single)


#sort the coefficients of model_all to find the top 4 predictors
sort(abs(model_all$coefficients), decreasing=TRUE)


#build the reduced model
train_df <- subset(train_df, select = c('Competitive','sellerRating','ClosePrice', 'OpenPrice', 'Category_Automotive', 'Category_Clothing/Accessories', 
                                  'Category_Coins/Stamps', 'Category_EverythingElse', 'Category_Health/Beauty', 'Category_SportingGoods',
                                  'currency_GBP', 'endDay_Mon'))
test_df <- subset(test_df, select = c('Competitive','sellerRating','ClosePrice', 'OpenPrice', 'Category_Automotive', 'Category_Clothing/Accessories', 
                                        'Category_Coins/Stamps', 'Category_EverythingElse', 'Category_Health/Beauty', 'Category_SportingGoods',
                                        'currency_GBP', 'endDay_Mon'))
model_reduced<- glm(train_df$Competitive ~., data = train_df, family = binomial(link="logit"))
summary(model_reduced)


#run anova test to compare two models- model_all and model_reduced
aov<- anova(model_reduced, model_all, test='Chisq')
print(aov)

#Dispersion test on data
residual_df<- aov[2,1]
residual_deviance<- aov[2,2]
dispersion = residual_deviance / residual_df
print(dispersion)
