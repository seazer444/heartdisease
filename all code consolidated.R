heart <- read.csv(file.choose()) # import dataset 


library(ggplot2)
library(tidyverse)
library(janitor)
library(factoextra)
library(vegan)
library(car)
library(AICcmodavg)
library(emmeans) # load all necessary packages 


head(heart) # double check dataset file loaded in correctly 


variable.names(heart) # check initial variable names
heart<-heart |> 
janitor::clean_names() # clean variable names for more coherent and easier analysis 


variable.names(heart) # check that variable names are cleaned as expected 


# code for analysis of variables prior to running models 


# pie chart for resting ECG
table_ecg<-table(heart$resting_ecg)


lbls<-paste(names(table_ecg), "\n", table_ecg, sep="")
pie(table_ecg, labels=lbls,
    main="Pie Chart of Resting ECG\n (with sample sizes)")
# pie chart for sex
table_sex<-table(heart$sex)


lbls<-paste(names(table_sex), "\n", table_sex, sep="")
pie(table_sex, labels=lbls,
    main="Pie Chart of Sex\n (with sample sizes)")
# pie chart for exercise-induced angina
table_angina<-table(heart$exercise_angina)


lbls<-paste(names(table_angina), "\n", table_angina, sep="")
pie(table_angina, labels=lbls,
    main="Pie Chart of Exercise-Induced Angina\n (with sample sizes)")
# pie chart for heart disease
table_heartd<-table(heart$heart_disease)


lbls<-paste(names(table_heartd), "\n", table_heartd, sep="")
pie(table_heartd, labels=lbls,
    main="Pie Chart of Heart Disease\n (with sample sizes)")


# full code for running PCA [a]


# remove values of 0 for cholesterol (NA values)
heart$cholesterol[heart$cholesterol==0] <-NA 
heart<-heart[!is.na(heart$cholesterol), ]
sum(is.na(heart$cholesterol)) # check if 0s removed – should return output [1] 0


#convert categorical variables to numeric
heart$sex<-as.numeric(as.factor(heart$sex))-1
heart$exercise_angina<-as.numeric(as.factor(heart$exercise_angina))-1
heart$resting_ecg <- as.numeric(factor(heart$resting_ecg, levels = c("Normal", "ST", "LVH")))-1
# check if factors have been successfully converted
is.numeric(heart$sex)
is.numeric(heart$exercise_angina)
is.numeric(heart$resting_ecg)


# run pca using only variables of interest 
pca_variables <- heart[, c("age", "sex", "cholesterol", "resting_ecg", "exercise_angina", "heart_disease")]
heart.pca <- prcomp(pca_variables, scale = TRUE)


# adjust labels used for variable names used in generated PCA output shown in presentation, for better understanding by viewers
pca_labels <- c("Age", "Sex", "Cholesterol", "Resting Electrocardiogram (ECG)", "Exercise-Induced Angina", "Heart Disease")
# apply to row names of rotation matrix (used as labels in fviz_pca_var)
rownames(heart.pca$rotation) <- pca_labels


# extract the percentage of variance explained by the first two principal components
pc_var <- summary(heart.pca)$importance[2, 1:2] * 100


# plot the points in PCA1 vs PCA2 space
fviz_pca_var(heart.pca,
             col.var = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE,               # prevents label overlap
             labelsize = 5,              # adjust label text size
             title = "PCA Variable Contributions",
             xlab = paste0("PC1 (", round(pc_var[1], 1), "%)"),
             ylab = paste0("PC2 (", round(pc_var[2], 1), "%)"),
             alpha.var = 0.8             # makes arrows slightly transparent
)


# generate a scree plot to understand significance of PCs 
screeplot(heart.pca, type = "lines")
fviz_eig(heart.pca)  # optional: better-looking scree plot using ggplot2 style
summary(heart.pca)          # summary of PCA (eigenvalues, variance explained)
heart.pca$rotation          # loadings (eigenvectors)


# run tests of ‘significance’ on scree plot with the broken stick model
bstick(heart.pca)
screeplot(heart.pca, bstick = TRUE, type = "lines")


# full code for running regression (ANCOVA)[b]


# code below aims to answer question 1: Given that age and sex are both predictors of heart disease, can age and sex predict an exercise-induced angina?


str(heart$exercise_angina) # double check variable type, was previously converted for PCA - should be numeric (as a binary variable) where 0 = no exercise-induced angina and 1 = exercise-induced angina


heart$sex <- factor(heart$sex, levels = c(0, 1), labels = c("F", "M")) # convert sex to factor, as it was converted to numeric for PCA
str(heart$sex) # double check sex was converted to factor w/ two levels 


# testing for whether or not there is a significant interaction for sex/age 
fit.ancova_inter_sa <-glm(exercise_angina~sex * age, data=heart, family=binomial)
fit.ancova_nointer_sa <-glm(exercise_angina~sex + age, data=heart, family=binomial)
anova(fit.ancova_inter_sa, fit.ancova_nointer_sa)
# the p-value is not significant, therefore they both form an equally good prediction and the simpler model should be taken, as the more complex model with the interaction is not necessary 


# use a same slopes model 
fit.ancova_sex_age <-glm(exercise_angina~sex + age, data=heart, family=binomial) 
plot(fit.ancova_sex_age) # residual plots generated are to look for outliers, since the model is a generalized linear model that prevents checking model assumptions based on plot shapes due to not following a normal distribution. There are no major outliers discerned that would skew data. 
summary(fit.ancova_sex_age) # summary to inspect if coefficients are different from 0 – results show that men are 2.77x more likely to get exercise-induced angina, which is statistically significant as p < 0.001. Every year increase in age results in 1.06x more likelihood in getting exercise-induced angina, which is statistically significant as p < 0.001 (These and subsequent summary results are derived from log-odds ratio) 


Anova(fit.ancova_sex_age, type=3) # type 3 is chosen because we want to look at it regardless of order – there is no specific required order 
# the large chi-square values from both sex and age show that both the predictors have a strong impact on explaining the outcome/both contribute to the model — sex and age are both strong predictors for exercise-induced angina (both p values are statistically significant) 


# code below aims to answer question 2: can cholesterol levels and resting ECG results be predictive of an exercise-induced angina?


# check variable types
str(heart$cholesterol) # numeric – leave as is 
str(heart$resting_ecg) # must convert to factor; numeric due to PCA 
heart$resting_ecg<-factor(heart$resting_ecg, levels=c("0","1","2"))
str(heart$resting_ecg) # check if successfully converted to factor 
str(heart$exercise_angina) # numeric – leave as is 


# test for an interaction between all variables 
fit.ancova_inter_ce <-glm(exercise_angina~cholesterol * resting_ecg, data=heart, family=binomial)
fit.ancova_nointer_ce <-glm(exercise_angina~ cholesterol + resting_ecg, data=heart, family=binomial)
anova(fit.ancova_inter_ce, fit.ancova_nointer_ce) # the order these are put in does not matter
# p-value is significant (0.03313), so the more complex model with interactions should be taken


# use separate slopes model
fit.ancova_cholesterol_ecg <-glm(exercise_angina~cholesterol * resting_ecg, data=heart, family=binomial)
plot(fit.ancova_cholesterol_ecg) # residual plots generated to identify outliers — no significant outliers discerned 
summary(fit.ancova_cholesterol_ecg) # summary taken to see if coefficients are distinct from 0




Anova(fit.ancova_cholesterol_ecg, type=3) # output shows that cholesterol (p ≈ 0.0398) and cholesterol & resting ecg interaction (p ≈ 0.0331) are significant, while resting ecg alone (p ≈ 0.122) is not 


# code below aims to answer question 3: Does the occurrence of an exercise-induced angina predict heart disease?


# double check variable types
str(heart$exercise_angina) # numeric 
str(heart$heart_disease) # also numeric 


# use glm and binomial (binary regression) because both the dependent and independent variables are binary in nature 
fit.ancova_exercise_angina <-glm(heart_disease~exercise_angina, data=heart, family=binomial)
plot(fit.ancova_exercise_angina) # residual plots look for outliers — no major outliers 
summary(fit.ancova_exercise_angina) # glm model summary to see if coefficients are distinct from 0 (no need to look for interactions for this question). Output shows those with a history of exercise-induced angina are 13.54x more likely to suffer from heart disease, p < 0.001. 


anova(fit.ancova_exercise_angina) # in this model, exercise-induced angina is a significant predictor of heart disease as p < 0.001. 


# full code for running AIC [c]


# test nested models for question 1
# test interaction model 
fitanc.int.q1 <- glm(exercise_angina~age*sex, data=heart, family=binomial)
plot(fitanc.int.q1) # check residual plots – there are no major outliers that can skew the data
summary(fitanc.int.q1) # age seems to explain the most variability in this model, as it is comparatively the most marginally significant but still p > 0.05 
anova(fitanc.int.q1) # interaction p value > 0.05; it is not significant 


# test no interaction model
fitanc.noint.q1 <- glm(exercise_angina~age + sex, data=heart, family=binomial)
plot(fitanc.noint.q1) # check residual plots – there are no major outliers that can skew data 
summary(fitanc.noint.q1) # both age and sex are significant, as both p < 0.001; sex seems to explain more variability than age 
anova(fitanc.noint.q1) # both age and sex have significant individual effects on exercise-induced angina 


models.q1 <- list(fitanc.int.q1, fitanc.noint.q1)
model.names.q1 <- c("fitanc.int.q1", "fitanc.noint.q1")


# run AIC 
aictab(cand.set=models.q1, modnames=model.names.q1) 
anova(fitanc.int.q1, fitanc.noint.q1) # there is no significant difference between the two models – less complex model without interaction taken since the AIC value differences <2 and age/sex are insignificant within the interaction model 


# test nested models for question 2
# test interaction model
fitanc.int.q2 <- glm(exercise_angina~cholesterol*resting_ecg, data=heart, family=binomial)
plot(fitanc.int.q2) # no significant outliers 
summary(fitanc.int.q2) 
anova(fitanc.int.q2) # each variable significantly improves the model, as each p value <0.05 (cholesterol: p = 0.02, resting ecg: p = 0.001, interaction: p = 0.03) 


# test no interaction model 
fitanc.noint.q2 <- glm(exercise_angina~cholesterol + resting_ecg, data=heart, family=binomial)
plot(fitanc.noint.q2) # no significant outliers 
summary(fitanc.noint.q2) # both resting ecg and cholesterol are significant independently  
anova(fitanc.noint.q2) # confirms that both are independently significant, as each p value <0.05 (cholesterol: p = 0.018, resting ecg: p = 0.001)  


models.q2 <- list(fitanc.int.q2, fitanc.noint.q2)
model.names.q2 <- c("fitanc.int.q2", "fitanc.noint.q2")


# run AIC
aictab(cand.set=models.q2, modnames=model.names.q2) # model with no interaction is the best fit 
anova(fitanc.int.q2, fitanc.noint.q2) # p value is significant (0.03313). However, the AIC value between the two models is very little and barely higher than 2. Because ANCOVA says the interaction is significant, we will take the interaction model. 


# ggplots for glms [d]
# logistic regression plot: Age vs. Exercise-Induced Angina
ggplot(heart, aes(x = age, y = exercise_angina)) +
  geom_point() +  # plot individual data points
  geom_smooth(method = "glm", method.args = list(family = "quasibinomial"),
              fill = "#00BFC4", color = "#0072B2") +  # change fill and line color
  theme(text = element_text(size = 20)) +
  labs(x = "Age", y = "Exercise-Induced Angina")  # label the axes


# logistic regression plot: Sex vs. Exercise-Induced Angina
ggplot(
  data.frame(sex = c(0, 1)) %>%
    mutate(
      pred = predict(glm(exercise_angina ~ sex, data = heart, family = "quasibinomial"),
                     newdata = ., type = "response"),  # predict probabilities using the model
      sex_label = c("Female", "Male")  # label the axes
    ),
  aes(x = sex_label, y = pred, fill = sex_label)  # map fill color to sex label
) +
  geom_col() +  # plot predicted probabilities as bars
  scale_fill_manual(values = c("Female" = "#F8766D", "Male" = "#00BFC4")) +  # assign custom colors
  labs(x = "Sex", y = "Exercise-Induced Angina") +
  theme(text = element_text(size = 20), legend.position = "none")  # remove legend 


# logistic regression plot: Cholesterol vs. Exercise-Induced Angina
ggplot(heart, aes(x = cholesterol, y = exercise_angina)) +
  geom_point() +  # plot individual observations
  geom_smooth(method = "glm", method.args = list(family = "quasibinomial"),
              fill = "#00BFC4", color = "#0072B2") +  # set custom fill and line color
  theme(text = element_text(size = 20)) +
  labs(x = "Cholesterol", y = "Exercise-Induced Angina")  # label axes


# logistic regression plot: Resting ECG vs. Exercise-Induced Angina
# create a new data frame with ECG codes: 0 = Normal, 1 = ST, 2 = LVH
ggplot(
  data.frame(resting_ecg = c(0, 1, 2)) %>%
    mutate(
      # predict the probability of having exercise-induced angina for each ECG category
      pred = predict(glm(exercise_angina ~ factor(resting_ecg), data = heart, family = "quasibinomial"),
                     newdata = ., type = "response"),
      # add labels and set display order
      ecg_label = factor(c("Normal", "ST", "LVH"), levels = c("Normal", "ST", "LVH"))
    ),
  aes(x = ecg_label, y = pred, fill = ecg_label)  # Map fill to ECG label for colored bars
) +
  # create a bar graph showing model-based predicted probabilities
  geom_col() +
  scale_fill_manual(values = c("Normal" = "#F8766D", "ST" = "#00BA38", "LVH" = "#619CFF")) +  # custom colors
  labs(x = "Resting ECG", y = "Exercise-Induced Angina") +
  theme(text = element_text(size = 20), legend.position = "none")  # remove legend


# logistic regression plot: Exercise-Induced Angina vs. Heart Disease
ggplot(
  data.frame(exercise_angina = c(0, 1)) %>%
    mutate(
      # use the model to predict the probability of heart disease for each angina group
      pred = predict(glm(heart_disease ~ exercise_angina, data = heart, family = "quasibinomial"),
                     newdata = ., type = "response"),
      # label the angina values for plotting
      angina_label = c("No", "Yes")
    ),
  aes(x = angina_label, y = pred, fill = angina_label)  # Use angina labels for bar colors
) +
  # create a bar graph showing predicted probabilities
  geom_col() +
  scale_fill_manual(values = c("No" = "#F8766D", "Yes" = "#00BA38")) +  # custom colors
  labs(x = "Exercise-Induced Angina", y = "Heart Disease") +
  theme(text = element_text(size = 20), legend.position = "none")  # hide legend


[a]PCA starts here
[b]ANCOVA regression starts here
[c]AIC
[d]ggplots