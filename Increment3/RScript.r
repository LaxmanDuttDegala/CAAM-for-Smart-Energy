library(rattle)

# This log generally records the process of building a model. However, with very 
# little effort the log can be used to score a new dataset. The logical variable 
# 'building' is used to toggle between generating transformations, as when building 
# a model, and simply using the transformations, as when scoring a dataset.

building <- TRUE
scoring  <- ! building

# The colorspace package is used to generate the colours used in plots, if available.
library(colorspace)
# A pre-defined value is used to reset the random seed so that results are repeatable.
crv$seed <- 42 

# Build the training/validate/test datasets.
set.seed(crv$seed) 
crs$nobs <- nrow(crs$dataset) # 4382 observations 
crs$sample <- crs$train <- sample(nrow(crs$dataset), 0.7*crs$nobs) # 3067 observations
crs$validate <- sample(setdiff(seq_len(nrow(crs$dataset)), crs$train), 0.15*crs$nobs) # 657 observations
crs$test <- setdiff(setdiff(seq_len(nrow(crs$dataset)), crs$train), crs$validate) # 658 observations

# The following variable selections have been noted.
crs$input <- c("MULTSTV", "MICRO", "TOASTER", "COFFEE",
     "NUMFRIG", "CWASHER", "WASHLOAD", "DRYER",
     "NUMCFAN", "TVCOLOR", "BIGTV", "DVD",
     "PLAYSTA", "TVONWD", "TVONWE", "STEREO",
     "NUMPC", "LAPTOPPC", "PCTYPE1", "PCPRINT",
     "AIRCOND", "LGT12", "NOUTLGTNT")

crs$numeric <- c("MULTSTV", "MICRO", "TOASTER", "COFFEE",
     "NUMFRIG", "CWASHER", "WASHLOAD", "DRYER",
     "NUMCFAN", "TVCOLOR", "BIGTV", "DVD",
     "PLAYSTA", "TVONWD", "TVONWE", "STEREO",
     "NUMPC", "LAPTOPPC", "PCTYPE1", "PCPRINT",
     "AIRCOND", "LGT12", "NOUTLGTNT")

crs$categoric <- NULL
crs$target  <- "DOLLAREL"
crs$risk    <- NULL
crs$ident   <- "DOEID"
crs$ignore  <- NULL
crs$weights <- NULL

#============================================================
# Rattle timestamp: 2014-04-01 13:29:11 x86_64-w64-mingw32 
# Box Plot 
# The 'ggplot2' package provides the 'ggplot' function.
require(ggplot2, quietly=TRUE)

# Box Plot for NUMFRIG
pp <- with(crs,
           ggplot(dataset[sample,]) + 
           geom_boxplot(aes("All", NUMFRIG), notch=TRUE) + 
           geom_boxplot(aes(DOLLAREL, NUMFRIG), notch=TRUE) + 
           xlab("DOLLAREL\n\nRattle 2014-Apr-01 13:29:13 Laxman Dutt Degala") + 
           ggtitle("Distribution of NUMFRIG (sample)") + 
           theme(legend.position="none")
          )
print(pp)

# Box Plot for DOLLAREL
pp <- with(crs,
           ggplot(dataset[sample,]) + 
           geom_boxplot(aes("All", DOLLAREL), notch=TRUE) + 
           geom_boxplot(aes(DOLLAREL, DOLLAREL), notch=TRUE) + 
           xlab("DOLLAREL\n\nRattle 2014-Apr-01 13:30:18 Laxman Dutt Degala") + 
           ggtitle("Distribution of DOLLAREL (sample)") + 
           theme(legend.position="none")
          )
print(pp)


# Decision Tree 
# The 'rpart' package provides the 'rpart' function.
require(rpart, quietly=TRUE)

# Reset the random number seed to obtain the same results each time.
set.seed(crv$seed)

# Build the Decision Tree model.
crs$rpart <- rpart(DOLLAREL ~ .,
    data=crs$dataset[crs$train, c(crs$input, crs$target)],
    method="anova",
    parms=list(split="information"),
    control=rpart.control(usesurrogate=0, 
        maxsurrogate=0))

# Generate a textual view of the Decision Tree model.
print(crs$rpart)
printcp(crs$rpart)
cat("\n")

# List the rules from the tree using a Rattle support function.
asRules(crs$rpart)


# Plot the resulting Decision Tree. 
# We use the rpart.plot package.
fancyRpartPlot(crs$rpart, main="Decision Tree Data for 25 attrubutes.csv $ DOLLAREL")

# Random Forest 
# The 'randomForest' package provides the 'randomForest' function.
require(randomForest, quietly=TRUE)

# Build the Random Forest model.
set.seed(crv$seed)
crs$rf <- randomForest(DOLLAREL ~ .,
      data=crs$dataset[crs$sample,c(crs$input, crs$target)], 
      ntree=500,
      mtry=4,
      importance=TRUE,
      na.action=na.roughfix,
      replace=FALSE)

# Generate textual output of 'Random Forest' model.
crs$rf

# List the importance of the variables.
rn <- round(importance(crs$rf), 2)
rn[order(rn[,1], decreasing=TRUE),]

# Decision Tree 
# The 'rpart' package provides the 'rpart' function.
require(rpart, quietly=TRUE)

# Reset the random number seed to obtain the same results each time.
set.seed(crv$seed)

# Build the Decision Tree model.
crs$rpart <- rpart(DOLLAREL ~ .,
    data=crs$dataset[crs$train, c(crs$input, crs$target)],
    method="anova",
    parms=list(split="information"),
    control=rpart.control(usesurrogate=0, 
        maxsurrogate=0))

# Generate a textual view of the Decision Tree model.

print(crs$rpart)
printcp(crs$rpart)
cat("\n")

# Plot the resulting Decision Tree. 
# We use the rpart.plot package.
fancyRpartPlot(crs$rpart, main="Decision Tree Data for 25 attrubutes.csv $ DOLLAREL")

# Risk Chart: requires the ggplot2 package.

require(ggplot2, quietly=TRUE)

# Generate a risk chart.

# Rattle provides evaluateRisk() and riskchart().
crs$pr <- predict(crs$rpart, newdata=crs$dataset[crs$test, c(crs$input, crs$target)])
crs$eval <- evaluateRisk(crs$pr, crs$dataset[crs$test, c(crs$input, crs$target)]$DOLLAREL)
print(riskchart(crs$pr, 
                crs$dataset[crs$test, c(crs$input, crs$target)]$DOLLAREL, 
                title="Performance Chart Decision Tree Data for 25 attrubutes.csv [test] ", show.lift=FALSE, show.precision=FALSE))



# Evaluate model performance. 
# Risk Chart: requires the ggplot2 package.
require(ggplot2, quietly=TRUE)

# Generate a risk chart.
# Rattle provides evaluateRisk() and riskchart().

crs$pr <- predict(crs$rpart, newdata=crs$dataset)
crs$eval <- evaluateRisk(crs$pr, crs$dataset$DOLLAREL)
print(riskchart(crs$pr, 
                crs$dataset$DOLLAREL, 
                title="Performance Chart Decision Tree Data for 25 attrubutes.csv ", show.lift=FALSE, show.precision=FALSE))


# Decision Tree 
# The 'rpart' package provides the 'rpart' function.
require(rpart, quietly=TRUE)

# Reset the random number seed to obtain the same results each time.
set.seed(crv$seed)
# Build the Decision Tree model.
crs$rpart <- rpart(DOLLAREL ~ .,
    data=crs$dataset[crs$train, c(crs$input, crs$target)],
    method="anova",
    parms=list(split="information"),
    control=rpart.control(usesurrogate=0, 
        maxsurrogate=0))

# Generate a textual view of the Decision Tree model.

print(crs$rpart)
printcp(crs$rpart)
cat("\n")

# Time taken: 0.07 secs

#============================================================
# Rattle timestamp: 2014-04-01 20:38:38 x86_64-w64-mingw32 

# Random Forest 

# The 'randomForest' package provides the 'randomForest' function.

require(randomForest, quietly=TRUE)

# Build the Random Forest model.

set.seed(crv$seed)
crs$rf <- randomForest(DOLLAREL ~ .,
      data=crs$dataset[crs$sample,c(crs$input, crs$target)], 
      ntree=500,
      mtry=4,
      importance=TRUE,
      na.action=na.roughfix,
      replace=FALSE)

# Generate textual output of 'Random Forest' model.

crs$rf

# List the importance of the variables.

rn <- round(importance(crs$rf), 2)
rn[order(rn[,1], decreasing=TRUE),]

# Time taken: 7.69 secs

#============================================================
# Rattle timestamp: 2014-04-01 20:38:46 x86_64-w64-mingw32 

# Regression model 

# Build a Regression model.

crs$glm <- lm(DOLLAREL ~ ., data=crs$dataset[crs$train,c(crs$input, crs$target)])

# Generate a textual view of the Linear model.

print(summary(crs$glm))
cat('==== ANOVA ====

')
print(anova(crs$glm))
print("
")

# Time taken: 0.03 secs

#============================================================
# Rattle timestamp: 2014-04-01 20:38:46 x86_64-w64-mingw32 

# Neural Network 

# Build a neural network model using the nnet package.

require(nnet, quietly=TRUE)

# Build the NNet model.

set.seed(199)
crs$nnet <- nnet(DOLLAREL ~ .,
    data=crs$dataset[crs$sample,c(crs$input, crs$target)],
    size=10, linout=TRUE, skip=TRUE, MaxNWts=10000, trace=FALSE, maxit=100)

# Print the results of the modelling.

cat(sprintf("A %s network with %d weights.\n",
    paste(crs$nnet$n, collapse="-"),
    length(crs$nnet$wts)))
cat(sprintf("Inputs: %s.\n",
    paste(crs$nnet$coefnames, collapse=", ")))
cat(sprintf("Output: %s.\n",
    names(attr(crs$nnet$terms, "dataClasses"))[1]))
cat(sprintf("Sum of Squares Residuals: %.4f.\n",
    sum(residuals(crs$nnet) ^ 2)))
cat("\n")
print(summary(crs$nnet))
cat('\n')

# Time taken: 0.92 secs

#============================================================
# Rattle timestamp: 2014-04-01 20:39:29 x86_64-w64-mingw32 

# GENERATE A REPORT 

# The odfWeave package processes ODT document templates.

require(odfWeave, quietly=TRUE)

#============================================================
# Rattle timestamp: 2014-04-01 20:45:10 x86_64-w64-mingw32 

# Save the project data (variable crs) to file.

save(crs, file="C:/Users\Laxman Dutt Degala\Documents\CAAM.rattle", compress=TRUE)

# List the rules from the tree using a Rattle support function.

asRules(crs$rpart)

# List the rules from the tree using a Rattle support function.

asRules(crs$rpart)

# Display tree number 1.

printRandomForests(crs$rf, 1)

# Plot the error rate against the number of trees.

plot(crs$rf, main="")
legend("topright", c(""), text.col=1:6, lty=1:3, col=1:3)
title(main="Error Rates Random Forest Data for 25 attrubutes.csv",
    sub=paste("Rattle", format(Sys.time(), "%Y-%b-%d %H:%M:%S"), Sys.info()["user"]))

# Plot the OOB ROC curve.

require(verification)
aucc <- roc.area(as.integer(as.factor(crs$dataset[crs$sample, crs$target]))-1,
                 crs$rf$votes[,2])$A
roc.plot(as.integer(as.factor(crs$dataset[crs$sample, crs$target]))-1,
         crs$rf$votes[,2], main="")
legend("bottomright", bty="n",
       sprintf("Area Under the Curve (AUC) = %1.3f", aucc))
title(main="OOB ROC Curve Random Forest Data for 25 attrubutes.csv",
    sub=paste("Rattle", format(Sys.time(), "%Y-%b-%d %H:%M:%S"), Sys.info()["user"]))

# Plot the OOB ROC curve.

require(verification)
aucc <- roc.area(as.integer(as.factor(crs$dataset[crs$sample, crs$target]))-1,
                 crs$rf$votes[,2])$A
roc.plot(as.integer(as.factor(crs$dataset[crs$sample, crs$target]))-1,
         crs$rf$votes[,2], main="")
legend("bottomright", bty="n",
       sprintf("Area Under the Curve (AUC) = %1.3f", aucc))
title(main="OOB ROC Curve Random Forest Data for 25 attrubutes.csv",
    sub=paste("Rattle", format(Sys.time(), "%Y-%b-%d %H:%M:%S"), Sys.info()["user"]))

# Plot the OOB ROC curve.

require(verification)
aucc <- roc.area(as.integer(as.factor(crs$dataset[crs$sample, crs$target]))-1,
                 crs$rf$votes[,2])$A
roc.plot(as.integer(as.factor(crs$dataset[crs$sample, crs$target]))-1,
         crs$rf$votes[,2], main="")
legend("bottomright", bty="n",
       sprintf("Area Under the Curve (AUC) = %1.3f", aucc))
title(main="OOB ROC Curve Random Forest Data for 25 attrubutes.csv",
    sub=paste("Rattle", format(Sys.time(), "%Y-%b-%d %H:%M:%S"), Sys.info()["user"]))

#============================================================
# Rattle timestamp: 2014-04-01 22:52:48 x86_64-w64-mingw32 

# Score a dataset. 

# Obtain predictions for the Decision Tree model on Data for 25 attrubutes.csv [validate].

crs$pr <- predict(crs$rpart, newdata=crs$dataset[crs$validate, c(crs$input)])

# Obtain predictions for the Random Forest model on Data for 25 attrubutes.csv [validate].

crs$pr <- predict(crs$rf, newdata=na.omit(crs$dataset[crs$validate, c(crs$input)]))

# Obtain predictions for the Linear model on Data for 25 attrubutes.csv [validate].

crs$pr <- predict(crs$glm, type="response", newdata=crs$dataset[crs$validate, c(crs$input)])

# Obtain predictions for the Neural Net model on Data for 25 attrubutes.csv [validate].

crs$pr <- predict(crs$nnet, newdata=crs$dataset[crs$validate, c(crs$input)])

# Extract the relevant variables from the dataset.

sdata <- subset(crs$dataset[crs$validate,], select=c("DOEID", "DOLLAREL"))

# Output the combined data.

write.csv(cbind(sdata, crs$pr), file="C:\Users\Laxman Dutt Degala\Documents\Data_for_25_attrubutes_validate_score_idents.csv", row.names=FALSE)

#============================================================
# Rattle timestamp: 2014-04-01 22:57:51 x86_64-w64-mingw32 

# Evaluate model performance. 

# Risk Chart: requires the ggplot2 package.

require(ggplot2, quietly=TRUE)

# Generate a risk chart.

# Rattle provides evaluateRisk() and riskchart().

crs$pr <- predict(crs$rpart, newdata=crs$dataset[crs$sample, c(crs$input, crs$target)])
crs$eval <- evaluateRisk(crs$pr, crs$dataset[crs$sample, c(crs$input, crs$target)]$DOLLAREL)
print(riskchart(crs$pr, 
                crs$dataset[crs$sample, c(crs$input, crs$target)]$DOLLAREL, 
                title="Performance Chart Decision Tree Data for 25 attrubutes.csv [**train**] ", show.lift=FALSE, show.precision=FALSE))


#============================================================
# Rattle timestamp: 2014-04-01 22:59:36 x86_64-w64-mingw32 

# Score a dataset. 

# Obtain predictions for the Decision Tree model on Data for 25 attrubutes.csv [**train**].

crs$pr <- predict(crs$rpart, newdata=crs$dataset[crs$sample, c(crs$input)])

# Obtain predictions for the Random Forest model on Data for 25 attrubutes.csv [**train**].

crs$pr <- predict(crs$rf, newdata=na.omit(crs$dataset[crs$sample, c(crs$input)]))

# Obtain predictions for the Linear model on Data for 25 attrubutes.csv [**train**].

crs$pr <- predict(crs$glm, type="response", newdata=crs$dataset[crs$sample, c(crs$input)])

# Obtain predictions for the Neural Net model on Data for 25 attrubutes.csv [**train**].

crs$pr <- predict(crs$nnet, newdata=crs$dataset[crs$sample, c(crs$input)])

# Extract the relevant variables from the dataset.

sdata <- subset(crs$dataset[crs$sample,], select=c("DOEID", "DOLLAREL"))

# Output the combined data.

write.csv(cbind(sdata, crs$pr), file="C:\Users\Laxman Dutt Degala\Documents\Data_for_25_attrubutes_train_score_idents_train.csv", row.names=FALSE)

#============================================================
# Rattle timestamp: 2014-04-01 23:01:25 x86_64-w64-mingw32 

# Score a dataset. 

# Obtain predictions for the Decision Tree model on Data for 25 attrubutes.csv [test].

crs$pr <- predict(crs$rpart, newdata=crs$dataset[crs$test, c(crs$input)])

# Obtain predictions for the Random Forest model on Data for 25 attrubutes.csv [test].

crs$pr <- predict(crs$rf, newdata=na.omit(crs$dataset[crs$test, c(crs$input)]))

# Obtain predictions for the Linear model on Data for 25 attrubutes.csv [test].

crs$pr <- predict(crs$glm, type="response", newdata=crs$dataset[crs$test, c(crs$input)])

# Obtain predictions for the Neural Net model on Data for 25 attrubutes.csv [test].

crs$pr <- predict(crs$nnet, newdata=crs$dataset[crs$test, c(crs$input)])

# Extract the relevant variables from the dataset.

sdata <- subset(crs$dataset[crs$test,], select=c("DOEID", "DOLLAREL"))

# Output the combined data.

write.csv(cbind(sdata, crs$pr), file="C:\Users\Laxman Dutt Degala\Documents\Data_for_25_attrubutes_test_score_idents_test.csv", row.names=FALSE)

#============================================================
# Rattle timestamp: 2014-04-17 18:11:58 x86_64-w64-mingw32 

# Reload the project data (variable crs) from file.

load("C:\Users\Laxman Dutt Degala\Documents\CAAM.rattle")