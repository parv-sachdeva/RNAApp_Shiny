# first we build a model which performs differential gene expression analysis - code 1
# then, we build an app which internally uses this model and is user interactive - code 2

if (!require("BiocManager", quietly = TRUE))
    install.packages("BiocManager")

BiocManager::install("MLSeq")


install.packages("kernlab")

library(MLSeq)
library(DESeq2)
library(edgeR)
library(VennDiagram)
library(pamr)
library(caret)
library(gplots)
library(tidyverse)
library(party)

filepath <- system.file("extdata/cervical.txt", package = "MLSeq")

cervical <- read.table(filepath, header=TRUE)

head(cervical[ ,1:10]) # Mapped counts for first 6 features of 10 subjects.

class <- DataFrame(condition = factor(rep(c("N","T"), c(29, 29))))
class

library(DESeq2)
set.seed(2128)
# We do not perform a differential expression analysis to select differentially
# expressed genes. However, in practice, DE analysis might be performed before
# fitting classifiers. Here, we selected top 100 features having the highest
# gene-wise variances in order to decrease computational cost.
vars <- sort(apply(cervical, 1, var, na.rm = TRUE), decreasing = TRUE)
data <- cervical[names(vars)[1:100], ]
nTest <- ceiling(ncol(data) * 0.3)
ind <- sample(ncol(data), nTest, FALSE)
# Minimum count is set to 1 in order to prevent 0 division problem within
# classification models.
data.train <- as.matrix(data[ ,-ind] + 1)
data.test <- as.matrix(data[ ,ind] + 1)
classtr <- DataFrame(condition = class[-ind, ])
classts <- DataFrame(condition = class[ind, ])

classts$condition

data.trainS4 = DESeqDataSetFromMatrix(countData = data.train, colData = classtr,
design = formula(~condition))
data.testS4 = DESeqDataSetFromMatrix(countData = data.test, colData = classts,
design = formula(~condition))

set.seed(2128)
# Support vector machines with radial basis function kernel
fit.svm <- classify(data = data.trainS4, method = "svmRadial",
preProcessing = "deseq-vst", ref = "T", tuneLength = 10,
control = trainControl(method = "repeatedcv", number = 5,
repeats = 10, classProbs = TRUE))
show(fit.svm)

trained(fit.svm)

plot(fit.svm)

set.seed(2128)

# Define control lists.
ctrl.continuous <- trainControl(method = "repeatedcv", number = 5, repeats = 10)
ctrl.discrete <- discreteControl(method = "repeatedcv", number = 5, repeats = 10,
                             tuneLength = 10)
# ctrl.voom <- voomControl(method = "repeatedcv", number = 5, repeats = 10,
#                              tuneLength = 10)

# 1. Continuous classifiers, SVM and NSC
fit.svm <- classify(data = data.trainS4, method = "svmRadial",
                 preProcessing = "deseq-vst", ref = "T", tuneLength = 10,
                 control = ctrl.continuous)
fit.cforest <- classify(data = data.trainS4, method = "cforest",
                 preProcessing = "deseq-vst", ref = "T", tuneLength = 10,
                 control = ctrl.continuous)

# fit.NSC <- classify(data = data.trainS4, method = "pam",
#                  preProcessing = "deseq-vst", ref = "T", tuneLength = 10,
#                  control = ctrl.continuous)

# # 2. Discrete classifiers
# fit.plda <- classify(data = data.trainS4, method = "PLDA", normalize = "deseq",
#                      ref = "T", control = ctrl.discrete)

# fit.plda2 <- classify(data = data.trainS4, method = "PLDA2", normalize = "deseq",
#                      ref = "T", control = ctrl.discrete)

# fit.nblda <- classify(data = data.trainS4, method = "NBLDA", normalize = "deseq",
#                      ref = "T", control = ctrl.discrete)

# # 3. voom-based classifiers
# fit.voomDLDA <- classify(data = data.trainS4, method = "voomDLDA",
#                          normalize = "deseq", ref = "T", control = ctrl.voom)

# fit.voomNSC <- classify(data = data.trainS4, method = "voomNSC",
#                          normalize = "deseq", ref = "T", control = ctrl.voom)

# 4. Predictions
pred.svm <- predict(fit.svm, data.testS4)
pred.cforest <- predict(fit.cforest, data.testS4)
# pred.NSC <- predict(fit.NSC, data.testS4)
# pred.plda <- predict(fit.plda, data.testS4)
# pred.nblda <- predict(fit.nblda, data.testS4)
# pred.voomDLDA <- predict(fit.voomDLDA, data.testS4)
# pred.voomNSC <- predict(fit.voomNSC, data.testS4)


plot(fit.svm)

conf_matrix <- confusionMatrix(pred.svm, data.testS4$condition)
print(conf_matrix)

# Get the confusion matrix as a table
conf_matrix_table <- as.matrix(conf_matrix$table)

# Plot heatmap with classic color scheme and annotations
heatmap.2(conf_matrix_table, 
          col = rev(colorRampPalette(c("red", "white"))(10)),  # Classic heatmap colors
          scale = "none",           # Optional: whether to scale rows and/or columns
          Rowv = NA,                # Optional: don't cluster rows
          Colv = NA,                # Optional: don't cluster columns
          margins = c(5, 10),       # Add extra space for labels
          main = "Confusion Matrix Heatmap",  # Title of the plot
          xlab = "Predicted Class",            # Label for x-axis
          ylab = "Actual Class",               # Label for y-axis
          density.info = "histogram",         # Display histogram of values
          trace = "none",                     # Turn off trace lines
          cellnote = conf_matrix_table,       # Add cell annotations
          notecol = "black",                  # Annotation text color
          notecex = 2)                      # Annotation text size

conf_matrix <- confusionMatrix(pred.cforest, data.testS4$condition)
print(conf_matrix)

# Get the confusion matrix as a table
conf_matrix_table <- as.matrix(conf_matrix$table)

# Plot heatmap with classic color scheme and annotations
heatmap.2(conf_matrix_table, 
          col = rev(colorRampPalette(c("red", "white"))(10)),  # Classic heatmap colors
          scale = "none",           # Optional: whether to scale rows and/or columns
          Rowv = NA,                # Optional: don't cluster rows
          Colv = NA,                # Optional: don't cluster columns
          margins = c(5, 10),       # Add extra space for labels
          main = "Confusion Matrix Heatmap",  # Title of the plot
          xlab = "Predicted Class",            # Label for x-axis
          ylab = "Actual Class",               # Label for y-axis
          density.info = "histogram",         # Display histogram of values
          trace = "none",                     # Turn off trace lines
          cellnote = conf_matrix_table,       # Add cell annotations
          notecol = "black",                  # Annotation text color
          notecex = 2)                      # Annotation text size

library(xtable)

actual <- data.testS4$condition
nn <- length(actual)
diag.svm <- sum(diag(table(pred.svm, actual)))
diag.cforest <- sum(diag(table(pred.cforest, actual)))

# diag.NSC <- sum(diag(table(pred.NSC, actual)))
# diag.plda <- sum(diag(table(pred.plda, actual)))
# diag.nblda <- sum(diag(table(pred.nblda, actual)))
# diag.voomDLDA <- sum(diag(table(pred.voomDLDA, actual)))
# diag.voomNSC <- sum(diag(table(pred.voomNSC, actual)))

acc <- c(diag.svm, diag.cforest) / nn
# sparsity <- c(NA, trained(fit.NSC)$finalModel$nonzero/nrow(data.testS4),
#               length(selectedGenes(fit.plda))/nrow(data.testS4), NA, NA,
#               length(selectedGenes(fit.voomNSC))/nrow(data.testS4))

tbl <- data.frame(Classifier = c("SVM", "Random Forest"), Accuracy = round(acc*100, 2))
tbl

# xtbl <- xtable(tbl, caption = "Classification results for cervical data.", label = "tbl:accRes", align = "lp{4cm}p{2cm}c")

# digits(xtbl) <- c(0, 0, 3, 3)
# # print.xtable(xtbl, caption.placement = "top", include.rownames = FALSE, booktabs = TRUE)
# tbl

availableMethods() %>% sort()
