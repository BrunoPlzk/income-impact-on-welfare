# Clément Rieux
# Bruno Pilarczyk
#------------
#------------
############# Applied econometrics : Empirical Project ######################################
############# Impact of the income on happiness according to the social context #############
############# Script ########################################################################

#### Require MacTex or equivalent

# PACKAGES INSTALLATION
install.packages("texreg")
install.packages("foreign")
install.packages("nnet")
install.packages("stargazer")
install.packages("mlogit")
install.packages("margins")
install.packages("dplyr")
install.packages("MASS")
install.packages("AER")
install.packages("xtable")
install.packages("tikzDevice")
install.packages("erer")
install.packages("ordinal")
install.packages("reshape2")
install.packages("ggplot2")

# PACKAGES REQUIRED ---------------------
library(texreg)
library(foreign)
library(nnet)
library(stargazer)
library(mlogit)
library(margins)
library(dplyr)
library(MASS)
library(AER)
library(xtable)
library(tikzDevice)
library(erer)
library(ordinal)
require(reshape2)
require(ggplot2)

# Importing the data---------------------------------
library(foreign)
read.dct <- function(dct, labels.included = "yes") {
  temp <- readLines(dct)
  temp <- temp[grepl("_column", temp)]
  switch(labels.included,
         yes = {
           pattern <- "_column\\(([0-9]+)\\)\\s+([a-z0-9]+)\\s+(.*)\\s+%([0-9]+)[a-z]\\s+(.*)"
           classes <- c("numeric", "character", "character", "numeric", "character")
           N <- 5
           NAMES <- c("StartPos", "Str", "ColName", "ColWidth", "ColLabel")
         },
         no = {
           pattern <- "_column\\(([0-9]+)\\)\\s+([a-z0-9]+)\\s+(.*)\\s+%([0-9]+).*"
           classes <- c("numeric", "character", "character", "numeric")
           N <- 4
           NAMES <- c("StartPos", "Str", "ColName", "ColWidth")
         })
  temp_metadata <- setNames(lapply(1:N, function(x) {
    out <- gsub(pattern, paste("\\", x, sep = ""), temp)
    out <- gsub("^\\s+|\\s+$", "", out)
    out <- gsub('\"', "", out, fixed = TRUE)
    class(out) <- classes[x] ; out }), NAMES)
  temp_metadata[["ColName"]] <- make.names(gsub("\\s", "", temp_metadata[["ColName"]]))
  temp_metadata
}

read.dat <- function(dat, metadata_var, labels.included = "yes") {
  read.fwf(dat, widths = metadata_var[["ColWidth"]], col.names = metadata_var[["ColName"]])
}

GSS_metadata <- read.dct("GSS.dct")
GSS_ascii <- read.dat("GSS.dat", GSS_metadata)
attr(GSS_ascii, "col.label") <- GSS_metadata[["ColLabel"]]
GSS <- GSS_ascii

# Cleaning the data (we erase 8,9, 98,99 : people who does not want to answer or answers
# innaplicable and we also erase missing observations into the column of REALINC)
data<-GSS[!(GSS$HAPPY==9 | GSS$HAPPY==8),] 

data<- data[!(data$DEGREE==8 | data$DEGREE == 9),]
data<- data[!(data$MARITAL==9),]
data<- data[!(data$RELIG ==98 | data$RELIG == 99),]
data<- data[!(data$WRKSTAT==8),]
data<- data[!(data$AGE ==98 | data$AGE == 99),]
data<- data[!(data$REALINC==0),]

# Simplifying and renaming variables (changing categorical variables into dummies and
# renaming variables with simpler designation)
data$work <- ifelse(data$WRKSTAT < 3, 1,0) # simplifier au max, on ne tient pas compte des étudiants et des tretraités
data$married <- ifelse(data$MARITAL == 1, 1,0)
data$relig <- ifelse(data$RELIG == 4, 0,1 )
data$nonwhite <- ifelse(data$RACE == 1, 0,1)
data$female <- ifelse(data$SEX == 2, 1,0)
data$educ <- ifelse(data$DEGREE == 3 | data$DEGREE == 4 , 1,0)

database <- data[, -c(3:7)]
database <- database[, -c(5:7)]

colnames(database) <- c("id","welfare","age","child","inc","work","married","relig","nonwhite","female",
                         "educ")

(summary(database))

#Arrangement welfare (in order to have : - welfare = 0 (Not too happy)
#                                        - welfare = 1 (Pretty happy)
#                                        - welfare = 2 (Very happy)

database$welfare[database$welfare==1] <- "5"
database$welfare[database$welfare==2] <- "1"
database$welfare[database$welfare==3] <- "4"

database$welfare[database$welfare==4] <- "0"
database$welfare[database$welfare==5] <- "2"


# Changing numerical variables into factors
database$welfare <- ordered(database$welfare,
                             levels = c(0,1,2),
                             labels = c("Not too happy", "Pretty happy", "Very happy"))

database$married <- factor(database$married,
                    levels = c(0,1),
                    labels = c("single", "married"))
database$relig <- factor(database$relig,
                            levels = c(0,1),
                            labels = c("atheist", "religious"))
database$female <- factor(database$female,
                          levels = c(0,1),
                          labels = c("male", "female"))
database$educ <- factor(database$educ,
                          levels = c(0,1),
                          labels = c("not graduated", "graduated"))
database$work <- factor(database$work,
                          levels = c(0,1),
                          labels = c("unemployed", "working"))
database$nonwhite <- factor(database$nonwhite,
                             levels = c(0,1),
                             labels = c("white", "non-white"))

#### We erase the first column (the id column)
database <- database[,-1]

###### We divide inc by 10000 in order to have a more interpretable estimated coefficient of inc
database$inc <- (database$inc)/10000


######## We add the square of inc
database$inc2 <- (database$inc)^2

######## Reorganization of the columns

database <- database[,c(1,4,11,2,9,7,6,10,5,3,8)]


#### Median income

median(database$inc)   ### median income

# Ordered Logistic Regression (M1) : 
M1 <- polr(welfare ~ inc + age + female + relig + married 
              + educ + work + child + nonwhite,
              data = database, Hess = TRUE, method ="logistic")
summary(M1)                                                 

# Ordered Logisic Regression (M2) (with inc2) :

M2 <- polr(welfare ~ inc + inc2  + age + female + relig + married 
           + educ + work + child + nonwhite,
           data = database, Hess = TRUE, method ="logistic")
summary(M2)

# Graphical analysis 

summary(database)

###### -> now we know the average point in order to set those characteristics for the
######    graphical analysis

# (1) Predicted probabilities of welfare according to inc at the average point

## (1) Predicted probabilities of welfare according to inc at the average point using model (M1)
inc_M1 <- data.frame(inc = seq(from = 0, length = 150, by =0.1) , female = "female", 
                        nonwhite = "white" , age =49,
                        educ = "not graduated", work = "working", child = 2
                        , married = "single", relig ="religious")

inc_M1 <- cbind(inc_M1, predict(M1, inc_M1, type ="probs"))
p_inc_M1 <- melt(inc_M1, id.vars = c("inc", "female", "nonwhite", "age", "educ","work", "child","married","relig" ), variable.name = "welfare", value.name = "Probability")
plot_inc_M1 <- ggplot(p_inc_M1, aes(x = inc, y = Probability, colour = welfare )) + geom_line()
plot_inc_M1

### (1bis) Different representation (M1) : 

p_inc_M1_bis <- melt(inc_M1, id.vars = c("inc", "female", "nonwhite", "age", "educ","work", "child","married","relig" ), value.name = "Probability")
plot_inc_M1_bis <- ggplot(p_inc_M1_bis, aes(x = inc, y = Probability)) + geom_line() +facet_grid(variable ~., scales =)
plot_inc_M1_bis

## (1) Predicted probabilities of welfare according to inc at the average point using model (M2)

inc_M2 <- data.frame(inc =seq(from = 0, length = 150, by =0.1), female = "female", 
                        nonwhite = "white" , age =49, 
                        educ = "not graduated", work = "working", child = 2
                        , married = "single", relig ="religious")
inc_M2$inc2 <- (inc_M2$inc)^2
inc_M2 <- inc_M2[,c(1,10,2,3,4,5,6,7,8,9)]

inc_M2 <- cbind(inc_M2, predict(M2, inc_M2, type = "probs"))
p_inc_M2 <- melt(inc_M2, id.vars = c("inc", "inc2", "female", "nonwhite", "age", "educ","work", "child","married","relig" ), variable.name = "welfare", value.name = "Probability")
plot_inc_M2 <- ggplot(p_inc_M2, aes(x = inc, y = Probability, colour = welfare )) + geom_line()
plot_inc_M2

### (1bis) Different representation (M2) : 
p_inc_M2_bis <- melt(inc_M2, id.vars = c("inc", "inc2", "female", "nonwhite", "age", "educ","work", "child","married","relig" ), value.name = "Probability")
plot_inc_M2_bis <- ggplot(p_inc_M2_bis, aes(x = inc, y = Probability)) + geom_line() +facet_grid(variable ~., scales = "free")
plot_inc_M2_bis

## (1) Outputs (for LateX) :

### (1) using (M1) :

 tikz(file = "inc_M1.tex", width = 5, height = 3)

l_inc_M1 <- ggplot(p_inc_M1, aes(x = inc, y = Probability, colour = welfare )) + 
  geom_line() +
  labs (x = "$inc$", y = "$P(welfare = j|X)$") +
  theme_bw()

print(l_inc_M1)

dev.off()

 ### (1bis) using (M1) :
tikz(file = "inc_M1_bis.tex", width = 5, height = 4)

l_inc_M1_bis <- ggplot(p_inc_M1_bis, aes(x = inc, y = Probability)) + 
  geom_line() +
  facet_grid(variable ~., scales = ) +       ### just have to remove or to add "free" after scale = 
  labs (x = "$inc$", y = "$P(welfare = j|X)$") +   ### in order to have a graph zoomed or not
  theme_bw()

print(l_inc_M1_bis)

dev.off()


### (1) using (M2) :

tikz(file = "inc_M2.tex", width = 5, height = 3)

l_inc_M2 <- ggplot(p_inc_M2, aes(x = inc, y = Probability, colour = welfare )) +
  geom_line() +
  labs (x = "$inc$", y = "$P(welfare = j|X)$") +
  theme_bw()

print(l_inc_M2)

dev.off()

### (1bis) using (M2) :
tikz(file = "inc_M2_bis.tex", width = 5, height = 4)

l_inc_M2_bis <- ggplot(p_inc_M2_bis, aes(x = inc, y = Probability)) + 
  geom_line() +
  facet_grid(variable ~., scales = ) +       ### just have to remove or to add "free" after scale = 
  labs (x = "$inc$", y = "$P(welfare = j|X)$") +   ### in order to have a graph zoomed or not
  theme_bw()

print(l_inc_M2_bis)

dev.off()


## (2) Predicted probabilities of welfare according to inc at the average point
##     whether the individual is married or not


## (2) Predicted probabilities of welfare according to inc at the average point
##     whether the individual is married or not using model  (M2)

married_M2 <- data.frame(inc = rep(seq(from = 0, length = 150, by =0.1), each = 2) , female = "female", 
                         nonwhite = "white" , age =49,
                         educ = "not graduated", work = "working", child = 2
                         , married = rep(c("single","married"), 2), relig ="religious")
married_M2$inc2 <- (married_M2$inc)^2
married_M2 <- married_M2[,c(1,10,2,3,4,5,6,7,8,9)]
married_M2 <- cbind(married_M2, predict(M2, married_M2, type ="probs")) #look at this object to compute the difference of predicted probabilities between a married person and single one for an income changing from $20,000 to $87,000
p_married_M2 <- melt(married_M2, id.vars = c("inc", "inc2", "female", "nonwhite", "age", "educ","work", "child","married","relig" ), value.name = "Probability")
plot_married_M2 <-ggplot(p_married_M2, aes(x = inc, y = Probability, color = married)) + geom_line() + facet_grid(variable ~., scales = )
plot_married_M2

## (2) Outputs (for LateX) :

### (2) using (M2) : 

tikz(file = "married_M2.tex", width = 6, height = 4)

l_married_M2 <- ggplot(p_married_M2, aes(x = inc, y = Probability, color = married)) +
  geom_line() +
  facet_grid(variable ~., scales =) +       ### just have to remove or to add "free" after scale = 
  labs (x = "$inc$", y = "$P(welfare = j|X)$") +   ### in order to have a graph zoomed or not
  theme_bw()

print(l_married_M2)

dev.off()

## (2) computation of the difference of probability of welfare = 2 | X between a married person
## and a single one, with an income changing from $26,000 to $87,000.
married_M2[54,13] - married_M2[53,13]
married_M2[176,13] - married_M2[175,13]
(married_M2[176,13] - married_M2[175,13]) - (married_M2[54,13] - married_M2[53,13]) # 2.56 percentage points


# (3) Predicted probabilities of welfare according to inc at the average point
#     whether the individual is graduated or not

## (3) Predicted probabilities of welfare according to inc at the average point
##    whether the individual is graduated or not using model (M2)

educ_M2 <- data.frame(inc = rep(seq(from = 0, length = 150, by =0.1), each = 2), female ="female", nonwhite="white", age=49, educ =rep(c("not graduated","graduated"), 2)
                      , work ="working", child =2, married = "single", relig = "religious")
educ_M2$inc2 <- (educ_M2$inc)^2
educ_M2 <- educ_M2[,c(1,10,2,3,4,5,6,7,8,9)]
educ_M2 <- cbind(educ_M2, predict(M2, educ_M2, type = "probs")) #look at this object to compute the difference of predicted probabilities between a graduated person and a not graduated one for an income changing from $20,000 to $87,000
p_educ_M2 <- melt(educ_M2, id.vars = c("inc", "inc2", "female", "nonwhite", "age", "educ","work", "child","married","relig" ), value.name = "Probability")
plot_educ_M2 <- ggplot(p_educ_M2, aes(x = inc, y = Probability, color = educ)) + geom_line() + facet_grid(variable ~., scales =)
plot_educ_M2

## (3) Outputs (for LateX) :

# (3) using (M2) :

tikz(file = "educ_M2.tex", width = 6, height = 4)
l_educ_M2 <- ggplot(p_educ_M2, aes(x = inc, y = Probability, color = educ)) +
  geom_line() +
  facet_grid(variable ~., scales =) +       ### just have to remove or to add "free" after scale = 
  labs (x = "$inc$", y = "$P(welfare = j|X)$") +   ### in order to have a graph zoomed or not
  theme_bw()
print(l_educ_M2)

dev.off()

## (3) computation of the difference of probability of welfare = 2 | X between a high educated person
## and a low educated one, with an income changing from $26,000 to $87,000.

educ_M2[54,13] - educ_M2[53,13]
educ_M2[176,13] - educ_M2[175,13]
(educ_M2[176,13] - educ_M2[175,13]) - (educ_M2[54,13] - educ_M2[53,13])

# (4) Predicted probabilities of welfare according to inc at the average point
#     whether the individual is working or not

## (4) Predicted probabilities of welfare according to inc at the average point
##    whether the individual is working or not using model (M2)

work_M2 <- data.frame(inc = rep(seq(from = 0, length = 150, by =0.1), each = 2), female ="female", nonwhite="white", age=49, educ ="not graduated"
                      , work =rep(c("unemployed","working"), 2), child =2, married = "single", relig = "religious")
work_M2$inc2 <- (work_M2$inc)^2
work_M2 <- work_M2[,c(1,10,2,3,4,5,6,7,8,9)]
work_M2 <- cbind(work_M2, predict(M2, work_M2, type = "probs")) #look at this object to compute the difference of predicted probabilities between a person who is working and a person who is unemployed for an income changing from $20,000 to $87,000
p_work_M2 <- melt(work_M2, id.vars = c("inc", "inc2", "female", "nonwhite", "age", "educ","work", "child","married","relig" ), value.name = "Probability")
plot_work_M2 <- ggplot(p_work_M2, aes(x = inc, y = Probability, color = work)) + geom_line() + facet_grid(variable ~., scales =)
plot_work_M2

## (4) Outputs (for LateX) :

### (4) using (M2) :

tikz( file = "work_M2.tex", width = 6, height = 4)
l_work_M2 <- ggplot(p_work_M2, aes(x = inc, y = Probability, color = work)) +
  geom_line() +
  facet_grid(variable ~., scales =) +       ### just have to remove or to add "free" after scale = 
  labs (x = "$inc$", y = "$P(welfare = j|X)$") +   ### in order to have a graph zoomed or not
  theme_bw()
print(l_work_M2)
dev.off()

## (4) computation of the difference of probability of welfare = 2 | X between a person who is working
## and an unemployed one, with an income changing from $26,000 to $87,000.

work_M2[54,13] - work_M2[53,13]
work_M2[176,13] - work_M2[175,13]
(work_M2[176,13] - work_M2[175,13]) - (work_M2[54,13] - work_M2[53,13])

# (5) Predicted probabilities of welfare according to inc at the average point
#     whether the individual is graduated or not/working or not

## (5) Predicted probabilities of welfare according to inc at the average point
##     whether the individual is graduated or not/working or not using model (M2)

educwork_M2 <- data.frame(
  female = "female", age = 49, child = 2, inc = rep(seq(from = 0, length = 150, by =0.1),each = 4), work = rep(c("working","unemployed"),4), married = "single", relig = "religious",
  nonwhite = "white", educ = rep(c("not graduated","graduated"),each =2))
educwork_M2$inc2 <- (educwork_M2$inc)^2
educwork_M2 <- educwork_M2[,c(1,10,2,3,4,5,6,7,8,9)]

educwork_M2 <- cbind(educwork_M2, predict(M2, educwork_M2, type = "probs"))


p_educwork_M2 <- melt(educwork_M2, id.vars = c("female", "age", "nonwhite", "educ", "relig","child", "inc","inc2", "work","married"),
                      variable.name = "welfare", value.name="Probability")

ggplot(p_educwork_M2, aes(x = inc, y = Probability, colour = welfare)) + 
  geom_line() + facet_grid(work ~ educ, labeller="label_both")

## (5) Outputs (for LateX) :

### (5) using (M2) :

tikz(file = "educwork_M2.tex", width = 6, height = 4)
l_educwork_M2 <- ggplot(p_educwork_M2, aes(x = inc, y = Probability, colour = welfare)) + 
  geom_line() +
  facet_grid(work ~ educ, labeller="label_both") +
  labs (x = "$inc$", y = "$P(welfare = j|X)$") +
  theme_bw()
print(l_educwork_M2)
dev.off()


# (6) Predicted probabilities of welfare according to inc and the number of children
# at the average point

## We first have to gather the observations in which individuals have 3 or more children
## and then execute again the regressions using model (M2)

child_data <- database
child_data$child[child_data$child == 0] <- "0"
child_data$child[child_data$child == 1] <- "1"
child_data$child[child_data$child == 2] <- "2"
child_data$child[child_data$child >= 3] <- "3+"

M2_child <- polr(welfare ~ inc + inc2 + age + female + relig + married 
                 + educ + work + child + nonwhite,
                 data = child_data, Hess = TRUE, method ="logistic")

## (6) Predicted probabilities of welfare according to inc and the number of children
## at the average point using model (M2)

child_M2 <- data.frame(
  female = "male", age = 49, child = rep(c("0","1","2","3+"),1), inc = rep(seq(from = 0, length = 150, by =0.1),each = 4), work = "working", married = "single", relig = "religious",
  nonwhite = "white", educ = "not graduated")
child_M2$inc2 <- (child_M2$inc)^2

child_M2 <- cbind(child_M2, predict(M2_child, child_M2, type = "probs"))
p_child_M2 <- melt(child_M2, id.vars = c("female", "age", "nonwhite", "educ", "relig","child", "inc","inc2", "work","married"),
                     variable.name = "welfare", value.name="Probability")
ggplot(p_child_M2, aes(x = inc, y = Probability, colour = welfare))  +
  geom_line() + facet_grid(.~ child, labeller="label_both")

## (6) Output (for LateX) :

### (6) using M2 : 

tikz( file = "child_M2.tex", width = 7.3, height = 3)
l_child_M2 <- ggplot(p_child_M2, aes(x = inc, y = Probability, colour = welfare))  +
  geom_line() +
  facet_grid(.~ child, labeller="label_both") +
  labs( x = "$inc$", y = "$P(welfare = j | X)$") +
  theme_bw()
print(l_child_M2)
dev.off()

# Woman A and woman B

status_effect <- data.frame( inc = c(2.6,2.6,8.7,8.7),female = c("female","female","female","female"),
                             child = c(3,0,3,0), work = c("unemployed", "working", "unemployed", "working"),
                             relig = c("atheist", "religious", "atheist", "religious"), age = c(49,49,49,49),
                             nonwhite = c("non-white","non-white","non-white","non-white"),
                             married = c("single", "married", "single", "married"),
                             educ = c("not graduated", "graduated", "not graduated", "graduated"))
status_effect$inc2 <- (status_effect$inc)^2
status_effect <- cbind(status_effect, predict(M2, status_effect, type = "probs"))
status_effect

status_effect[3,13] - status_effect[1,13]
status_effect[4,13] - status_effect[2,13]
(status_effect[4,13] - status_effect[3,13]) - (status_effect[2,13] - status_effect[1,13])
###############################

# Appendices 

N <- nrow(database)
### Table 2
table2 <- data.frame(Observations = c(mean(database$welfare == "Not too happy")*N,
                              mean(database$welfare == "Pretty happy")*N,
                              mean(database$welfare == "Very happy")*N),
                           Frequence = c(mean(database$welfare == "Not too happy"),
                                         mean(database$welfare == "Pretty happy"),
                                         mean(database$welfare == "Very happy")))
rownames(table2) <- c("Not too happy", "Pretty happy", "Very happy")
table2
### Table 3

m_female <- mean(database$female == "female")
m_nonwhite <- mean(database$nonwhite == "non-white")
m_educ <- mean(database$educ == "graduated")
m_work <- mean(database$work == "working")
m_married <- mean(database$married == "married")
m_relig <- mean(database$relig == "religious")

table3 <- t(data.frame( female_1 = N*m_female, female_0 = N*(1 - m_female),
            nonwhite_1 = N*m_nonwhite, nonwhite_0 = N*(1 - m_nonwhite),
            educ_1 = N*m_educ , educ_0 = N*(1 - m_educ),
            work_1 = N*m_work, work_0 = N*(1 - m_work),
            married_1 = N*m_married, work_0 = N*(1 - m_married),
            relig_1 = N*m_relig, relig_0 = N*(1 - m_relig)))
rownames(table3) <- c("female = 1", "female = 0", "nonwhite = 1", "nonwhite = 0",
                      "educ = 1", "educ = 0", "work = 1", "work = 0",
                      "married = 1", "married = 0", "relig = 1", "relig = 0")
colnames(table3) <- "Observations"
table3
### Table 4
table4 <- t(data.frame( inc = c(min(database$inc), mean(database$inc), max(database$inc), sd(database$inc)),
                      age = c(min(database$age), mean(database$age), max(database$age), sd(database$age)),
                      child = c(min(database$child), mean(database$child), max(database$child), sd(database$child))))
colnames(table4) <- c("Min", "Average", "Max", "SD")
table4
### Table 5
table5 <- t(data.frame(Not_too_happy = c(mean(database$inc[database$welfare == "Not too happy"]),
                                       mean(database$age[database$welfare == "Not too happy"]),
                                       mean(database$child[database$welfare == "Not too happy"])),
                      Pretty_happy = c(mean(database$inc[database$welfare == "Pretty happy"]),
                                       mean(database$age[database$welfare == "Pretty happy"]),
                                       mean(database$child[database$welfare == "Pretty happy"])),
                      Very_happy = c(mean(database$inc[database$welfare == "Very happy"]),
                                     mean(database$age[database$welfare == "Very happy"]),
                                     mean(database$child[database$welfare == "Very happy"]))))
colnames(table5) <- c("Avg.inc", "Avg.age", "Avg.child")
table5

### Table 6

table6 <- t(data.frame(female = c(min(database$female == "female"), m_female, max(database$female == "female")),
                     nonwhite = c(min(database$nonwhite == "non-white"), m_nonwhite, max(database$nonwhite == "non-white")),
                     educ = c(min(database$educ == "graduated"), m_educ, max(database$educ == "graduated")),
                     work = c(min(database$work == "working"), m_work, max(database$work == "working")),
                     married = c(min(database$married == "married"), m_married, max(database$married == "married")),
                     relig = c(min(database$relig == "religious"), m_relig, max(database$relig == "religious"))))
colnames(table6) <- c("Min", "Average", "Max")
table6

### Table 7
female1 <- as.data.frame(subset(database$female,database$welfare == "Not too happy"))
female2 <- as.data.frame(subset(database$female,database$welfare == "Pretty happy"))
female3 <- as.data.frame(subset(database$female,database$welfare == "Very happy"))

nonwhite1 <- as.data.frame(subset(database$nonwhite,database$welfare == "Not too happy"))
nonwhite2 <- as.data.frame(subset(database$nonwhite,database$welfare == "Pretty happy"))
nonwhite3 <- as.data.frame(subset(database$nonwhite,database$welfare == "Very happy"))

educ1 <- as.data.frame(subset(database$educ,database$welfare == "Not too happy"))
educ2 <- as.data.frame(subset(database$educ,database$welfare == "Pretty happy"))
educ3 <- as.data.frame(subset(database$educ,database$welfare == "Very happy"))

work1 <- as.data.frame(subset(database$work,database$welfare == "Not too happy"))
work2 <- as.data.frame(subset(database$work,database$welfare == "Pretty happy"))
work3 <- as.data.frame(subset(database$work,database$welfare == "Very happy"))

married1 <- as.data.frame(subset(database$married,database$welfare == "Not too happy"))
married2 <- as.data.frame(subset(database$married,database$welfare == "Pretty happy"))
married3 <- as.data.frame(subset(database$married,database$welfare == "Very happy"))

relig1 <- as.data.frame(subset(database$relig,database$welfare == "Not too happy"))
relig2 <- as.data.frame(subset(database$relig,database$welfare == "Pretty happy"))
relig3 <- as.data.frame(subset(database$relig,database$welfare == "Very happy"))





table7 <- data.frame(
                     female  = c(mean(female1$`subset(database$female, database$welfare == "Not too happy")` == "female"),
                                 mean(female2$`subset(database$female, database$welfare == "Pretty happy")` == "female"),
                                 mean(female3$`subset(database$female, database$welfare == "Very happy")` == "female")),
                     nonwhite  = c(mean(nonwhite1$`subset(database$nonwhite, database$welfare == "Not too happy")` == "non-white"),
                                   mean(nonwhite2$`subset(database$nonwhite, database$welfare == "Pretty happy")` == "non-white"),
                                   mean(nonwhite3$`subset(database$nonwhite, database$welfare == "Very happy")` == "non-white")),
                     educ  = c(mean(educ1$`subset(database$educ, database$welfare == "Not too happy")` == "graduated"),
                               mean(educ2$`subset(database$educ, database$welfare == "Pretty happy")` == "graduated"),
                               mean(educ3$`subset(database$educ, database$welfare == "Very happy")` == "graduated")),
                     work  = c(mean(work1$`subset(database$work, database$welfare == "Not too happy")` == "working"),
                               mean(work2$`subset(database$work, database$welfare == "Pretty happy")` == "working"),
                               mean(work3$`subset(database$work, database$welfare == "Very happy")` == "working")),
                     married  = c(mean(married1$`subset(database$married, database$welfare == "Not too happy")` == "married"),
                                  mean(married2$`subset(database$married, database$welfare == "Pretty happy")` == "married"),
                                  mean(married3$`subset(database$married, database$welfare == "Very happy")` == "married")),
                     relig  = c(mean(relig1$`subset(database$relig, database$welfare == "Not too happy")` == "religious"),
                                mean(relig2$`subset(database$relig, database$welfare == "Pretty happy")` == "religious"),
                                mean(relig3$`subset(database$relig, database$welfare == "Very happy")` == "religious")))
rownames(table7) <- c("Not too happy","Pretty happy","Very happy")
table7

### Table 8

#### (M1)

# P-value (M1)

m1 <- summary(M1)
m1
ctable1 <- coef(m1)
p1 <- pnorm(abs(ctable1[, "t value"]), lower.tail = FALSE) * 2
ctable1 <- cbind(ctable1, "p value" = p1)
ctable1

# Log-Likehood (M1)
llf_M1 <- logLik(M1)

# Mc Fadden's R-Squared (M1)
ll_M1 <- logLik(update(M1, . ~ 1))
MCR2_M1 <- 1 - (llf_M1/ll_M1)
MCR2_M1

# Likelihood Ratio (M1)
LR_M1 <- -2*(ll_M1 - llf_M1)
LR_M1

# Prob>chi : (probability at least one parameter is equal to 0, here Prob = 0) (M1)
chi11 <- qchisq(0.95, df = 11)
max(LR_M1, chi11)

#### (M2)

# P-value (M2)
m2 <- summary(M2)
m2
ctable2 <- coef(m2)
p2 <- pnorm(abs(ctable2[, "t value"]), lower.tail = FALSE) * 2
ctable2 <- cbind(ctable2, "p value" = p2)
ctable2

# Log-Likehood  (M2)
llf_M2 <- logLik(M2)

# Mc Fadden's R-Squared (M2)
ll_M2 <- logLik(update(M2, . ~ 1))
MCR2_M2 <- 1 - (llf_M2/ll_M2)
MCR2_M2

# Likelihood Ratio (M2)
LR_M2 <- -2*(ll_M2 - llf_M2)
LR_M2

# Prob>chi : (probability at least one parameter is equal to 0, here Prob = 0) (M2)
chi12 <- qchisq(0.95, df = 12)
max(LR_M2, chi12)

### Table 9

# Marginal effects (M1)
margins_M1 <- ocME(M1, rev.dum= TRUE, digits = 6)
margins_M1

m_nottoohappy_M1 <- cbind(margins_M1$out$`ME.Not too happy`[,-(2:4)])
m_prettyhappy_M1 <- cbind(margins_M1$out$`ME.Pretty happy`[,-(2:4)])
m_veryhappy_M1 <- cbind(margins_M1$out$`ME.Very happy`[,-(2:4)])

p_nottoohappy_M1 <- cbind(margins_M1$out$`ME.Not too happy`[,-c(1:3)])
p_prettyhappy_M1 <- cbind(margins_M1$out$`ME.Pretty happy`[,-c(1:3)])
p_veryhappy_M1 <- cbind(margins_M1$out$`ME.Very happy`[,-c(1:3)])

(p_marginal_M1 <- cbind(p_nottoohappy_M1, p_prettyhappy_M1, p_veryhappy_M1))

effect_M1 <- cbind(m_nottoohappy_M1, m_prettyhappy_M1, m_veryhappy_M1
                   , p_nottoohappy_M1, p_prettyhappy_M1, p_veryhappy_M1  )
colnames(effect_M1) <- c("Effect Not too happy", "Effect Pretty happy", "Effect Very Happy", "P-value Not too happy", "P-value Pretty happy", "P-value Very happy")
effect_M1

effect_M1 <- effect_M1[,-(4:6)]
stargazer(effect_M1)   # not possible to use stargazer() with margins_M1 so we have to extract
# the marginal effects from there

effect_M1

### Table 10

# Marginal effects (M2)
margins_M2 <- ocME(M2, rev.dum= TRUE, digits = 6)
margins_M2

m_nottoohappy_M2 <- cbind(margins_M2$out$`ME.Not too happy`[,-(2:4)])
m_prettyhappy_M2 <- cbind(margins_M2$out$`ME.Pretty happy`[,-(2:4)])
m_veryhappy_M2 <- cbind(margins_M2$out$`ME.Very happy`[,-(2:4)])

p_nottoohappy_M2 <- cbind(margins_M2$out$`ME.Not too happy`[,-c(1:3)])
p_prettyhappy_M2 <- cbind(margins_M2$out$`ME.Pretty happy`[,-c(1:3)])
p_veryhappy_M2 <- cbind(margins_M2$out$`ME.Very happy`[,-c(1:3)])

(p_marginal_M2 <- cbind(p_nottoohappy_M2, p_prettyhappy_M2, p_veryhappy_M2))

effect_M2 <- cbind(m_nottoohappy_M2, m_prettyhappy_M2, m_veryhappy_M2
                   , p_nottoohappy_M2, p_prettyhappy_M2, p_veryhappy_M2  )
colnames(effect_M2) <- c("Effect Not too happy", "Effect Pretty happy", "Effect Very Happy", "P-value Not too happy", "P-value Pretty happy", "P-value Very happy")
effect_M2

effect_M2 <- effect_M2[,-(4:6)]
stargazer(effect_M2)    # not possible to use stargazer() with margins_M2 so we have to extract
# the marginal effects from there

### Important note : ocME(.) computes marginal effects very well for simple variables, by
### simple variables we mean variables which are not interaction variable (with another variable or with itself)
### ocME(.) computes the marginal effects of inc and inc2 (inc to the square) separately, the algorithm
### distinguishes inc and inc2 as if there were not the same variable. So we have to compute the marginal
### effect of inc at the average point by ourselves.

### First we collect the coefficients associated to inc and associated to inc2

zeta1 <- M2$coefficients[1]
zeta2 <- M2$coefficients[2]

### Then we have to compute the probability density function for each level of happiness
### 0 for "Not too happy", 1 for "Very happy", 2 for " Very happy"
lambda0 <- (effect_M2[1,1])/zeta1    ### We use inc to compute lambda0,1,2 but we could also use any other
lambda1 <- (effect_M2[1,2])/zeta1    ### variable (married, female, educ, etc.)
lambda2 <- (effect_M2[1,3])/zeta1

### Thus we can compute the marginal effect of inc at the average point with (M2) by the formula given
### in the paper 
effect_inc_M2_0 <- lambda0*(zeta1 + 2*zeta2*mean(database$inc))
effect_inc_M2_1 <- lambda1*(zeta1 + 2*zeta2*mean(database$inc))
effect_inc_M2_2 <- lambda2*(zeta1 + 2*zeta2*mean(database$inc))

effect_inc_M2_0
effect_inc_M2_1
effect_inc_M2_2

### In order to have a correct table we replace these values in effect_M2 and erase the row associated
### to inc2
effect_M2[1,] <- c(effect_inc_M2_0, effect_inc_M2_1, effect_inc_M2_2)
effect_M2 <- effect_M2[-2,]

stargazer(effect_M2)    # not possible to use stargazer() with margins_M2 so we have to extract
# the marginal effects from there

effect_M2



###########################


### Log file 

sink("logfile.txt")

# Table 2

table2

# Table 3

table3

# Table 4

table4

# Table 5

table5

# Table 6

table6

#  Table 7

table7

# Table 8

#### (M1)

# P-value (M1)

m1 <- summary(M1)
m1
ctable1 <- coef(m1)
p1 <- pnorm(abs(ctable1[, "t value"]), lower.tail = FALSE) * 2
ctable1 <- cbind(ctable1, "p value" = p1)
ctable1

# Log-Likehood (M1)
llf_M1 <- logLik(M1)

# Mc Fadden's R-Squared (M1)
ll_M1 <- logLik(update(M1, . ~ 1))
MCR2_M1 <- 1 - (llf_M1/ll_M1)
MCR2_M1

# Likelihood Ratio (M1)
LR_M1 <- -2*(ll_M1 - llf_M1)
LR_M1

# Prob>chi : (probability at least one parameter is equal to 0, here Prob = 0) (M1)
chi11 <- qchisq(0.95, df = 11)
max(LR_M1, chi11)

#### (M2)

# P-value (M2)
m2 <- summary(M2)
m2
ctable2 <- coef(m2)
p2 <- pnorm(abs(ctable2[, "t value"]), lower.tail = FALSE) * 2
ctable2 <- cbind(ctable2, "p value" = p2)
ctable2

# Log-Likehood  (M2)
llf_M2 <- logLik(M2)

# Mc Fadden's R-Squared (M2)
ll_M2 <- logLik(update(M2, . ~ 1))
MCR2_M2 <- 1 - (llf_M2/ll_M2)
MCR2_M2

# Likelihood Ratio (M2)
LR_M2 <- -2*(ll_M2 - llf_M2)
LR_M2

# Prob>chi : (probability at least one parameter is equal to 0, here Prob = 0) (M2)
chi12 <- qchisq(0.95, df = 12)
max(LR_M2, chi12)

# Table 9

effect_M1

# Table 10

effect_M2


sink()




