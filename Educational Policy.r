library(dplyr)
library(haven)
library(vtable)



# TIMSS 2019
acgczem7 <- read_sav("/Users/tomasnovak/Desktop/hejny/TIMSS_2019_datove_soubory_narodni/acgczem7.sav")
asgczem7 <- read_sav("/Users/tomasnovak/Desktop/hejny/TIMSS_2019_datove_soubory_narodni/asgczem7.sav")
ashczem7 <- read_sav("/Users/tomasnovak/Desktop/hejny/TIMSS_2019_datove_soubory_narodni/ashczem7.sav")
astczem7 <- read_sav("/Users/tomasnovak/Desktop/hejny/TIMSS_2019_datove_soubory_narodni/astczem7.sav")
atgczem7 <- read_sav("/Users/tomasnovak/Desktop/hejny/TIMSS_2019_datove_soubory_narodni/atgczem7.sav")
questionnaire <- read_sav("/Users/tomasnovak/Desktop/hejny/Doplnek2019/TIMSS_2019_Vyuka_Mat.sav")

# number of schools
length(unique(asgczem7[["IDSCHOOL"]]))
# number of tested students
NROW(asgczem7$IDSTUD)
# number of classrooms
length(unique(asgczem7[["IDCLASS"]]))

# Yes H, Yes Books
length(which(questionnaire$Ucebnice==1))
my_data1 = subset(questionnaire, Ucebnice == 1)
my_data1$Metoda <- 1

# Yes H, No Books
my_data3 = subset(questionnaire[c(9,10,11,28,29,34,35,38,39,40,46,47,53,66,75,101,102,120,129,148,149,166,167), ])
my_data3$Metoda <- 3

# No H, No Books
my_data2 <- questionnaire[-c(9,10,11,28,29,34,35,38,39,40,46,47,53,66,75,101,102,120,129,148,149,166,167), ]
my_data2 <- my_data2[!grepl("1", my_data2$Ucebnice),]
my_data2$Metoda <- 2

final <- my_data1 %>% full_join(my_data2) %>% full_join(my_data3)

# merge all three dataframes
merge <- merge(x = asgczem7, y = final, by = "IDCLASS", all.y = TRUE)

# test for na in merge
sum(is.na(merge$Metoda))

names(atgczem7)[names(atgczem7) == "IDTEACH" ] <- "IDCLASS"

testdf1 <- atgczem7[c("ATBM01","ATBS01B","IDCLASS","ATBG02","ATBG03","ATBG10A","ATBG01","ATBG04","ATBG05AA","ATBG05AB")]
testdf1 = testdf1[!duplicated(testdf1$IDCLASS),]
testdf2 <- ashczem7[c("IDSTUD","ASBH15A","ASBH15B","ASBH10","ASBGHRL")]
testdf3 <- asgczem7[c("IDSCHOOL","IDCLASS","IDSTUD","ASMMAT01","ASSSCI01","ASBG01")]
testdf4 <- final[c("IDCLASS","Metoda")]
testdf552 <- merge(testdf3, testdf4, by="IDCLASS")
testdf552 <- merge(testdf552, testdf2, by="IDSTUD")
testdf552 <- merge(x = testdf552, y = testdf1, by = "IDCLASS", all.x = TRUE)

# Teacher Age
testdf552$ATBG03[testdf552$ATBG03 == 1] <- 25
testdf552$ATBG03[testdf552$ATBG03 == 2] <- 27
testdf552$ATBG03[testdf552$ATBG03 == 3] <- 34.5
testdf552$ATBG03[testdf552$ATBG03 == 4] <- 44.5
testdf552$ATBG03[testdf552$ATBG03 == 5] <- 54.5
testdf552$ATBG03[testdf552$ATBG03 == 6] <- 60

# Student Gender (female)
testdf552$ASBG01[testdf552$ASBG01 == 2] <- 0

# Rename Variables
names(testdf552)[names(testdf552) == "ASBG01" ] <- "Female_Student"
names(testdf552)[names(testdf552) == "Metoda" ] <- "Method"
names(testdf552)[names(testdf552) == "ATBG02" ] <- "Female_Teacher"
names(testdf552)[names(testdf552) == "ASBH10" ] <- "Books101"
names(testdf552)[names(testdf552) == "ATBG03" ] <- "Teacher_Age"
names(testdf552)[names(testdf552) == "ATBG10A" ] <- "Class_Size"
names(testdf552)[names(testdf552) == "ATBG01" ] <- "Teacher_Experience"
names(testdf552)[names(testdf552) == "ASBGHRL" ] <- "Soc_Econ_Index"
names(testdf552)[names(testdf552) == "ATBG04" ] <- "Major_Degree"
names(testdf552)[names(testdf552) == "ATBG05AA" ] <- "Elementary_Edu_Spec"
names(testdf552)[names(testdf552) == "ATBG05AB" ] <- "General_Edu_Spec"
names(testdf552)[names(testdf552) == "ASBH15A" ] <- "Mother_Degree"
names(testdf552)[names(testdf552) == "ASBH15B" ] <- "Father_Degree"
names(testdf552)[names(testdf552) == "ATBM01" ] <- "Math_Hours"
names(testdf552)[names(testdf552) == "ATBS01B" ] <- "Science_Hours"
names(testdf552)[names(testdf552) == "ASMMAT01" ] <- "Math_Score"
names(testdf552)[names(testdf552) == "ASSSCI01" ] <- "Science_Score"

# Books > 100
testdf552$Books101[testdf552$Books101 == 1] <- 0
testdf552$Books101[testdf552$Books101 == 2] <- 0
testdf552$Books101[testdf552$Books101 == 3] <- 0
testdf552$Books101[testdf552$Books101 == 4] <- 1
testdf552$Books101[testdf552$Books101 == 5] <- 1

# Female_Teacher
testdf552$Female_Teacher[testdf552$Female_Teacher == 2] <- 0


# Major_Degree
testdf552$Major_Degree[testdf552$Major_Degree <= 1] <- 0
testdf552$Major_Degree[testdf552$Major_Degree == 2] <- 0
testdf552$Major_Degree[testdf552$Major_Degree == 3] <- 0
testdf552$Major_Degree[testdf552$Major_Degree == 4] <- 0
testdf552$Major_Degree[testdf552$Major_Degree == 5] <- 0
testdf552$Major_Degree[testdf552$Major_Degree == 6] <- 1
testdf552$Major_Degree[testdf552$Major_Degree == 7] <- 1

# Elementary_Edu_Spec
testdf552$Elementary_Edu_Spec[testdf552$Elementary_Edu_Spec == 2] <- 0

# General_Edu_Spec
testdf552$General_Edu_Spec[testdf552$General_Edu_Spec == 2] <- 0

# Mother_Degree
testdf552$Mother_Degree[testdf552$Mother_Degree == 1] <- 0
testdf552$Mother_Degree[testdf552$Mother_Degree == 2] <- 0
testdf552$Mother_Degree[testdf552$Mother_Degree == 3] <- 0
testdf552$Mother_Degree[testdf552$Mother_Degree == 4] <- 0
testdf552$Mother_Degree[testdf552$Mother_Degree == 5] <- 0
testdf552$Mother_Degree[testdf552$Mother_Degree == 6] <- 0
testdf552$Mother_Degree[testdf552$Mother_Degree == 7] <- 0
testdf552$Mother_Degree[testdf552$Mother_Degree == 8] <- 1
testdf552$Mother_Degree[testdf552$Mother_Degree == 9] <- 1
testdf552$Mother_Degree[testdf552$Mother_Degree == 10] <- 0

# Father_Degree
testdf552$Father_Degree[testdf552$Father_Degree == 1] <- 0
testdf552$Father_Degree[testdf552$Father_Degree == 2] <- 0
testdf552$Father_Degree[testdf552$Father_Degree == 3] <- 0
testdf552$Father_Degree[testdf552$Father_Degree == 4] <- 0
testdf552$Father_Degree[testdf552$Father_Degree == 5] <- 0
testdf552$Father_Degree[testdf552$Father_Degree == 6] <- 0
testdf552$Father_Degree[testdf552$Father_Degree == 7] <- 0
testdf552$Father_Degree[testdf552$Father_Degree == 8] <- 1
testdf552$Father_Degree[testdf552$Father_Degree == 9] <- 1
testdf552$Father_Degree[testdf552$Father_Degree == 10] <- 0

summary(testdf552)
mean(testdf552$Female_Student, na.rm=TRUE)
mean(testdf552$Mother_Degree, na.rm=TRUE)
sd(testdf552$Mother_Degree, na.rm=TRUE)



# New DataFrame (Yes/No Hejny)

yhnh <- testdf552
yhnh$Method[yhnh$Method == 3] <- 1
yhnh$Method[yhnh$Method == 2] <- 0
#yhnh$Method[yhnh$Method == 1] <- "YH"
#yhnh$Method[yhnh$Method == 2] <- "NH"

# Minutes to hours
yhnh$Math_Hours <- yhnh$Math_Hours/60
yhnh$Science_Hours <- yhnh$Science_Hours/60

#Fix N/A
yhnh$Female_Student[is.na(yhnh$Female_Student)] = 0.4881367
yhnh$Mother_Degree[is.na(yhnh$Mother_Degree)] = 0.245735
yhnh$Father_Degree[is.na(yhnh$Father_Degree)] = 0.233871
yhnh$Books101[is.na(yhnh$Books101)] = 0.4734177
yhnh$Soc_Econ_Index[is.na(yhnh$Soc_Econ_Index)] = 10.80876
yhnh$Math_Hours[is.na(yhnh$Math_Hours)] = 226.625
yhnh$Science_Hours[is.na(yhnh$Science_Hours)] = 75.40265
yhnh$Female_Teacher[is.na(yhnh$Female_Teacher)] = 0.9639986
yhnh$Teacher_Age[is.na(yhnh$Teacher_Age)] = 47.41752
yhnh$Class_Size[is.na(yhnh$Class_Size)] = 23.53442
yhnh$Teacher_Experience[is.na(yhnh$Teacher_Experience)] = 20.86229
yhnh$Major_Degree[is.na(yhnh$Major_Degree)] = 0.931842
yhnh$Elementary_Edu_Spec[is.na(yhnh$Elementary_Edu_Spec)] = 0.8465866
yhnh$General_Edu_Spec[is.na(yhnh$General_Edu_Spec)] = 0.1211553


summary(yhnh %>% filter(Method == "YH") %>% .$Teacher_Age)
summary(yhnh %>% filter(Method == "NH") %>% .$Teacher_Age)

#summary(yhnh %>% filter(Method == "Y") %>% .$Soc_Econ_Index)
#summary(yhnh %>% filter(Method == "N") %>% .$Soc_Econ_Index)

ggplot(yhnh, aes(Method, Soc_Econ_Index)) +
  geom_boxplot()

#T-Test
t.test(Class_Size ~ Method, data = yhnh)
#F-Test
var.test(Class_Size ~ Method, data = yhnh)

st(yhnh, group = 'Method', group.test = TRUE)


print(xtable(st(yhnh, group = 'Method', group.test = TRUE, type = "latex"), file = "filename2.tex"))


################# Unit Fixed Effect Model #################

lm(Math_Score ~ Method, data = yhnh)

 
plm(Math_Score ~ Method + Science_Score,
           data = yhnh,
           index = "IDSTUD",
           method = "within")

lm(Math_Score ~ Method, data=yhnh)



plm(Science_Score ~ Method*Math_Score + Method + Math_Score,
              data= yhnh,
              index = "IDSTUD")
summary(dummyvar)

test = lm.cluster(Science_Score ~ Method*Mathematics_Score + as.factor(IDSTUD), 
                  cluster = 'Method',
                  data = yhnh)



ourpanel <- plm(Math_Score ~ Method,    # check ?plm() to see whats happening here
                index = c("IDSTUD", "IDCLASS"),   # sets the indices that plm() uses for effects
                model = "within",            # tells plm we want to use the within estimator
                effects = "twoways",         # tells plm to use individual and time effects
                data = yhnh)

# Fixed Effect Regression
# library(plm)
# model1 <- plm(log(Math_Score) ~ as.factor(Method),
#               data = yhnh,
#               index = c("IDCLASS"),
#               model = "within")
# summary(model1)


# lm(Math_Score ~ Method * Math_Score + Method + Math_Score, 
#    data = yhnh, index = "IDCLASS")


############# Serious Modelling :)

# Linear model

mylm <- lm(Math_Score ~ -1 + Method + Science_Score + Books101 + Teacher_Age + Teacher_Experience,
   data = yhnh)
summary(mylm)


# Unit fixed effect model - pooling
install.packages("plm")

myplm <- plm(log(Math_Score) ~ Method + Science_Score + Books101 + Teacher_Age + Teacher_Experience,
             data = yhnh,
             model = "pooling")
summary(myplm)

# Unit fixed effect model
myplm2 <- plm(Math_Dummy ~ Method*Math_Score + Method + Math_Score,
             state = "Math_Dummy",
             data = yhnh2,
             model = "within")
summary(myplm2)

#################### Difference between math and science ####################
yhnh <- as.data.table(yhnh)
yhnh[, subject_diff := Math_Score - Science_Score, by = IDSTUD]

mylm2 <- lm(subject_diff ~ -1 + Method*Math_Score + Method + Science_Score + Books101 + Teacher_Age + Teacher_Experience,
           data = yhnh)
summary(mylm2)
# Manual OLS
# betahat = (X'X)^-1 X'y

# y <- log(yhnh[, "Math_Hours"])
# str(y)

# X <- yhnh[, .("Method", "Science_Score", "Books101", "Teacher_Age", "Teacher_Experience")]
# X <- cbind(1, X)

# Within Transfortmation
# library(data.table)
# library(lavaan)

# yhnh <- as.data.table(yhnh)

# yhnh[, logmath := log(Math_Score)]
# yhnh[, math_within := Math_Score - mean(Math_Score, na.rm = T), by = IDCLASS]
# yhnh[, science_within := Science_Score - mean(Science_Score, na.rm = T), by = IDCLASS]
# yhnh[, teacher_age_within := Teacher_Age - mean(Teacher_Age, na.rm = T), by = IDCLASS]
# yhnh[, teacher_exp_within := Teacher_Experience - mean(Teacher_Experience, na.rm = T), by = IDCLASS]
# yhnh[, method_within := Method - mean(Method, na.rm = T), by = IDCLASS]

# y <- yhnh[, math_within]

# x <- as.matrix(yhnh[, .(method_within, teacher_age_within, teacher_exp_within)])

# solve(t(x) %*% x) %*% t(x) %*% y


# mylm <- lm(math_within ~ method_within + teacher_age_within + teacher_exp_within,
#            data = yhnh)
# summary(mylm)

# Trying to make fixed a effect column
# cols_to_make <-rlang::parse_exprs(paste0('ifelse(any(c(IDSTUD,Math_Score,Science_Score)==',1:max(yhnh),'),1,0)'))
# df2 <- rowwise(yhnh) %>% mutate(!!!cols_to_make)  %>% ungroup
# names(df2) <- c(names(yhnh),1:max(yhnh))




###### OLS REGRESSIONS
install.packages("etable")
library(etable)
library(fixest)

# mylm <- lm(Math_Score ~ Method,
#           data = yhnh)
# summary(mylm)


# Load new excel file

install.packages("writexl")
library(writexl)
write_xlsx(yhnh,"/Users/tomasnovak/Desktop/S1930/yhnh10.xlsx")

############ NEW DATA FILE ########

library(readxl)
yhnh2 <- read_excel("/Users/tomasnovak/Desktop/S1930/yhnh3.xlsx", col_names = TRUE)
yhnh2 <- as.data.frame(yhnh2)

# Fixed Effect Model - Subject FE, no teacher controls, no student controls

res1 = feols(Math_Score ~ Method | Math_Dummy, data = yhnh2)
# By default, the SEs are clustered according to the first fixed-effect
summary(res1)

etable(res1, se = "cluster",
       cluster = c("IDSCHOOL"))


# Fixed Effect Model - Subject FE, yes teacher controls, no student controls

res2 = feols(Math_Score ~ Method + Female_Teacher + Teacher_Age | Math_Dummy, data = yhnh2)
# By default, the SEs are clustered according to the first fixed-effect
summary(res2)

etable(res2, se = "cluster",
       cluster = c("IDSCHOOL"))





