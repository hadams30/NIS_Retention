setwd("~/Desktop/DAV 610/ipeds final")
library(dplyr)
library(corrplot)
library(olsrr)

##----NOTES---------------------------------------------------------------------
##Still left to do: correlation matric between all variables
##stepwise regression / multiple regression / confounding variables as a model


##----Revenue and tuition variables---------------------------------------------
rev<-read.csv("revenue.csv")
View(rev)
colnames(rev)[3]="tuition_21_22"
colnames(rev)[4]="revenues_21_22"
colnames(rev)[6]="tuition_20_21"
colnames(rev)[7]="revenues_20_21"
colnames(rev)[9]="tuition_19_20"
colnames(rev)[10]="revenues_19_20"
colnames(rev)[12]="tuition_18_19"
colnames(rev)[13]="revenues_18_19"

rev$revenues_21_22 <- as.numeric(rev$revenues_21_22)
rev$revenues_20_21 <- as.numeric(rev$revenues_20_21)
rev$revenues_19_20 <- as.numeric(rev$revenues_19_20)
rev$revenues_18_19 <- as.numeric(rev$revenues_18_19)

rev$tuition_21_22 <- as.numeric(rev$tuition_21_22)
rev$tuition_20_21 <- as.numeric(rev$tuition_20_21)
rev$tuition_19_20 <- as.numeric(rev$tuition_19_20)
rev$tuition_18_19 <- as.numeric(rev$tuition_18_19)

rev$tut_p_21_22 <- (rev$tuition_21_22/rev$revenues_21_22)*100
rev$tut_p_20_21 <- (rev$tuition_20_21/rev$revenues_20_21)*100
rev$tut_p_19_20 <- (rev$tuition_19_20/rev$revenues_19_20)*100
rev$tut_p_18_19 <- (rev$tuition_18_19/rev$revenues_18_19)*100

rev$tut_p_avg <- (rev$tut_p_21_22+rev$tut_p_20_21+rev$tut_p_19_20+rev$tut_p_18_19)/4

rev$rev_avg <- (rev$revenues_21_22+rev$revenues_20_21+rev$revenues_19_20+rev$revenues_18_19)/4
rev$tut_avg <- (rev$tuition_21_22+rev$tuition_20_21+rev$tuition_19_20+rev$tuition_18_19)/4
rev <- rev %>% select(c(UnitID, tut_avg, rev_avg, tut_p_avg))
colnames(rev)[1]="unitid"
View(rev)

##----Expenses variables--------------------------------------------------------
exp<-read.csv("expenses.csv")
View(exp)
colnames(exp)[1]="unitid"
colnames(exp)[3]="exp_21_22"
colnames(exp)[4]="exp_20_21"
colnames(exp)[5]="exp_19_20"
colnames(exp)[6]="exp_18_19"

exp$exp_21_22 <- as.numeric(exp$exp_21_22)
exp$exp_20_21 <- as.numeric(exp$exp_20_21)
exp$exp_19_20 <- as.numeric(exp$exp_19_20)
exp$exp_18_19 <- as.numeric(exp$exp_18_19)

exp$exp_avg <- (exp$exp_21_22+exp$exp_20_21+exp$exp_19_20+exp$exp_18_19)/4
exp <- exp %>% select(c(unitid, exp_avg))

##----Faculty-student ratios----------------------------------------------------

fac_stu<-read.csv("fac_stu_ratio.csv")
View(fac_stu)

colnames(fac_stu)[3]="fac_stu_22_23"
colnames(fac_stu)[4]="fac_stu_21_22"
colnames(fac_stu)[5]="fac_stu_20_21"
colnames(fac_stu)[6]="fac_stu_19_20"

data2 <- fac_stu %>% select(c(UnitID, fac_stu_22_23, fac_stu_21_22, fac_stu_20_21, fac_stu_19_20))
d2<-data2

d2$fac_stu_19_20 <- (1 / d2$fac_stu_19_20) *100
d2$fac_stu_20_21 <- (1 / d2$fac_stu_20_21) *100
d2$fac_stu_21_22 <- (1 / d2$fac_stu_21_22) *100
d2$fac_stu_22_23 <- (1 / d2$fac_stu_22_23) *100

View(d2)
colnames(d2)[1]="unitid"

d2$fac_stu_avg<-(d2$fac_stu_19_20+d2$fac_stu_20_21+d2$fac_stu_21_22+d2$fac_stu_22_23)/4

##So now d2$fac_stu_avg holds the average number of faculty members 
##as a percentage of the student headcount over four years
##and same for rev and exp

##----All other variables-------------------------------------------------------

d2019<-read.csv("2019.csv")
d2020<-read.csv("2020.csv")
d2021<-read.csv("2021.csv")
d2022<-read.csv("2022.csv")
d2023<-read.csv("2023.csv")

View(d2019)
View(d2020)
View(d2021)
View(d2022)
View(d2023)

colnames(d2019)[4]="NIS_19_20"
colnames(d2020)[5]="NIS_20_21"
colnames(d2021)[4]="NIS_21_22"
colnames(d2022)[5]="NIS_22_23"

colnames(d2023)[8]="Stu_22_23"
colnames(d2022)[4]="Stu_21_22"
colnames(d2021)[5]="Stu_20_21"
colnames(d2020)[4]="Stu_19_20"

colnames(d2023)[9]="Bach_23"
colnames(d2023)[10]="Mast_23"
colnames(d2022)[6]="FT_4"
colnames(d2022)[7]="FT_6"
colnames(d2022)[8]="FT_8"

d2019 <- d2019 %>% select(c(unitid, NIS_19_20, institution.name))
d2020 <- d2020 %>% select(c(unitid, Stu_19_20, NIS_20_21))
d2021 <- d2021 %>% select(c(unitid, Stu_20_21, NIS_21_22))
d2022 <- d2022 %>% select(c(unitid, Stu_21_22, NIS_22_23))
d2023 <- d2023 %>% select(c(unitid, Stu_22_23))

df_merge <- merge(d2019,d2020,by="unitid")
df_merge2 <- merge(df_merge,d2021,by="unitid")
df_merge3 <- merge(df_merge2,d2022,by="unitid")
df_merge4 <- merge(df_merge3,d2023,by="unitid")
df_merge5 <- merge(df_merge4,d2,by="unitid")
df_merge6 <- merge(df_merge5,rev,by="unitid")
df_merge7 <- merge(df_merge6,exp,by="unitid")
data <- df_merge7

data$NIS_Stu_19_20 <- (data$NIS_19_20 / data$Stu_19_20) *100
data$NIS_Stu_20_21 <- (data$NIS_20_21 / data$Stu_20_21) *100
data$NIS_Stu_21_22 <- (data$NIS_21_22 / data$Stu_21_22) *100
data$NIS_Stu_22_23 <- (data$NIS_22_23 / data$Stu_22_23) *100

##finding the average ratio for each institution across the 4 year time span
##and getting rid of all of the old variables for each year
data$NIS_Stu_avg <- (data$NIS_Stu_19_20 + data$NIS_Stu_20_21 + data$NIS_Stu_21_22 + data$NIS_Stu_22_23) /4

View(data)

data <- data %>% select(c(unitid, institution.name, 
                          NIS_Stu_avg,
                          fac_stu_avg,
                          #tut_avg,
                          tut_p_avg,
                          rev_avg,
                          exp_avg))

##----Retention rate 2022-2023--------------------------------------------------

retention<-read.csv("2022_ft_retention_pt_retention_student-to-faculty.csv")
colnames(retention)[4]="ft_retention_22_23"
data$retention_22_23 <- retention$ft_retention_22_23

##----final data cleaning-------------------------------------------------------

View(data)
data$NIS_Stu_avg <- as.numeric(data$NIS_Stu_avg)
data$retention_22_23 <- as.numeric(data$retention_22_23)
data$exp_avg <- as.numeric(data$exp_avg)
data$rev_avg <- as.numeric(data$rev_avg)
data$fac_stu_avg <- as.numeric(data$fac_stu_avg)
data<-na.omit(data)

##----Understanding the data & simple linear regression-------------------------
hist(data$NIS_Stu_avg)
##Seeing the histogram of the average staff to student ratios, I saw that there 
##are a number of institutions that are >70% as many non-instructional staff as there are students.
##Looking back at this dataframe and sorting descending by the average ratios,

##I see that there are a number of specialty and religious institutions with very 
##high numbers of staff: Talmudic College of Florida, Curtis Institute of Music,
##St. John Vianney College Seminary, Pennsylvania Academy of the Fine Arts. 
##RISD is also at about 25%.

##The Talmudic College of Florida is the outlier: it has 77% as many non-instructional staff
##as students over this four year period. Because of its status as an outlier, I should 
##remove it from the dataframe?

##This initial plot of the relationship between staff to student ratios & retention seems to possibly
##be correlated at first glance?!
par(xaxs = "i", yaxs = "i") #sets it so the 0's actually cross at the center
plot(data$NIS_Stu_avg,data$retention_22_23,
     main = "Staff/Student Ratios and Retention",
     xlab = "Number of staff as a percentage of student headcount (2019-2023)",
     ylab = "Retention rates in 2022-2023",
     bty = "l", #removes the line from the top and right of the plot
     las = 1, #rotates the y axis labels to read correctly
     xlim = c(0, 90), 
     ylim = c(0, 110),
     cex.axis = 0.8, #changes the size of the axis font
     tcl  = -0.5, #changes the length and direction of the tick marks
     cex = 0.6, #changes the size of the marks
     pch = 16, #select a plotting symbol
     col = "orange", #change the color of the plotting symbols
)


## Compute correlation
cor.test(data$NIS_Stu_avg,data$retention_22_23)

##define a regression model 
model <- lm(data$retention_22_23~data$NIS_Stu_avg)
abline(model, col="red")
summary(model)


##----Adding more potential variables-------------------------------------------
##Considering the relationships between the variables overall 

##correlation matrix between different variables 
View(data)
dataNoIDs<-data %>% select(c(retention_22_23,
                             NIS_Stu_avg,
                             fac_stu_avg,
                             #tut_avg,
                             tut_p_avg,
                             rev_avg,
                             exp_avg))
  
cor(dataNoIDs)
cormatrix <- cor(dataNoIDs)
corrplot(cormatrix)

corrplot(cor(dataNoIDs), method="color",
        col= colorRampPalette(c("white","orange","red"))(10) ,
        addgrid.col="white", addCoef.col = 'black')

##----Multiple regressions------------------------------------------------------
##first, just throwing everything in 
modelAll<-lm(dataNoIDs)
summary(modelAll)

##now stepwise regression 
modelStep <- lm(retention_22_23~., data=dataNoIDs)
summary(modelStep)
ols_step_forward_r2(modelStep)
ols_step_backward_r2(modelStep)
ols_step_both_r2(modelStep)

dataSelected<-data %>% select(c(retention_22_23,
                                           NIS_Stu_avg,
                                           rev_avg))

modelStep2 <- lm(retention_22_23~., data=dataSelected)
summary(modelStep2)
ols_step_forward_r2(modelStep2)
ols_step_backward_r2(modelStep)
ols_step_both_r2(modelStep2)

View(dataSelected)
