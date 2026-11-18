data('UCBAdmissions')
ucba<-data.frame(UCBAdmissions)

head(ucba)
tail(ucba)
cross<-xtabs(Freq~Gender+Admit,data=ucba)
cross
(cross<-xtabs(Freq~Gender+Admit,data=ucba))#shortcut of the above code
prop.table(cross,1)#proportions used in interpretations
prop.table(cross,2)
###simpson's paradox
(cross2 <-xtabs(Freq~Gender+Admit,data=ucba[ucba$Dept=="F"]))
barplot(as.matrix(data),beside=TRUE,col=c("Red","Green"))
##side by side bar chart vs stacked bar chart
#CHI SQUARE TEST OF INDEPENDENCE
#scatterplot
plot(WEIGHT~HEIGHT,data=data_class)
abline(lm(HEIGHT~WEIGHT,data=data_class)$coefficient)
data_lm<-lm(HEIGHT~WEIGHT,data=data_class)
summary(data_lm)
#adjusted R-Squared determines accuracy(best aCCURACY lies in 90% and above)
#main="weight" this is used when u want to create title in R graphs(our title here is weight)
#assumptions to be checked to decide if a linear model is to be used
#1.Linearity
#2.Independence of Residuals (Errors)
#3. Homoscedasticity (Constant Variance)
#4.Normality of Residuals\
#Linear Model interpretation
#attach(height) this means R will bring the word height in dta class i have
#cbind and rbind function
Sys.Date()
dob<-as.Date("2000-02-10")
today<-as.Date("2026-10-25")
difftime(today,dob,units = "days")
difftime(today,dob,units = "hours")
dob<-as.Date("2000-05-01")
today<-as.Date("2026-10-25")
difftime(today,dob,units = "days")
difftime(today,dob,units = "hours")
#handling missing values
#1.deleting all observation with missing data(listwise delection)
#2.replacing the missing by the average of the remaining data (to keep having the general idea of the data)
is.na(leadership[])
#dyplr is a package that contains functions that allows to select specific rows u want , certain observation you want..(need to read more of this)
install.packages("nycflights13")
install.packages("tidyverse")
library(dplyr)
##filter function helps selecting the observation (rows)for specific character you want for instance a certain month(wanting all data corresponding to only january)
#view(flights)
#attach(flights)
#jan_dec<-filter(flights, month==1|month==12)
##Arrange Function it sorts
#arrange(flights,desc(arr_delay)) this will arrange all fights that delayed in descending order
##Select function helps selecting the variables(columns) for specific character 
#time_var<-select(flights, year,month,day)
#select(flights, year:day)
#select(flights,-(year:day)) selecting all data except the ones specificied
##mutate funtion it helps in adding a new variable by through creating, modifying and deteting variables(columns)
#flights2<-select(flights, year:day, ends_with("delay"),distance, air_time)
#mutate()
install.packages("nycflights13")
library(dplyr)
## side by side bar chart
##legend funtion
##ggplot2 it allows you to visualise complex datasets in new creative way-some work need some packages to get done some of the graph are created using R'S Base graphics system
library(ggplot2)
data(mtcars)
View(mtcars)
table(mtcars$cyl)
View(mtcars$cyl)
#geom(adding a thing based on the base one) forexample: geom_point, geom_line,..

#sunday class
#distributions
?runif
(x<-runif(1,0,10))
(x<-runif(20,0,100))
x<-runif(20,0,100)
hist(x)
y<-rnorm(20,0,100)  
hist(y)     
x<-runif(1,0,10)
x
if (x>3){
  print("x is greater than tree")
} else{
    print("x is less or equal to three")
}
for (i in 1:10)
x<-("a","b","c","d","e")
x[3]
count<-1
while (count<10) {
  print(count)
  count<-count+1
}
count<-5
while (count<10) {
  print(count)
  count<-count+1
}
?set.seed() #if u want to get the same random number in ur working u set the same initial value(seed)
z<-5
z
set.seed(1)
while(z>=3 && z<=10){
  coin<-rbinom(1,1,0.5)
  if (coin ==1){
    z=z+1
  }
  else{
    z=z-1
  }
}
print(z)
#Funtion in funtion when u don't specify parameter , it is considered as constant
functionname<-funtion(parameter){
  statements
  return(value)
}
f1<-function(x){
  result<-x^2+2
  return(result)
}
f2<-function(x,y){
  result<-x^2+y^2+2*x*y-3
  return(result)
}
   f2(2,3)
#apply() 1 used for rows and 2 for columns forexample when we use margin=1 or margin=2
#split() this function is important function while dealing with big data
#ASSIGNMENT 3: Application of dealing vapply,lapply,..or ..ply function
#TITANIC DATASETS
view(Titanic)
View(Titanic)
apply(Titanic,c(1,3),sum) # 1 resembles column and 3 column
#rnorm() it has two characters which are mean and standard deviation thus when u write rnorm(number) the automation counting mean of those numbers to 0 and standard deviation to 1 of those numbe

quadratic <- function(a, b, c,d) {

  
  discriminant <- b^2 - 4*a*c
  
  if (a == 0) {
    return("Not a quadratic equation")
  }
  
  if (discriminant > 0) {
    x1 <- (-b + sqrt(discriminant))/(2*a)
    x2 <- (-b - sqrt(discriminant))/(2*a)
    
    return(c(x1, x2))
  }
  
  else if (discriminant == 0) {
    x <- -b/(2*a)
    return(x)
  }
  
  else {
    real <- -b/(2*a)
    imaginary <- sqrt(-discriminant)/(2*a)
    
    return(c(
      complex(real = real, imaginary = imaginary),
      complex(real = real, imaginary = -imaginary)
    ))
  }
}
#Main Tools used for Debugging in R
#1 Traceback() prints out the function call stack after an error has taken occurei<-function(d)"a"+d
h<-function(c) i(c)
g<-function(b) h(b)
f<-function(a) g(a)
f(5)
?traceback()
roots<-compute_roots(coefficients$a, coefficients$b,coefficients$c)
?Debug()
