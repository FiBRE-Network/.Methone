# Mock Exam - S1 2023

### Flu vaccine

#Question 1
sum(dbinom(0:3, size=17, p=0.5))

# Question 2
sum(dbinom(4:8, size=17, p=0.5))

## Question 4

## Let x=number of students contraced flu

# p-value = P(X<=x)

# We should find x where p-value<=0.05

## Check for different values of x 

sum(dbinom(0:1, size=10, p=0.5))

## Question 5

## Probability of type II error = P(do not reject Ho| Ho is false)

## from Q4, x=1

## P(Type II error) = P(x>1|p=0.3)

sum(dbinom(2:10, size=10, p=0.3))

## Question 6

#Moderate evidence



## Question 7

mu = 20.2
sd = 7.5

z=(24-mu)/sd

1-pnorm(z)

1-pnorm(24, mean=mu, sd=sd)

## Question 8
n=5
SE = sd/sqrt(n) 
pnorm(18, mean=mu, sd=sd/sqrt(n))



#######################################

## Cat ownership
cat = read.csv("Cats.csv")

## Question 3

aggregate(Age~PE, cat, mean)


## Question 4

addmargins(table(cat$Ownership, cat$PE))



## Question 5
chisq.test(table(cat$Ownership, cat$PE))$expected



## Question 7
chisq.test(table(cat$Ownership, cat$PE))$statistic

## Question 8
chisq.test(cat$Ownership, cat$PE)$p.value



## Question 9
addmargins(table(cat$PE))



## Question 10

cat$PE = ifelse(cat$PE=="Yes", 1, 0)

summary(glm(PE ~ Ownership+Age, data=cat, family = "binomial"))

summary(glm(PE ~ Ownership+Age, data=owner, family = "binomial"))$coef[3,1]


## Question 11

qnorm(0.975)*0.1092

qnorm(0.975)*summary(glm(PE ~ Ownership+Age, data=cat, family = "binomial"))$coef[3,2]



## Question 12


lnodds = predict(glm(PE~Ownership+Age, data=cat, family="binomial"), 
            newdata=data.frame(Ownership="Cat", Age=12))

exp(lnodds)/(1+exp(lnodds))




###########################################
## Sleep Quality

sleep = read.csv("GPA.csv")


## Question 3
t.test(SQI~Gender, data=sleep, alternative="less")

## Question 6
summary(lm(GPA~SQI, data=sleep))

summary(lm(GPA~SQI, data=sleep))$coef[2,1]

## Question 7
n=length(sleep$GPA)
n
summary(lm(GPA~SQI, data=sleep))$coef[2,1]-summary(lm(GPA~SQI, data=sleep))$coef[2,2]*qt(0.975, n-2)

## Question 9
summary(lm(GPA~SQI, data=sleep))$coef[2,4]/2

## Question 10
summary(lm(GPA~SQI+Exercise+Alcohol, data=sleep))

## Question 13
predict(lm(GPA~SQI+Exercise+Alcohol, data=sleep), 
        newdata=data.frame(SQI=8.9, Exercise=9, Alcohol="Yes"))


## Question 14
sleep$Resid = resid(lm(GPA~SQI+Exercise+Alcohol, data=sleep))
sleep$Fitted = predict(lm(GPA~SQI+Exercise+Alcohol, data=sleep))

plot(sleep$Fitted, sleep$Resid)

plot(lm(GPA~SQI+Exercise+Alcohol, data=sleep))

#############################################################

# Scenario 4



## Penguines questions creation drafts

## Question 3
pt(-1.342, 22)

## Question 4
LL=42.59
UL = 47.13
MOE = (UL-LL)/2
MOE

## Question 8
SSG = 533.2
SSR = 130.6
SSG/(SSG+SSR)

## Question 9
MSG = SSG/2
MSR = SSR/21
MSG/MSR

## Question 10
p = 1-pf(42.8683, 2, 21)
p