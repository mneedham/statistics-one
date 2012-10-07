library(psych)

ratings <- read.table('~/Documents/Statistics/supplemental_stats1_ex01.txt')

layout(matrix(c(1,2,3,4), 2, 2, byrow = TRUE))

hist(ratings$FourPlay, xlab = "Rating")
hist(ratings$HobNob, xlab = "Rating")
hist(ratings$RedTruck, xlab = "Rating")
hist(ratings$WoopWoop, xlab = "Rating")

class(ratings)
names(ratings)
describe(ratings)

memory <- read.table('~/Documents/Statistics/DAA.01.txt', header = T)

des <- subset(memory, cond=='des')
aer <- subset(memory, cond=='aer')

hist(des$pre.wm.s, xlab = "Rating")
hist(aer$pre.wm.s, xlab = "Rating")

hist(des$post.wm.s, xlab = "Rating")
hist(aer$post.wm.s, xlab = "Rating")

describe(des$pre.wm.s)
describe(des$post.wm.s)

describe(aer$pre.wm.s)
describe(aer$post.wm.s)

describe(des$pre.wm.v)
describe(des$post.wm.v)

describe(aer$pre.wm.v)
describe(aer$post.wm.v)


hist(des$pre.wm.v, xlab = "Rating")
hist(aer$pre.wm.v, xlab = "Rating")

hist(des$post.wm.v, xlab = "Rating")
hist(aer$post.wm.v, xlab = "Rating")

hist(memory$post.wm.v, xlab = "Rating")
hist(memory$pre.wm.v, xlab = "Rating")
hist(memory$post.wm.v, xlab = "Rating")

impact <- read.table('~/Documents/Statistics/supplemental_STATS1.EX.02.txt', header = T)

hist(impact$memory.visual, xlab = "Visual Memory", main = "Histogram", col = "red")

describe(impact)

plot(impact$memory.verbal ~ impact$memory.visual, main = "Scatterplot", ylab = "Verbal memory", xlab = "Visual memory")

abline(lm(impact$memory.verbal ~ impact$memory.visual), col = "blue")

cor(impact$memory.verbal, impact$memory.visual)

cor.test(impact$memory.verbal, impact$memory.visual)

cor(impact)

names(impact)

library(gclus)
impact.r = abs(cor(impact))
impact.col = dmat.color(impact.r)
impact.o <- order.single(impact.r)

cpairs(impact, impact.o, panel.colors=impact.col, gap=.5, main = "Variables Ordered and Coloured by Correlation")

# test/re-test reliability analysis

setwd("~/Documents/Statistics")

impact.col <- read.table("supplemental_STATS1.EX.03.COL.txt", header = T)

names(impact.col)
describe(impact.col)

cor.test(impact.col$memory.verbal.A, impact.col$memory.verbal.B)

cor(impact.col$memory.verbal.A, impact.col$memory.verbal.B)
cor(impact.col$memory.visual.A, impact.col$memory.visual.B)
cor(impact.col$speed.vismotor.A, impact.col$speed.vismotor.B)
cor(impact.col$speed.general.A, impact.col$speed.general.B)
cor(impact.col$impulse.control.A, impact.col$impulse.control.B)

impact.row <- read.table("supplemental_STATS1.EX.03.ROW.txt", header = T)
describeBy(impact.row, impact.row$test)

cor(impact.row$memory.verbal[impact.row$test=="A"], impact.row$memory.verbal[impact.row$test=="B"])
cor(impact.row$memory.visual[impact.row$test=="A"], impact.row$memory.visual[impact.row$test=="B"])

assignment2 <- read.table("DAA.02.txt", header = T)

describe(assignment2)

cor(assignment2$pre.wm.s1[assignment2$cond=="des"], assignment2$pre.wm.s2[assignment2$cond=="des"])
cor(assignment2$pre.wm.s1[assignment2$cond=="aer"], assignment2$pre.wm.s2[assignment2$cond=="aer"])
cor(assignment2$pre.wm.v1[assignment2$cond=="des"], assignment2$pre.wm.v2[assignment2$cond=="des"])
cor(assignment2$pre.wm.v1[assignment2$cond=="aer"], assignment2$pre.wm.v2[assignment2$cond=="aer"])


cor(assignment2$pre.wm.s1[assignment2$cond=="aer"], assignment2$post.wm.s1[assignment2$cond=="aer"])
cor(assignment2$pre.wm.s2[assignment2$cond=="aer"], assignment2$post.wm.s2[assignment2$cond=="aer"])
cor(assignment2$pre.wm.v1[assignment2$cond=="aer"], assignment2$post.wm.v1[assignment2$cond=="aer"])
cor(assignment2$pre.wm.v2[assignment2$cond=="aer"], assignment2$post.wm.v2[assignment2$cond=="aer"])


cor(assignment2$pre.wm.s1[assignment2$cond=="des"], assignment2$post.wm.s1[assignment2$cond=="des"])
cor(assignment2$pre.wm.s2[assignment2$cond=="des"], assignment2$post.wm.s2[assignment2$cond=="des"])
cor(assignment2$pre.wm.v1[assignment2$cond=="des"], assignment2$post.wm.v1[assignment2$cond=="des"])
cor(assignment2$pre.wm.v2[assignment2$cond=="des"], assignment2$post.wm.v2[assignment2$cond=="des"])


# Multiple regression analysis

endur <- read.table("supplemental_STATS1.EX.04.txt", header = T)
plot(endur$endurance ~ endur$age, main = "Scatterplot", ylab = "Endurance", xlab = "Age")
abline(lm(endur$endurance ~ endur$age), col = "blue")

plot(endur$endurance ~ endur$activeyears, main = "Scatterplot", ylab = "Endurance", xlab = "Active Years")
abline(lm(endur$endurance ~ endur$activeyears), col = "blue")

describe(endur)

model1 = lm(endur$endurance ~ endur$age)
summary(model1)

model2 = lm(endur$endurance ~ endur$activeyears)
summary(model2)

model3 = lm(endur$endurance ~ endur$age + endur$activeyears)
summary(model3)

# standardized regression coefficients - what's the difference?
# anova = analysis of variance

model1.z= lm(scale(endur$endurance) ~ scale(endur$age))
summary(model1.z)

model2.z= lm(scale(endur$endurance) ~ scale(endur$activeyears))
summary(model2.z)

model3.z= lm(scale(endur$endurance) ~ scale(endur$activeyears) + scale(endur$age))
summary(model3.z)

comp1 = anova(model1.z, model3.z)
comp1

comp2 = anova(model2.z, model3.z)
comp2

lm(formula = scale(endur$endurance) ~ scale(endur$activeyears))

# week 3 assignment

week3 <- read.table("DAA.03.txt", header = T)

cor(week3$age, week3$endurance)

ageEndurance = lm(week3$endurance ~ week3$age)
summary(ageEndurance)

standardizedAgeEndurance = lm(scale(week3$endurance) ~ scale(week3$age))
summary(standardizedAgeEndurance)

ageActiveYearsEndurance = lm(week3$endurance ~ week3$age + week3$activeyears)
summary(ageActiveYearsEndurance)

standardizedAgeActiveYearsEndurance = lm(scale(week3$endurance) ~ scale(week3$age) + scale(week3$activeyears))
summary(standardizedAgeActiveYearsEndurance)

cor(week3$activeyears, week3$endurance)

activeYearsEndurance = lm(week3$endurance ~ week3$activeyears)
summary(activeYearsEndurance)

standardizedActiveYearsEndurance = lm(scale(week3$endurance) ~ scale(week3$activeyears))
summary(standardizedActiveYearsEndurance)

plot(week3$endurance ~ week3$activeyears, main = "Scatterplot", ylab = "Endurance", xlab = "Active Years")
abline(lm(week3$endurance ~ week3$activeyears), col = "blue")

plot(scale(week3$endurance) ~ scale(week3$activeyears), main = "Scatterplot", ylab = "Endurance", xlab = "Active Years")
abline(lm(scale(week3$endurance) ~ scale(week3$activeyears)), col = "blue")

# Mediation analysis

# X is extraversion
# Y is happiness
# M is diversity of life experience

med <- read.table("supplemental_STATS1.EX.05.txt", header=T) 

describe(med)
# how do you work out kurtosis?

hist(med$happy)
hist(med$extra)
hist(med$diverse)

plot(med$happy ~ med$extra)
abline(lm(med$happy ~ med$extra))

plot(med$diverse ~ med$extra)
abline(lm(med$diverse ~ med$extra))

plot(med$happy ~ med$diverse)
abline(lm(med$happy ~ med$diverse))

# residuals shouldn't be a function of any of the variables - i.e. shouldn't be predictable from X, Y, M

model1= lm(med$happy ~ med$extra)
summary(model1)
# happy = 2.19 + 0.275(extra)

model2 = lm(med$diverse ~ med$extra)
summary(model2)
# diverse = 1.63 + 0.284(extra)

model3 = lm(med$happy ~ med$extra + med$diverse)
summary(model3)
# happy = 1.886 + 0.222(extra) + 0.1868(diverse)
# partial mediation - because there is still a correlation between extraversion and happiness even though
# we've now included diversity in the equation

# Sobel test
# tests the null hypothesis that the indirect effect through the mediator is 0

# need to read up on what exactly z value is - to do with standard deviation
# we have a z value associated with an error rate? e.g. z value for non directional p of 0.05 is 1.96
# checking if it's statistically significant...

library(multilevel)

indirect = sobel(med$extra, med$diverse, med$happy)
indirect

# Moderation analysis
# X extraversion
# Y happiness
# Z SES

mod <- read.table("supplemental_STATS1.EX.06.txt", header = T)

no.mod.model = lm(mod$happy ~ mod$extra + mod$ses)
summary(no.mod.model)

mod.model = lm(mod$happy ~ mod$extra + mod$ses + mod$mod)
summary(mod.model)

anova(no.mod.model, mod.model)