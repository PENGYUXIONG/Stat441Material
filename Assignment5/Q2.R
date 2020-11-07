setwd("./Desktop/stat 441/Assignment5/")

library(MASS)
suicide.data = read.table("suicide.data", header=T,na.strings=c(""))

suicide.poison = xtabs(POISON~Sex+Age, data = suicide.data)
suicide.gas = xtabs(GAS~Sex+Age, data = suicide.data)
suicide.hang = xtabs(HANG~Sex+Age, data = suicide.data)
suicide.drown = xtabs(DROWN~Sex+Age, data = suicide.data)
suicide.gun = xtabs(GUN~Sex+Age, data= suicide.data)
suicide = xtabs(POISON+GAS+HANG+DROWN+GUN~Sex+Age, data = suicide.data)

mosaicplot(suicide.poison, color=c("lightgray","darkgray"), title(main = 'Poison suicide freq'))
mosaicplot(suicide.gas, color=c("lightgray","darkgray"), title(main='Gas suicide freq'))
mosaicplot(suicide.hang, color=c("lightgray","darkgray"), title(main='Hang Suicide freq'))
mosaicplot(suicide.drown, color=c("lightgray","darkgray"), title(main='Drown Suicide freq'))
mosaicplot(suicide.gun, color=c("lightgray","darkgray"), title(main='Gun suicide freq'))
mosaicplot(suicide, color=c("lightgray","darkgray"), title(main='Suicide freq'))

suicide_canonical_result = corresp(suicide)
suicide_poison_cannonical_result = corresp(suicide.poison)
suicide_gas_cannonical_result = corresp(suicide.gas)
suicide.hang_cannonical_result = corresp(suicide.hang)
suicide.drown_cannonical_result = corresp(suicide.drown)
plot(suicide_canonical_result); title("suicide canonical analysis")
plot(suicide_poison_cannonical_result); title("poison suicide canonical analysis")
plot(suicide_gas_cannonical_result)
plot(suicide.hang_cannonical_result)
plot(suicide.drown_cannonical_result); title("drown suicide canonical analysis")

# log linear model
suicide_glm = glm(POISON+GAS+HANG+DROWN+GUN~Sex*Age, family = poisson, data = suicide.data)
anova(suicide_glm, test="Chisq")
suicide_glm = glm(POISON+GAS+HANG+DROWN+GUN~Sex+Age, family = poisson, data = suicide.data)
suicide.pred = predict(suicide_glm, type = "response")
glm_suicide = xtabs(suicide.pred~., cbind(suicide.data[, 1:2], suicide.pred))
# residual
glm_res = residuals(suicide_glm, type = "response")
suicide_res = xtabs(glm_res~., cbind(suicide.data[,1:2], glm_res))
