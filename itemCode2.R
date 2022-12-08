library(cmdstanr)
library(HDInterval)
library(bayesplot)
library(loo)
load(file = "dummy.RData")

itemList = unique(dummy$item)[order(unique(dummy$item))]
sessionList = unique(dummy$s)[order(unique(dummy$s))]
pidgeonList = unique 

newData = NULL
item=1
session=1
for (item in 1:length(itemList)){
  for (session in 1:length(sessionList)){
    selectedData = dummy[which(dummy$s == sessionList[session] &
                                 dummy$item == itemList[item]),]
    newData = rbind(newData,
                    data.frame(
                      y = sum(selectedData$y),
                      e = unique(selectedData$e),
                      c = unique(selectedData$c),
                      s = sessionList[session],
                      item = itemList[item]
                    )
    )
  }
}

## City-Block Model
CityBlock_Model_Syntax = "

data {
  int<lower=0> nObs;     // rows of observations
  int<lower=0> nItems;   // number of items
  array[nObs] int item;
  vector[nObs] c;
  vector[nObs] session;
  vector[nObs] session2;
  array[nObs] int<lower=0, upper=2> y;
}

parameters {
  real beta0;
   // real betac; -- for next time
  real betaD;
  real betaD2;
  vector[nItems] itemRandomEffect;
  real<lower=0> itemSD;
}

// ## number of successes out of 2 trials (per day)
model {
  beta0 ~ normal(0, 1);
  betaD ~ normal(0, .1);
  betaD2 ~ normal(0, .1);
  itemSD ~ exponential(1);
  itemRandomEffect ~ normal(0, itemSD);
  for (obs in 1:nObs){
    y[obs] ~ binomial(2,inv_logit(beta0 + betaD * session[obs] + itemRandomEffect[item[obs]]));
  }å
  // + betaD * session[obs] + betaD2 * session2[obs]
  // (betaC * cityblock -- for next model
}

//generated quantities (R2)

//Model comparison
//For each value of acc in the iteration, get the LL
"

CityBlock_Model_Stan = cmdstan_model(stan_file = write_stan_file(CityBlock_Model_Syntax))


mode04_StanData = list(
  nObs = nrow(newData),
  nItems = length(itemList)+1,
  item = as.integer(newData$item),
  c = newData$c,
  session = newData$s,
  session2 = newData$s^2,
  y = newData$y
)

CityBlock_Model_Samples = CityBlock_Model_Stan$sample(
  data = mode04_StanData,
  seed = 190920221,
  chains = 4,
  parallel_chains = 4,
  iter_warmup = 400,
  iter_sampling = 400
)

# assess convergence: summary of all parameters

CityBlock_Model_Samples$summary(variables = c("beta0", "betaD", "itemSD"))
