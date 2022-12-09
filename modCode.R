load("temp.RData")

# dummy code birds
newData = a
birdDummyCodes = model.matrix(object = formula(~1+factor(newData$Bird)))[,2:4]

# 43R is reference bird

## EMPTY Model
CityBlock_Model_Syntax = "

data {
  int<lower=0> nBirds;   // number of birds
  int<lower=0> nObs;     // rows of observations
  int<lower=0> nItems;   // number of items
  array[nObs, nBirds-1] int birdDummy;  // which bird provides each data point
  array[nObs] int item;
  vector[nObs] cityblock;
  vector[nObs] session;
  array[nObs] int<lower=0, upper=2> y;
}

transformed data{
  vector[nObs] logSession = log(session);
}

parameters {
  real beta0;
  real betaC; 
  real betaD;
  real betaB1;
  real betaB2;
  real betaB3;
  real betaDB1;
  real betaDB2;
  real betaDB3;
  vector[nItems] itemRandomEffect;
  real<lower=0> itemSD;
}

// ## number of successes out of 2 trials (per day)
model {
  beta0 ~ normal(0, 1);
  betaB1 ~ normal(0, 1);
  betaB2 ~ normal(0, 1);
  betaB3 ~ normal(0, 1);
  betaDB1 ~ normal(0, 1);
  betaDB2 ~ normal(0, 1);
  betaDB3 ~ normal(0, 1);
  betaD ~ normal(0, .1);
  betaC ~ normal(0, 1);
  itemSD ~ exponential(1);
  itemRandomEffect ~ normal(0, itemSD);
  for (obs in 1:nObs){
    y[obs] ~ binomial(2,
    inv_logit(
      beta0 + betaD * logSession[obs] + itemRandomEffect[item[obs]]  +
      betaB1*birdDummy[obs, 1] + betaB2*birdDummy[obs, 2] + betaB3*birdDummy[obs, 3] + 
      betaDB1*birdDummy[obs, 1] * logSession[obs] + betaDB2*birdDummy[obs, 2] * logSession[obs] +
      betaDB3*birdDummy[obs, 3] * logSession[obs] 
      
      
      )
      );
  }
  
  
}

//generated quantities (R2)

//Model comparison
//For each value of acc in the iteration, get the LL
"

CityBlock_Model_Stan = cmdstan_model(stan_file = write_stan_file(CityBlock_Model_Syntax))


mode04_StanData = list(
  nBirds = 4,
  birdDummy = birdDummyCodes,
  nObs = nrow(newData),
  nItems = length(itemList)+1,
  item = as.integer(newData$Item),
  cityblock = newData$Cityblock,
  session = newData$Session,
  y = newData$y2
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

# maximum R-hat
max(CityBlock_Model_Samples$summary()$rhat, na.rm = TRUE)


## CITYBLOCK Model
CityBlock_Model_Syntax = "

data {
  int<lower=0> nBirds;   // number of birds
  int<lower=0> nObs;     // rows of observations
  int<lower=0> nItems;   // number of items
  array[nObs, nBirds-1] int birdDummy;  // which bird provides each data point
  array[nObs] int item;
  vector[nObs] cityblock;
  vector[nObs] session;
  array[nObs] int<lower=0, upper=2> y;
}

transformed data{
  vector[nObs] logSession = log(session);
}

parameters {
  real beta0;
  real betaC; 
  real betaD;
  real betaB1;
  real betaB2;
  real betaB3;
  real betaDB1;
  real betaDB2;
  real betaDB3;
  real betaCB1;
  real betaCB2;
  real betaCB3;
  vector[nItems] itemRandomEffect;
  real<lower=0> itemSD;
}

// ## number of successes out of 2 trials (per day)
model {
  beta0 ~ normal(0, 1);
  betaB1 ~ normal(0, 1);
  betaB2 ~ normal(0, 1);
  betaB3 ~ normal(0, 1);
  betaDB1 ~ normal(0, 1);
  betaDB2 ~ normal(0, 1);
  betaDB3 ~ normal(0, 1);
  betaCB1 ~ normal(0, 1);
  betaCB2 ~ normal(0, 1);
  betaCB3 ~ normal(0, 1);
  betaD ~ normal(0, .1);
  betaC ~ normal(0, 1);
  itemSD ~ exponential(1);
  itemRandomEffect ~ normal(0, itemSD);
  for (obs in 1:nObs){
    y[obs] ~ binomial(2,
    inv_logit(
      beta0 + betaD * logSession[obs] + itemRandomEffect[item[obs]]  + betaC*cityblock[obs] +
      betaB1*birdDummy[obs, 1] + betaB2*birdDummy[obs, 2] + betaB3*birdDummy[obs, 3] + 
      betaDB1*birdDummy[obs, 1] * logSession[obs] + betaDB2*birdDummy[obs, 2] * logSession[obs] +
      betaDB3*birdDummy[obs, 3] * logSession[obs] + betaCB1*cityblock[obs]*birdDummy[obs, 1] +
       betaCB2*cityblock[obs]*birdDummy[obs, 2] + betaCB3*cityblock[obs]*birdDummy[obs, 3] 
      
      )
      );
  }
  
  
}

//generated quantities (R2)

//Model comparison
//For each value of acc in the iteration, get the LL
"

CityBlock_Model_Stan = cmdstan_model(stan_file = write_stan_file(CityBlock_Model_Syntax))


mode04_StanData = list(
  nBirds = 4,
  birdDummy = birdDummyCodes,
  nObs = nrow(newData),
  nItems = length(itemList)+1,
  item = as.integer(newData$Item),
  cityblock = newData$Cityblock,
  session = newData$Session,
  y = newData$y2
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

# maximum R-hat
max(CityBlock_Model_Samples$summary()$rhat, na.rm = TRUE)

## Euclidean Model
CityBlock_Model_Syntax = "

data {
  int<lower=0> nBirds;   // number of birds
  int<lower=0> nObs;     // rows of observations
  int<lower=0> nItems;   // number of items
  array[nObs, nBirds-1] int birdDummy;  // which bird provides each data point
  array[nObs] int item;
  vector[nObs] euclidean;
  vector[nObs] session;
  array[nObs] int<lower=0, upper=2> y;
}

transformed data{
  vector[nObs] logSession = log(session);
}

parameters {
  real beta0;
  real betaC; 
  real betaD;
  real betaB1;
  real betaB2;
  real betaB3;
  real betaDB1;
  real betaDB2;
  real betaDB3;
  real betaCB1;
  real betaCB2;
  real betaCB3;
  vector[nItems] itemRandomEffect;
  real<lower=0> itemSD;
}

// ## number of successes out of 2 trials (per day)
model {
  beta0 ~ normal(0, 1);
  betaB1 ~ normal(0, 1);
  betaB2 ~ normal(0, 1);
  betaB3 ~ normal(0, 1);
  betaDB1 ~ normal(0, 1);
  betaDB2 ~ normal(0, 1);
  betaDB3 ~ normal(0, 1);
  betaCB1 ~ normal(0, 1);
  betaCB2 ~ normal(0, 1);
  betaCB3 ~ normal(0, 1);
  betaD ~ normal(0, .1);
  betaC ~ normal(0, 1);
  itemSD ~ exponential(1);
  itemRandomEffect ~ normal(0, itemSD);
  for (obs in 1:nObs){
    y[obs] ~ binomial(2,
    inv_logit(
      beta0 + betaD * logSession[obs] + itemRandomEffect[item[obs]]  + betaC*euclidean[obs] +
      betaB1*birdDummy[obs, 1] + betaB2*birdDummy[obs, 2] + betaB3*birdDummy[obs, 3] + 
      betaDB1*birdDummy[obs, 1] * logSession[obs] + betaDB2*birdDummy[obs, 2] * logSession[obs] +
      betaDB3*birdDummy[obs, 3] * logSession[obs] + betaCB1*euclidean[obs]*birdDummy[obs, 1] +
       betaCB2*euclidean[obs]*birdDummy[obs, 2] + betaCB3*euclidean[obs]*birdDummy[obs, 3] 
      
      )
      );
  }
  
  
}

//generated quantities (R2)

//Model comparison
//For each value of acc in the iteration, get the LL
"

CityBlock_Model_Stan = cmdstan_model(stan_file = write_stan_file(CityBlock_Model_Syntax))


mode04_StanData = list(
  nBirds = 4,
  birdDummy = birdDummyCodes,
  nObs = nrow(newData),
  nItems = length(itemList)+1,
  item = as.integer(newData$Item),
  euclidean = newData$Euclidean,
  session = newData$Session,
  y = newData$y2
)

CityBlock_Model_Samples = CityBlock_Model_Stan$sample(
  data = mode04_StanData,
  seed = 190920221,
  chains = 4,
  parallel_chains = 4,
  iter_warmup = 400,
  iter_sampling = 400
)