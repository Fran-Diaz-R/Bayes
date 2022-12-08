Data$a_n <- paste(Data$areaIncorrect, Data$numIncorrect, sep = '_')

Data$a_n2 = 0

for (n in length(unique(Data$a_n))) {
  Data$a_n2 = Data$a_n2 + 1 
}

x= 1:81


Data$a_n2 = factor(Data$a_n, labels = c(unique(x)))
Data$a_n2 = as.numeric(Data$a_n2)

for (b in unique(Data$bird)){
  session = 1
  for (d in unique(Data$date[Data$bird == b])){
    fullcheck = length(Data$session[Data$bird == b & Data$date == d]) == trials
    if (fullcheck){
      Data$session[Data$bird == b & Data$date == d] = session
      session = session + 1
    }
  }
}