# suppose that we have ...
probability   odd 
.25           .3333333

# how to calculate the odd?
.25 / (1-.25) # odd = 0.3333333

# how to calculate the log odds
p       odds     logodds  
.25   .3333333  -1.098612

.25 / (1-.25) # odds = 0.3333333

log(0.3333333) # logodds = -1.098612

# how to calculate the odd using the log odds?
exp(-1.098612) # odd = 0.3333334

# how to calculate the probability?
exp(-1.098612) / (1 + exp(-1.098612)) # probability = 0.2500001
