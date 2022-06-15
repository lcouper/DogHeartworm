# Dog Heartworm

## Working Docs ##

Manuscript: https://docs.google.com/document/d/1mBBMrbIyOUMV21rdmh0MmGNLvhc1uEgw8Von9YLKAlc/edit     
Background lit review: https://docs.google.com/document/d/1Zg_nIOlhlgXVmr2NUPHglen3gVPq5e9mMfqH90NS53c/edit   
Methods notes: https://docs.google.com/document/d/1hY7sos5XK_RxLHgmWM97U1i8WC7OKNtP1CoghQdlnFE/edit   
Methods workflow: https://docs.google.com/document/d/1BogzO9ykmIimr-thRqLZEKozIp5Ohv96KspCF5MFO3c/edit    

## Currently working on ##

Q2: GBM identifying which climate / land cover predictors are important for each species

## Just done ##

- Increased nrounds of Bayesian optimization & run on sherlock
- set up bootstrapoing to get values for pdp plots (can get pdp for single model)
- converted dog pop sizes to density (DogHeartworm_CountyPopSizes_2009_2021 datasheet)
- ran pipeline for Ae aegypti

## Next to do ##

- q1: pdps for each species
- q2: run scripts for Ae. albopictus (starting with model fitting/ finding hyperparameters)

## Scripts workflow ##

- hyperparameter tuning (identify optimal hyperparams)
- boostrap 100 iteractions (calculate AUC, gain)
- plot AUC, gain for top 10 features







