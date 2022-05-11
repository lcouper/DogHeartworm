# Dog Heartworm

## Working Docs ##

Manuscript: https://docs.google.com/document/d/1mBBMrbIyOUMV21rdmh0MmGNLvhc1uEgw8Von9YLKAlc/edit     
Background lit review: https://docs.google.com/document/d/1Zg_nIOlhlgXVmr2NUPHglen3gVPq5e9mMfqH90NS53c/edit   
Methods notes: https://docs.google.com/document/d/1hY7sos5XK_RxLHgmWM97U1i8WC7OKNtP1CoghQdlnFE/edit

## Currently working on ##

Q2: GBM identifying which climate / land cover predictors are important for each species

## Next to do ##

- one-hot encoding on any factors (since XGboost can't handle factor data. Need to convert to dummy variables)
- figure out why model is R squared so low
- look into metrics used by Nick Skaff  & Van Buren: change in Gini impurity criterion, 'gain' (Van Buren) & overall predictor error
- how to calculate marginal effects / partial dependence plots (new package: https://github.com/bgreenwell/pdp)
- How to calculate prediction error? 


