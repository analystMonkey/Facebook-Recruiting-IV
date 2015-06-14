##Bot or Not. Facebook Recruiting IV: Human or Robot?

The following code my the solution for the fourth Facebook recruitment competition hosted by Kaggle on: https://www.kaggle.com/c/facebook-recruiting-iv-human-or-bot

The main scripts are the following

- FacebookRaw.R: This is the main code, it contains all the EDAs and models tried
- BotOrNot.Rmd: For a better explaination of the code please refer to this RMarkdown file. The code is publicly available on: http://rpubs.com/wacax/85239
- bidderId2Boilerplate.R; dispersionTimeFeatures.R; featureLengths.R;  timeNumericFeatures.R; simultaneousAuctionTimeFinder.R
are functions used to mine the data. They extract statistical dispersion features, probability of variables, numeric features (counts), and simoultaneous actions or more precisely, actions happening within a time range defined by the user.

WARNING: This code contains the xgboost library which is not currently hosted by CRAN. Please refer to their website for instalation instructions https://github.com/dmlc/xgboost

##EDA 
Repeated Cross Validation Performance of the model. approximately 0.927, top 25% code.
![Imgur](http://i.imgur.com/GazfQBt.png)
Variable Importance of the top 50 variables of the best model.
![Imgur](http://i.imgur.com/6H1XVJe.png)