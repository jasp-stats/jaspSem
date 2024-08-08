
### Help on Estimators
Some of the estimators come with options that are set in the background and may be hidden for the user:

- Estimators without extra effects: 
	- ML, GLS, WLS, ULS, DWLS, DLS

- Extensions of ML-estimators with extra effects: 
	- MLM: classic robust se (se="robust.sem"), Satorra-Bentler test statistic (test="satorra.bentler")
	- MLMV: classic robust se, scaled and shifted statistic (test="scaled.shifted")
	- MLMVS: classic robust se, mean and var adjusted Satterthwaite style (test="mean.var.adjusted")
	- MLF: first-order standard se (information="first.order"), regular test?
	- MLR: Huber-White robust se (se="robust.huber.white"), Yuan-Bentler T2 star statistic (test="yuan.bentler.mplus")

- Others: 
	- WLSM: implies DWLS with scaled test and robust se
	- WLSMV: implies DWLS with mean and var adjusted test and robust se
	
	- ULSM: implies ULS with scaled test and robust se
	- ULSMV: implies ULS with mean-var adjusted test and robust se