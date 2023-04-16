# STAT_447B_Project
This is the repository for the STAT 447B project.
The code used here requires a dataset which can be found here: https://www.kaggle.com/datasets/austinreese/craigslist-carstrucks-data (version 10).
The csv containing the data is too large to upload (even after compression), to run these files download the kaggle dataset and run the files in their numbered sequence.
It includes the following files:
<ul>
<li> 01-dataClean.ipynb : an ipython notebook which contains a function to clean the data. It produces 02-vehicles-cleaned.csv </li>
<li> 02-dataSplit.ipynb: a notebook which splits the data into a training and testing set to prepare for EDA. It produces 03a-vehicles-sample.csv and 03b-vehicles-holdout.csv </li>
<li> 03a-EDA.ipynb: a notebook which explores relationships and reccomends binning and transformations for the wrangling stage </li>
<li> 03b-vehicles-wrangling.ipynb: a notebook which explains the rationale for certain transformations and binnings that are made to the datasets </li>
<li> 04-featureSelection.ipynb: a notebook which explains the process of creating the feature_selection function in functions.R </li>
<li> 05a-linearRegression.ipynb: a notebook which compares the linear models over kfold cv </li>
<li> 05b-WLS.ipynb: a notebook which compares WLS over kfold cv </li>
<li> 05c-tree.ipynb: a notebook which compares regression trees over kfold cv </li>
<li> 05d-random-forest.ipynb: a notebook which compares quantile random forest over k fold cv</li>
<li> 05e-LR_with_interactions.ipynb: a notebook which compares linear models with interactions over k fold cv</li>
<li> 06-Validation.ipynb: A notebook which takes the best models from the 05 series of notebooks and examines their model assumptions and compares them over the holdout </li>
<li> 05d-RF.rds: a saved model containing the random forest model trained on the full training set. This was done to save computational time in 06-Validation.ipynb </li>
<li> 06-Validation.ipynb: a notebook detailing to validation process and comparing methods on the holdout set. </li>
<li> functions.R: a repository of commonly called functions </li>
</ul>
