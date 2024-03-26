# QualityTraits & NIRS Data Calibration

The actual repo contains an R workflow that allows to pull Nirs and quality traits Data using API calls,apply preprocessing and smoothing methods to spectral data, fit models, compare and visualize models predictions and performance metrics.
The Rshiny version of the workflow initially allows data extraction based on user specifications of quality lab, crop, country ..etc. Data quality metrics and column wise statistics are computed for both Quality traits data and corresponding NIRS Data.
Multiple smoothing methods are available for processing NIRS Data. Trait to be modeled is also displyed to capture spectral signatures across Traits. 
Original vs processed data is initially displayed. 

Next steps,Implement multivariate analysis (PCA, Correlations etc), calibrate/test and make predictions.

26-03-2024, Work in progress...


