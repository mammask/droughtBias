<div style="text-align: justify">
This is a repository with codebase and results of the publication: "Characterization of Bias During Meteorological Drought Calculation in Time Series Out-of-Sample Validation"

<hr>

#### Abstract

The Standardized Precipitation Index (SPI) is used for characterizing and predicting meteorological droughts on a range of time scales. However, in     forecasting applications, when SPI is computed on the entire available dataset, prior to model-validation, significant biases are introduced, \KM{especially under changing climatic conditions}. In this paper, we investigate the theoretical and numerical implications that arise when SPI is computed under stationary and non-stationary probability distributions. We demonstrate that both the stationary SPI and non-stationary SPI (NSPI) lead to increased information leakage to the training set with increased scales, which significantly affects the characterisation of drought severity. The analysis is performed across about 36,500 basins in Sweden, and indicates that the  stationary SPI is unable to capture the increased rainfall trend during the last decades and leads to systematic underestimation of wet events in the training set, affecting up to \KM{22\%} of the drought events. NSPI captures the non-stationary characteristics of accumulated rainfall; however it introduces biases to the training data affecting 19\% of the drought events. The variability of NSPI bias has also been observed along the country's climatic gradient with regions in snow climates strongly being affected. The findings propose that drought assessments under changing climatic conditions can be significantly influenced by the potential misuse of both SPI and NSPI, inducing bias in the characterization of drought events in the training data.
<hr>

#### How to use the code

The codebase of the publication can be found in the `main.R` script. The supporting functions to compute the bias and generate the analytical outcomes can be found in the `/src` folder. To parameterize the script and select the spi scale and the types of graphs that will be generated use the `config.cfg` file. The steps described below should be followed to reproduce the results:

1. Define the parameters in `config.cfg`
2. Run `main.R`
3. Under `/scr` run `runReporting.R`

**Important Note**: The available data has been provided by SMHI, therefore they are not available in this repository.