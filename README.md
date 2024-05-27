# Meta-analysis in R Scripts (MARS)

Run meta-analysis using Rscript based on httr and plumber.

## Development

Assuming you are using a Ubuntu Linux environment for development.
First, install R and it's better to use a virtual environment to manage it:

```
conda create --name mars_r44 r-essentials r-base
conda activate mars_r44
```

Second, install the following packages.

Install `sodium-dev` from system:

```
sudo apt install libsodium-dev
```

Install the following packages and tools

```
conda install cmake
```

Then, install R packages

```
install.packages('httr')
install.packages('plumber')
```

Then, install all R packages related to meta-analysis. 

```
install.packages('meta')
```


If anything works, you can run the following command to start the web service:

```
Rscript main.R
```

It will listen `http://localhost:12345` to serve. A test script `test.sh` can be used to test the service. Run it by `./test.sh`.

## Editor

To leverage the configured conda environment, you can setup `VSCode` + `radian` + `R extension` to faciliate the debugging. You can try with the following post `https://www.codydehaan.com/blog/vscode-miniconda-r/` to setup the environment. For MacOS, the process is similar.

## backtransf

The original meta-analysis results are **NOT** back-transformed.
Although the `print` function will show the back-transformed results, the variables are still in their original format. 

Therefore, we need to manually call `backtransf()` on the results. The details are listed in the `https://github.com/cran/meta/blob/master/R/meta-transf.R` and `https://github.com/cran/meta/blob/master/R/print.meta.R`. We need to repeat the same process to reproduce the correct value.