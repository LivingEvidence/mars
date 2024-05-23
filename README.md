# Meta-analysis in R Scripts (MARS)

Run meta-analysis using Rscript based on httr and plumber.

## Development

First, install R and RStudio.

Second, install the following packages.

Install `sodium-dev`:

```
sudo apt install libsodium-dev
```


Then, install R packages

```
install.packages('httr')
install.packages('plumber')
```

Then, install all R packages related to meta-analysis. If anything works, you can run the following command to start the web service:

```
Rscript main.R
```

It will listen `http://localhost:12345` to serve.
