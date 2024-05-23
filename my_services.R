library(meta)
library(jsonlite)

###########################################################
# Incidence Analysis
###########################################################
#* Run incidence analysis on the given input data frame
#* @param data The dataframe to be analyzed
#* @param cfg The configuration to be used in the analysis
#* @post /INCD
function(data, cfg){
    results_incd <- metaprop(
        Et, 
        Nt,  
        data = data, 
        studlab = study, 
        sm = cfg$sm, 
        method = cfg$pooling_method, 
        method.tau = cfg$tau_estimation_method, 
        hakn = cfg$hakn_adjustment,
        adhoc.hakn = cfg$adhoc_hakn,
        backtransf = TRUE
    )

    ret_str <- toJSON(list(
        incdma = results_incd,
        summary_incd = summary_incd,
        primma = c(),
        cumuma = c(),
        version = list(
            jsonlite = packageVersion('jsonlite'),
            meta = packageVersion('meta')
        )
    ), force=TRUE)

    fromJSON(ret_str)
}


###########################################################
# Pairwise meta-analysis
###########################################################


###########################################################
# Network meta-analysis
###########################################################



###########################################################
# Example services
###########################################################

#* Echo the parameter that was sent in
#* @param msg The message to echo back.
#* @get /echo
function(msg=""){
      list(msg = paste0("The message is: '", msg, "'"))
}

#* Plot out data from the iris dataset
#* @param spec If provided, filter the data to only this species (e.g. 'setosa')
#* @get /plot
#* @serializer png
function(spec){
      myData <- iris
  title <- "All Species"

    # Filter if the species was specified
    if (!missing(spec)){
            title <- paste0("Only the '", spec, "' Species")
      myData <- subset(iris, Species == spec)
        }

    plot(myData$Sepal.Length, myData$Petal.Length,
                main=title, xlab="Sepal Length", ylab="Petal Length")
}
