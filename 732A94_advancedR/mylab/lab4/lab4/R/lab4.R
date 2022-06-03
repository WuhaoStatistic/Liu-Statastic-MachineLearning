#' liner regression like what lm() does
#'
#' @field regco: regression coefficient (beta with a hat)
#' @field fitted_value: fitted value (y with a hat)
#' @field residuals : residuals (e with a hat)
#' @field deg_of_fredom : degree of fredom
#' @field resi_var: variance of residuals
#' @field regco_var: variance of regression coefficient
#' @field t_regco: t value of regression coefficient
#' @field object: input formula
#' @field data : input dataframe(the data reference of formula)
#' @field dataname: name of dataframe
#' @field fname : string of formula(without bracket)
#' @field mat1: created by model.matrix()
#' @field y: true value in dataframe not estimated value
#' 
#' @import methods
#' @importFrom ggplot2 ggplot
#' @importFrom plyr is.formula
#' 
#' @export linreg
#' @exportClass linreg

linreg<-setRefClass("linreg",
  fields = list(regco = "matrix",fitted_value="matrix",residuals = "matrix",
                deg_of_fredom = "numeric",resi_var = "numeric",regco_var = "matrix",
                t_regco = "matrix", object = "formula", data = "data.frame",
                dataname = "character",fname = "character",mat1 = "matrix",y = "numeric"
                ),
  method = list(
  initialize = function(object,data)
  {
    if(!plyr::is.formula(object)|!is.data.frame(data))
      stop()
    fname <<- Reduce(paste, deparse(object))
    dataname <<- deparse(substitute(data))
    object <<- object
    data <<- data
    mat1 <<- model.matrix(object,data)
    namey <- all.vars(object)[1]
    y <<- data[,namey]
    regco <<- solve(t(mat1)%*%mat1)%*%t(mat1)%*%y
    fitted_value <<- mat1%*%regco
    residuals <<- y - fitted_value
    deg_of_fredom <<- length(y) - length(all.vars(object))
    resi_var <<- (t(residuals)%*%residuals)[1,1]/deg_of_fredom
    regco_var <<- resi_var*solve(t(mat1)%*%mat1)
    t_regco <<- regco/sqrt(diag(regco_var))
  },
  
  print = function()
  {
    should_print <- paste0('linreg(formula = ',fname)
    should_print <- paste0(should_print,', data = ')
    should_print <- paste0(should_print,dataname)
    should_print <- paste0(should_print,')')
    base::print(should_print)
    base::print(regco[,1])
  },
  
  QR = function()
  {
    matQ <- qr.Q(qr(mat1))
    matR <- qr.R(qr(mat1))
    regco <<- solve(matR)%*%t(matQ)%*%y
    regco_var <<- resi_var*solve(matR)%*%t(solve(matR))
  },
  
  resid = function()
  {
    return(residuals)
  },
  
  pred = function()
  {
    return(fitted_value)
  },
  
  coef = function()
  {
    vec <- as.vector(regco)
    names(vec)<-row.names(regco)
    return(vec)
  },
  summary = function()
  {
    p_val = 2*pt(abs(t_regco),df = deg_of_fredom,lower.tail = FALSE)
    std_e <- round((sqrt(diag(regco_var))),3)
    m <- as.data.frame(matrix(c(round(regco,3), std_e, round(t_regco,3), format(p_val, scientific = 2)), ncol = 4))
    for(i in 1:ncol(mat1)){
      m[i,5]<-"***"
      
    }
    colnames(m) <- c("Estimate", "Std. Error", "t value", "Pr(>|t|)","")
    rownames(m) <- colnames(mat1)
    base::print(m)
    base::print(paste("Residual standard error:", round(sqrt(resi_var),3), "on", deg_of_fredom, "degrees of freedom"))
  },
  plot = function()
  {
    df = as.data.frame(cbind(fitted_value,residuals,sqrt(abs((residuals-mean(residuals))/sd(residuals)))))
    #base::print(df[,1])
    p1<-ggplot2::ggplot(data = df,ggplot2::aes(x = df[,1],y = df[,2]))+
    ggplot2::geom_point(shape=21, size=3)+
    ggplot2::labs(x =paste0(paste0('Fitted value\nlm(',fname),')'), y ='Residuals')+
    ggplot2::ggtitle('Residuals vs Fitted')+
    ggplot2::geom_hline(yintercept = 0, col="grey", linetype="dotted")+
    ggplot2::stat_summary(fun=median, color="red", geom="line")+
    ggplot2::theme_test()+
    ggplot2::theme(plot.title = ggplot2::element_text(hjust=0.5))
    
    p2<-ggplot2::ggplot(data = df,ggplot2::aes(x = df[,1],y = df[,3]))+
      ggplot2::geom_point(shape=21, size=3)+
      ggplot2::labs(x =paste0(paste0('Fitted value\nlm(',fname),')'), y ='sqrt(|Standardized Residuals|)')+
      ggplot2::ggtitle('Scale_Location')+
      ggplot2::stat_summary(fun=median, color="red", geom="line")+
      ggplot2::theme_test()+
      ggplot2::theme(plot.title = ggplot2::element_text(hjust=0.5))
    base::print(p1)
    base::print(p2)
  }
  )
)


