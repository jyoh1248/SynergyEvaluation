#' DoseEffect() Function
#'
#' This function produces effects from dose-effect 4P logistic parameter model.
#' @param x : amount of dose 
#' @param Emin : lower asymptote
#' @param Emax : upper asymptote
#' @param m : slope (Hill's coefficient)
#' @param IC50 : Relative IC50 
#' @keywords DoseEffect, 4P logistic model
#' @return Efffect that is produced by x amount of doses
#' @export
#' @examples
#' DoseEffect()
DoseEffect<-function(x,Emin,Emax,m,IC50){Emin+(Emax-Emin)/(1+(IC50/x)^m)}


#' InverseDoseEffect() Function
#'
#' Inversed dose effect function
#' @param x : effect
#' @param Emin : lower asymptote
#' @param Emax : upper asymptote
#' @param m : slope (Hill's coefficient)
#' @param IC50 : Relative IC50
#' @keywords InverseDoseEffect, 4P logistic model
#' @return Dose is needed to produce x effect
#' @export
#' @examples
#' InverseDoseEffect()
InverseDoseEffect<-function(x,Emin,Emax,m,IC50){IC50*((Emax-x)/(x-Emin))^(-1/m)}

#' ICrelative() Function
#'
#' Obtain IC relative
#' @param Emin : lower asymptote
#' @param Emax : higher axymptote
#' @param m : slope (Hill's coefficient)
#' @param IC50 : Absolute IC50
#' @keywords Relative IC50
#' @return Dose is needed to produce x effect
#' @export
#' @examples
#' ICrelative()
ICrelative<-function(Emin,Emax,m,IC50){
  IC50*((((Emax-Emin)/(50-Emin))-1)^(1/m))
}


#' ParameterData() Function
#'
#' Returns the parameter data frame
#' @param Emin : lower asymptote
#' @param Emax : higher asymptote
#' @param m : slope (Hill's coefficient)
#' @param IC50 : Absolute IC50
#' @keywords Relative IC50
#' @return Emin, Emax, m, IC50abs, IC50rel
#' @export
#' @examples
#' ParameterData()
ParameterData<-function(Emin,Emax,m,IC50){
  Eminx = Emin
  Emaxx = Emax
  mx = m
  IC50abs = IC50
  IC50rel = ICrelative(Eminx, Emaxx, mx, IC50abs)
  
  data = data.frame(Emin = Eminx, Emax = Emaxx, m = mx, IC50abs = IC50abs, IC50rel = IC50rel)
  return(data)
}
