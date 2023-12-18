"partial.omega2" <-
function(eff.Var,eff.df,err.Var,err.df,eff.lab=NULL) 
{
    if (is.null(eff.lab)) {
        effetti <- length(eff.Var)
        eff.lab <- letters[1:effetti]
    }   
        
    n <- sum(eff.df,err.df)+1
    est.Var <- eff.df*(eff.Var-err.Var)/n
    omega <- round(est.Var/(est.Var+err.Var),4)
    names(omega) <- eff.lab
    return(omega)
}

