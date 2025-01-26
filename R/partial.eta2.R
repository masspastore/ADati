"partial.eta2" <-
function(SS.eff,SS.err,eff.lab=NULL)
{
    if (is.null(eff.lab)) {
        effetti <- length(SS.eff)
        eff.lab <- letters[1:effetti]
    }  
    
    eta <- round(SS.eff/(SS.eff+SS.err),4)
    names(eta) <- eff.lab
    return(eta)
}
