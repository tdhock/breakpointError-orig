display.coefs <- function
### Calculate exponents that actually appear in the learned penalty.
(intercept,
### numeric scalar.
 weights,
### numeric vector of learned coefficients after scaling.
 mu,
### numeric vector of variable means for scaling.
 sigma
### numeric vector of variable sd for scaling.
 ){
  stopifnot(length(intercept)==1)
  stopifnot(length(weights)==length(mu))
  stopifnot(length(weights)==length(sigma))
  nu <- intercept-sum(weights*mu/sigma)
  v <- weights/sigma
  rbind(data.frame(variable=names(v),coef=v),
        data.frame(variable="intercept",coef=nu))
}
