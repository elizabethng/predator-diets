# Checking model convergence interactively

startval <- Obj$par
startval[2] <- -1

Opt <- TMBhelper::fit_tmb(
  startpar = startval,
  obj = Obj,
  lower = TmbList[["Lower"]],
  upper = TmbList[["Upper"]],
  getsd = TRUE,
  savedir = DateFile,
  bias.correct = use_bias_correct,
  newtonsteps = 3,
  bias.correct.control = list(
    sd=FALSE, split=NULL, nsplit=1, vars_to_correct = "Index_cyl")
)

Obj$par <- startval
Opt$identifiable <- TMBhelper::Check_Identifiable(Obj)



RowMax = apply(jj$Eigen$vectors[, jj$WhichBad, drop = FALSE], 
               MARGIN = 1, 
               function(vec){max(abs(vec))})
myrowmax = jj$Eigen$vectors[, jj$WhichBad] %>% abs()
identical(RowMax,myrowmax)

Param_check = ifelse(RowMax > 0.1, "Bad", "OK")
param_ok = myrowmax < 0.1
