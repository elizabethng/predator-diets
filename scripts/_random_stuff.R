# UTM of centers
Kmeans$centers

# Size of knot/clusters
Kmeans$centers

if ("D_xct" %in% names(Report)) {
  if (is.null(Year_Set)) 
    Year_Set = 1:dim(Report$D_xct)[3]
  if (is.null(Years2Include)) 
    Years2Include = 1:dim(Report$D_xct)[3]
  if (is.null(category_names)) 
    category_names = 1:dim(Report$D_xct)[2]
  Ncategories = dim(Report$D_xct)[2]
  Nyears = dim(Report$D_xct)[3]
}

if (plot_num == 3) {
  if ("D_xt" %in% names(Report)) 
    Array_xct = log(Report$D_xt)
  if ("D_xct" %in% names(Report)) 
    Array_xct = log(Report$D_xct)
  if ("D_xcy" %in% names(Report)) 
    Array_xct = log(Report$D_xcy)
  if ("dhat_ktp" %in% names(Report)) 
    Array_xct = aperm(Report$dhat_ktp[, , cI], c(1, 
                                                 3, 2))
  if ("dpred_ktp" %in% names(Report)) 
    Array_xct = aperm(Report$dpred_ktp[, , cI], 
                      c(1, 3, 2))

if (Nknots < Inf) {
  NN_plot = stats::kmeans(x = PlotDF[, c("Lon", "Lat")], 
                          centers = Nknots, iter.max = 50, nstart = 2, trace = 0)
  Match = match(1:Nknots, NN_plot$cluster)
  PlotDF = PlotDF[Match, ]
  message("Restricted plotting locations to ", Nknots, 
          " locations")
}

# access variance Save$Opt$SD