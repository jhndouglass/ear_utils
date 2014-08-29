##################################################################
# PROGRAM: RandomFeatureSample.R
# USE: RANDOM SAMPLING OF SPATIAL OBJECTS
# REQUIRES: SP CLASS SpatialDataFrame OBJECT
#
# FUNCTIONS:  
#   RandSP(x, ns=10, reps=1, replace=FALSE) 
#
# ARGUMENTS: 
#   x        SP CLASS SpatialDataFrame OBJECT (POINT, POLYGON, LINE, PIXEL)
#   n        NUMBER OF RANDOM SAMPLES
#   reps     NUMBER OF REPLICATES (STRATA)
#   replace  SAMPLE WITH REPLACEMENT (DEFAULT FALSE)
#   ...      OPTIONAL ARGUMENTS TO PASS TO sample
#
# VALUE:
#    SUBSAMPLE OF SP FEATURECLASS AS SAME SP CLASS
#
# NOTES:
#   IF REPLACEMENT IS FALSE FEATURES ARE REMOVED FROM CONSIDERATION IN SUBSEQUENT 
#   REPS  
#     THIS MEANS THAT IF TRUE, THAT A FEATURE CAN BE SELECTED MULTIPLE TIMES ACROSS 
#     REPLICATES. NOT RELEVANT IF REP=1. 
#   
# EXAMPLES:
#   require(rgdal)
#   require(sp)
#   # READ SHAPEFILE USING RGDAL
#     shape <- readOGR(dsn="C:/test", layer="polys", driver="ESRI Shapefile")
#
#   # CREATES 4 REPLCATES WITH 5 RANDOM SAMPLES EACH WITHOUT REPLACEMENT
#     ssample <- RandSP(shape, n=5, reps=4, replace=FALSE) 
#
#   # CREATES 1 REPLCATE WITH 10% RANDOM SAMPLE WITHOUT REPLACEMENT
#     ssample <- RandSP(shape, n=round(dim(shape)[1]*0.10), digits=0), reps=1, replace=FALSE) 
#
#   # WRITE RANDOM SAMPLE SHAPEFILE USING RGDAL
#     writeOGR(ssample, dsn="C:/test", layer="RandSample", driver="ESRI Shapefile")
#
#   # PLOT RANDOM SAMPLES COLORED BY REPLICATE   
#     spplot(ssample, "REP", col.regions=terrain.colors(n=length(unique(ssample@data$REP))))
#
# REFERENCES:
#    B.D. Ripley, 1981. Spatial Statistics, Wiley 
#       
# CONTACT: 
#     Jeffrey S. Evans 
#     Senior Landscape Ecologist 
#     The Nature Conservancy - Central Science
#     Laramie, WY
#     (970)672-6766
#     jeffrey_evans@tnc.org
##################################################################
RandSP <- function(x, n=10, reps=1, replace=FALSE, ...)
{
  if (!require(sp)) stop("sp PACKAGE MISSING")
  if (!inherits(x@data, "data.frame")) stop("MUST BE SP SpatialDataFrame OBJECT")
  spx <- x   
  if ( dim(spx)[1] > n ) { 
    s <- sample(1:nrow(spx@data), n) 
    results <- spx[s, ]
    results@data <- data.frame(results@data, REP=1)	 
    if(replace==FALSE) { spx <- spx[-s, ] } 
    if(reps > 1) {
      for (i in 2:reps ) {    
        s <- sample(1:nrow(spx@data), n) 
        ssamp <- spx[s, ] 
        ssamp@data <- data.frame(ssamp@data, REP=i)
        results <- rbind(results, ssamp)	   
        if(replace==FALSE) { spx <- spx[-s, ] }	   
      }
    }
  }
  return( results )
}