#LH lineage analysis
library(catnat)
library(elmr)

#get all neurons from CATMAID > 100 nodes
all = catmaid_fetch("/1/skeletons/?nodecount_gt=100")
#all.neurons = read.neurons.catmaid(all, OmitFailures = TRUE, .progress = 'text') #replaced below

#transform to FCWB
#all.fcwb = xform_brain(all.neurons, sample = FAFB13, reference = FCWB) #replaced below
all.fcwb = fetchn_fafb(all, mirror = FALSE, reference = FCWB)

#define volume - TODO - remove hard-coded volume, allow substitutions
LH.right = subset(FCWBNP.surf, "LH_R")

#get neurons with significant nodes in right LH
inLH = all.fcwb[sapply(all.fcwb, function(x) sum(pointsinside(xyzmatrix(x), LH.right)) > 100)]

#get primary neurites
inLH.pns = primary.neurite.neuronlist(inLH)

#select only neurons with somas
#somasInLH = unlist(sapply(inLH, function(x) if (!is.null(x$tags$soma)) x$tags$soma))
inLH.withSomas = inLH[sapply(inLH, function(x) !is.null(x$tags$soma))]
inLH.withSomas.pns = primary.neurite.neuronlist(inLH.withSomas)



#connections with DA1 PNs
DA1PNs = c(27295, 57311, 57315, 57323, 57353, 755022, 57381, 61221)
skidsInLH = as.integer(names(inLH.withSomas))
LHandPNs = c(skidsInLH, DA1PNs)
names(LHandPNs) = sapply(c(0:(length(LHandPNs) - 1)), function(x) paste0("skeleton_ids[", as.character(x), "]"))

connectivity = catmaid_fetch("1/skeletons/confidence-compartment-subgraph", body = as.list(LHandPNs))
connectivity.flattened = lapply(connectivity$edges, function(x) unlist(x))
connectivity.table = do.call(rbind.data.frame, connectivity.flattened)
names(connectivity.table) = c("skid_1", "skid_2", "conn_1", "conn_2", "conn_3", "conn_4", "conn_5")

DA1connections = connectivity.table[(connectivity.table$skid_1 %in% DA1PNs)|(connectivity.table$skid_2 %in% DA1PNs),]
DA1connections.uniqueSKIDs = as.character(unique(c(unique(DA1connections$skid_1), unique(DA1connections$skid_2))))
DA1connections.neurons = inLH.withSomas[DA1connections.uniqueSKIDs]
DA1connections.neurons.pns = inLH.withSomas.pns[DA1connections.uniqueSKIDs]

DA1connections.inverse = inLH.withSomas[!names(inLH.withSomas) %in% DA1connections.uniqueSKIDs]
DA1connections.inverse.pns = inLH.withSomas.pns[!names(inLH.withSomas.pns) %in% DA1connections.uniqueSKIDs]
