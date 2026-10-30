## TODO: move this code to make.J.est.R??

## J.names <- list(profile.id=pids,
##                 chromosome=annotated.chroms,
##                 segments=1:global.kmax)
## J.est <- array(NA,sapply(J.names,length),J.names)
## segmentations <- list()
## for(pid in pids){
##   for(chrom in annotated.chroms){
##     one.chrom <- annotated.chrom.probes[[pid]][[chrom]]
##     results <- cghseg.chrom(one.chrom)
##     J.est[pid,chrom,1:length(results$J.est)] <- results$J.est
##     segmentations[[pid]][[chrom]] <- results
##   }
## }

