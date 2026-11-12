set -o errexit
R -e 'for(p in c("zoo","RColorBrewer","tikzDevice","flsa","xtable","plyr","ggplot2","directlabels"))if(!requireNamespace(p))install.packages(p,repos="https://cloud.r-project.org")'
for TGZ in Segmentor3IsBack_2.0.tar.gz bams_1.0.tar.gz cghseg_0.0.1.tar.gz changepoint_1.1.1.tar.gz ; do
    PKG=$(echo $TGZ | sed 's/_.*//')
    if [ ! -f $TGZ ]; then
	URL=https://cloud.r-project.org/src/contrib/Archive/$PKG/$TGZ
	wget $URL
    fi
    R CMD INSTALL $TGZ
done
R CMD INSTALL breakpointError_2016.10.25.tar.gz # not on CRAN
R --vanilla < figure-motivation.R 
R --vanilla < figure-breakpoint-error-pieces.R 
R --vanilla < data/variable.density.signals.R
R --vanilla < data/variable.density.show.R
R --vanilla < figure-variable-density-signals.R 
R --vanilla < figure-variable-density-berr.R
R --vanilla < figure-variable-density-error-train.R 
R --vanilla < figure-variable-density-error-alpha.R
R --vanilla < data/variable.breaks.constant.size.R
R --vanilla < data/variable.breaks.constant.size.show.R
R --vanilla < figure-variable-breaks-constant-size.R
R --vanilla < figure-variable-breaks-constant-size-berr.R
R --vanilla < figure-variable-breaks-constant-size-alpha.R
R --vanilla < data/variable.size.signals.R
R --vanilla < data/variable.size.show.R
R --vanilla < figure-variable-size-signals.R
R --vanilla < data/variable.size.alpha.beta.R
R --vanilla < figure-variable-size-error-alpha-beta.R 
R --vanilla < figure-variable-density-berr-flsa.R
R --vanilla < figure-variable-density-error-alpha-flsa.R
R --vanilla < figure-variable-density-annotation-cost.R
R --vanilla < figure-variable-density-sigerr.R 
R --vanilla < table-penalty-real-data.R
pdflatex HOCKING-breakpointError
bibtex HOCKING-breakpointError
pdflatex HOCKING-breakpointError
pdflatex HOCKING-breakpointError
