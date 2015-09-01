HOCKING-breakpointError.pdf: HOCKING-breakpointError.tex refs.bib figure-variable-density-signals.png figure-variable-density-error-alpha.tex figure-variable-density-berr.tex figure-variable-density-error-train.tex figure-variable-scale-signals.tex figure-variable-scale-berr.tex figure-variable-scale-error-alpha.tex figure-breakpoint-error-pieces.tex figure-variable-size-signals.png figure-variable-size-berr.tex figure-variable-size-error-alpha.tex figure-variable-breaks-constant-size.pdf figure-variable-breaks-constant-size-berr.tex figure-variable-breaks-constant-size-alpha.tex figure-variable-density-error-alpha-flsa.tex figure-variable-density-berr-flsa.tex figure-variable-density-sigerr.tex tables/penalty-real-data.tex figure-motivation.pdf figure-variable-size-error-alpha-beta.pdf figure-variable-density-annotation-cost.png
	rm -f *.aux *.bbl
	pdflatex -interaction errorstopmode HOCKING-breakpointError
	bibtex HOCKING-breakpointError
	pdflatex HOCKING-breakpointError
	pdflatex HOCKING-breakpointError
figure-motivation.pdf: figure-motivation.R
	R --no-save < $<
figure-clusterpath-moons-slide.png: figure-clusterpath-moons-slide.R data/moon.results.RData
	R --no-save < $<
figure-ireg-slide-compare-models.pdf: figure-ireg-slide-compare-models.R data/model.comparison.RData
	R --no-save < $<
figure-ireg-systematic.png: figure-ireg-systematic.R data/annotation.sets.RData data/signal.list.RData
	R --no-save < $<
figure-ireg-scatter-slide-log-hall-line.png: figure-ireg-scatter-slides.R data/L.min.max.RData data/signal.features.RData scripts/display.coefs.R
	R --no-save < $<
figure-ireg-slides-max-margin.tex: figure-ireg-slides.R scripts/interval-regression.R data/signal.list.RData data/signal.features.RData data/segmentation.list.RData data/exact.breakpoints.RData data/demo.csv
	R --no-save < $<
tables/penalty-real-data-slide.tex: tables/penalty-real-data-slide.R data/all.stats.RData
	R --no-save < $<
figure-clusterpath-interpretation.tex: figure-clusterpath-interpretation.R
	R --no-save < $<
figure-clusterpath-normweights.tex: figure-clusterpath-normweights.R figure-clusterpath-normweights-template.tex
	R --no-save < $<
figure-variable-density-slides-sig1-model-4.png: figure-variable-density-slides.R
	R --no-save < $<

## delete all intermediate data files. useful if we want to test
## remaking from scratch.
clean: 
	rm -f */*.tex */*.RData */*.pdf */*.csv figure-*.png figure-*.eps

data/demo.csv: scripts/annotate_breakpoints.py data/profiles.csv 
	python $^ $@

data/profiles.csv: data/make-profiles.R data/clinical-limited.csv
	R --no-save < $<

# figures made using R. The first dependency should be the R file that
# generates it, so we can use that in the recipe with $<. Figures also
# usually depend on other data files.
figure-bams-profiles-noanns.png: figure-bams-profiles.R data/clinical-limited.csv data/profiles.csv scripts/geom_tallrect.R scripts/run.cghseg.R
	R --no-save < $<
figure-bams-global.tex: figure-bams-global.R
	R --no-save < $<
figure-bams-kinetics-pres.tex: figure-bams-kinetics-pres.R
	R --no-save < $<
figure-subdifferential.tex: figure-subdifferential.R
	R --no-save < $<
figure-convexity.tex: figure-convexity.R 
	R --no-save < $<
figure-ireg-compare-model-error.tex: figure-ireg-compare-model-error.R data/model.comparison.RData
	R --no-save < $<
figure-acgh.tex: figure-acgh.R
	R --no-save < $<
figure-ireg-overfitting-path.tex: figure-ireg-overfitting-path.R data/overfit.df.R data/overfitting.RData data/min.test.df.RData scripts/display.coefs.R data/overfit.df.RData
	R --no-save < $<
figure-ireg-scatter-noise-lambda.png: figure-ireg-scatter-noise-lambda.R data/signal.features.RData data/L.min.max.RData scripts/left.right.colors.R
	R --no-save < $<
figure-iris-splom.png: figure-iris-splom.R
	R --no-save < $<
figure-clusterpath-gaussian.png: figure-clusterpath-gaussian.R data/gaussian.RData
	R --no-save < $<
figure-clusterpath-l1path.tex: figure-clusterpath-l1path.R
	R --no-save < $<
figure-clusterpath-l2split.tex: figure-clusterpath-l2split.R
	R --no-save < $<
figure-clusterpath-cvx-allnorms.tex: figure-clusterpath-cvx-allnorms.R data/sim.cvx.RData
	R --no-save < $< 
figure-clusterpath-moons-iris.tex: figure-clusterpath-moons-iris.R data/moons.iris.RData
	R --no-save < $<
figure-rforge-time.tex: figure-rforge-time.R
	R --no-save < $<
figure-regex.tex: figure-regex.R
	R --no-save < $<
figure-lasso-labels.tex: figure-lasso-labels.R	
	R --no-save < $<
figure-iris-grid.tex: figure-iris-grid.R
	R --no-save < $<
figure-iris-non-convex.tex: figure-iris-non-convex.R
	R --no-save < $<
figure-variable-density-sigerr-offpage.tex: figure-variable-density-sigerr-offpage.R data/variable.density.show.RData scripts/fp.fn.colors.R
	R --no-save < $<
figure-qp-labels.tex: figure-qp-labels.R
	R --no-save < $<
figure-dens-confusing.tex: figure-dens-confusing.R
	R --no-save < $<
figure-rat-unreadable.tex: figure-rat-unreadable.R
	R --no-save < $<
figure-bams-smoothing.tex: figure-bams-smoothing.R scripts/signal.colors.R scripts/breakpoint.colors.R
	R --no-save < $<
figure-bams-roc.tex: figure-bams-roc.R data/all.stats.RData scripts/algo.colors.R
	R --no-save < $<
figure-bams-learning-curves.tex: figure-bams-learning-curves.R data/all.stats.RData scripts/fp.fn.colors.R
	R --no-save < $<
figure-bams-kinetics.tex: figure-bams-kinetics.R data/all.stats.RData scripts/algo.colors.R
	R --no-save < $<
figure-variable-density-sigerr.tex: figure-variable-density-sigerr.R data/variable.density.show.RData scripts/fp.fn.colors.R
	R --no-save < $<
figure-variable-density-annotation-cost.png: figure-variable-density-annotation-cost.R data/variable.density.show.RData scripts/geom_tallrect.R scripts/breakpoint.colors.R scripts/signal.colors.R
	R --no-save < $<
figure-variable-breaks-constant-size-alpha.tex: figure-variable-breaks-constant-size-alpha.R data/variable.breaks.constant.size.RData
	R --no-save < $<
figure-variable-breaks-constant-size.pdf: figure-variable-breaks-constant-size.R data/variable.breaks.constant.size.show.RData scripts/signal.colors.R
	R --no-save < $<
figure-variable-breaks-constant-size-berr.tex: figure-variable-breaks-constant-size-berr.R data/variable.breaks.constant.size.show.RData
	R --no-save < $<
figure-variable-size-berr.tex: figure-variable-size-berr.R data/variable.size.show.RData
	R --no-save < $<
figure-variable-size-error-alpha.tex: figure-variable-size-error-alpha.R data/variable.size.signals.RData
	R --no-save < $<
figure-breakpoint-error-pieces.tex: figure-breakpoint-error-pieces.R scripts/signal.colors.R
	R --no-save < $<
figure-variable-size-signals.png: figure-variable-size-signals.R data/variable.size.show.RData scripts/signal.colors.R
	R --no-save < $<
figure-variable-scale-signals.tex: figure-variable-scale-signals.R data/variable.scale.show.RData scripts/signal.colors.R
	R --no-save < $<
figure-variable-scale-berr.tex: figure-variable-scale-berr.R data/variable.scale.show.RData
	R --no-save < $<
figure-variable-density-error-train.tex: figure-variable-density-error-train.R data/variable.density.signals.RData
	R --no-save < $<
figure-variable-density-error-alpha.tex: figure-variable-density-error-alpha.R data/variable.density.signals.RData 
	R --no-save < $<
figure-variable-density-error-alpha-flsa.tex: figure-variable-density-error-alpha-flsa.R data/variable.density.signals.RData 
	R --no-save < $<
figure-variable-scale-error-alpha.tex: figure-variable-scale-error-alpha.R data/variable.scale.signals.RData 
	R --no-save < $<
figure-variable-density-signals.png: figure-variable-density-signals.R data/variable.density.show.RData scripts/signal.colors.R
	R --no-save < $<
figure-variable-density-berr.tex: figure-variable-density-berr.R data/variable.density.show.RData
	R --no-save < $<
figure-variable-density-berr-flsa.tex: figure-variable-density-berr-flsa.R data/variable.density.show.RData
	R --no-save < $<
figure-penalty-2-size.tex: figure-penalty-2-size.R
	R --no-save < $<
figure-penalty-1-points.tex: figure-penalty-1-points.R
	R --no-save < $<
figure-penalty-4-variance.tex: figure-penalty-4-variance.R
	R --no-save < $<
figure-penalty-1-cghseg.tex: figure-penalty-1-cghseg.R
	R --no-save < $<
figure-clusterpath-geometry.tex: figure-clusterpath-geometry.R
	R --no-save < $<
figure-clusterpath-moons.png: figure-clusterpath-moons.R data/moon.results.RData
	R --no-save < $<

# tables!
tables/penalty-real-data.tex: tables/penalty-real-data.R data/all.stats.RData
	R --no-save < $<
tables/bams-annotation-chrom-counts.tex: tables/bams-annotation-chrom-counts.R
	R --no-save < $<
tables/bams-annotation-profile-counts.tex: tables/bams-annotation-profile-counts.R
	R --no-save < $<
tables/bams-error-on-unseen-profiles.tex: tables/bams-error-on-unseen-profiles.R data/all.stats.RData scripts/algos.in.tables.R
	R --no-save < $<
tables/bams-generalization-error-global-models.tex: tables/bams-generalization-error-global-models.R data/all.stats.RData scripts/algos.in.tables.R
	R --no-save < $<
tables/clinical-line.tex: tables/clinical-line.R data/clinical-line.txt
	R --no-save < $<
tables/moon-results.tex: tables/moon-results.R data/moon.results.RData
	R --no-save < $<
tables/coefficients.tex: tables/coefficients.R data/model.comparison.RData
	R --no-save < $<

# other
scripts/algo.colors.tex: scripts/make.algo.colors.tex.R scripts/algo.colors.R 
	R --no-save < $<


# (big) files to download from my CBIO web space. These are files that
# are essential to build the PHD, but are not edited by hand so are
# not stored in version control. Instead, I upload a copy to
# ~thocking/thesis and use download.sh to retreive it.
figure-negr1.png: scripts/download.sh
	bash $< $@
figure-Karyo-both.png: scripts/download.sh
	bash $< $@
figure-ucsc.png: scripts/download.sh
	bash $< $@
figure-rforge.png: scripts/download.sh
	bash $< $@
data/clinical-line.txt: scripts/download.sh
	bash $< $@
figure-directlabels-design.tex: scripts/download.sh
	bash $< $@ 
figure-breakpoint-annotator.png: scripts/download.sh
	bash $< $@
figure-label-breaks-zoomed.png: scripts/download.sh
	bash $< $@
data/all.stats.RData: scripts/download.sh
	bash $< $@
data/variable-density-signals-regions.csv: scripts/download.sh
	bash $< $@
figure-apply.tex: scripts/download.sh
	bash $< $@
data/SimuProfiles.csv: scripts/download.sh
	bash $< $@
data/annotations.csv: scripts/download.sh
	bash $< $@

# RData files made using R. The first dependency should be the R
# script, so we can use $< to generate it.
data/moon.results.RData: data/moon.results.R
	R --no-save < $<
data/sim.cvx.RData: data/sim.cvx.R
	R --no-save < $<
data/variable.density.signals.RData: data/variable.density.signals.R data/precise.breakpoint.cost.R data/run.cghseg.R
	R --no-save < $<
data/variable.density.show.RData: data/variable.density.show.R data/variable.density.signals.RData data/variable-density-signals-regions.csv
	R --no-save < $<
data/gaussian.RData: data/gaussian.R
	R --no-save < $<
data/moons.iris.RData: data/moons.iris.R
	R --no-save < $<

data/variable.scale.signals.RData: data/variable.scale.signals.R data/precise.breakpoint.cost.R data/run.cghseg.R
	R --no-save < $<
data/variable.scale.show.RData: data/variable.scale.show.R data/variable.scale.signals.RData
	R --no-save < $<

data/variable.size.signals.RData: data/variable.size.signals.R 
	R --no-save < $<
data/variable.size.show.RData: data/variable.size.show.R data/variable.size.signals.RData
	R --no-save < $<

data/variable.breaks.constant.size.RData: data/variable.breaks.constant.size.R 
	R --no-save < $<
data/variable.breaks.constant.size.show.RData: data/variable.breaks.constant.size.show.R data/variable.breaks.constant.size.RData
	R --no-save < $<




##### intReg
## All these figures are made by executing the first dependency.
figure-ireg-max-margin.tex: figure-ireg-max-margin.R data/signal.list.RData data/L.min.max.RData data/exact.cost.RData
	R --no-save < $<
figure-ireg-check-sim.tex: figure-ireg-check-sim.R data/signal.list.RData data/annotation.sets.RData
	R --no-save < $<
figure-ireg-compare-models.tex: figure-ireg-compare-models.R data/model.comparison.RData
	R --no-save < $<
figure-ireg-regularization-path.pdf: figure-ireg-regularization-path.R figure-ireg-overfitting.tex data/overfitting.RData data/min.test.df.RData
	R --no-save < $<
figure-ireg-overfitting.tex: figure-ireg-overfitting.R data/overfit.df.RData data/min.test.df.RData
	R --no-save < $<
figure-ireg-optimization-results.tex: figure-ireg-optimization-results.R data/optimization.results.RData
	R --no-save < $<
figure-ireg-phi-deriv.tex: figure-ireg-phi-deriv.R
	R --no-save < $<
figure-ireg-relaxations.tex: figure-ireg-relaxations.R scripts/interval-regression.R data/signal.list.RData data/L.min.max.RData data/exact.cost.RData
	R --no-save < $<
figure-ireg-compare-losses.tex: figure-ireg-compare-losses.R data/signal.list.RData data/L.min.max.RData data/exact.cost.RData loss.colors.R
	R --no-save < $<

## These 3 figures are pretty similar, but these are the differences:

## show the red circles to show that the exact algo works.
figure-ireg-exact-kstar-cost-grid.tex: figure-ireg-exact-kstar-cost-grid.R data/exact.cost.RData data/segmentation.list.RData data/cost.matrices.RData scripts/ireg.signals.R
	R --no-save < $<
## show 2 signals and their curves to get a feeling about what we are
## doing.
figure-ireg-exact-kstar-cost-2.tex: figure-ireg-exact-kstar-cost-2.R data/exact.cost.RData data/signal.list.RData data/segmentation.list.RData data/annotation.sets.RData scripts/ireg.signals.R scripts/geom_tallrect.R scripts/breakpoint.colors.R scripts/signal.colors.R
	R --no-save < $<
## show the exact curves for 1 signal, just to establish terminology
## in the introduction.
figure-ireg-exact-kstar-cost.tex: figure-ireg-exact-kstar-cost.R data/exact.cost.RData scripts/ireg.signals.R 
	R --no-save < $<

figure-ireg-cv-results.tex: figure-ireg-cv-results.R data/cv.results.RData loss.colors.R
	R --no-save < $<
figure-ireg-cv-results-all.tex: figure-ireg-cv-results-all.R data/cv.results.RData loss.colors.R
	R --no-save < $<
figure-ireg-path.tex: figure-ireg-path.R data/lambda.matrices.RData active.rankers.R
	R --no-save < $<
figure-ireg-exact-breakpoints.tex: figure-ireg-exact-breakpoints.R data/segmentation.list.RData
	R --no-save < $<
figure-ireg-active.tex: figure-ireg-active.R data/active.results.RData
	R --no-save < $<
figure-ireg-sigma-learning.tex: figure-ireg-sigma-learning.R data/signal.features.RData data/signal.list.RData data/L.min.max.RData scripts/left.right.colors.R
	R --no-save < $<
figure-ireg-smoothness-variance.tex: figure-ireg-smoothness-variance.R data/signal.list.RData scripts/pick.best.index.R scripts/left.right.colors.R data/lambda.matrices.RData data/L.min.max.RData data/exact.cost.RData
	R --no-save < $<
figure-ireg-active-old-new.tex: figure-ireg-active-old-new.R scripts/pick.best.index.R data/lambda.matrices.RData
	R --no-save < $<
figure-ireg-lambda-diagnostics.tex: figure-ireg-lambda-diagnostics.R data/lambda.matrices.RData
	R --no-save < $<
figure-ireg-hinge-loss.tex: figure-ireg-hinge-loss.R scripts/left.right.colors.R data/signal.list.RData data/lambda.matrices.RData
	R --no-save < $<
figure-ireg-coefficients.tex: figure-ireg-coefficients.R data/model.comparison.RData
	R --no-save < $<

## All these long intermediate calculations are stored in RData
## files. Each is made by executing the script which is the first
## dependency.
data/min.test.df.RData: data/min.test.df.R data/overfit.df.RData
	R --no-save < $<
data/overfit.df.RData: data/overfit.df.R data/overfitting.RData
	R --no-save < $<
data/lambda.matrices.RData: data/lambda.matrices.R data/cost.matrices.RData data/segmentation.list.RData
	R --no-save < $<
data/model.comparison.RData: data/model.comparison.R data/lambda.matrices.RData data/signal.features.RData data/L.min.max.RData data/exact.cost.RData scripts/interval-regression.R scripts/pick.best.index.R scripts/annotation.error.R scripts/display.coefs.R
	R --no-save < $<
data/overfitting.RData: data/overfitting.R data/signal.features.RData data/L.min.max.RData data/exact.cost.RData data/cost.matrices.RData scripts/interval-regression.R scripts/annotation.error.R
	R --no-save < $<
data/signal.features.RData: data/signal.features.R data/signal.list.RData data/segmentation.list.RData
	R --no-save < $<
data/active.results.RData: data/active.results.R data/lambda.matrices.RData active.rankers.R 
	R --no-save < $<
data/optimization.results.RData: data/optimization.results.R data/signal.list.RData data/L.min.max.RData optimization.R
	R --no-save < $<
data/cv.results.RData: data/cv.results.R data/lambda.matrices.RData data/signal.list.RData data/L.min.max.RData data/exact.cost.RData scripts/interval-regression.R scripts/annotation.error.R
	R --no-save < $<
data/exact.cost.RData: data/exact.cost.R data/cost.matrices.RData data/exact.breakpoints.RData
	R --no-save < $<
data/L.min.max.RData: data/L.min.max.R data/exact.cost.RData
	R --no-save < $<
data/exact.breakpoints.RData: data/exact.breakpoints.R data/segmentation.list.RData
	R --no-save < $<
data/signal.list.RData: data/signal.list.R data/SimuProfiles.csv
	R --no-save < $<
data/segmentation.list.RData: data/segmentation.list.R data/signal.list.RData
	R --no-save < $<
data/annotation.sets.RData: data/annotation.sets.R data/annotations.csv
	R --no-save < $<
data/cost.matrices.RData: data/cost.matrices.R data/segmentation.list.RData data/annotation.sets.RData
	R --no-save < $<
## for reviews 7 April 2015
data/variable.size.alpha.beta.RData: data/variable.size.alpha.beta.R data/variable.size.signals.RData
	R --no-save < $<
figure-variable-size-error-alpha-beta.pdf: figure-variable-size-error-alpha-beta.R data/variable.size.alpha.beta.RData
	R --no-save < $<