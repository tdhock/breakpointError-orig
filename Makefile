HOCKING-breakpointError.pdf: HOCKING-breakpointError.tex refs.bib figure-variable-density-signals.png figure-variable-density-error-alpha.tex figure-variable-density-berr.tex figure-variable-density-error-train.tex figure-variable-scale-signals.tex figure-variable-scale-berr.tex figure-variable-scale-error-alpha.tex figure-breakpoint-error-pieces.tex figure-variable-size-signals.png figure-variable-size-berr.tex figure-variable-size-error-alpha.tex figure-variable-breaks-constant-size.pdf figure-variable-breaks-constant-size-berr.tex figure-variable-breaks-constant-size-alpha.tex figure-variable-density-error-alpha-flsa.tex figure-variable-density-berr-flsa.tex figure-variable-density-sigerr.tex table-penalty-real-data.tex figure-motivation.pdf figure-variable-size-error-alpha-beta.pdf figure-variable-density-annotation-cost.png
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
table-penalty-real-data-slide.tex: table-penalty-real-data-slide.R data/all.stats.RData
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
table-penalty-real-data.tex: table-penalty-real-data.R data/all.stats.RData
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