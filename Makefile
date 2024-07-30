build:
	python preprocessing_first_best_dist_speed.py; Rscript decathlete_first_best_analysis_GLM_dist_speed.r; Rscript coef_bootstrap_CIs.r; python CI_matrix_graph_maker.py

preprocess:
	python preprocessing_first_best_dist_speed.py

r:
	Rscript decathlete_first_best_analysis_GLM_dist_speed.r

boot:
	Rscript coef_bootstrap_CIs.r

graphs:
	python CI_matrix_graph_maker.py

