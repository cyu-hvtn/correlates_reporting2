export module=cor_threshold
cd ..
Rscript -e "bookdown::render_book(input = 'index_cor.Rmd', output_file = '${module}_$TRIAL.pdf', config_file = '_bookdown_$module.yml', output_format = bookdown::pdf_document2(toc_depth=3), quiet=TRUE)"	
cd $module
