.PHONY : _build thesis.pdf watch clean

BUILD_DIR=_build
TOOLS_DIR=tools

thesis.pdf : opts.gen.tex ${BUILD_DIR}
	latexmk -pdf -jobname=${BUILD_DIR}/thesis thesis.tex

watch : opts.gen.tex ${BUILD_DIR}
	latexmk -pvc -pdf -jobname=${BUILD_DIR}/thesis thesis.tex

opts.gen.tex :
	python ${TOOLS_DIR}/gen-opts.py draft comments > $@

${BUILD_DIR} :
	mkdir -p $@

clean :
	latexmk -C
	rm -rf ${BUILD_DIR} *.gen.tex *.thm
