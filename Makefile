.PHONY : _build thesis.pdf watch content.gen.tex clean

BUILD_DIR=_build
TOOLS_DIR=tools

thesis.pdf : content.gen.tex opts.gen.tex ${BUILD_DIR}
	latexmk -pdf -jobname=${BUILD_DIR}/thesis thesis.tex

watch : content.gen.tex opts.gen.tex ${BUILD_DIR}
	latexmk -pvc -pdf -jobname=${BUILD_DIR}/thesis thesis.tex

opts.gen.tex :
	python ${TOOLS_DIR}/gen-opts.py draft comments > $@

content.gen.tex :
	python ${TOOLS_DIR}/gen-content.py content > $@

${BUILD_DIR} :
	mkdir -p $@

clean :
	latexmk -C
	rm -rf ${BUILD_DIR} *.gen.tex *.thm
