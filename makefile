
# run the files
run:
	rm -f *.cmo *.cmi *.out *.o *.dot *.png
	ocaml int64list.ml
	ocaml decisiontree.ml
	ocaml dotGest.ml


# clean the files
clean:
	rm -f *.cmo *.cmi *.out *.o *.dot *.png

# run plot_data_p.ipynb
plot:
	jupyter notebook plot_data_p.ipynb


all:
	make run
	make plot


install:
	pip install matplotlib
	pip install pandas