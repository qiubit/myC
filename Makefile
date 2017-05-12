all:
	happy -gca ParMyC.y
	alex -g LexMyC.x
	ghc --make Interpreter.hs -o Interpreter

clean:
	-rm -f *.log *.aux *.hi *.o *.dvi

distclean: clean
	-rm -f DocMyC.* LexMyC.* ParMyC.* LayoutMyC.* SkelMyC.* PrintMyC.* TestMyC.* AbsMyC.* TestMyC ErrM.* SharedString.* ComposOp.* myC.dtd XMLMyC.* Makefile*
	

