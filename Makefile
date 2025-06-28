# GNU Prolog <http://www.gprolog.org>
GPROLOG=gprolog
GPLC=gplc

# SWI-Prolog <https://www.swi-prolog.org>
SWIPL=swipl

# Ciao <https://ciao-lang.org>
CIAO=ciao
CIAOC=ciaoc

about:
	@echo 'Targets:'
	@echo '  run-gprolog   - run using the GNU Prolog intepreter'
	@echo '  build-gprolog - build lunar executable using GNU Prolog'
	@echo '  run-swipl     - run using the SWI-Prolog interpreter'
	@echo '  run-ciao      - run using the Ciao interpreter'
	@echo '  build-ciao    - build lunar executable using Ciao'
	@echo '  clean         - remove all generated files'

.PHONY: clean
clean:
	- $(RM) lunar
	- $(RM) lunar.itf
	- $(RM) lunar.po

run-gprolog: lunar.pl
	$(GPROLOG) --consult lunar.pl

build-gprolog: lunar.pl
	$(GPLC) lunar.pl

run-swipl: lunar.pl
	$(SWIPL) lunar.pl

run-ciao: lunar.pl
	$(CIAO) run lunar.pl

build-ciao: lunar.pl
	$(CIAOC) lunar.pl