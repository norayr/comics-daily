

# comics reader

## build

several options:

* open in lazarus ide project1.lpr or project1.lpi and press the build button.
* use default makefile which calls `lazbuild` command.
* there is a Maemakefile, so `make -f Maemakefile` should build it on maemo-leste or devuan chimaera.
* as of now alpine only has fpc, but not lazarus. you should unpack lazarus source somewhere, let's say `/home/user/lazarus`, and do `make all` from inside that directory to build it. then, there is an alpine makefile which contains abovementioned path to lazarus source, so typing `make -f Alpinemakefile` in comics-daily source directory should build it on postmarketos or alpine.

these makefiles contain lazarus paths, adjust if necessary.

more [here](https://xn--y9azesw6bu.xn--y9a3aq/content/24165919/)


