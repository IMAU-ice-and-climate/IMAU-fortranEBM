.suffixes: .f .o
#
#
FC		= gfortran		#-mp-4.7
#
FFLAGS  =  -g -c -O3 -ftree-vectorize -ffree-form -fbounds-check -fdefault-real-8  \
                 -funroll-loops -ffast-math
OFLAGS  =  -g

sources.f = \
	ebmmodel.f \
	fluxes.f \
	functions.f \
	initial.f \
	inittables.f \
	input.f \
	massbalance.f \
	newalbedo.f \
	output.f \
	radiation.f \
	routines.f \
	skintemperature.f \
	snowgrid.f \
	snowmodel.f
target = ebmmodel.x

OBJECTS = ${sources.f:.f=.o}

ebmmodel.x : $(OBJECTS) 
	$(FC) $(OFLAGS) -o $@ $(OBJECTS) 

.f.o:
	$(FC) -c $(FFLAGS) -o $*.o  $<

clean:
	mv inittables.o ../
	rm -rf *.{mod,o,x*,txt}
	mv ../inittables.o .

cleanall:
	rm -rf *.{mod,o,x*,txt}

depend:
	makedepend  $(sources.f)

# DO NOT DELETE THIS LINE -- makedepend depends on it.
