#### PATH TO HDF5 AND MPI LIBRARY AND INCLUDES - to be modified by user (to be completed)

#module load intel-compiler/14.0.0
#module load intel-mkl/11.1.0
#module load intel-mpi/4.0.0.028
#module load hdf5/1.8.12

#LIBHDF5 = -L/opt/san/bibliotheques/hdf5/1.8.12/lib/ -lhdf5 -lhdf5_hl -lhdf5_fortran
#INCLUDEHDF5 = -I/opt/san/bibliotheques/hdf5/1.8.12/include
#LIBMPI = -L/opt/san/intel15/impi/5.0.2.044/lib64/ -lmpi -lmpi_dbg -lmpi_mt -lmpigf -lmpi_ilp64 
#INCLUDEMPI = -I/opt/san/intel15/impi/5.0.2.044/include64

EXEC = autoTest.exe
#FC   = ifort
FC   = mpiifort

#### FROM THIS POINT ON THE USER SHOULD NOT MODIFY
RM := rm -rf

#Specifying the sources "f90" and "f"
SRCS = $(wildcard *.f90 *.f) 

#Create a ".f90" for each source
#OBJS = $(SRCS:.f90=.o) #SYNTAX NOT WORKING

OBJS += ./main_AutoTest.o \
	./charFunctions.o \
	./systemUt_RF.o \
	./constants_Auto.o \
	./fileManag_Auto.o

LIBS = $(LIBHDF5) $(LIBMPI) 
INCLUDE = $(INCLUDEHDF5) $(INCLUDEMPI)

#Dependencies
main_AutoTest.o   : systemUt_RF.o charFunctions.o constants_Auto.o fileManag_Auto.o
fileManag_Auto.o  : charFunctions.o constants_Auto.o

# Making all the ".o" from the ".f90"
%.o: ../SRC/%.f90
	@echo 'Building file: $<'
	@echo 'Invoking: Fortran Compiler'
	$(FC) -o "$@" $(INCLUDE) -c "$<"
	@echo 'Finished building: $<'
	@echo ' '

	
# All Target
all: autoTest

# Tool invocations
autoTest: $(OBJS)
	@echo 'Building target: $@'
	@echo 'Invoking: Fortran Linker'
	$(FC) -o $(EXEC) $(OBJS) $(INCLUDE) $(LIBS)
	@echo 'Finished building target: $@'
	@echo ' '

# Other Targets
clean:
	-$(RM) $(EXECUTABLE) $(OBJS) *.mod
	-@echo ' '

.PHONY: all clean dependents
.SECONDARY:

