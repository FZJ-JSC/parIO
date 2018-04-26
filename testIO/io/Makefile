first: all
include ../Makefile.in

FILES    =  testio_io_basics \
            testio_io

F90FILES = $(FILES:%=%.F90)
OFILES   = $(FILES:%=$(OBJDIR)/%.o)
MODFILES = $(FILES:%=$(MODDIR)/%.mod)

LIBFILE  = lib_io.a

all: Makefile ../Makefile.in
	$(MAKE) $(LIBFILE)
	
.PHONY: $(LIBFILE)
$(LIBFILE): $(OFILES)
	@$(CHK_DIR_EXISTS) $(LIBDIR) || $(MKDIR) $(LIBDIR)
	$(AR) $(LIBDIR)/$(LIBFILE) $(OFILES)

$(OBJDIR)/%.o $(MODDIR)/%.mod: %.F90
	@$(CHK_DIR_EXISTS) $(OBJDIR) || $(MKDIR) $(OBJDIR)
	@$(CHK_DIR_EXISTS) $(MODDIR) || $(MKDIR) $(MODDIR)
	$(DEL_FILE) $(OBJDIR)/$*.o
	$(DEL_FILE) $(MODDIR)/$*.mod	
	$(F90) $(PPDEFS) $(F90FLAGS) $(INCFLAGS) -c $*.F90 -o $(OBJDIR)/$*.o $(MODFLAGS)

clean:
	@for i in $(SUBGROUPS); do make clean -C $$i; done
	@$(DEL_FILE) $(TMPFILES)
	@$(DEL_FILE) $(OFILES)
	@$(DEL_FILE) $(MODFILES)
	@$(DEL_FILE) $(LIBDIR)/$(LIBFILE)
	@$(DEL_FILE) *.fpptmp

######### dependencies #########
ifeq ($(MAKECMDGOALS),$(LIBFILE))
-include $(FILES:%=$(DEPSDIR)/%.deps)
endif

ifeq ($(DEPS_BUILD),YES)
$(DEPSDIR)/%.deps: %.F90 
	@$(CHK_DIR_EXISTS) $(DEPSDIR) || $(MKDIR) $(DEPSDIR)
	@echo "auto-create dependencies for $*.F90"
	-@$(FPP2TMP)
	@$(PYTHON) $(HOMEDIR)/src/f90_mod_deps.py \
       --output $(DEPSDIR)/$*.deps --prefix '$$(OBJDIR)/' \
       --dep-static '$$(HOMEDIR)/src/pario_defines.inc' \
       --dep-re '(pario_.*)' --dep-template '$$(MODDIR)/\1.mod' \
       --mod-re '(pario_.*)' --mod-template '$$(MODDIR)/\1.mod' $*.fpptmp
	@$(DEL_FILE) $*.fpptmp
endif
