include ./Makefile.in

SHELL = /bin/sh

# Files
CODE     = testio
FILES    = testio
F90FILES = $(FILES:%=%.F90)
OFILES   = $(FILES:%=$(OBJDIR)/%.o)
MODFILES = $(FILES:%=$(MODDIR)/%.mod)

LIBRARIES_COMPILE = io
LIBRARIES_LINK    = io

# .PHONY: rules shall fire, even target is up to date

# build targets
.PHONY: all
all: release 

.PHONY: release
release:
	@$(MAKE) "F90FLAGS = $(F90FLAGS_RELEASE)" \
	      "TGT_NAME = release" build_tgt

.PHONY: debug
debug:
	@$(MAKE) "F90FLAGS = $(F90FLAGS_DEBUG)" \
	      "TGT_NAME = debug" build_tgt

.PHONY: profile
profile:
	@$(MAKE) "F90FLAGS = $(F90FLAGS_PROFILE)" \
	      "TGT_NAME = profile" build_tgt

.PHONY: build_tgt
build_tgt: Makefile Makefile.in
	@echo "======= BUILD $(TGT_NAME) ======="
	@$(MAKE) "CODE   = $(CODE)_$(TGT_NAME)"    \
		     "OBJDIR = $(OBJDIR)/$(TGT_NAME)_" \
	         "LIBDIR = $(LIBDIR)/$(TGT_NAME)_" \
	         "MODDIR = $(MODDIR)/$(TGT_NAME)_" \
	         "DEPSDIR= $(DEPSDIR)/$(TGT_NAME)_" bin

.PHONY: bin
bin: $(OBJDIR)/testio_data.o libs
	@$(MAKE) $(OFILES)
	@echo "======= LINK $(TGT_NAME) ======="
	$(LD) $(OFILES) $(LIBRARIES_LINK:%=$(LIBDIR)/lib_%.a) $(OBJDIR)/testio_data.o $(LDFLAGS) -o $(CODE)  

.PHONY: libs 
libs:
	@for dir in $(LIBRARIES_COMPILE); do (cd $$dir; $(MAKE) all); done
		
$(OBJDIR)/%.o $(MODDIR)/%.mod: %.F90
	@$(CHK_DIR_EXISTS) $(OBJDIR) || $(MKDIR) $(OBJDIR)
	@$(CHK_DIR_EXISTS) $(MODDIR) || $(MKDIR) $(MODDIR)
	@$(DEL_FILE) $(OBJDIR)/$*.o
	@$(DEL_FILE) $(MODDIR)/$*.mod
	$(F90) $(PPDEFS) $(F90FLAGS) $(INCFLAGS) -c $*.F90 -o $(OBJDIR)/$*.o $(MODFLAGS)
	
# clean targets
.PHONY: clean
clean:
	@$(MAKE) clean_debug
	@$(MAKE) clean_release
	@$(MAKE) clean_profile

.PHONY: clean_release
clean_release:
	@$(MAKE) "F90FLAGS = $(F90FLAGS_RELEASE)" \
	      "TGT_NAME = release" clean_tgt

.PHONY: clean_debug
clean_debug:
	@$(MAKE) "F90FLAGS = $(F90FLAGS_DEBUG)" \
	      "TGT_NAME = debug" clean_tgt

.PHONY: clean_profile
clean_profile:
	@$(MAKE) "F90FLAGS = $(F90FLAGS_PROFILE)" \
	      "TGT_NAME = profile" clean_tgt

.PHONY: clean_tgt
clean_tgt: Makefile Makefile.in
	@echo "======= CLEAN $(TGT_NAME) ======="
	@$(MAKE) "OBJDIR = $(OBJDIR)/$(TGT_NAME)_" \
	      "LIBDIR = $(LIBDIR)/$(TGT_NAME)_" \
	      "MODDIR = $(MODDIR)/$(TGT_NAME)_" clean_up

.PHONY: clean_up
clean_up: 
	@for i in $(LIBRARIES_COMPILE); do make -C $$i clean; done
	@$(DEL_FILE) $(TMPFILES)
	@$(DEL_FILE) $(OFILES)
	@$(DEL_FILE) $(MODFILES)
	@$(DEL_FILE) *.fpptmp
	      
.PHONY: distclean
distclean: clean
	$(DEL_DIR) ../doc/doxygen
	$(DEL_DIR) $(OBJDIR)
	$(DEL_DIR) $(LIBDIR)
	$(DEL_DIR) $(MODDIR)
	$(DEL_DIR) $(DEPSDIR)
	$(DEL_FILE) $(CODE)_debug $(CODE)_release
