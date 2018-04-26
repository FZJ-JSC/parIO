include ./Makefile.in

SHELL = /bin/sh

# Files
LIBFILE  := libpario.a
FILES    := pario_consts \
           pario_data \
           pario_funcs \
           pario_dset2dset \
           pario_gdat \
           pario_gval \
           pario_hysl2dset \
           pario_hysl2hysl \
           pario_pdat \
           pario_pval \
           pario_gprop \
           pario
F90FILES := $(FILES:%=%.F90)
OFILES   := $(FILES:%=$(OBJDIR)/%.o)
MODFILES := $(FILES:%=$(MODDIR)/%.mod)

OFILES   := $(OBJDIR)/pario_c.o $(OFILES)

# .PHONY: rules shall fire, even target is up to date

# build targets
.PHONY: all
all: release

.PHONY: release
release:
	@$(MAKE) "F90FLAGS = $(F90FLAGS_RELEASE)" "TGT_NAME = release" setup_lib

.PHONY: debug
debug:
	@$(MAKE) "F90FLAGS = $(F90FLAGS_DEBUG)" "TGT_NAME = debug" setup_lib

.PHONY: setup_lib
setup_lib: Makefile Makefile.in
	@echo "======= BUILD $(TGT_NAME) ======="
	@$(MAKE) "CODE   = $(CODE)_$(TGT_NAME)"    \
		     "OBJDIR = $(OBJDIR)/$(TGT_NAME)_" \
	         "LIBDIR = $(LIBDIR)/$(TGT_NAME)_" \
	         "MODDIR = $(MODDIR)/$(TGT_NAME)_" \
	         "DEPSDIR= $(DEPSDIR)/$(TGT_NAME)_" build_lib

.PHONY: build_lib
build_lib: interf
	@$(MAKE) $(OFILES)
	@echo "======= PACK TO LIB $(TGT_NAME) ======="
	@$(CHK_DIR_EXISTS) $(LIBDIR) || $(MKDIR) $(LIBDIR)
	$(AR) $(LIBDIR)/$(LIBFILE) $(OFILES)
	@echo "======= MOVE TO lib/include ======="
	$(COPY_FILE) $(LIBDIR)/$(LIBFILE) $(LIBDIR)/..
	$(COPY_FILE) $(MODDIR)/*.mod $(MODDIR)/..

.PHONY: interf
interf:
	@$(MAKE) -C interf all

$(OBJDIR)/%.o $(MODDIR)/%.mod: %.F90
	@echo "======= COMPILE FORTRAN FILE $(*.F90) with '$(F90FLAGS)' ======="
	@$(CHK_DIR_EXISTS) $(OBJDIR) || $(MKDIR) $(OBJDIR)
	@$(CHK_DIR_EXISTS) $(MODDIR) || $(MKDIR) $(MODDIR)
	$(F90) $(PPDEFS) $(F90FLAGS) $(INCFLAGS) -c $*.F90 -o $(OBJDIR)/$*.o $(MODFLAGS)

$(OBJDIR)/%.o: %.c
	@echo "======= COMPILE C-WRAPPER FILE $(*.c)  ======="
	@$(CHK_DIR_EXISTS) $(OBJDIR) || $(MKDIR) $(OBJDIR)
	$(CC) $(INCFLAGS) -c $*.c -o $(OBJDIR)/$*.o

# clean targets

.PHONY: clean
clean: clean_debug clean_release

.PHONY: clean_release
clean_release:
	@$(MAKE) "F90FLAGS = $(F90FLAGS_RELEASE)" "TGT_NAME = release" clean_setup

.PHONY: clean_debug
clean_debug:
	@$(MAKE) "F90FLAGS = $(F90FLAGS_DEBUG)" "TGT_NAME = debug" clean_setup

.PHONY: clean_setup
clean_setup: Makefile Makefile.in
	@echo "======= CLEAN $(TGT_NAME) ======="
	@$(MAKE) "OBJDIR = $(OBJDIR)/$(TGT_NAME)_" \
	         "LIBDIR = $(LIBDIR)/$(TGT_NAME)_" \
	         "MODDIR = $(MODDIR)/$(TGT_NAME)_" clean_lib
	      
.PHONY: clean_lib
clean_lib: 
	@$(MAKE) -C interf clean
	$(DEL_FILE) $(TMPFILES)
	$(DEL_DIR) $(OBJDIR)
	$(DEL_DIR) $(LIBDIR)
	$(DEL_DIR) $(MODDIR)

.PHONY: distclean
distclean: clean
	$(DEL_DIR) ../doc/doxygen
	$(DEL_DIR) $(OBJDIR)
	$(DEL_DIR) $(LIBDIR)
	$(DEL_DIR) $(MODDIR)

.PHONY: doxygen
doxygen:
	cd $(HOMEDIR)/doc/;	$(DOXYGEN) doxyfile

# dependencies
$(OBJDIR)/pario.o:  \
            $(INTERFDIR)/dset2dset/dset2dset_1d_real4.F90 \
            $(INTERFDIR)/dset2dset/dset2dset_2d_real4.F90 \
            $(INTERFDIR)/dset2dset/dset2dset_3d_real4.F90 \
            \
            $(INTERFDIR)/dset2dset/dset2dset_1d_real8.F90 \
            $(INTERFDIR)/dset2dset/dset2dset_2d_real8.F90 \
            $(INTERFDIR)/dset2dset/dset2dset_3d_real8.F90 \
            \
            $(INTERFDIR)/dset2dset/dset2dset_1d_integer4.F90 \
            $(INTERFDIR)/dset2dset/dset2dset_2d_integer4.F90 \
            $(INTERFDIR)/dset2dset/dset2dset_3d_integer4.F90 \
            \
            \
            $(INTERFDIR)/hysl2dset/hysl2dset_1d_real4.F90 \
            $(INTERFDIR)/hysl2dset/hysl2dset_2d_real4.F90 \
            $(INTERFDIR)/hysl2dset/hysl2dset_3d_real4.F90 \
            \
            $(INTERFDIR)/hysl2dset/hysl2dset_1d_real8.F90 \
            $(INTERFDIR)/hysl2dset/hysl2dset_2d_real8.F90 \
            $(INTERFDIR)/hysl2dset/hysl2dset_3d_real8.F90 \
            \
            $(INTERFDIR)/hysl2dset/hysl2dset_1d_integer4.F90 \
            $(INTERFDIR)/hysl2dset/hysl2dset_2d_integer4.F90 \
            $(INTERFDIR)/hysl2dset/hysl2dset_3d_integer4.F90 \
            \
            \
            $(INTERFDIR)/hysl2hysl/hysl2hysl_1d_real4.F90 \
            $(INTERFDIR)/hysl2hysl/hysl2hysl_2d_real4.F90 \
            $(INTERFDIR)/hysl2hysl/hysl2hysl_3d_real4.F90 \
            \
            $(INTERFDIR)/hysl2hysl/hysl2hysl_1d_real8.F90 \
            $(INTERFDIR)/hysl2hysl/hysl2hysl_2d_real8.F90 \
            $(INTERFDIR)/hysl2hysl/hysl2hysl_3d_real8.F90 \
            \
            $(INTERFDIR)/hysl2hysl/hysl2hysl_1d_integer4.F90 \
            $(INTERFDIR)/hysl2hysl/hysl2hysl_2d_integer4.F90 \
            $(INTERFDIR)/hysl2hysl/hysl2hysl_3d_integer4.F90 \
            \
            \
            $(INTERFDIR)/gdat/gdat_1d_real4.F90 \
            $(INTERFDIR)/gdat/gdat_2d_real4.F90 \
            $(INTERFDIR)/gdat/gdat_3d_real4.F90 \
            \
            $(INTERFDIR)/gdat/gdat_1d_real8.F90 \
            $(INTERFDIR)/gdat/gdat_2d_real8.F90 \
            $(INTERFDIR)/gdat/gdat_3d_real8.F90 \
            \
            $(INTERFDIR)/gdat/gdat_1d_integer4.F90  \
            $(INTERFDIR)/gdat/gdat_2d_integer4.F90  \
            $(INTERFDIR)/gdat/gdat_3d_integer4.F90  \
            \
            \
            $(INTERFDIR)/gval/gval_1d_real4.F90 \
            $(INTERFDIR)/gval/gval_2d_real4.F90 \
            $(INTERFDIR)/gval/gval_3d_real4.F90 \
            \
            $(INTERFDIR)/gval/gval_1d_real8.F90 \
            $(INTERFDIR)/gval/gval_2d_real8.F90 \
            $(INTERFDIR)/gval/gval_3d_real8.F90 \
            \
            $(INTERFDIR)/gval/gval_1d_integer4.F90  \
            $(INTERFDIR)/gval/gval_2d_integer4.F90  \
            $(INTERFDIR)/gval/gval_3d_integer4.F90  \
            \
            $(INTERFDIR)/gval/gval_1d_complex4.F03 \
            $(INTERFDIR)/gval/gval_2d_complex4.F03 \
            $(INTERFDIR)/gval/gval_3d_complex4.F03 \
            \
            $(INTERFDIR)/gval/gval_1d_complex8.F03 \
            $(INTERFDIR)/gval/gval_2d_complex8.F03 \
            $(INTERFDIR)/gval/gval_3d_complex8.F03 \
            \
            \
            $(INTERFDIR)/pdat/pdat_1d_real4.F90 \
            $(INTERFDIR)/pdat/pdat_2d_real4.F90 \
            $(INTERFDIR)/pdat/pdat_3d_real4.F90 \
            \
            $(INTERFDIR)/pdat/pdat_1d_real8.F90 \
            $(INTERFDIR)/pdat/pdat_2d_real8.F90 \
            $(INTERFDIR)/pdat/pdat_3d_real8.F90 \
            \
            $(INTERFDIR)/pdat/pdat_1d_integer4.F90  \
            $(INTERFDIR)/pdat/pdat_2d_integer4.F90  \
            $(INTERFDIR)/pdat/pdat_3d_integer4.F90  \
            \
            \
            $(INTERFDIR)/pval/pval_1d_real4.F90 \
            $(INTERFDIR)/pval/pval_2d_real4.F90 \
            $(INTERFDIR)/pval/pval_3d_real4.F90 \
            \
            $(INTERFDIR)/pval/pval_1d_real8.F90 \
            $(INTERFDIR)/pval/pval_2d_real8.F90 \
            $(INTERFDIR)/pval/pval_3d_real8.F90 \
            \
            $(INTERFDIR)/pval/pval_1d_integer4.F90  \
            $(INTERFDIR)/pval/pval_2d_integer4.F90  \
            $(INTERFDIR)/pval/pval_3d_integer4.F90  \
            \
            $(INTERFDIR)/pval/pval_1d_complex4.F03 \
            $(INTERFDIR)/pval/pval_2d_complex4.F03 \
            $(INTERFDIR)/pval/pval_3d_complex4.F03 \
            \
            $(INTERFDIR)/pval/pval_1d_complex8.F03 \
            $(INTERFDIR)/pval/pval_2d_complex8.F03 \
            $(INTERFDIR)/pval/pval_3d_complex8.F03
