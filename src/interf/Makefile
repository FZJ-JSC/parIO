first: all
include ../Makefile.in

TEMPL    =  \
            $(INTERFDIR)/dset2dset/dset2dset_1d_real4.F90 \
            $(INTERFDIR)/dset2dset/dset2dset_2d_real4.F90 \
            $(INTERFDIR)/dset2dset/dset2dset_3d_real4.F90 \
            $(INTERFDIR)/dset2dset/dset2dset_4d_real4.F90 \
            \
            $(INTERFDIR)/dset2dset/dset2dset_1d_real8.F90 \
            $(INTERFDIR)/dset2dset/dset2dset_2d_real8.F90 \
            $(INTERFDIR)/dset2dset/dset2dset_3d_real8.F90 \
            $(INTERFDIR)/dset2dset/dset2dset_4d_real8.F90 \
            \
            $(INTERFDIR)/dset2dset/dset2dset_1d_integer4.F90 \
            $(INTERFDIR)/dset2dset/dset2dset_2d_integer4.F90 \
            $(INTERFDIR)/dset2dset/dset2dset_3d_integer4.F90 \
            $(INTERFDIR)/dset2dset/dset2dset_4d_integer4.F90 \
            \
            \
            $(INTERFDIR)/hysl2dset/hysl2dset_1d_real4.F90 \
            $(INTERFDIR)/hysl2dset/hysl2dset_2d_real4.F90 \
            $(INTERFDIR)/hysl2dset/hysl2dset_3d_real4.F90 \
            $(INTERFDIR)/hysl2dset/hysl2dset_4d_real4.F90 \
            \
            $(INTERFDIR)/hysl2dset/hysl2dset_1d_real8.F90 \
            $(INTERFDIR)/hysl2dset/hysl2dset_2d_real8.F90 \
            $(INTERFDIR)/hysl2dset/hysl2dset_3d_real8.F90 \
            $(INTERFDIR)/hysl2dset/hysl2dset_4d_real8.F90 \
            \
            $(INTERFDIR)/hysl2dset/hysl2dset_1d_integer4.F90 \
            $(INTERFDIR)/hysl2dset/hysl2dset_2d_integer4.F90 \
            $(INTERFDIR)/hysl2dset/hysl2dset_3d_integer4.F90 \
            $(INTERFDIR)/hysl2dset/hysl2dset_4d_integer4.F90 \
            \
            \
            $(INTERFDIR)/hysl2hysl/hysl2hysl_1d_real4.F90 \
            $(INTERFDIR)/hysl2hysl/hysl2hysl_2d_real4.F90 \
            $(INTERFDIR)/hysl2hysl/hysl2hysl_3d_real4.F90 \
            $(INTERFDIR)/hysl2hysl/hysl2hysl_4d_real4.F90 \
            \
            $(INTERFDIR)/hysl2hysl/hysl2hysl_1d_real8.F90 \
            $(INTERFDIR)/hysl2hysl/hysl2hysl_2d_real8.F90 \
            $(INTERFDIR)/hysl2hysl/hysl2hysl_3d_real8.F90 \
            $(INTERFDIR)/hysl2hysl/hysl2hysl_4d_real8.F90 \
            \
            $(INTERFDIR)/hysl2hysl/hysl2hysl_1d_integer4.F90 \
            $(INTERFDIR)/hysl2hysl/hysl2hysl_2d_integer4.F90 \
            $(INTERFDIR)/hysl2hysl/hysl2hysl_3d_integer4.F90 \
            $(INTERFDIR)/hysl2hysl/hysl2hysl_4d_integer4.F90 \
            \
            \
            $(INTERFDIR)/gdat/gdat_1d_real4.F90 \
            $(INTERFDIR)/gdat/gdat_2d_real4.F90 \
            $(INTERFDIR)/gdat/gdat_3d_real4.F90 \
            $(INTERFDIR)/gdat/gdat_4d_real4.F90 \
            \
            $(INTERFDIR)/gdat/gdat_1d_real8.F90 \
            $(INTERFDIR)/gdat/gdat_2d_real8.F90 \
            $(INTERFDIR)/gdat/gdat_3d_real8.F90 \
            $(INTERFDIR)/gdat/gdat_4d_real8.F90 \
            \
            $(INTERFDIR)/gdat/gdat_1d_integer4.F90  \
            $(INTERFDIR)/gdat/gdat_2d_integer4.F90  \
            $(INTERFDIR)/gdat/gdat_3d_integer4.F90  \
            $(INTERFDIR)/gdat/gdat_4d_integer4.F90  \
            \
            \
            $(INTERFDIR)/gval/gval_1d_real4.F90 \
            $(INTERFDIR)/gval/gval_2d_real4.F90 \
            $(INTERFDIR)/gval/gval_3d_real4.F90 \
            $(INTERFDIR)/gval/gval_4d_real4.F90 \
            \
            $(INTERFDIR)/gval/gval_1d_real8.F90 \
            $(INTERFDIR)/gval/gval_2d_real8.F90 \
            $(INTERFDIR)/gval/gval_3d_real8.F90 \
            $(INTERFDIR)/gval/gval_4d_real8.F90 \
            \
            $(INTERFDIR)/gval/gval_1d_integer4.F90  \
            $(INTERFDIR)/gval/gval_2d_integer4.F90  \
            $(INTERFDIR)/gval/gval_3d_integer4.F90  \
            $(INTERFDIR)/gval/gval_4d_integer4.F90  \
            \
            $(INTERFDIR)/gval/gval_1d_complex4.F03 \
            $(INTERFDIR)/gval/gval_2d_complex4.F03 \
            $(INTERFDIR)/gval/gval_3d_complex4.F03 \
            $(INTERFDIR)/gval/gval_4d_complex4.F03 \
            \
            $(INTERFDIR)/gval/gval_1d_complex8.F03 \
            $(INTERFDIR)/gval/gval_2d_complex8.F03 \
            $(INTERFDIR)/gval/gval_3d_complex8.F03 \
            $(INTERFDIR)/gval/gval_4d_complex8.F03 \
            \
            \
            $(INTERFDIR)/pdat/pdat_1d_real4.F90 \
            $(INTERFDIR)/pdat/pdat_2d_real4.F90 \
            $(INTERFDIR)/pdat/pdat_3d_real4.F90 \
            $(INTERFDIR)/pdat/pdat_4d_real4.F90 \
            \
            $(INTERFDIR)/pdat/pdat_1d_real8.F90 \
            $(INTERFDIR)/pdat/pdat_2d_real8.F90 \
            $(INTERFDIR)/pdat/pdat_3d_real8.F90 \
            $(INTERFDIR)/pdat/pdat_4d_real8.F90 \
            \
            $(INTERFDIR)/pdat/pdat_1d_integer4.F90  \
            $(INTERFDIR)/pdat/pdat_2d_integer4.F90  \
            $(INTERFDIR)/pdat/pdat_3d_integer4.F90  \
            $(INTERFDIR)/pdat/pdat_4d_integer4.F90  \
            \
            $(INTERFDIR)/pval/pval_1d_real4.F90 \
            $(INTERFDIR)/pval/pval_2d_real4.F90 \
            $(INTERFDIR)/pval/pval_3d_real4.F90 \
            $(INTERFDIR)/pval/pval_4d_real4.F90 \
            \
            $(INTERFDIR)/pval/pval_1d_real8.F90 \
            $(INTERFDIR)/pval/pval_2d_real8.F90 \
            $(INTERFDIR)/pval/pval_3d_real8.F90 \
            $(INTERFDIR)/pval/pval_4d_real8.F90 \
            \
            $(INTERFDIR)/pval/pval_1d_integer4.F90  \
            $(INTERFDIR)/pval/pval_2d_integer4.F90  \
            $(INTERFDIR)/pval/pval_3d_integer4.F90  \
            $(INTERFDIR)/pval/pval_4d_integer4.F90  \
            \
            $(INTERFDIR)/pval/pval_1d_complex4.F03 \
            $(INTERFDIR)/pval/pval_2d_complex4.F03 \
            $(INTERFDIR)/pval/pval_3d_complex4.F03 \
            $(INTERFDIR)/pval/pval_4d_complex4.F03 \
            \
            $(INTERFDIR)/pval/pval_1d_complex8.F03 \
            $(INTERFDIR)/pval/pval_2d_complex8.F03 \
            $(INTERFDIR)/pval/pval_3d_complex8.F03 \
            $(INTERFDIR)/pval/pval_4d_complex8.F03
            
.PHONY: all
all: Makefile ../Makefile.in $(TEMPL)

clean:
	@$(DEL_DIR) $(INTERFDIR)/pval
	@$(DEL_DIR) $(INTERFDIR)/pdat
	@$(DEL_DIR) $(INTERFDIR)/gval
	@$(DEL_DIR) $(INTERFDIR)/gdat
	@$(DEL_DIR) $(INTERFDIR)/dset2dset
	@$(DEL_DIR) $(INTERFDIR)/hysl2dset
	@$(DEL_DIR) $(INTERFDIR)/hysl2hysl

#################################
#                               #
#   interf_dset2dset.templ  #
#                               #
#################################
##### real4 #####
$(INTERFDIR)/dset2dset/dset2dset_1d_real4.F90: Makefile ../Makefile.in Makefile ../Makefile.in interf_dset2dset.templ
	@$(CHK_DIR_EXISTS) $(INTERFDIR)/dset2dset || $(MKDIR) $(INTERFDIR)/dset2dset
	$(SED) -e 's/\[_IO_MEMVAR_WHAT_\]/_IO_MEMVAR_isREAL_/g' \
	     -e 's/\[_IO_MEMVAR_TYPE_\]/real/g'                 \
	     -e 's/\[_IO_MEMVAR_KIND_\]/4/g'                    \
	     -e 's/\[_IO_MEMVAR_DIMS_\]/1/g'                    \
	     -e 's/\[_IO_MEMVAR_DDOT_\]/:/g'                    \
	     -e 's/\[_IO_MEMVAR_DTXT_\]/mem_dims\(1\)/g' < interf_dset2dset.templ > $(INTERFDIR)/dset2dset/dset2dset_1d_real4.F90

$(INTERFDIR)/dset2dset/dset2dset_2d_real4.F90: Makefile ../Makefile.in interf_dset2dset.templ
	@$(CHK_DIR_EXISTS) $(INTERFDIR)/dset2dset || $(MKDIR) $(INTERFDIR)/dset2dset
	$(SED) -e 's/\[_IO_MEMVAR_WHAT_\]/_IO_MEMVAR_isREAL_/g' \
	     -e 's/\[_IO_MEMVAR_TYPE_\]/real/g'                 \
	     -e 's/\[_IO_MEMVAR_KIND_\]/4/g'                    \
	     -e 's/\[_IO_MEMVAR_DIMS_\]/2/g'                    \
	     -e 's/\[_IO_MEMVAR_DDOT_\]/:,:/g'                  \
	     -e 's/\[_IO_MEMVAR_DTXT_\]/mem_dims\(1\),mem_dims\(2\)/g' < interf_dset2dset.templ > $(INTERFDIR)/dset2dset/dset2dset_2d_real4.F90

$(INTERFDIR)/dset2dset/dset2dset_3d_real4.F90: Makefile ../Makefile.in interf_dset2dset.templ
	@$(CHK_DIR_EXISTS) $(INTERFDIR)/dset2dset || $(MKDIR) $(INTERFDIR)/dset2dset
	$(SED) -e 's/\[_IO_MEMVAR_WHAT_\]/_IO_MEMVAR_isREAL_/g' \
	     -e 's/\[_IO_MEMVAR_TYPE_\]/real/g'                 \
	     -e 's/\[_IO_MEMVAR_KIND_\]/4/g'                    \
	     -e 's/\[_IO_MEMVAR_DIMS_\]/3/g'                    \
	     -e 's/\[_IO_MEMVAR_DDOT_\]/:,:,:/g'                \
	     -e 's/\[_IO_MEMVAR_DTXT_\]/mem_dims\(1\),mem_dims\(2\),mem_dims\(3\)/g' < interf_dset2dset.templ > $(INTERFDIR)/dset2dset/dset2dset_3d_real4.F90

$(INTERFDIR)/dset2dset/dset2dset_4d_real4.F90: Makefile ../Makefile.in interf_dset2dset.templ
	@$(CHK_DIR_EXISTS) $(INTERFDIR)/dset2dset || $(MKDIR) $(INTERFDIR)/dset2dset
	$(SED) -e 's/\[_IO_MEMVAR_WHAT_\]/_IO_MEMVAR_isREAL_/g' \
	     -e 's/\[_IO_MEMVAR_TYPE_\]/real/g'                 \
	     -e 's/\[_IO_MEMVAR_KIND_\]/4/g'                    \
	     -e 's/\[_IO_MEMVAR_DIMS_\]/4/g'                    \
	     -e 's/\[_IO_MEMVAR_DDOT_\]/:,:,:,:/g'                \
	     -e 's/\[_IO_MEMVAR_DTXT_\]/mem_dims\(1\),mem_dims\(2\),mem_dims\(3\),mem_dims\(4\)/g' < interf_dset2dset.templ > $(INTERFDIR)/dset2dset/dset2dset_4d_real4.F90
##### real8 #####
$(INTERFDIR)/dset2dset/dset2dset_1d_real8.F90: Makefile ../Makefile.in interf_dset2dset.templ
	@$(CHK_DIR_EXISTS) $(INTERFDIR)/dset2dset || $(MKDIR) $(INTERFDIR)/dset2dset
	$(SED) -e 's/\[_IO_MEMVAR_WHAT_\]/_IO_MEMVAR_isREAL_/g' \
	     -e 's/\[_IO_MEMVAR_TYPE_\]/real/g'                 \
	     -e 's/\[_IO_MEMVAR_KIND_\]/8/g'                    \
	     -e 's/\[_IO_MEMVAR_DIMS_\]/1/g'                    \
	     -e 's/\[_IO_MEMVAR_DDOT_\]/:/g'                    \
	     -e 's/\[_IO_MEMVAR_DTXT_\]/mem_dims\(1\)/g' < interf_dset2dset.templ > $(INTERFDIR)/dset2dset/dset2dset_1d_real8.F90

$(INTERFDIR)/dset2dset/dset2dset_2d_real8.F90: Makefile ../Makefile.in interf_dset2dset.templ
	@$(CHK_DIR_EXISTS) $(INTERFDIR)/dset2dset || $(MKDIR) $(INTERFDIR)/dset2dset
	$(SED) -e 's/\[_IO_MEMVAR_WHAT_\]/_IO_MEMVAR_isREAL_/g' \
	     -e 's/\[_IO_MEMVAR_TYPE_\]/real/g'                 \
	     -e 's/\[_IO_MEMVAR_KIND_\]/8/g'                    \
	     -e 's/\[_IO_MEMVAR_DIMS_\]/2/g'                    \
	     -e 's/\[_IO_MEMVAR_DDOT_\]/:,:/g'                  \
	     -e 's/\[_IO_MEMVAR_DTXT_\]/mem_dims\(1\),mem_dims\(2\)/g' < interf_dset2dset.templ > $(INTERFDIR)/dset2dset/dset2dset_2d_real8.F90

$(INTERFDIR)/dset2dset/dset2dset_3d_real8.F90: Makefile ../Makefile.in interf_dset2dset.templ
	@$(CHK_DIR_EXISTS) $(INTERFDIR)/dset2dset || $(MKDIR) $(INTERFDIR)/dset2dset
	$(SED) -e 's/\[_IO_MEMVAR_WHAT_\]/_IO_MEMVAR_isREAL_/g' \
	     -e 's/\[_IO_MEMVAR_TYPE_\]/real/g'                 \
	     -e 's/\[_IO_MEMVAR_KIND_\]/8/g'                    \
	     -e 's/\[_IO_MEMVAR_DIMS_\]/3/g'                    \
	     -e 's/\[_IO_MEMVAR_DDOT_\]/:,:,:/g'                \
	     -e 's/\[_IO_MEMVAR_DTXT_\]/mem_dims\(1\),mem_dims\(2\),mem_dims\(3\)/g' < interf_dset2dset.templ > $(INTERFDIR)/dset2dset/dset2dset_3d_real8.F90

$(INTERFDIR)/dset2dset/dset2dset_4d_real8.F90: Makefile ../Makefile.in interf_dset2dset.templ
	@$(CHK_DIR_EXISTS) $(INTERFDIR)/dset2dset || $(MKDIR) $(INTERFDIR)/dset2dset
	$(SED) -e 's/\[_IO_MEMVAR_WHAT_\]/_IO_MEMVAR_isREAL_/g' \
	     -e 's/\[_IO_MEMVAR_TYPE_\]/real/g'                 \
	     -e 's/\[_IO_MEMVAR_KIND_\]/8/g'                    \
	     -e 's/\[_IO_MEMVAR_DIMS_\]/4/g'                    \
	     -e 's/\[_IO_MEMVAR_DDOT_\]/:,:,:,:/g'                \
	     -e 's/\[_IO_MEMVAR_DTXT_\]/mem_dims\(1\),mem_dims\(2\),mem_dims\(3\),mem_dims\(4\)/g' < interf_dset2dset.templ > $(INTERFDIR)/dset2dset/dset2dset_4d_real8.F90
##### integer4  #####
$(INTERFDIR)/dset2dset/dset2dset_1d_integer4.F90: Makefile ../Makefile.in interf_dset2dset.templ
	@$(CHK_DIR_EXISTS) $(INTERFDIR)/dset2dset || $(MKDIR) $(INTERFDIR)/dset2dset
	$(SED) -e 's/\[_IO_MEMVAR_WHAT_\]/_IO_MEMVAR_isINTEGER_/g' \
	     -e 's/\[_IO_MEMVAR_TYPE_\]/integer/g'              \
	     -e 's/\[_IO_MEMVAR_KIND_\]/4/g'                    \
	     -e 's/\[_IO_MEMVAR_DIMS_\]/1/g'                    \
	     -e 's/\[_IO_MEMVAR_DDOT_\]/:/g'                    \
	     -e 's/\[_IO_MEMVAR_DTXT_\]/mem_dims\(1\)/g' < interf_dset2dset.templ > $(INTERFDIR)/dset2dset/dset2dset_1d_integer4.F90

$(INTERFDIR)/dset2dset/dset2dset_2d_integer4.F90: Makefile ../Makefile.in interf_dset2dset.templ
	@$(CHK_DIR_EXISTS) $(INTERFDIR)/dset2dset || $(MKDIR) $(INTERFDIR)/dset2dset
	$(SED) -e 's/\[_IO_MEMVAR_WHAT_\]/_IO_MEMVAR_isINTEGER_/g' \
	     -e 's/\[_IO_MEMVAR_TYPE_\]/integer/g'              \
	     -e 's/\[_IO_MEMVAR_KIND_\]/4/g'                    \
	     -e 's/\[_IO_MEMVAR_DIMS_\]/2/g'                    \
	     -e 's/\[_IO_MEMVAR_DDOT_\]/:,:/g'                  \
	     -e 's/\[_IO_MEMVAR_DTXT_\]/mem_dims\(1\),mem_dims\(2\)/g' < interf_dset2dset.templ > $(INTERFDIR)/dset2dset/dset2dset_2d_integer4.F90

$(INTERFDIR)/dset2dset/dset2dset_3d_integer4.F90: Makefile ../Makefile.in interf_dset2dset.templ
	@$(CHK_DIR_EXISTS) $(INTERFDIR)/dset2dset || $(MKDIR) $(INTERFDIR)/dset2dset
	$(SED) -e 's/\[_IO_MEMVAR_WHAT_\]/_IO_MEMVAR_isINTEGER_/g' \
	     -e 's/\[_IO_MEMVAR_TYPE_\]/integer/g'              \
	     -e 's/\[_IO_MEMVAR_KIND_\]/4/g'                    \
	     -e 's/\[_IO_MEMVAR_DIMS_\]/3/g'                    \
	     -e 's/\[_IO_MEMVAR_DDOT_\]/:,:,:/g'                \
	     -e 's/\[_IO_MEMVAR_DTXT_\]/mem_dims\(1\),mem_dims\(2\),mem_dims\(3\)/g' < interf_dset2dset.templ > $(INTERFDIR)/dset2dset/dset2dset_3d_integer4.F90

$(INTERFDIR)/dset2dset/dset2dset_4d_integer4.F90: Makefile ../Makefile.in interf_dset2dset.templ
	@$(CHK_DIR_EXISTS) $(INTERFDIR)/dset2dset || $(MKDIR) $(INTERFDIR)/dset2dset
	$(SED) -e 's/\[_IO_MEMVAR_WHAT_\]/_IO_MEMVAR_isINTEGER_/g' \
	     -e 's/\[_IO_MEMVAR_TYPE_\]/integer/g'              \
	     -e 's/\[_IO_MEMVAR_KIND_\]/4/g'                    \
	     -e 's/\[_IO_MEMVAR_DIMS_\]/4/g'                    \
	     -e 's/\[_IO_MEMVAR_DDOT_\]/:,:,:,:/g'                \
	     -e 's/\[_IO_MEMVAR_DTXT_\]/mem_dims\(1\),mem_dims\(2\),mem_dims\(3\),mem_dims\(4\)/g' < interf_dset2dset.templ > $(INTERFDIR)/dset2dset/dset2dset_4d_integer4.F90

#################################
#                               #
#   interf_hysl2dset.templ  #
#                               #
#################################
##### real4 #####
$(INTERFDIR)/hysl2dset/hysl2dset_1d_real4.F90: Makefile ../Makefile.in interf_hysl2dset.templ
	@$(CHK_DIR_EXISTS) $(INTERFDIR)/hysl2dset || $(MKDIR) $(INTERFDIR)/hysl2dset
	$(SED) -e 's/\[_IO_MEMVAR_WHAT_\]/_IO_MEMVAR_isREAL_/g' \
	     -e 's/\[_IO_MEMVAR_TYPE_\]/real/g'                 \
	     -e 's/\[_IO_MEMVAR_KIND_\]/4/g'                    \
	     -e 's/\[_IO_MEMVAR_DIMS_\]/1/g'                    \
	     -e 's/\[_IO_MEMVAR_DDOT_\]/:/g'                    \
	     -e 's/\[_IO_MEMVAR_DTXT_\]/mem_dims\(1\)/g' < interf_hysl2dset.templ > $(INTERFDIR)/hysl2dset/hysl2dset_1d_real4.F90

$(INTERFDIR)/hysl2dset/hysl2dset_2d_real4.F90: Makefile ../Makefile.in interf_hysl2dset.templ
	@$(CHK_DIR_EXISTS) $(INTERFDIR)/hysl2dset || $(MKDIR) $(INTERFDIR)/hysl2dset
	$(SED) -e 's/\[_IO_MEMVAR_WHAT_\]/_IO_MEMVAR_isREAL_/g' \
	     -e 's/\[_IO_MEMVAR_TYPE_\]/real/g'                 \
	     -e 's/\[_IO_MEMVAR_KIND_\]/4/g'                    \
	     -e 's/\[_IO_MEMVAR_DIMS_\]/2/g'                    \
	     -e 's/\[_IO_MEMVAR_DDOT_\]/:,:/g'                  \
	     -e 's/\[_IO_MEMVAR_DTXT_\]/mem_dims\(1\),mem_dims\(2\)/g' < interf_hysl2dset.templ > $(INTERFDIR)/hysl2dset/hysl2dset_2d_real4.F90

$(INTERFDIR)/hysl2dset/hysl2dset_3d_real4.F90: Makefile ../Makefile.in interf_hysl2dset.templ
	@$(CHK_DIR_EXISTS) $(INTERFDIR)/hysl2dset || $(MKDIR) $(INTERFDIR)/hysl2dset
	$(SED) -e 's/\[_IO_MEMVAR_WHAT_\]/_IO_MEMVAR_isREAL_/g' \
	     -e 's/\[_IO_MEMVAR_TYPE_\]/real/g'                 \
	     -e 's/\[_IO_MEMVAR_KIND_\]/4/g'                    \
	     -e 's/\[_IO_MEMVAR_DIMS_\]/3/g'                    \
	     -e 's/\[_IO_MEMVAR_DDOT_\]/:,:,:/g'                \
	     -e 's/\[_IO_MEMVAR_DTXT_\]/mem_dims\(1\),mem_dims\(2\),mem_dims\(3\)/g' < interf_hysl2dset.templ > $(INTERFDIR)/hysl2dset/hysl2dset_3d_real4.F90

$(INTERFDIR)/hysl2dset/hysl2dset_4d_real4.F90: Makefile ../Makefile.in interf_hysl2dset.templ
	@$(CHK_DIR_EXISTS) $(INTERFDIR)/hysl2dset || $(MKDIR) $(INTERFDIR)/hysl2dset
	$(SED) -e 's/\[_IO_MEMVAR_WHAT_\]/_IO_MEMVAR_isREAL_/g' \
	     -e 's/\[_IO_MEMVAR_TYPE_\]/real/g'                 \
	     -e 's/\[_IO_MEMVAR_KIND_\]/4/g'                    \
	     -e 's/\[_IO_MEMVAR_DIMS_\]/4/g'                    \
	     -e 's/\[_IO_MEMVAR_DDOT_\]/:,:,:,:/g'                \
	     -e 's/\[_IO_MEMVAR_DTXT_\]/mem_dims\(1\),mem_dims\(2\),mem_dims\(3\),mem_dims\(4\)/g' < interf_hysl2dset.templ > $(INTERFDIR)/hysl2dset/hysl2dset_4d_real4.F90
##### real8 #####
$(INTERFDIR)/hysl2dset/hysl2dset_1d_real8.F90: Makefile ../Makefile.in interf_hysl2dset.templ
	@$(CHK_DIR_EXISTS) $(INTERFDIR)/hysl2dset || $(MKDIR) $(INTERFDIR)/hysl2dset
	$(SED) -e 's/\[_IO_MEMVAR_WHAT_\]/_IO_MEMVAR_isREAL_/g' \
	     -e 's/\[_IO_MEMVAR_TYPE_\]/real/g'                 \
	     -e 's/\[_IO_MEMVAR_KIND_\]/8/g'                    \
	     -e 's/\[_IO_MEMVAR_DIMS_\]/1/g'                    \
	     -e 's/\[_IO_MEMVAR_DDOT_\]/:/g'                    \
	     -e 's/\[_IO_MEMVAR_DTXT_\]/mem_dims\(1\)/g' < interf_hysl2dset.templ > $(INTERFDIR)/hysl2dset/hysl2dset_1d_real8.F90

$(INTERFDIR)/hysl2dset/hysl2dset_2d_real8.F90: Makefile ../Makefile.in interf_hysl2dset.templ
	@$(CHK_DIR_EXISTS) $(INTERFDIR)/hysl2dset || $(MKDIR) $(INTERFDIR)/hysl2dset
	$(SED) -e 's/\[_IO_MEMVAR_WHAT_\]/_IO_MEMVAR_isREAL_/g' \
	     -e 's/\[_IO_MEMVAR_TYPE_\]/real/g'                 \
	     -e 's/\[_IO_MEMVAR_KIND_\]/8/g'                    \
	     -e 's/\[_IO_MEMVAR_DIMS_\]/2/g'                    \
	     -e 's/\[_IO_MEMVAR_DDOT_\]/:,:/g'                  \
	     -e 's/\[_IO_MEMVAR_DTXT_\]/mem_dims\(1\),mem_dims\(2\)/g' < interf_hysl2dset.templ > $(INTERFDIR)/hysl2dset/hysl2dset_2d_real8.F90

$(INTERFDIR)/hysl2dset/hysl2dset_3d_real8.F90: Makefile ../Makefile.in interf_hysl2dset.templ
	@$(CHK_DIR_EXISTS) $(INTERFDIR)/hysl2dset || $(MKDIR) $(INTERFDIR)/hysl2dset
	$(SED) -e 's/\[_IO_MEMVAR_WHAT_\]/_IO_MEMVAR_isREAL_/g' \
	     -e 's/\[_IO_MEMVAR_TYPE_\]/real/g'                 \
	     -e 's/\[_IO_MEMVAR_KIND_\]/8/g'                    \
	     -e 's/\[_IO_MEMVAR_DIMS_\]/3/g'                    \
	     -e 's/\[_IO_MEMVAR_DDOT_\]/:,:,:/g'                \
	     -e 's/\[_IO_MEMVAR_DTXT_\]/mem_dims\(1\),mem_dims\(2\),mem_dims\(3\)/g' < interf_hysl2dset.templ > $(INTERFDIR)/hysl2dset/hysl2dset_3d_real8.F90

$(INTERFDIR)/hysl2dset/hysl2dset_4d_real8.F90: Makefile ../Makefile.in interf_hysl2dset.templ
	@$(CHK_DIR_EXISTS) $(INTERFDIR)/hysl2dset || $(MKDIR) $(INTERFDIR)/hysl2dset
	$(SED) -e 's/\[_IO_MEMVAR_WHAT_\]/_IO_MEMVAR_isREAL_/g' \
	     -e 's/\[_IO_MEMVAR_TYPE_\]/real/g'                 \
	     -e 's/\[_IO_MEMVAR_KIND_\]/8/g'                    \
	     -e 's/\[_IO_MEMVAR_DIMS_\]/4/g'                    \
	     -e 's/\[_IO_MEMVAR_DDOT_\]/:,:,:,:/g'                \
	     -e 's/\[_IO_MEMVAR_DTXT_\]/mem_dims\(1\),mem_dims\(2\),mem_dims\(3\),mem_dims\(4\)/g' < interf_hysl2dset.templ > $(INTERFDIR)/hysl2dset/hysl2dset_4d_real8.F90
##### integer4  #####
$(INTERFDIR)/hysl2dset/hysl2dset_1d_integer4.F90: Makefile ../Makefile.in interf_hysl2dset.templ
	@$(CHK_DIR_EXISTS) $(INTERFDIR)/hysl2dset || $(MKDIR) $(INTERFDIR)/hysl2dset
	$(SED) -e 's/\[_IO_MEMVAR_WHAT_\]/_IO_MEMVAR_isINTEGER_/g' \
	     -e 's/\[_IO_MEMVAR_TYPE_\]/integer/g'              \
	     -e 's/\[_IO_MEMVAR_KIND_\]/4/g'                    \
	     -e 's/\[_IO_MEMVAR_DIMS_\]/1/g'                    \
	     -e 's/\[_IO_MEMVAR_DDOT_\]/:/g'                    \
	     -e 's/\[_IO_MEMVAR_DTXT_\]/mem_dims\(1\)/g' < interf_hysl2dset.templ > $(INTERFDIR)/hysl2dset/hysl2dset_1d_integer4.F90

$(INTERFDIR)/hysl2dset/hysl2dset_2d_integer4.F90: Makefile ../Makefile.in interf_hysl2dset.templ
	@$(CHK_DIR_EXISTS) $(INTERFDIR)/hysl2dset || $(MKDIR) $(INTERFDIR)/hysl2dset
	$(SED) -e 's/\[_IO_MEMVAR_WHAT_\]/_IO_MEMVAR_isINTEGER_/g' \
	     -e 's/\[_IO_MEMVAR_TYPE_\]/integer/g'              \
	     -e 's/\[_IO_MEMVAR_KIND_\]/4/g'                    \
	     -e 's/\[_IO_MEMVAR_DIMS_\]/2/g'                    \
	     -e 's/\[_IO_MEMVAR_DDOT_\]/:,:/g'                  \
	     -e 's/\[_IO_MEMVAR_DTXT_\]/mem_dims\(1\),mem_dims\(2\)/g' < interf_hysl2dset.templ > $(INTERFDIR)/hysl2dset/hysl2dset_2d_integer4.F90

$(INTERFDIR)/hysl2dset/hysl2dset_3d_integer4.F90: Makefile ../Makefile.in interf_hysl2dset.templ
	@$(CHK_DIR_EXISTS) $(INTERFDIR)/hysl2dset || $(MKDIR) $(INTERFDIR)/hysl2dset
	$(SED) -e 's/\[_IO_MEMVAR_WHAT_\]/_IO_MEMVAR_isINTEGER_/g' \
	     -e 's/\[_IO_MEMVAR_TYPE_\]/integer/g'              \
	     -e 's/\[_IO_MEMVAR_KIND_\]/4/g'                    \
	     -e 's/\[_IO_MEMVAR_DIMS_\]/3/g'                    \
	     -e 's/\[_IO_MEMVAR_DDOT_\]/:,:,:/g'                \
	     -e 's/\[_IO_MEMVAR_DTXT_\]/mem_dims\(1\),mem_dims\(2\),mem_dims\(3\)/g' < interf_hysl2dset.templ > $(INTERFDIR)/hysl2dset/hysl2dset_3d_integer4.F90

$(INTERFDIR)/hysl2dset/hysl2dset_4d_integer4.F90: Makefile ../Makefile.in interf_hysl2dset.templ
	@$(CHK_DIR_EXISTS) $(INTERFDIR)/hysl2dset || $(MKDIR) $(INTERFDIR)/hysl2dset
	$(SED) -e 's/\[_IO_MEMVAR_WHAT_\]/_IO_MEMVAR_isINTEGER_/g' \
	     -e 's/\[_IO_MEMVAR_TYPE_\]/integer/g'              \
	     -e 's/\[_IO_MEMVAR_KIND_\]/4/g'                    \
	     -e 's/\[_IO_MEMVAR_DIMS_\]/4/g'                    \
	     -e 's/\[_IO_MEMVAR_DDOT_\]/:,:,:,:/g'                \
	     -e 's/\[_IO_MEMVAR_DTXT_\]/mem_dims\(1\),mem_dims\(2\),mem_dims\(3\),mem_dims\(4\)/g' < interf_hysl2dset.templ > $(INTERFDIR)/hysl2dset/hysl2dset_4d_integer4.F90

#################################
#                               #
#   interf_hysl2hysl.templ  #
#                               #
#################################
##### real4 #####
$(INTERFDIR)/hysl2hysl/hysl2hysl_1d_real4.F90: Makefile ../Makefile.in interf_hysl2hysl.templ
	@$(CHK_DIR_EXISTS) $(INTERFDIR)/hysl2hysl || $(MKDIR) $(INTERFDIR)/hysl2hysl
	$(SED) -e 's/\[_IO_MEMVAR_WHAT_\]/_IO_MEMVAR_isREAL_/g' \
	     -e 's/\[_IO_MEMVAR_TYPE_\]/real/g'                 \
	     -e 's/\[_IO_MEMVAR_KIND_\]/4/g'                    \
	     -e 's/\[_IO_MEMVAR_DIMS_\]/1/g'                    \
	     -e 's/\[_IO_MEMVAR_DDOT_\]/:/g'                    \
	     -e 's/\[_IO_MEMVAR_DTXT_\]/mem_dims\(1\)/g' < interf_hysl2hysl.templ > $(INTERFDIR)/hysl2hysl/hysl2hysl_1d_real4.F90

$(INTERFDIR)/hysl2hysl/hysl2hysl_2d_real4.F90: Makefile ../Makefile.in interf_hysl2hysl.templ
	@$(CHK_DIR_EXISTS) $(INTERFDIR)/hysl2hysl || $(MKDIR) $(INTERFDIR)/hysl2hysl
	$(SED) -e 's/\[_IO_MEMVAR_WHAT_\]/_IO_MEMVAR_isREAL_/g' \
	     -e 's/\[_IO_MEMVAR_TYPE_\]/real/g'                 \
	     -e 's/\[_IO_MEMVAR_KIND_\]/4/g'                    \
	     -e 's/\[_IO_MEMVAR_DIMS_\]/2/g'                    \
	     -e 's/\[_IO_MEMVAR_DDOT_\]/:,:/g'                  \
	     -e 's/\[_IO_MEMVAR_DTXT_\]/mem_dims\(1\),mem_dims\(2\)/g' < interf_hysl2hysl.templ > $(INTERFDIR)/hysl2hysl/hysl2hysl_2d_real4.F90

$(INTERFDIR)/hysl2hysl/hysl2hysl_3d_real4.F90: Makefile ../Makefile.in interf_hysl2hysl.templ
	@$(CHK_DIR_EXISTS) $(INTERFDIR)/hysl2hysl || $(MKDIR) $(INTERFDIR)/hysl2hysl
	$(SED) -e 's/\[_IO_MEMVAR_WHAT_\]/_IO_MEMVAR_isREAL_/g' \
	     -e 's/\[_IO_MEMVAR_TYPE_\]/real/g'                 \
	     -e 's/\[_IO_MEMVAR_KIND_\]/4/g'                    \
	     -e 's/\[_IO_MEMVAR_DIMS_\]/3/g'                    \
	     -e 's/\[_IO_MEMVAR_DDOT_\]/:,:,:/g'                \
	     -e 's/\[_IO_MEMVAR_DTXT_\]/mem_dims\(1\),mem_dims\(2\),mem_dims\(3\)/g' < interf_hysl2hysl.templ > $(INTERFDIR)/hysl2hysl/hysl2hysl_3d_real4.F90

$(INTERFDIR)/hysl2hysl/hysl2hysl_4d_real4.F90: Makefile ../Makefile.in interf_hysl2hysl.templ
	@$(CHK_DIR_EXISTS) $(INTERFDIR)/hysl2hysl || $(MKDIR) $(INTERFDIR)/hysl2hysl
	$(SED) -e 's/\[_IO_MEMVAR_WHAT_\]/_IO_MEMVAR_isREAL_/g' \
	     -e 's/\[_IO_MEMVAR_TYPE_\]/real/g'                 \
	     -e 's/\[_IO_MEMVAR_KIND_\]/4/g'                    \
	     -e 's/\[_IO_MEMVAR_DIMS_\]/4/g'                    \
	     -e 's/\[_IO_MEMVAR_DDOT_\]/:,:,:,:/g'                \
	     -e 's/\[_IO_MEMVAR_DTXT_\]/mem_dims\(1\),mem_dims\(2\),mem_dims\(3\),mem_dims\(4\)/g' < interf_hysl2hysl.templ > $(INTERFDIR)/hysl2hysl/hysl2hysl_4d_real4.F90
##### real8 #####
$(INTERFDIR)/hysl2hysl/hysl2hysl_1d_real8.F90: Makefile ../Makefile.in interf_hysl2hysl.templ
	@$(CHK_DIR_EXISTS) $(INTERFDIR)/hysl2hysl || $(MKDIR) $(INTERFDIR)/hysl2hysl
	$(SED) -e 's/\[_IO_MEMVAR_WHAT_\]/_IO_MEMVAR_isREAL_/g' \
	     -e 's/\[_IO_MEMVAR_TYPE_\]/real/g'                 \
	     -e 's/\[_IO_MEMVAR_KIND_\]/8/g'                    \
	     -e 's/\[_IO_MEMVAR_DIMS_\]/1/g'                    \
	     -e 's/\[_IO_MEMVAR_DDOT_\]/:/g'                    \
	     -e 's/\[_IO_MEMVAR_DTXT_\]/mem_dims\(1\)/g' < interf_hysl2hysl.templ > $(INTERFDIR)/hysl2hysl/hysl2hysl_1d_real8.F90

$(INTERFDIR)/hysl2hysl/hysl2hysl_2d_real8.F90: Makefile ../Makefile.in interf_hysl2hysl.templ
	@$(CHK_DIR_EXISTS) $(INTERFDIR)/hysl2hysl || $(MKDIR) $(INTERFDIR)/hysl2hysl
	$(SED) -e 's/\[_IO_MEMVAR_WHAT_\]/_IO_MEMVAR_isREAL_/g' \
	     -e 's/\[_IO_MEMVAR_TYPE_\]/real/g'                 \
	     -e 's/\[_IO_MEMVAR_KIND_\]/8/g'                    \
	     -e 's/\[_IO_MEMVAR_DIMS_\]/2/g'                    \
	     -e 's/\[_IO_MEMVAR_DDOT_\]/:,:/g'                  \
	     -e 's/\[_IO_MEMVAR_DTXT_\]/mem_dims\(1\),mem_dims\(2\)/g' < interf_hysl2hysl.templ > $(INTERFDIR)/hysl2hysl/hysl2hysl_2d_real8.F90

$(INTERFDIR)/hysl2hysl/hysl2hysl_3d_real8.F90: Makefile ../Makefile.in interf_hysl2hysl.templ
	@$(CHK_DIR_EXISTS) $(INTERFDIR)/hysl2hysl || $(MKDIR) $(INTERFDIR)/hysl2hysl
	$(SED) -e 's/\[_IO_MEMVAR_WHAT_\]/_IO_MEMVAR_isREAL_/g' \
	     -e 's/\[_IO_MEMVAR_TYPE_\]/real/g'                 \
	     -e 's/\[_IO_MEMVAR_KIND_\]/8/g'                    \
	     -e 's/\[_IO_MEMVAR_DIMS_\]/3/g'                    \
	     -e 's/\[_IO_MEMVAR_DDOT_\]/:,:,:/g'                \
	     -e 's/\[_IO_MEMVAR_DTXT_\]/mem_dims\(1\),mem_dims\(2\),mem_dims\(3\)/g' < interf_hysl2hysl.templ > $(INTERFDIR)/hysl2hysl/hysl2hysl_3d_real8.F90

$(INTERFDIR)/hysl2hysl/hysl2hysl_4d_real8.F90: Makefile ../Makefile.in interf_hysl2hysl.templ
	@$(CHK_DIR_EXISTS) $(INTERFDIR)/hysl2hysl || $(MKDIR) $(INTERFDIR)/hysl2hysl
	$(SED) -e 's/\[_IO_MEMVAR_WHAT_\]/_IO_MEMVAR_isREAL_/g' \
	     -e 's/\[_IO_MEMVAR_TYPE_\]/real/g'                 \
	     -e 's/\[_IO_MEMVAR_KIND_\]/8/g'                    \
	     -e 's/\[_IO_MEMVAR_DIMS_\]/4/g'                    \
	     -e 's/\[_IO_MEMVAR_DDOT_\]/:,:,:,:/g'                \
	     -e 's/\[_IO_MEMVAR_DTXT_\]/mem_dims\(1\),mem_dims\(2\),mem_dims\(3\),mem_dims\(4\)/g' < interf_hysl2hysl.templ > $(INTERFDIR)/hysl2hysl/hysl2hysl_4d_real8.F90
##### integer4  #####
$(INTERFDIR)/hysl2hysl/hysl2hysl_1d_integer4.F90: Makefile ../Makefile.in interf_hysl2hysl.templ
	@$(CHK_DIR_EXISTS) $(INTERFDIR)/hysl2hysl || $(MKDIR) $(INTERFDIR)/hysl2hysl
	$(SED) -e 's/\[_IO_MEMVAR_WHAT_\]/_IO_MEMVAR_isINTEGER_/g' \
	     -e 's/\[_IO_MEMVAR_TYPE_\]/integer/g'              \
	     -e 's/\[_IO_MEMVAR_KIND_\]/4/g'                    \
	     -e 's/\[_IO_MEMVAR_DIMS_\]/1/g'                    \
	     -e 's/\[_IO_MEMVAR_DDOT_\]/:/g'                    \
	     -e 's/\[_IO_MEMVAR_DTXT_\]/mem_dims\(1\)/g' < interf_hysl2hysl.templ > $(INTERFDIR)/hysl2hysl/hysl2hysl_1d_integer4.F90

$(INTERFDIR)/hysl2hysl/hysl2hysl_2d_integer4.F90: Makefile ../Makefile.in interf_hysl2hysl.templ
	@$(CHK_DIR_EXISTS) $(INTERFDIR)/hysl2hysl || $(MKDIR) $(INTERFDIR)/hysl2hysl
	$(SED) -e 's/\[_IO_MEMVAR_WHAT_\]/_IO_MEMVAR_isINTEGER_/g' \
	     -e 's/\[_IO_MEMVAR_TYPE_\]/integer/g'              \
	     -e 's/\[_IO_MEMVAR_KIND_\]/4/g'                    \
	     -e 's/\[_IO_MEMVAR_DIMS_\]/2/g'                    \
	     -e 's/\[_IO_MEMVAR_DDOT_\]/:,:/g'                  \
	     -e 's/\[_IO_MEMVAR_DTXT_\]/mem_dims\(1\),mem_dims\(2\)/g' < interf_hysl2hysl.templ > $(INTERFDIR)/hysl2hysl/hysl2hysl_2d_integer4.F90

$(INTERFDIR)/hysl2hysl/hysl2hysl_3d_integer4.F90: Makefile ../Makefile.in interf_hysl2hysl.templ
	@$(CHK_DIR_EXISTS) $(INTERFDIR)/hysl2hysl || $(MKDIR) $(INTERFDIR)/hysl2hysl
	$(SED) -e 's/\[_IO_MEMVAR_WHAT_\]/_IO_MEMVAR_isINTEGER_/g' \
	     -e 's/\[_IO_MEMVAR_TYPE_\]/integer/g'              \
	     -e 's/\[_IO_MEMVAR_KIND_\]/4/g'                    \
	     -e 's/\[_IO_MEMVAR_DIMS_\]/3/g'                    \
	     -e 's/\[_IO_MEMVAR_DDOT_\]/:,:,:/g'                \
	     -e 's/\[_IO_MEMVAR_DTXT_\]/mem_dims\(1\),mem_dims\(2\),mem_dims\(3\)/g' < interf_hysl2hysl.templ > $(INTERFDIR)/hysl2hysl/hysl2hysl_3d_integer4.F90

$(INTERFDIR)/hysl2hysl/hysl2hysl_4d_integer4.F90: Makefile ../Makefile.in interf_hysl2hysl.templ
	@$(CHK_DIR_EXISTS) $(INTERFDIR)/hysl2hysl || $(MKDIR) $(INTERFDIR)/hysl2hysl
	$(SED) -e 's/\[_IO_MEMVAR_WHAT_\]/_IO_MEMVAR_isINTEGER_/g' \
	     -e 's/\[_IO_MEMVAR_TYPE_\]/integer/g'              \
	     -e 's/\[_IO_MEMVAR_KIND_\]/4/g'                    \
	     -e 's/\[_IO_MEMVAR_DIMS_\]/4/g'                    \
	     -e 's/\[_IO_MEMVAR_DDOT_\]/:,:,:,:/g'                \
	     -e 's/\[_IO_MEMVAR_DTXT_\]/mem_dims\(1\),mem_dims\(2\),mem_dims\(3\),mem_dims\(4\)/g' < interf_hysl2hysl.templ > $(INTERFDIR)/hysl2hysl/hysl2hysl_4d_integer4.F90

###############################
#                             #
#   interf_gdat.templ         #
#                             #
###############################
##### real4 #####
$(INTERFDIR)/gdat/gdat_1d_real4.F90: Makefile ../Makefile.in interf_gdat.templ
	@$(CHK_DIR_EXISTS) $(INTERFDIR)/gdat || $(MKDIR) $(INTERFDIR)/gdat
	$(SED) -e 's/\[_IO_MEMVAR_WHAT_\]/_IO_MEMVAR_isREAL_/g' \
	     -e 's/\[_IO_MEMVAR_TYPE_\]/real/g'                 \
	     -e 's/\[_IO_MEMVAR_KIND_\]/4/g'                    \
	     -e 's/\[_IO_MEMVAR_DIMS_\]/1/g'                    \
	     -e 's/\[_IO_MEMVAR_DDOT_\]/:/g'                    \
	     -e 's/\[_IO_MEMVAR_DTXT_\]/mem_dims\(1\)/g' < interf_gdat.templ > $(INTERFDIR)/gdat/gdat_1d_real4.F90

$(INTERFDIR)/gdat/gdat_2d_real4.F90: Makefile ../Makefile.in interf_gdat.templ
	@$(CHK_DIR_EXISTS) $(INTERFDIR)/gdat || $(MKDIR) $(INTERFDIR)/gdat
	$(SED) -e 's/\[_IO_MEMVAR_WHAT_\]/_IO_MEMVAR_isREAL_/g' \
	     -e 's/\[_IO_MEMVAR_TYPE_\]/real/g'                 \
	     -e 's/\[_IO_MEMVAR_KIND_\]/4/g'                    \
	     -e 's/\[_IO_MEMVAR_DIMS_\]/2/g'                    \
	     -e 's/\[_IO_MEMVAR_DDOT_\]/:,:/g'                  \
	     -e 's/\[_IO_MEMVAR_DTXT_\]/mem_dims\(1\),mem_dims\(2\)/g' < interf_gdat.templ > $(INTERFDIR)/gdat/gdat_2d_real4.F90

$(INTERFDIR)/gdat/gdat_3d_real4.F90: Makefile ../Makefile.in interf_gdat.templ
	@$(CHK_DIR_EXISTS) $(INTERFDIR)/gdat || $(MKDIR) $(INTERFDIR)/gdat
	$(SED) -e 's/\[_IO_MEMVAR_WHAT_\]/_IO_MEMVAR_isREAL_/g' \
	     -e 's/\[_IO_MEMVAR_TYPE_\]/real/g'                 \
	     -e 's/\[_IO_MEMVAR_KIND_\]/4/g'                    \
	     -e 's/\[_IO_MEMVAR_DIMS_\]/3/g'                    \
	     -e 's/\[_IO_MEMVAR_DDOT_\]/:,:,:/g'                \
	     -e 's/\[_IO_MEMVAR_DTXT_\]/mem_dims\(1\),mem_dims\(2\),mem_dims\(3\)/g' < interf_gdat.templ > $(INTERFDIR)/gdat/gdat_3d_real4.F90

$(INTERFDIR)/gdat/gdat_4d_real4.F90: Makefile ../Makefile.in interf_gdat.templ
	@$(CHK_DIR_EXISTS) $(INTERFDIR)/gdat || $(MKDIR) $(INTERFDIR)/gdat
	$(SED) -e 's/\[_IO_MEMVAR_WHAT_\]/_IO_MEMVAR_isREAL_/g' \
	     -e 's/\[_IO_MEMVAR_TYPE_\]/real/g'                 \
	     -e 's/\[_IO_MEMVAR_KIND_\]/4/g'                    \
	     -e 's/\[_IO_MEMVAR_DIMS_\]/4/g'                    \
	     -e 's/\[_IO_MEMVAR_DDOT_\]/:,:,:,:/g'                \
	     -e 's/\[_IO_MEMVAR_DTXT_\]/mem_dims\(1\),mem_dims\(2\),mem_dims\(3\),mem_dims\(4\)/g' < interf_gdat.templ > $(INTERFDIR)/gdat/gdat_4d_real4.F90
##### real8 #####
$(INTERFDIR)/gdat/gdat_1d_real8.F90: Makefile ../Makefile.in interf_gdat.templ
	@$(CHK_DIR_EXISTS) $(INTERFDIR)/gdat || $(MKDIR) $(INTERFDIR)/gdat
	$(SED) -e 's/\[_IO_MEMVAR_WHAT_\]/_IO_MEMVAR_isREAL_/g' \
	     -e 's/\[_IO_MEMVAR_TYPE_\]/real/g'                 \
	     -e 's/\[_IO_MEMVAR_KIND_\]/8/g'                    \
	     -e 's/\[_IO_MEMVAR_DIMS_\]/1/g'                    \
	     -e 's/\[_IO_MEMVAR_DDOT_\]/:/g'                    \
	     -e 's/\[_IO_MEMVAR_DTXT_\]/mem_dims\(1\)/g' < interf_gdat.templ > $(INTERFDIR)/gdat/gdat_1d_real8.F90

$(INTERFDIR)/gdat/gdat_2d_real8.F90: Makefile ../Makefile.in interf_gdat.templ
	@$(CHK_DIR_EXISTS) $(INTERFDIR)/gdat || $(MKDIR) $(INTERFDIR)/gdat
	$(SED) -e 's/\[_IO_MEMVAR_WHAT_\]/_IO_MEMVAR_isREAL_/g' \
	     -e 's/\[_IO_MEMVAR_TYPE_\]/real/g'                 \
	     -e 's/\[_IO_MEMVAR_KIND_\]/8/g'                    \
	     -e 's/\[_IO_MEMVAR_DIMS_\]/2/g'                    \
	     -e 's/\[_IO_MEMVAR_DDOT_\]/:,:/g'                  \
	     -e 's/\[_IO_MEMVAR_DTXT_\]/mem_dims\(1\),mem_dims\(2\)/g' < interf_gdat.templ > $(INTERFDIR)/gdat/gdat_2d_real8.F90

$(INTERFDIR)/gdat/gdat_3d_real8.F90: Makefile ../Makefile.in interf_gdat.templ
	@$(CHK_DIR_EXISTS) $(INTERFDIR)/gdat || $(MKDIR) $(INTERFDIR)/gdat
	$(SED) -e 's/\[_IO_MEMVAR_WHAT_\]/_IO_MEMVAR_isREAL_/g' \
	     -e 's/\[_IO_MEMVAR_TYPE_\]/real/g'                 \
	     -e 's/\[_IO_MEMVAR_KIND_\]/8/g'                    \
	     -e 's/\[_IO_MEMVAR_DIMS_\]/3/g'                    \
	     -e 's/\[_IO_MEMVAR_DDOT_\]/:,:,:/g'                \
	     -e 's/\[_IO_MEMVAR_DTXT_\]/mem_dims\(1\),mem_dims\(2\),mem_dims\(3\)/g' < interf_gdat.templ > $(INTERFDIR)/gdat/gdat_3d_real8.F90

$(INTERFDIR)/gdat/gdat_4d_real8.F90: Makefile ../Makefile.in interf_gdat.templ
	@$(CHK_DIR_EXISTS) $(INTERFDIR)/gdat || $(MKDIR) $(INTERFDIR)/gdat
	$(SED) -e 's/\[_IO_MEMVAR_WHAT_\]/_IO_MEMVAR_isREAL_/g' \
	     -e 's/\[_IO_MEMVAR_TYPE_\]/real/g'                 \
	     -e 's/\[_IO_MEMVAR_KIND_\]/8/g'                    \
	     -e 's/\[_IO_MEMVAR_DIMS_\]/4/g'                    \
	     -e 's/\[_IO_MEMVAR_DDOT_\]/:,:,:,:/g'                \
	     -e 's/\[_IO_MEMVAR_DTXT_\]/mem_dims\(1\),mem_dims\(2\),mem_dims\(3\),mem_dims\(4\)/g' < interf_gdat.templ > $(INTERFDIR)/gdat/gdat_4d_real8.F90
##### integer4  #####
$(INTERFDIR)/gdat/gdat_1d_integer4.F90: Makefile ../Makefile.in interf_gdat.templ
	@$(CHK_DIR_EXISTS) $(INTERFDIR)/gdat || $(MKDIR) $(INTERFDIR)/gdat
	$(SED) -e 's/\[_IO_MEMVAR_WHAT_\]/_IO_MEMVAR_isINTEGER_/g' \
	     -e 's/\[_IO_MEMVAR_TYPE_\]/integer/g'              \
	     -e 's/\[_IO_MEMVAR_KIND_\]/4/g'                    \
	     -e 's/\[_IO_MEMVAR_DIMS_\]/1/g'                    \
	     -e 's/\[_IO_MEMVAR_DDOT_\]/:/g'                    \
	     -e 's/\[_IO_MEMVAR_DTXT_\]/mem_dims\(1\)/g' < interf_gdat.templ > $(INTERFDIR)/gdat/gdat_1d_integer4.F90

$(INTERFDIR)/gdat/gdat_2d_integer4.F90: Makefile ../Makefile.in interf_gdat.templ
	@$(CHK_DIR_EXISTS) $(INTERFDIR)/gdat || $(MKDIR) $(INTERFDIR)/gdat
	$(SED) -e 's/\[_IO_MEMVAR_WHAT_\]/_IO_MEMVAR_isINTEGER_/g' \
	     -e 's/\[_IO_MEMVAR_TYPE_\]/integer/g'              \
	     -e 's/\[_IO_MEMVAR_KIND_\]/4/g'                    \
	     -e 's/\[_IO_MEMVAR_DIMS_\]/2/g'                    \
	     -e 's/\[_IO_MEMVAR_DDOT_\]/:,:/g'                  \
	     -e 's/\[_IO_MEMVAR_DTXT_\]/mem_dims\(1\),mem_dims\(2\)/g' < interf_gdat.templ > $(INTERFDIR)/gdat/gdat_2d_integer4.F90

$(INTERFDIR)/gdat/gdat_3d_integer4.F90: Makefile ../Makefile.in interf_gdat.templ
	@$(CHK_DIR_EXISTS) $(INTERFDIR)/gdat || $(MKDIR) $(INTERFDIR)/gdat
	$(SED) -e 's/\[_IO_MEMVAR_WHAT_\]/_IO_MEMVAR_isINTEGER_/g' \
	     -e 's/\[_IO_MEMVAR_TYPE_\]/integer/g'              \
	     -e 's/\[_IO_MEMVAR_KIND_\]/4/g'                    \
	     -e 's/\[_IO_MEMVAR_DIMS_\]/3/g'                    \
	     -e 's/\[_IO_MEMVAR_DDOT_\]/:,:,:/g'                \
	     -e 's/\[_IO_MEMVAR_DTXT_\]/mem_dims\(1\),mem_dims\(2\),mem_dims\(3\)/g' < interf_gdat.templ > $(INTERFDIR)/gdat/gdat_3d_integer4.F90

$(INTERFDIR)/gdat/gdat_4d_integer4.F90: Makefile ../Makefile.in interf_gdat.templ
	@$(CHK_DIR_EXISTS) $(INTERFDIR)/gdat || $(MKDIR) $(INTERFDIR)/gdat
	$(SED) -e 's/\[_IO_MEMVAR_WHAT_\]/_IO_MEMVAR_isINTEGER_/g' \
	     -e 's/\[_IO_MEMVAR_TYPE_\]/integer/g'              \
	     -e 's/\[_IO_MEMVAR_KIND_\]/4/g'                    \
	     -e 's/\[_IO_MEMVAR_DIMS_\]/4/g'                    \
	     -e 's/\[_IO_MEMVAR_DDOT_\]/:,:,:,:/g'                \
	     -e 's/\[_IO_MEMVAR_DTXT_\]/mem_dims\(1\),mem_dims\(2\),mem_dims\(3\),mem_dims\(4\)/g' < interf_gdat.templ > $(INTERFDIR)/gdat/gdat_4d_integer4.F90

###############################
#                             #
#   interf_gval.templ  #
#                             #
###############################
##### real4 #####
$(INTERFDIR)/gval/gval_1d_real4.F90: Makefile ../Makefile.in interf_gval.templ
	@$(CHK_DIR_EXISTS) $(INTERFDIR)/gval || $(MKDIR) $(INTERFDIR)/gval
	$(SED) -e 's/\[_IO_MEMVAR_WHAT_\]/_IO_MEMVAR_isREAL_/g' \
	     -e 's/\[_IO_MEMVAR_TYPE_\]/real/g'                 \
	     -e 's/\[_IO_MEMVAR_KIND_\]/4/g'                    \
	     -e 's/\[_IO_MEMVAR_DIMS_\]/1/g'                    \
	     -e 's/\[_IO_MEMVAR_DDOT_\]/:/g'                    \
	     -e 's/\[_IO_MEMVAR_DTXT_\]/ddims\(1\)/g' < interf_gval.templ > $(INTERFDIR)/gval/gval_1d_real4.F90

$(INTERFDIR)/gval/gval_2d_real4.F90: Makefile ../Makefile.in interf_gval.templ
	@$(CHK_DIR_EXISTS) $(INTERFDIR)/gval || $(MKDIR) $(INTERFDIR)/gval
	$(SED) -e 's/\[_IO_MEMVAR_WHAT_\]/_IO_MEMVAR_isREAL_/g' \
	     -e 's/\[_IO_MEMVAR_TYPE_\]/real/g'                 \
	     -e 's/\[_IO_MEMVAR_KIND_\]/4/g'                    \
	     -e 's/\[_IO_MEMVAR_DIMS_\]/2/g'                    \
	     -e 's/\[_IO_MEMVAR_DDOT_\]/:,:/g'                  \
	     -e 's/\[_IO_MEMVAR_DTXT_\]/ddims\(1\),ddims\(2\)/g' < interf_gval.templ > $(INTERFDIR)/gval/gval_2d_real4.F90

$(INTERFDIR)/gval/gval_3d_real4.F90: Makefile ../Makefile.in interf_gval.templ
	@$(CHK_DIR_EXISTS) $(INTERFDIR)/gval || $(MKDIR) $(INTERFDIR)/gval
	$(SED) -e 's/\[_IO_MEMVAR_WHAT_\]/_IO_MEMVAR_isREAL_/g' \
	     -e 's/\[_IO_MEMVAR_TYPE_\]/real/g'                 \
	     -e 's/\[_IO_MEMVAR_KIND_\]/4/g'                    \
	     -e 's/\[_IO_MEMVAR_DIMS_\]/3/g'                    \
	     -e 's/\[_IO_MEMVAR_DDOT_\]/:,:,:/g'                \
	     -e 's/\[_IO_MEMVAR_DTXT_\]/ddims\(1\),ddims\(2\),ddims\(3\)/g' < interf_gval.templ > $(INTERFDIR)/gval/gval_3d_real4.F90

$(INTERFDIR)/gval/gval_4d_real4.F90: Makefile ../Makefile.in interf_gval.templ
	@$(CHK_DIR_EXISTS) $(INTERFDIR)/gval || $(MKDIR) $(INTERFDIR)/gval
	$(SED) -e 's/\[_IO_MEMVAR_WHAT_\]/_IO_MEMVAR_isREAL_/g' \
	     -e 's/\[_IO_MEMVAR_TYPE_\]/real/g'                 \
	     -e 's/\[_IO_MEMVAR_KIND_\]/4/g'                    \
	     -e 's/\[_IO_MEMVAR_DIMS_\]/4/g'                    \
	     -e 's/\[_IO_MEMVAR_DDOT_\]/:,:,:,:/g'                \
	     -e 's/\[_IO_MEMVAR_DTXT_\]/ddims\(1\),ddims\(2\),ddims\(3\),ddims\(4\)/g' < interf_gval.templ > $(INTERFDIR)/gval/gval_4d_real4.F90
##### real8 #####
$(INTERFDIR)/gval/gval_1d_real8.F90: Makefile ../Makefile.in interf_gval.templ
	@$(CHK_DIR_EXISTS) $(INTERFDIR)/gval || $(MKDIR) $(INTERFDIR)/gval
	$(SED) -e 's/\[_IO_MEMVAR_WHAT_\]/_IO_MEMVAR_isREAL_/g' \
	     -e 's/\[_IO_MEMVAR_TYPE_\]/real/g'                 \
	     -e 's/\[_IO_MEMVAR_KIND_\]/8/g'                    \
	     -e 's/\[_IO_MEMVAR_DIMS_\]/1/g'                    \
	     -e 's/\[_IO_MEMVAR_DDOT_\]/:/g'                    \
	     -e 's/\[_IO_MEMVAR_DTXT_\]/ddims\(1\)/g' < interf_gval.templ > $(INTERFDIR)/gval/gval_1d_real8.F90

$(INTERFDIR)/gval/gval_2d_real8.F90: Makefile ../Makefile.in interf_gval.templ
	@$(CHK_DIR_EXISTS) $(INTERFDIR)/gval || $(MKDIR) $(INTERFDIR)/gval
	$(SED) -e 's/\[_IO_MEMVAR_WHAT_\]/_IO_MEMVAR_isREAL_/g' \
	     -e 's/\[_IO_MEMVAR_TYPE_\]/real/g'                 \
	     -e 's/\[_IO_MEMVAR_KIND_\]/8/g'                    \
	     -e 's/\[_IO_MEMVAR_DIMS_\]/2/g'                    \
	     -e 's/\[_IO_MEMVAR_DDOT_\]/:,:/g'                  \
	     -e 's/\[_IO_MEMVAR_DTXT_\]/ddims\(1\),ddims\(2\)/g' < interf_gval.templ > $(INTERFDIR)/gval/gval_2d_real8.F90

$(INTERFDIR)/gval/gval_3d_real8.F90: Makefile ../Makefile.in interf_gval.templ
	@$(CHK_DIR_EXISTS) $(INTERFDIR)/gval || $(MKDIR) $(INTERFDIR)/gval
	$(SED) -e 's/\[_IO_MEMVAR_WHAT_\]/_IO_MEMVAR_isREAL_/g' \
	     -e 's/\[_IO_MEMVAR_TYPE_\]/real/g'                 \
	     -e 's/\[_IO_MEMVAR_KIND_\]/8/g'                    \
	     -e 's/\[_IO_MEMVAR_DIMS_\]/3/g'                    \
	     -e 's/\[_IO_MEMVAR_DDOT_\]/:,:,:/g'                \
	     -e 's/\[_IO_MEMVAR_DTXT_\]/ddims\(1\),ddims\(2\),ddims\(3\)/g' < interf_gval.templ > $(INTERFDIR)/gval/gval_3d_real8.F90

$(INTERFDIR)/gval/gval_4d_real8.F90: Makefile ../Makefile.in interf_gval.templ
	@$(CHK_DIR_EXISTS) $(INTERFDIR)/gval || $(MKDIR) $(INTERFDIR)/gval
	$(SED) -e 's/\[_IO_MEMVAR_WHAT_\]/_IO_MEMVAR_isREAL_/g' \
	     -e 's/\[_IO_MEMVAR_TYPE_\]/real/g'                 \
	     -e 's/\[_IO_MEMVAR_KIND_\]/8/g'                    \
	     -e 's/\[_IO_MEMVAR_DIMS_\]/4/g'                    \
	     -e 's/\[_IO_MEMVAR_DDOT_\]/:,:,:,:/g'                \
	     -e 's/\[_IO_MEMVAR_DTXT_\]/ddims\(1\),ddims\(2\),ddims\(3\),ddims\(4\)/g' < interf_gval.templ > $(INTERFDIR)/gval/gval_4d_real8.F90
##### integer4  #####
$(INTERFDIR)/gval/gval_1d_integer4.F90: Makefile ../Makefile.in interf_gval.templ
	@$(CHK_DIR_EXISTS) $(INTERFDIR)/gval || $(MKDIR) $(INTERFDIR)/gval
	$(SED) -e 's/\[_IO_MEMVAR_WHAT_\]/_IO_MEMVAR_isINTEGER_/g' \
	     -e 's/\[_IO_MEMVAR_TYPE_\]/integer/g'              \
	     -e 's/\[_IO_MEMVAR_KIND_\]/4/g'                    \
	     -e 's/\[_IO_MEMVAR_DIMS_\]/1/g'                    \
	     -e 's/\[_IO_MEMVAR_DDOT_\]/:/g'                    \
	     -e 's/\[_IO_MEMVAR_DTXT_\]/ddims\(1\)/g' < interf_gval.templ > $(INTERFDIR)/gval/gval_1d_integer4.F90

$(INTERFDIR)/gval/gval_2d_integer4.F90: Makefile ../Makefile.in interf_gval.templ
	@$(CHK_DIR_EXISTS) $(INTERFDIR)/gval || $(MKDIR) $(INTERFDIR)/gval
	$(SED) -e 's/\[_IO_MEMVAR_WHAT_\]/_IO_MEMVAR_isINTEGER_/g' \
	     -e 's/\[_IO_MEMVAR_TYPE_\]/integer/g'              \
	     -e 's/\[_IO_MEMVAR_KIND_\]/4/g'                    \
	     -e 's/\[_IO_MEMVAR_DIMS_\]/2/g'                    \
	     -e 's/\[_IO_MEMVAR_DDOT_\]/:,:/g'                  \
	     -e 's/\[_IO_MEMVAR_DTXT_\]/ddims\(1\),ddims\(2\)/g' < interf_gval.templ > $(INTERFDIR)/gval/gval_2d_integer4.F90

$(INTERFDIR)/gval/gval_3d_integer4.F90: Makefile ../Makefile.in interf_gval.templ
	@$(CHK_DIR_EXISTS) $(INTERFDIR)/gval || $(MKDIR) $(INTERFDIR)/gval
	$(SED) -e 's/\[_IO_MEMVAR_WHAT_\]/_IO_MEMVAR_isINTEGER_/g' \
	     -e 's/\[_IO_MEMVAR_TYPE_\]/integer/g'              \
	     -e 's/\[_IO_MEMVAR_KIND_\]/4/g'                    \
	     -e 's/\[_IO_MEMVAR_DIMS_\]/3/g'                    \
	     -e 's/\[_IO_MEMVAR_DDOT_\]/:,:,:/g'                \
	     -e 's/\[_IO_MEMVAR_DTXT_\]/ddims\(1\),ddims\(2\),ddims\(3\)/g' < interf_gval.templ > $(INTERFDIR)/gval/gval_3d_integer4.F90

$(INTERFDIR)/gval/gval_4d_integer4.F90: Makefile ../Makefile.in interf_gval.templ
	@$(CHK_DIR_EXISTS) $(INTERFDIR)/gval || $(MKDIR) $(INTERFDIR)/gval
	$(SED) -e 's/\[_IO_MEMVAR_WHAT_\]/_IO_MEMVAR_isINTEGER_/g' \
	     -e 's/\[_IO_MEMVAR_TYPE_\]/integer/g'              \
	     -e 's/\[_IO_MEMVAR_KIND_\]/4/g'                    \
	     -e 's/\[_IO_MEMVAR_DIMS_\]/4/g'                    \
	     -e 's/\[_IO_MEMVAR_DDOT_\]/:,:,:,:/g'                \
	     -e 's/\[_IO_MEMVAR_DTXT_\]/ddims\(1\),ddims\(2\),ddims\(3\),ddims\(4\)/g' < interf_gval.templ > $(INTERFDIR)/gval/gval_4d_integer4.F90
##### complex4  #####
$(INTERFDIR)/gval/gval_1d_complex4.F03: Makefile ../Makefile.in interf_gval_complex.templ
	@$(CHK_DIR_EXISTS) $(INTERFDIR)/gval || $(MKDIR) $(INTERFDIR)/gval
	$(SED) -e 's/\[_IO_MEMVAR_WHAT_\]/_IO_MEMVAR_isCOMPLEX_/g' \
	     -e 's/\[_IO_MEMVAR_TYPE_\]/complex/g'              \
	     -e 's/\[_IO_MEMVAR_KIND_\]/4/g'                    \
	     -e 's/\[_IO_MEMVAR_DIMS_\]/1/g'                    \
	     -e 's/\[_IO_MEMVAR_DDOT_\]/:/g'                    \
	     -e 's/\[_IO_MEMVAR_DTXT_\]/ddims\(1\)/g' < interf_gval_complex.templ > $(INTERFDIR)/gval/gval_1d_complex4.F03

$(INTERFDIR)/gval/gval_2d_complex4.F03: Makefile ../Makefile.in interf_gval_complex.templ
	@$(CHK_DIR_EXISTS) $(INTERFDIR)/gval || $(MKDIR) $(INTERFDIR)/gval
	$(SED) -e 's/\[_IO_MEMVAR_WHAT_\]/_IO_MEMVAR_isINTEGER_/g' \
	     -e 's/\[_IO_MEMVAR_TYPE_\]/complex/g'              \
	     -e 's/\[_IO_MEMVAR_KIND_\]/4/g'                    \
	     -e 's/\[_IO_MEMVAR_DIMS_\]/2/g'                    \
	     -e 's/\[_IO_MEMVAR_DDOT_\]/:,:/g'                  \
	     -e 's/\[_IO_MEMVAR_DTXT_\]/ddims\(1\),ddims\(2\)/g' < interf_gval_complex.templ > $(INTERFDIR)/gval/gval_2d_complex4.F03

$(INTERFDIR)/gval/gval_3d_complex4.F03: Makefile ../Makefile.in interf_gval_complex.templ
	@$(CHK_DIR_EXISTS) $(INTERFDIR)/gval || $(MKDIR) $(INTERFDIR)/gval
	$(SED) -e 's/\[_IO_MEMVAR_WHAT_\]/_IO_MEMVAR_isINTEGER_/g' \
	     -e 's/\[_IO_MEMVAR_TYPE_\]/complex/g'              \
	     -e 's/\[_IO_MEMVAR_KIND_\]/4/g'                    \
	     -e 's/\[_IO_MEMVAR_DIMS_\]/3/g'                    \
	     -e 's/\[_IO_MEMVAR_DDOT_\]/:,:,:/g'                \
	     -e 's/\[_IO_MEMVAR_DTXT_\]/ddims\(1\),ddims\(2\),ddims\(3\)/g' < interf_gval_complex.templ > $(INTERFDIR)/gval/gval_3d_complex4.F03

$(INTERFDIR)/gval/gval_4d_complex4.F03: Makefile ../Makefile.in interf_gval_complex.templ
	@$(CHK_DIR_EXISTS) $(INTERFDIR)/gval || $(MKDIR) $(INTERFDIR)/gval
	$(SED) -e 's/\[_IO_MEMVAR_WHAT_\]/_IO_MEMVAR_isINTEGER_/g' \
	     -e 's/\[_IO_MEMVAR_TYPE_\]/complex/g'              \
	     -e 's/\[_IO_MEMVAR_KIND_\]/4/g'                    \
	     -e 's/\[_IO_MEMVAR_DIMS_\]/4/g'                    \
	     -e 's/\[_IO_MEMVAR_DDOT_\]/:,:,:,:/g'                \
	     -e 's/\[_IO_MEMVAR_DTXT_\]/ddims\(1\),ddims\(2\),ddims\(3\),ddims\(4\)/g' < interf_gval_complex.templ > $(INTERFDIR)/gval/gval_4d_complex4.F03
##### complex8  #####
$(INTERFDIR)/gval/gval_1d_complex8.F03: Makefile ../Makefile.in interf_gval_complex.templ
	@$(CHK_DIR_EXISTS) $(INTERFDIR)/gval || $(MKDIR) $(INTERFDIR)/gval
	$(SED) -e 's/\[_IO_MEMVAR_WHAT_\]/_IO_MEMVAR_isCOMPLEX_/g' \
	     -e 's/\[_IO_MEMVAR_TYPE_\]/complex/g'              \
	     -e 's/\[_IO_MEMVAR_KIND_\]/8/g'                    \
	     -e 's/\[_IO_MEMVAR_DIMS_\]/1/g'                    \
	     -e 's/\[_IO_MEMVAR_DDOT_\]/:/g'                    \
	     -e 's/\[_IO_MEMVAR_DTXT_\]/ddims\(1\)/g' < interf_gval_complex.templ > $(INTERFDIR)/gval/gval_1d_complex8.F03

$(INTERFDIR)/gval/gval_2d_complex8.F03: Makefile ../Makefile.in interf_gval_complex.templ
	@$(CHK_DIR_EXISTS) $(INTERFDIR)/gval || $(MKDIR) $(INTERFDIR)/gval
	$(SED) -e 's/\[_IO_MEMVAR_WHAT_\]/_IO_MEMVAR_isINTEGER_/g' \
	     -e 's/\[_IO_MEMVAR_TYPE_\]/complex/g'              \
	     -e 's/\[_IO_MEMVAR_KIND_\]/8/g'                    \
	     -e 's/\[_IO_MEMVAR_DIMS_\]/2/g'                    \
	     -e 's/\[_IO_MEMVAR_DDOT_\]/:,:/g'                  \
	     -e 's/\[_IO_MEMVAR_DTXT_\]/ddims\(1\),ddims\(2\)/g' < interf_gval_complex.templ > $(INTERFDIR)/gval/gval_2d_complex8.F03

$(INTERFDIR)/gval/gval_3d_complex8.F03: Makefile ../Makefile.in interf_gval_complex.templ
	@$(CHK_DIR_EXISTS) $(INTERFDIR)/gval || $(MKDIR) $(INTERFDIR)/gval
	$(SED) -e 's/\[_IO_MEMVAR_WHAT_\]/_IO_MEMVAR_isINTEGER_/g' \
	     -e 's/\[_IO_MEMVAR_TYPE_\]/complex/g'              \
	     -e 's/\[_IO_MEMVAR_KIND_\]/8/g'                    \
	     -e 's/\[_IO_MEMVAR_DIMS_\]/3/g'                    \
	     -e 's/\[_IO_MEMVAR_DDOT_\]/:,:,:/g'                \
	     -e 's/\[_IO_MEMVAR_DTXT_\]/ddims\(1\),ddims\(2\),ddims\(3\)/g' < interf_gval_complex.templ > $(INTERFDIR)/gval/gval_3d_complex8.F03

$(INTERFDIR)/gval/gval_4d_complex8.F03: Makefile ../Makefile.in interf_gval_complex.templ
	@$(CHK_DIR_EXISTS) $(INTERFDIR)/gval || $(MKDIR) $(INTERFDIR)/gval
	$(SED) -e 's/\[_IO_MEMVAR_WHAT_\]/_IO_MEMVAR_isINTEGER_/g' \
	     -e 's/\[_IO_MEMVAR_TYPE_\]/complex/g'              \
	     -e 's/\[_IO_MEMVAR_KIND_\]/8/g'                    \
	     -e 's/\[_IO_MEMVAR_DIMS_\]/4/g'                    \
	     -e 's/\[_IO_MEMVAR_DDOT_\]/:,:,:,:/g'                \
	     -e 's/\[_IO_MEMVAR_DTXT_\]/ddims\(1\),ddims\(2\),ddims\(3\),ddims\(4\)/g' < interf_gval_complex.templ > $(INTERFDIR)/gval/gval_4d_complex8.F03		     

###############################
#                             #
#   interf_pdat.templ         #
#                             #
###############################
##### real4 #####
$(INTERFDIR)/pdat/pdat_1d_real4.F90: Makefile ../Makefile.in interf_pdat.templ
	@$(CHK_DIR_EXISTS) $(INTERFDIR)/pdat || $(MKDIR) $(INTERFDIR)/pdat
	$(SED) -e 's/\[_IO_MEMVAR_WHAT_\]/_IO_MEMVAR_isREAL_/g' \
	     -e 's/\[_IO_MEMVAR_TYPE_\]/real/g'                 \
	     -e 's/\[_IO_MEMVAR_KIND_\]/4/g'                    \
	     -e 's/\[_IO_MEMVAR_DIMS_\]/1/g'                    \
	     -e 's/\[_IO_MEMVAR_DDOT_\]/:/g'                    \
	     -e 's/\[_IO_MEMVAR_DTXT_\]/mem_dims\(1\)/g' < interf_pdat.templ > $(INTERFDIR)/pdat/pdat_1d_real4.F90

$(INTERFDIR)/pdat/pdat_2d_real4.F90: Makefile ../Makefile.in interf_pdat.templ
	@$(CHK_DIR_EXISTS) $(INTERFDIR)/pdat || $(MKDIR) $(INTERFDIR)/pdat
	$(SED) -e 's/\[_IO_MEMVAR_WHAT_\]/_IO_MEMVAR_isREAL_/g' \
	     -e 's/\[_IO_MEMVAR_TYPE_\]/real/g'                 \
	     -e 's/\[_IO_MEMVAR_KIND_\]/4/g'                    \
	     -e 's/\[_IO_MEMVAR_DIMS_\]/2/g'                    \
	     -e 's/\[_IO_MEMVAR_DDOT_\]/:,:/g'                  \
	     -e 's/\[_IO_MEMVAR_DTXT_\]/mem_dims\(1\),mem_dims\(2\)/g' < interf_pdat.templ > $(INTERFDIR)/pdat/pdat_2d_real4.F90

$(INTERFDIR)/pdat/pdat_3d_real4.F90: Makefile ../Makefile.in interf_pdat.templ
	@$(CHK_DIR_EXISTS) $(INTERFDIR)/pdat || $(MKDIR) $(INTERFDIR)/pdat
	$(SED) -e 's/\[_IO_MEMVAR_WHAT_\]/_IO_MEMVAR_isREAL_/g' \
	     -e 's/\[_IO_MEMVAR_TYPE_\]/real/g'                 \
	     -e 's/\[_IO_MEMVAR_KIND_\]/4/g'                    \
	     -e 's/\[_IO_MEMVAR_DIMS_\]/3/g'                    \
	     -e 's/\[_IO_MEMVAR_DDOT_\]/:,:,:/g'                \
	     -e 's/\[_IO_MEMVAR_DTXT_\]/mem_dims\(1\),mem_dims\(2\),mem_dims\(3\)/g' < interf_pdat.templ > $(INTERFDIR)/pdat/pdat_3d_real4.F90

$(INTERFDIR)/pdat/pdat_4d_real4.F90: Makefile ../Makefile.in interf_pdat.templ
	@$(CHK_DIR_EXISTS) $(INTERFDIR)/pdat || $(MKDIR) $(INTERFDIR)/pdat
	$(SED) -e 's/\[_IO_MEMVAR_WHAT_\]/_IO_MEMVAR_isREAL_/g' \
	     -e 's/\[_IO_MEMVAR_TYPE_\]/real/g'                 \
	     -e 's/\[_IO_MEMVAR_KIND_\]/4/g'                    \
	     -e 's/\[_IO_MEMVAR_DIMS_\]/4/g'                    \
	     -e 's/\[_IO_MEMVAR_DDOT_\]/:,:,:,:/g'                \
	     -e 's/\[_IO_MEMVAR_DTXT_\]/mem_dims\(1\),mem_dims\(2\),mem_dims\(3\),mem_dims\(4\)/g' < interf_pdat.templ > $(INTERFDIR)/pdat/pdat_4d_real4.F90
##### real8 #####
$(INTERFDIR)/pdat/pdat_1d_real8.F90: Makefile ../Makefile.in interf_pdat.templ
	@$(CHK_DIR_EXISTS) $(INTERFDIR)/pdat || $(MKDIR) $(INTERFDIR)/pdat
	$(SED) -e 's/\[_IO_MEMVAR_WHAT_\]/_IO_MEMVAR_isREAL_/g' \
	     -e 's/\[_IO_MEMVAR_TYPE_\]/real/g'                 \
	     -e 's/\[_IO_MEMVAR_KIND_\]/8/g'                    \
	     -e 's/\[_IO_MEMVAR_DIMS_\]/1/g'                    \
	     -e 's/\[_IO_MEMVAR_DDOT_\]/:/g'                    \
	     -e 's/\[_IO_MEMVAR_DTXT_\]/mem_dims\(1\)/g' < interf_pdat.templ > $(INTERFDIR)/pdat/pdat_1d_real8.F90

$(INTERFDIR)/pdat/pdat_2d_real8.F90: Makefile ../Makefile.in interf_pdat.templ
	@$(CHK_DIR_EXISTS) $(INTERFDIR)/pdat || $(MKDIR) $(INTERFDIR)/pdat
	$(SED) -e 's/\[_IO_MEMVAR_WHAT_\]/_IO_MEMVAR_isREAL_/g' \
	     -e 's/\[_IO_MEMVAR_TYPE_\]/real/g'                 \
	     -e 's/\[_IO_MEMVAR_KIND_\]/8/g'                    \
	     -e 's/\[_IO_MEMVAR_DIMS_\]/2/g'                    \
	     -e 's/\[_IO_MEMVAR_DDOT_\]/:,:/g'                  \
	     -e 's/\[_IO_MEMVAR_DTXT_\]/mem_dims\(1\),mem_dims\(2\)/g' < interf_pdat.templ > $(INTERFDIR)/pdat/pdat_2d_real8.F90

$(INTERFDIR)/pdat/pdat_3d_real8.F90: Makefile ../Makefile.in interf_pdat.templ
	@$(CHK_DIR_EXISTS) $(INTERFDIR)/pdat || $(MKDIR) $(INTERFDIR)/pdat
	$(SED) -e 's/\[_IO_MEMVAR_WHAT_\]/_IO_MEMVAR_isREAL_/g' \
	     -e 's/\[_IO_MEMVAR_TYPE_\]/real/g'                 \
	     -e 's/\[_IO_MEMVAR_KIND_\]/8/g'                    \
	     -e 's/\[_IO_MEMVAR_DIMS_\]/3/g'                    \
	     -e 's/\[_IO_MEMVAR_DDOT_\]/:,:,:/g'                \
	     -e 's/\[_IO_MEMVAR_DTXT_\]/mem_dims\(1\),mem_dims\(2\),mem_dims\(3\)/g' < interf_pdat.templ > $(INTERFDIR)/pdat/pdat_3d_real8.F90

$(INTERFDIR)/pdat/pdat_4d_real8.F90: Makefile ../Makefile.in interf_pdat.templ
	@$(CHK_DIR_EXISTS) $(INTERFDIR)/pdat || $(MKDIR) $(INTERFDIR)/pdat
	$(SED) -e 's/\[_IO_MEMVAR_WHAT_\]/_IO_MEMVAR_isREAL_/g' \
	     -e 's/\[_IO_MEMVAR_TYPE_\]/real/g'                 \
	     -e 's/\[_IO_MEMVAR_KIND_\]/8/g'                    \
	     -e 's/\[_IO_MEMVAR_DIMS_\]/4/g'                    \
	     -e 's/\[_IO_MEMVAR_DDOT_\]/:,:,:,:/g'                \
	     -e 's/\[_IO_MEMVAR_DTXT_\]/mem_dims\(1\),mem_dims\(2\),mem_dims\(3\),mem_dims\(4\)/g' < interf_pdat.templ > $(INTERFDIR)/pdat/pdat_4d_real8.F90
##### cmplx4 #####
$(INTERFDIR)/pdat/pdat_1d_cmplx4.F90: Makefile ../Makefile.in interf_pdat.templ
	@$(CHK_DIR_EXISTS) $(INTERFDIR)/pdat || $(MKDIR) $(INTERFDIR)/pdat
	$(SED) -e 's/\[_IO_MEMVAR_WHAT_\]/_IO_MEMVAR_isREAL_/g' \
	     -e 's/\[_IO_MEMVAR_TYPE_\]/cmplx/g'                 \
	     -e 's/\[_IO_MEMVAR_KIND_\]/4/g'                    \
	     -e 's/\[_IO_MEMVAR_DIMS_\]/1/g'                    \
	     -e 's/\[_IO_MEMVAR_DDOT_\]/:/g'                    \
	     -e 's/\[_IO_MEMVAR_DTXT_\]/mem_dims\(1\)/g' < interf_pdat.templ > $(INTERFDIR)/pdat/pdat_1d_cmplx4.F90

$(INTERFDIR)/pdat/pdat_2d_cmplx4.F90: Makefile ../Makefile.in interf_pdat.templ
	@$(CHK_DIR_EXISTS) $(INTERFDIR)/pdat || $(MKDIR) $(INTERFDIR)/pdat
	$(SED) -e 's/\[_IO_MEMVAR_WHAT_\]/_IO_MEMVAR_isREAL_/g' \
	     -e 's/\[_IO_MEMVAR_TYPE_\]/cmplx/g'                 \
	     -e 's/\[_IO_MEMVAR_KIND_\]/4/g'                    \
	     -e 's/\[_IO_MEMVAR_DIMS_\]/2/g'                    \
	     -e 's/\[_IO_MEMVAR_DDOT_\]/:,:/g'                  \
	     -e 's/\[_IO_MEMVAR_DTXT_\]/mem_dims\(1\),mem_dims\(2\)/g' < interf_pdat.templ > $(INTERFDIR)/pdat/pdat_2d_cmplx4.F90

$(INTERFDIR)/pdat/pdat_3d_cmplx4.F90: Makefile ../Makefile.in interf_pdat.templ
	@$(CHK_DIR_EXISTS) $(INTERFDIR)/pdat || $(MKDIR) $(INTERFDIR)/pdat
	$(SED) -e 's/\[_IO_MEMVAR_WHAT_\]/_IO_MEMVAR_isREAL_/g' \
	     -e 's/\[_IO_MEMVAR_TYPE_\]/cmplx/g'                 \
	     -e 's/\[_IO_MEMVAR_KIND_\]/4/g'                    \
	     -e 's/\[_IO_MEMVAR_DIMS_\]/3/g'                    \
	     -e 's/\[_IO_MEMVAR_DDOT_\]/:,:,:/g'                \
	     -e 's/\[_IO_MEMVAR_DTXT_\]/mem_dims\(1\),mem_dims\(2\),mem_dims\(3\)/g' < interf_pdat.templ > $(INTERFDIR)/pdat/pdat_3d_cmplx4.F90

$(INTERFDIR)/pdat/pdat_4d_cmplx4.F90: Makefile ../Makefile.in interf_pdat.templ
	@$(CHK_DIR_EXISTS) $(INTERFDIR)/pdat || $(MKDIR) $(INTERFDIR)/pdat
	$(SED) -e 's/\[_IO_MEMVAR_WHAT_\]/_IO_MEMVAR_isREAL_/g' \
	     -e 's/\[_IO_MEMVAR_TYPE_\]/cmplx/g'                 \
	     -e 's/\[_IO_MEMVAR_KIND_\]/4/g'                    \
	     -e 's/\[_IO_MEMVAR_DIMS_\]/4/g'                    \
	     -e 's/\[_IO_MEMVAR_DDOT_\]/:,:,:,:/g'                \
	     -e 's/\[_IO_MEMVAR_DTXT_\]/mem_dims\(1\),mem_dims\(2\),mem_dims\(3\),mem_dims\(4\)/g' < interf_pdat.templ > $(INTERFDIR)/pdat/pdat_4d_cmplx4.F90
##### cmplx8 #####
$(INTERFDIR)/pdat/pdat_1d_cmplx8.F90: Makefile ../Makefile.in interf_pdat.templ
	@$(CHK_DIR_EXISTS) $(INTERFDIR)/pdat || $(MKDIR) $(INTERFDIR)/pdat
	$(SED) -e 's/\[_IO_MEMVAR_WHAT_\]/_IO_MEMVAR_isREAL_/g' \
	     -e 's/\[_IO_MEMVAR_TYPE_\]/cmplx/g'                 \
	     -e 's/\[_IO_MEMVAR_KIND_\]/8/g'                    \
	     -e 's/\[_IO_MEMVAR_DIMS_\]/1/g'                    \
	     -e 's/\[_IO_MEMVAR_DDOT_\]/:/g'                    \
	     -e 's/\[_IO_MEMVAR_DTXT_\]/mem_dims\(1\)/g' < interf_pdat.templ > $(INTERFDIR)/pdat/pdat_1d_cmplx8.F90

$(INTERFDIR)/pdat/pdat_2d_cmplx8.F90: Makefile ../Makefile.in interf_pdat.templ
	@$(CHK_DIR_EXISTS) $(INTERFDIR)/pdat || $(MKDIR) $(INTERFDIR)/pdat
	$(SED) -e 's/\[_IO_MEMVAR_WHAT_\]/_IO_MEMVAR_isREAL_/g' \
	     -e 's/\[_IO_MEMVAR_TYPE_\]/cmplx/g'                 \
	     -e 's/\[_IO_MEMVAR_KIND_\]/8/g'                    \
	     -e 's/\[_IO_MEMVAR_DIMS_\]/2/g'                    \
	     -e 's/\[_IO_MEMVAR_DDOT_\]/:,:/g'                  \
	     -e 's/\[_IO_MEMVAR_DTXT_\]/mem_dims\(1\),mem_dims\(2\)/g' < interf_pdat.templ > $(INTERFDIR)/pdat/pdat_2d_cmplx8.F90

$(INTERFDIR)/pdat/pdat_3d_cmplx8.F90: Makefile ../Makefile.in interf_pdat.templ
	@$(CHK_DIR_EXISTS) $(INTERFDIR)/pdat || $(MKDIR) $(INTERFDIR)/pdat
	$(SED) -e 's/\[_IO_MEMVAR_WHAT_\]/_IO_MEMVAR_isREAL_/g' \
	     -e 's/\[_IO_MEMVAR_TYPE_\]/cmplx/g'                 \
	     -e 's/\[_IO_MEMVAR_KIND_\]/8/g'                    \
	     -e 's/\[_IO_MEMVAR_DIMS_\]/3/g'                    \
	     -e 's/\[_IO_MEMVAR_DDOT_\]/:,:,:/g'                \
	     -e 's/\[_IO_MEMVAR_DTXT_\]/mem_dims\(1\),mem_dims\(2\),mem_dims\(3\)/g' < interf_pdat.templ > $(INTERFDIR)/pdat/pdat_3d_cmplx8.F90

$(INTERFDIR)/pdat/pdat_4d_cmplx8.F90: Makefile ../Makefile.in interf_pdat.templ
	@$(CHK_DIR_EXISTS) $(INTERFDIR)/pdat || $(MKDIR) $(INTERFDIR)/pdat
	$(SED) -e 's/\[_IO_MEMVAR_WHAT_\]/_IO_MEMVAR_isREAL_/g' \
	     -e 's/\[_IO_MEMVAR_TYPE_\]/cmplx/g'                 \
	     -e 's/\[_IO_MEMVAR_KIND_\]/8/g'                    \
	     -e 's/\[_IO_MEMVAR_DIMS_\]/4/g'                    \
	     -e 's/\[_IO_MEMVAR_DDOT_\]/:,:,:,:/g'                \
	     -e 's/\[_IO_MEMVAR_DTXT_\]/mem_dims\(1\),mem_dims\(2\),mem_dims\(3\),mem_dims\(4\)/g' < interf_pdat.templ > $(INTERFDIR)/pdat/pdat_4d_cmplx8.F90

##### integer4  #####
$(INTERFDIR)/pdat/pdat_1d_integer4.F90: Makefile ../Makefile.in interf_pdat.templ
	@$(CHK_DIR_EXISTS) $(INTERFDIR)/pdat || $(MKDIR) $(INTERFDIR)/pdat
	$(SED) -e 's/\[_IO_MEMVAR_WHAT_\]/_IO_MEMVAR_isINTEGER_/g' \
	     -e 's/\[_IO_MEMVAR_TYPE_\]/integer/g'              \
	     -e 's/\[_IO_MEMVAR_KIND_\]/4/g'                    \
	     -e 's/\[_IO_MEMVAR_DIMS_\]/1/g'                    \
	     -e 's/\[_IO_MEMVAR_DDOT_\]/:/g'                    \
	     -e 's/\[_IO_MEMVAR_DTXT_\]/mem_dims\(1\)/g' < interf_pdat.templ > $(INTERFDIR)/pdat/pdat_1d_integer4.F90

$(INTERFDIR)/pdat/pdat_2d_integer4.F90: Makefile ../Makefile.in interf_pdat.templ
	@$(CHK_DIR_EXISTS) $(INTERFDIR)/pdat || $(MKDIR) $(INTERFDIR)/pdat
	$(SED) -e 's/\[_IO_MEMVAR_WHAT_\]/_IO_MEMVAR_isINTEGER_/g' \
	     -e 's/\[_IO_MEMVAR_TYPE_\]/integer/g'              \
	     -e 's/\[_IO_MEMVAR_KIND_\]/4/g'                    \
	     -e 's/\[_IO_MEMVAR_DIMS_\]/2/g'                    \
	     -e 's/\[_IO_MEMVAR_DDOT_\]/:,:/g'                  \
	     -e 's/\[_IO_MEMVAR_DTXT_\]/mem_dims\(1\),mem_dims\(2\)/g' < interf_pdat.templ > $(INTERFDIR)/pdat/pdat_2d_integer4.F90

$(INTERFDIR)/pdat/pdat_3d_integer4.F90: Makefile ../Makefile.in interf_pdat.templ
	@$(CHK_DIR_EXISTS) $(INTERFDIR)/pdat || $(MKDIR) $(INTERFDIR)/pdat
	$(SED) -e 's/\[_IO_MEMVAR_WHAT_\]/_IO_MEMVAR_isINTEGER_/g' \
	     -e 's/\[_IO_MEMVAR_TYPE_\]/integer/g'              \
	     -e 's/\[_IO_MEMVAR_KIND_\]/4/g'                    \
	     -e 's/\[_IO_MEMVAR_DIMS_\]/3/g'                    \
	     -e 's/\[_IO_MEMVAR_DDOT_\]/:,:,:/g'                \
	     -e 's/\[_IO_MEMVAR_DTXT_\]/mem_dims\(1\),mem_dims\(2\),mem_dims\(3\)/g' < interf_pdat.templ > $(INTERFDIR)/pdat/pdat_3d_integer4.F90

$(INTERFDIR)/pdat/pdat_4d_integer4.F90: Makefile ../Makefile.in interf_pdat.templ
	@$(CHK_DIR_EXISTS) $(INTERFDIR)/pdat || $(MKDIR) $(INTERFDIR)/pdat
	$(SED) -e 's/\[_IO_MEMVAR_WHAT_\]/_IO_MEMVAR_isINTEGER_/g' \
	     -e 's/\[_IO_MEMVAR_TYPE_\]/integer/g'              \
	     -e 's/\[_IO_MEMVAR_KIND_\]/4/g'                    \
	     -e 's/\[_IO_MEMVAR_DIMS_\]/4/g'                    \
	     -e 's/\[_IO_MEMVAR_DDOT_\]/:,:,:,:/g'                \
	     -e 's/\[_IO_MEMVAR_DTXT_\]/mem_dims\(1\),mem_dims\(2\),mem_dims\(3\),mem_dims\(4\)/g' < interf_pdat.templ > $(INTERFDIR)/pdat/pdat_4d_integer4.F90

###############################
#                             #
#   interf_pval.templ  #
#                             #
###############################
##### real4 #####
$(INTERFDIR)/pval/pval_1d_real4.F90: Makefile ../Makefile.in interf_pval.templ
	@$(CHK_DIR_EXISTS) $(INTERFDIR)/pval || $(MKDIR) $(INTERFDIR)/pval
	$(SED) -e 's/\[_IO_MEMVAR_WHAT_\]/_IO_MEMVAR_isREAL_/g' \
	     -e 's/\[_IO_MEMVAR_TYPE_\]/real/g'                 \
	     -e 's/\[_IO_MEMVAR_KIND_\]/4/g'                    \
	     -e 's/\[_IO_MEMVAR_DIMS_\]/1/g'                    \
	     -e 's/\[_IO_MEMVAR_DDOT_\]/:/g'                    \
	     -e 's/\[_IO_MEMVAR_DTXT_\]/ddims\(1\)/g' < interf_pval.templ > $(INTERFDIR)/pval/pval_1d_real4.F90

$(INTERFDIR)/pval/pval_2d_real4.F90: Makefile ../Makefile.in interf_pval.templ
	@$(CHK_DIR_EXISTS) $(INTERFDIR)/pval || $(MKDIR) $(INTERFDIR)/pval
	$(SED) -e 's/\[_IO_MEMVAR_WHAT_\]/_IO_MEMVAR_isREAL_/g' \
	     -e 's/\[_IO_MEMVAR_TYPE_\]/real/g'                 \
	     -e 's/\[_IO_MEMVAR_KIND_\]/4/g'                    \
	     -e 's/\[_IO_MEMVAR_DIMS_\]/2/g'                    \
	     -e 's/\[_IO_MEMVAR_DDOT_\]/:,:/g'                  \
	     -e 's/\[_IO_MEMVAR_DTXT_\]/ddims\(1\),ddims\(2\)/g' < interf_pval.templ > $(INTERFDIR)/pval/pval_2d_real4.F90

$(INTERFDIR)/pval/pval_3d_real4.F90: Makefile ../Makefile.in interf_pval.templ
	@$(CHK_DIR_EXISTS) $(INTERFDIR)/pval || $(MKDIR) $(INTERFDIR)/pval
	$(SED) -e 's/\[_IO_MEMVAR_WHAT_\]/_IO_MEMVAR_isREAL_/g' \
	     -e 's/\[_IO_MEMVAR_TYPE_\]/real/g'                 \
	     -e 's/\[_IO_MEMVAR_KIND_\]/4/g'                    \
	     -e 's/\[_IO_MEMVAR_DIMS_\]/3/g'                    \
	     -e 's/\[_IO_MEMVAR_DDOT_\]/:,:,:/g'                \
	     -e 's/\[_IO_MEMVAR_DTXT_\]/ddims\(1\),ddims\(2\),ddims\(3\)/g' < interf_pval.templ > $(INTERFDIR)/pval/pval_3d_real4.F90

$(INTERFDIR)/pval/pval_4d_real4.F90: Makefile ../Makefile.in interf_pval.templ
	@$(CHK_DIR_EXISTS) $(INTERFDIR)/pval || $(MKDIR) $(INTERFDIR)/pval
	$(SED) -e 's/\[_IO_MEMVAR_WHAT_\]/_IO_MEMVAR_isREAL_/g' \
	     -e 's/\[_IO_MEMVAR_TYPE_\]/real/g'                 \
	     -e 's/\[_IO_MEMVAR_KIND_\]/4/g'                    \
	     -e 's/\[_IO_MEMVAR_DIMS_\]/4/g'                    \
	     -e 's/\[_IO_MEMVAR_DDOT_\]/:,:,:,:/g'                \
	     -e 's/\[_IO_MEMVAR_DTXT_\]/ddims\(1\),ddims\(2\),ddims\(3\),ddims\(4\)/g' < interf_pval.templ > $(INTERFDIR)/pval/pval_4d_real4.F90
##### real8 #####
$(INTERFDIR)/pval/pval_1d_real8.F90: Makefile ../Makefile.in interf_pval.templ
	@$(CHK_DIR_EXISTS) $(INTERFDIR)/pval || $(MKDIR) $(INTERFDIR)/pval
	$(SED) -e 's/\[_IO_MEMVAR_WHAT_\]/_IO_MEMVAR_isREAL_/g' \
	     -e 's/\[_IO_MEMVAR_TYPE_\]/real/g'                 \
	     -e 's/\[_IO_MEMVAR_KIND_\]/8/g'                    \
	     -e 's/\[_IO_MEMVAR_DIMS_\]/1/g'                    \
	     -e 's/\[_IO_MEMVAR_DDOT_\]/:/g'                    \
	     -e 's/\[_IO_MEMVAR_DTXT_\]/ddims\(1\)/g' < interf_pval.templ > $(INTERFDIR)/pval/pval_1d_real8.F90

$(INTERFDIR)/pval/pval_2d_real8.F90: Makefile ../Makefile.in interf_pval.templ
	@$(CHK_DIR_EXISTS) $(INTERFDIR)/pval || $(MKDIR) $(INTERFDIR)/pval
	$(SED) -e 's/\[_IO_MEMVAR_WHAT_\]/_IO_MEMVAR_isREAL_/g' \
	     -e 's/\[_IO_MEMVAR_TYPE_\]/real/g'                 \
	     -e 's/\[_IO_MEMVAR_KIND_\]/8/g'                    \
	     -e 's/\[_IO_MEMVAR_DIMS_\]/2/g'                    \
	     -e 's/\[_IO_MEMVAR_DDOT_\]/:,:/g'                  \
	     -e 's/\[_IO_MEMVAR_DTXT_\]/ddims\(1\),ddims\(2\)/g' < interf_pval.templ > $(INTERFDIR)/pval/pval_2d_real8.F90

$(INTERFDIR)/pval/pval_3d_real8.F90: Makefile ../Makefile.in interf_pval.templ
	@$(CHK_DIR_EXISTS) $(INTERFDIR)/pval || $(MKDIR) $(INTERFDIR)/pval
	$(SED) -e 's/\[_IO_MEMVAR_WHAT_\]/_IO_MEMVAR_isREAL_/g' \
	     -e 's/\[_IO_MEMVAR_TYPE_\]/real/g'                 \
	     -e 's/\[_IO_MEMVAR_KIND_\]/8/g'                    \
	     -e 's/\[_IO_MEMVAR_DIMS_\]/3/g'                    \
	     -e 's/\[_IO_MEMVAR_DDOT_\]/:,:,:/g'                \
	     -e 's/\[_IO_MEMVAR_DTXT_\]/ddims\(1\),ddims\(2\),ddims\(3\)/g' < interf_pval.templ > $(INTERFDIR)/pval/pval_3d_real8.F90

$(INTERFDIR)/pval/pval_4d_real8.F90: Makefile ../Makefile.in interf_pval.templ
	@$(CHK_DIR_EXISTS) $(INTERFDIR)/pval || $(MKDIR) $(INTERFDIR)/pval
	$(SED) -e 's/\[_IO_MEMVAR_WHAT_\]/_IO_MEMVAR_isREAL_/g' \
	     -e 's/\[_IO_MEMVAR_TYPE_\]/real/g'                 \
	     -e 's/\[_IO_MEMVAR_KIND_\]/8/g'                    \
	     -e 's/\[_IO_MEMVAR_DIMS_\]/4/g'                    \
	     -e 's/\[_IO_MEMVAR_DDOT_\]/:,:,:,:/g'                \
	     -e 's/\[_IO_MEMVAR_DTXT_\]/ddims\(1\),ddims\(2\),ddims\(3\),ddims\(4\)/g' < interf_pval.templ > $(INTERFDIR)/pval/pval_4d_real8.F90
##### integer4  #####
$(INTERFDIR)/pval/pval_1d_integer4.F90: Makefile ../Makefile.in interf_pval.templ
	@$(CHK_DIR_EXISTS) $(INTERFDIR)/pval || $(MKDIR) $(INTERFDIR)/pval
	$(SED) -e 's/\[_IO_MEMVAR_WHAT_\]/_IO_MEMVAR_isINTEGER_/g' \
	     -e 's/\[_IO_MEMVAR_TYPE_\]/integer/g'              \
	     -e 's/\[_IO_MEMVAR_KIND_\]/4/g'                    \
	     -e 's/\[_IO_MEMVAR_DIMS_\]/1/g'                    \
	     -e 's/\[_IO_MEMVAR_DDOT_\]/:/g'                    \
	     -e 's/\[_IO_MEMVAR_DTXT_\]/ddims\(1\)/g' < interf_pval.templ > $(INTERFDIR)/pval/pval_1d_integer4.F90

$(INTERFDIR)/pval/pval_2d_integer4.F90: Makefile ../Makefile.in interf_pval.templ
	@$(CHK_DIR_EXISTS) $(INTERFDIR)/pval || $(MKDIR) $(INTERFDIR)/pval
	$(SED) -e 's/\[_IO_MEMVAR_WHAT_\]/_IO_MEMVAR_isINTEGER_/g' \
	     -e 's/\[_IO_MEMVAR_TYPE_\]/integer/g'              \
	     -e 's/\[_IO_MEMVAR_KIND_\]/4/g'                    \
	     -e 's/\[_IO_MEMVAR_DIMS_\]/2/g'                    \
	     -e 's/\[_IO_MEMVAR_DDOT_\]/:,:/g'                  \
	     -e 's/\[_IO_MEMVAR_DTXT_\]/ddims\(1\),ddims\(2\)/g' < interf_pval.templ > $(INTERFDIR)/pval/pval_2d_integer4.F90

$(INTERFDIR)/pval/pval_3d_integer4.F90: Makefile ../Makefile.in interf_pval.templ
	@$(CHK_DIR_EXISTS) $(INTERFDIR)/pval || $(MKDIR) $(INTERFDIR)/pval
	$(SED) -e 's/\[_IO_MEMVAR_WHAT_\]/_IO_MEMVAR_isINTEGER_/g' \
	     -e 's/\[_IO_MEMVAR_TYPE_\]/integer/g'              \
	     -e 's/\[_IO_MEMVAR_KIND_\]/4/g'                    \
	     -e 's/\[_IO_MEMVAR_DIMS_\]/3/g'                    \
	     -e 's/\[_IO_MEMVAR_DDOT_\]/:,:,:/g'                \
	     -e 's/\[_IO_MEMVAR_DTXT_\]/ddims\(1\),ddims\(2\),ddims\(3\)/g' < interf_pval.templ > $(INTERFDIR)/pval/pval_3d_integer4.F90

$(INTERFDIR)/pval/pval_4d_integer4.F90: Makefile ../Makefile.in interf_pval.templ
	@$(CHK_DIR_EXISTS) $(INTERFDIR)/pval || $(MKDIR) $(INTERFDIR)/pval
	$(SED) -e 's/\[_IO_MEMVAR_WHAT_\]/_IO_MEMVAR_isINTEGER_/g' \
	     -e 's/\[_IO_MEMVAR_TYPE_\]/integer/g'              \
	     -e 's/\[_IO_MEMVAR_KIND_\]/4/g'                    \
	     -e 's/\[_IO_MEMVAR_DIMS_\]/4/g'                    \
	     -e 's/\[_IO_MEMVAR_DDOT_\]/:,:,:,:/g'                \
	     -e 's/\[_IO_MEMVAR_DTXT_\]/ddims\(1\),ddims\(2\),ddims\(3\),ddims\(4\)/g' < interf_pval.templ > $(INTERFDIR)/pval/pval_4d_integer4.F90	     
##### complex4  #####
$(INTERFDIR)/pval/pval_1d_complex4.F03: Makefile ../Makefile.in interf_pval_complex.templ
	@$(CHK_DIR_EXISTS) $(INTERFDIR)/pval || $(MKDIR) $(INTERFDIR)/pval
	$(SED) -e 's/\[_IO_MEMVAR_WHAT_\]/_IO_MEMVAR_isCOMPLEX_/g' \
	     -e 's/\[_IO_MEMVAR_TYPE_\]/complex/g'              \
	     -e 's/\[_IO_MEMVAR_KIND_\]/4/g'                    \
	     -e 's/\[_IO_MEMVAR_DIMS_\]/1/g'                    \
	     -e 's/\[_IO_MEMVAR_DDOT_\]/:/g'                    \
	     -e 's/\[_IO_MEMVAR_DTXT_\]/cmem_dims\(1\)/g' < interf_pval_complex.templ > $(INTERFDIR)/pval/pval_1d_complex4.F03

$(INTERFDIR)/pval/pval_2d_complex4.F03: Makefile ../Makefile.in interf_pval_complex.templ
	@$(CHK_DIR_EXISTS) $(INTERFDIR)/pval || $(MKDIR) $(INTERFDIR)/pval
	$(SED) -e 's/\[_IO_MEMVAR_WHAT_\]/_IO_MEMVAR_isINTEGER_/g' \
	     -e 's/\[_IO_MEMVAR_TYPE_\]/complex/g'              \
	     -e 's/\[_IO_MEMVAR_KIND_\]/4/g'                    \
	     -e 's/\[_IO_MEMVAR_DIMS_\]/2/g'                    \
	     -e 's/\[_IO_MEMVAR_DDOT_\]/:,:/g'                  \
	     -e 's/\[_IO_MEMVAR_DTXT_\]/cmem_dims\(1\),cmem_dims\(2\)/g' < interf_pval_complex.templ > $(INTERFDIR)/pval/pval_2d_complex4.F03

$(INTERFDIR)/pval/pval_3d_complex4.F03: Makefile ../Makefile.in interf_pval_complex.templ
	@$(CHK_DIR_EXISTS) $(INTERFDIR)/pval || $(MKDIR) $(INTERFDIR)/pval
	$(SED) -e 's/\[_IO_MEMVAR_WHAT_\]/_IO_MEMVAR_isINTEGER_/g' \
	     -e 's/\[_IO_MEMVAR_TYPE_\]/complex/g'              \
	     -e 's/\[_IO_MEMVAR_KIND_\]/4/g'                    \
	     -e 's/\[_IO_MEMVAR_DIMS_\]/3/g'                    \
	     -e 's/\[_IO_MEMVAR_DDOT_\]/:,:,:/g'                \
	     -e 's/\[_IO_MEMVAR_DTXT_\]/cmem_dims\(1\),cmem_dims\(2\),cmem_dims\(3\)/g' < interf_pval_complex.templ > $(INTERFDIR)/pval/pval_3d_complex4.F03

$(INTERFDIR)/pval/pval_4d_complex4.F03: Makefile ../Makefile.in interf_pval_complex.templ
	@$(CHK_DIR_EXISTS) $(INTERFDIR)/pval || $(MKDIR) $(INTERFDIR)/pval
	$(SED) -e 's/\[_IO_MEMVAR_WHAT_\]/_IO_MEMVAR_isINTEGER_/g' \
	     -e 's/\[_IO_MEMVAR_TYPE_\]/complex/g'              \
	     -e 's/\[_IO_MEMVAR_KIND_\]/4/g'                    \
	     -e 's/\[_IO_MEMVAR_DIMS_\]/4/g'                    \
	     -e 's/\[_IO_MEMVAR_DDOT_\]/:,:,:,:/g'                \
	     -e 's/\[_IO_MEMVAR_DTXT_\]/cmem_dims\(1\),cmem_dims\(2\),cmem_dims\(3\),cmem_dims\(4\)/g' < interf_pval_complex.templ > $(INTERFDIR)/pval/pval_4d_complex4.F03
##### complex8  #####
$(INTERFDIR)/pval/pval_1d_complex8.F03: Makefile ../Makefile.in interf_pval_complex.templ
	@$(CHK_DIR_EXISTS) $(INTERFDIR)/pval || $(MKDIR) $(INTERFDIR)/pval
	$(SED) -e 's/\[_IO_MEMVAR_WHAT_\]/_IO_MEMVAR_isCOMPLEX_/g' \
	     -e 's/\[_IO_MEMVAR_TYPE_\]/complex/g'              \
	     -e 's/\[_IO_MEMVAR_KIND_\]/8/g'                    \
	     -e 's/\[_IO_MEMVAR_DIMS_\]/1/g'                    \
	     -e 's/\[_IO_MEMVAR_DDOT_\]/:/g'                    \
	     -e 's/\[_IO_MEMVAR_DTXT_\]/cmem_dims\(1\)/g' < interf_pval_complex.templ > $(INTERFDIR)/pval/pval_1d_complex8.F03

$(INTERFDIR)/pval/pval_2d_complex8.F03: Makefile ../Makefile.in interf_pval_complex.templ
	@$(CHK_DIR_EXISTS) $(INTERFDIR)/pval || $(MKDIR) $(INTERFDIR)/pval
	$(SED) -e 's/\[_IO_MEMVAR_WHAT_\]/_IO_MEMVAR_isINTEGER_/g' \
	     -e 's/\[_IO_MEMVAR_TYPE_\]/complex/g'              \
	     -e 's/\[_IO_MEMVAR_KIND_\]/8/g'                    \
	     -e 's/\[_IO_MEMVAR_DIMS_\]/2/g'                    \
	     -e 's/\[_IO_MEMVAR_DDOT_\]/:,:/g'                  \
	     -e 's/\[_IO_MEMVAR_DTXT_\]/cmem_dims\(1\),cmem_dims\(2\)/g' < interf_pval_complex.templ > $(INTERFDIR)/pval/pval_2d_complex8.F03

$(INTERFDIR)/pval/pval_3d_complex8.F03: Makefile ../Makefile.in interf_pval_complex.templ
	@$(CHK_DIR_EXISTS) $(INTERFDIR)/pval || $(MKDIR) $(INTERFDIR)/pval
	$(SED) -e 's/\[_IO_MEMVAR_WHAT_\]/_IO_MEMVAR_isINTEGER_/g' \
	     -e 's/\[_IO_MEMVAR_TYPE_\]/complex/g'              \
	     -e 's/\[_IO_MEMVAR_KIND_\]/8/g'                    \
	     -e 's/\[_IO_MEMVAR_DIMS_\]/3/g'                    \
	     -e 's/\[_IO_MEMVAR_DDOT_\]/:,:,:/g'                \
	     -e 's/\[_IO_MEMVAR_DTXT_\]/cmem_dims\(1\),cmem_dims\(2\),cmem_dims\(3\)/g' < interf_pval_complex.templ > $(INTERFDIR)/pval/pval_3d_complex8.F03	  

$(INTERFDIR)/pval/pval_4d_complex8.F03: Makefile ../Makefile.in interf_pval_complex.templ
	@$(CHK_DIR_EXISTS) $(INTERFDIR)/pval || $(MKDIR) $(INTERFDIR)/pval
	$(SED) -e 's/\[_IO_MEMVAR_WHAT_\]/_IO_MEMVAR_isINTEGER_/g' \
	     -e 's/\[_IO_MEMVAR_TYPE_\]/complex/g'              \
	     -e 's/\[_IO_MEMVAR_KIND_\]/8/g'                    \
	     -e 's/\[_IO_MEMVAR_DIMS_\]/4/g'                    \
	     -e 's/\[_IO_MEMVAR_DDOT_\]/:,:,:,:/g'                \
	     -e 's/\[_IO_MEMVAR_DTXT_\]/cmem_dims\(1\),cmem_dims\(2\),cmem_dims\(3\),cmem_dims\(4\)/g' < interf_pval_complex.templ > $(INTERFDIR)/pval/pval_4d_complex8.F03	   
