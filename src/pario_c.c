#include "hdf5.h"

extern herr_t
pario_disable_evictions_c(hid_t *facc_prp)
{
	herr_t ierr = -1;

	// fill config with default
	H5AC_cache_config_t mdc_config;
	mdc_config.version = H5AC__CURR_CACHE_CONFIG_VERSION;
	ierr = H5Pget_mdc_config(*facc_prp, &mdc_config);

	// set config, that changed metadata dumped to disk only at file close
	mdc_config.evictions_enabled = 0; // FALSE == 0
	mdc_config.incr_mode       = H5C_incr__off;
	mdc_config.flash_incr_mode = H5C_flash_incr__off;
	mdc_config.decr_mode       = H5C_decr__off;

	ierr = H5Pset_mdc_config(*facc_prp, &mdc_config);

	return ierr;
}
