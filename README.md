# gfstonc
utilities and modules to read NCEP gfs spectral, surface and nemsio files and convert to netcdf

([numpy](http://numpy.org), [netcdf4-python](https://github.com/Unidata/netcdf4-python) and
[gfortran](https://gcc.gnu.org/wiki/GFortran) are required, 
plus [shtns](https://bitbucket.org/nschaeff/shtns) library for spherical harmonic transforms.

* `python setup.py build`
   - setup.py will try to build `src/libw3nco_d.a`  and `src/libbacio_4.a` if they do not
already exist.
* `python setup.py install` (or `python setup.py install --user` to install in your 
home directory).

*Will not work on Windows!*

`utils/gfs_spectonc` converts gfs binary spectral files to netcdf.

`utils/gfs_sfctonc` converts gfs binary surface files to netcdf.

`utils/gfs_nemsiotonc` converts gfs nemsio surface files to netcdf.

Watch where setup.py installs the utility scripts, you will need to add that location to your `$PATH`.
