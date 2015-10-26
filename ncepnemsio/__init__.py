import numpy as np
class ncepnemsio(object):
    # read ncep gfs 'nemsio' file
    def __init__(self,filename):
        from _read_sigma_nemsio import read_nemsio_header, read_nemsio_griddata, read_nemsio_coords
        nlons,nlats,nlevs,idate,nfhour = read_nemsio_header(filename)
        vcoord,lats,lons = read_nemsio_coords(filename,nlons,nlats,nlevs)
        self.vcoord = vcoord[:,:2,0].T
        self._read_griddata = read_nemsio_griddata
        self.nlons = nlons; self.nlats = nlats
        self.nlevs = nlevs
        self.idate = '%04i%02i%02i%02i' % (idate[0],idate[1],idate[2],idate[3])
        self.fhour = nfhour
        self.filename = filename
        self.lats = lats
        self.lons = lons
    def griddata(self):
        ug,vg,tempg,zsg,psg,qg,ozg,cwmrg,dpresg,presg = self._read_griddata(self.filename,self.nlons,self.nlats,self.nlevs)
        return ug.T,vg.T,tempg.T,zsg.T,psg.T,qg.T,ozg.T,cwmrg.T,dpresg.T,presg.T
