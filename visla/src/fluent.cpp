
#include "fluent.h"
#include "mycrt.h"

#include <strstream>

static Tfluentattr _fluentattr [] = { 
	{Tfluentattr("UNKNOWN ATTRIBUTE")},
	{Tfluentattr("nodenumber")}, 
	{Tfluentattr("x-coordinate")},
	{Tfluentattr("y-coordinate")},
	{Tfluentattr("z-coordinate")},
	{Tfluentattr("pressure")},
	{Tfluentattr("pressure-coefficient")},
	{Tfluentattr("dynamic-pressure")},
	{Tfluentattr("absolute-pressure")},
	{Tfluentattr("total-pressure")},
	{Tfluentattr("rel-total-pressure")},
	{Tfluentattr("density")},
	{Tfluentattr("density-all")},
	{Tfluentattr("velocity-magnitude")},
	{Tfluentattr("x-velocity")},
	{Tfluentattr("y-velocity")},
	{Tfluentattr("z-velocity")},
	{Tfluentattr("stream-function")},
	{Tfluentattr("radial-velocity")},
	{Tfluentattr("tangential-velocity")},
	{Tfluentattr("rel-velocity-magnitude")},
	{Tfluentattr("relative-x-velocity")},
	{Tfluentattr("relative-y-velocity")},
	{Tfluentattr("relative-z-velocity")},
	{Tfluentattr("rel-tangential-velocity")},
	{Tfluentattr("grid-x-velocity")},
	{Tfluentattr("grid-y-velocity")},
	{Tfluentattr("grid-z-velocity")},
	{Tfluentattr("velocity-angle")},
	{Tfluentattr("relative-velocity-angle")},
	{Tfluentattr("vorticity-mag")},
	{Tfluentattr("cell-reynolds-number")},
	{Tfluentattr("temperature")},
	{Tfluentattr("total-temperature")},
	{Tfluentattr("enthalpy")},
	{Tfluentattr("rel-total-temperature")},
	{Tfluentattr("rothalpy")},
	{Tfluentattr("wall-temp-out-surf")},
	{Tfluentattr("wall-temp-in-surf")},
	{Tfluentattr("total-enthalpy")},
	{Tfluentattr("total-enthalpy-deviation")},
	{Tfluentattr("entropy")},
	{Tfluentattr("total-energy")},
	{Tfluentattr("internal-energy")},
	{Tfluentattr("absorption-coefficient")},
	{Tfluentattr("scattering-coefficient")},
	{Tfluentattr("radiation-temperature")},
	{Tfluentattr("incident-radiation")},
	{Tfluentattr("turb-kinetic-energy")},
	{Tfluentattr("turb-intensity")},
	{Tfluentattr("turb-diss-rate")},
	{Tfluentattr("production-of-k")},
	{Tfluentattr("viscosity-turb")},
	{Tfluentattr("viscosity-eff")},
	{Tfluentattr("viscosity-ratio")},
	{Tfluentattr("thermal-conductivity-eff")},
	{Tfluentattr("prandtl-number-eff")},
	{Tfluentattr("y-star")},
	{Tfluentattr("y-plus")},
	{Tfluentattr("c<s>")},
	{Tfluentattr("h2")},
	{Tfluentattr("o2")},
	{Tfluentattr("co2")},
	{Tfluentattr("h2o")},
	{Tfluentattr("n2")},
	{Tfluentattr("c")},
	{Tfluentattr("h")},
	{Tfluentattr("o")},
	{Tfluentattr("molef-c<s>")},
	{Tfluentattr("molef-h2")},
	{Tfluentattr("molef-o2")},
	{Tfluentattr("molef-co2")},
	{Tfluentattr("molef-h2o")},
	{Tfluentattr("molef-n2")},
	{Tfluentattr("molef-c")},
	{Tfluentattr("molef-h")},
	{Tfluentattr("molef-o")},
	{Tfluentattr("concentration-c<s>")},
	{Tfluentattr("concentration-h2")},
	{Tfluentattr("concentration-o2")},
	{Tfluentattr("concentration-co2")},
	{Tfluentattr("concentration-h2o")},
	{Tfluentattr("concentration-n2")},
	{Tfluentattr("concentration-c")},
	{Tfluentattr("concentration-h")},
	{Tfluentattr("concentration-o")},
	{Tfluentattr("relative-humidity")},
	{Tfluentattr("fmean")},
	{Tfluentattr("fmean2")},
	{Tfluentattr("fvar")},
	{Tfluentattr("fvar2")},
	{Tfluentattr("fvar-prod")},
	{Tfluentattr("fvar2-prod")},
	{Tfluentattr("dpm-mass-source")},
	{Tfluentattr("dpm-x-mom-source")},
	{Tfluentattr("dpm-y-mom-source")},
	{Tfluentattr("dpm-z-mom-source")},
	{Tfluentattr("dpm-sensible-enthalpy-source")},
	{Tfluentattr("dpm-total-enthalpy-source")},
	{Tfluentattr("dpm-absorption-coefficient")},
	{Tfluentattr("dpm-emission")},
	{Tfluentattr("dpm-scattering")},
	{Tfluentattr("dpm-burnout")},
	{Tfluentattr("dpm-devolatization")},
	{Tfluentattr("dpm-concentration")},
	{Tfluentattr("viscosity-lam")},
	{Tfluentattr("thermal-conductivity-lam")},
	{Tfluentattr("specific-heat-cp")},
	{Tfluentattr("prandtl-number-lam")},
	{Tfluentattr("mean-molecular-weight")},
	{Tfluentattr("wall-shear")},
	{Tfluentattr("x-wall-shear")},
	{Tfluentattr("y-wall-shear")},
	{Tfluentattr("z-wall-shear")},
	{Tfluentattr("skin-friction-coef")},
	{Tfluentattr("heat-flux")},
	{Tfluentattr("rad-heat-flux")},
	{Tfluentattr("heat-transfer-coef")},
	{Tfluentattr("nusselt-number")},
	{Tfluentattr("stanton-number")},
	{Tfluentattr("cell-partition")},
	{Tfluentattr("cell-element-type")},
	{Tfluentattr("cell-type")},
	{Tfluentattr("cell-zone")},
	{Tfluentattr("partition-neighbors")},
	{Tfluentattr("axial-coordinate")},
	{Tfluentattr("radial-coordinate")},
	{Tfluentattr("x-surface-area")},
	{Tfluentattr("y-surface-area")},
	{Tfluentattr("z-surface-area")},
	{Tfluentattr("x-face-area")},
	{Tfluentattr("y-face-area")},
	{Tfluentattr("z-face-area")},
	{Tfluentattr("cell-equiangle-skew")},
	{Tfluentattr("cell-equivolume-skew")},
	{Tfluentattr("cell-volume")},
	{Tfluentattr("cell-wall-distance")},
	{Tfluentattr("face-handedness")},
	{Tfluentattr("face-squish-index")},
	{Tfluentattr("cell-squish-index")},
	{Tfluentattr("adaption-function")},
	{Tfluentattr("existing-value")},
	{Tfluentattr("boundary-cell-dist")},
	{Tfluentattr("boundary-normal-dist")},
	{Tfluentattr("boundary-volume-dist")},
	{Tfluentattr("cell-volume-change")},
	{Tfluentattr("cell-surface-area")},
	{Tfluentattr("cell-warp")},
	{Tfluentattr("cell-children")},
	{Tfluentattr("cell-refine-level")},
	{Tfluentattr("mass-imbalance")},
	{Tfluentattr("strain-rate-mag")},
	{Tfluentattr("dx-velocity-dx")},
	{Tfluentattr("dy-velocity-dx")},
	{Tfluentattr("dz-velocity-dx")},
	{Tfluentattr("dx-velocity-dy")},
	{Tfluentattr("dy-velocity-dy")},
	{Tfluentattr("dz-velocity-dy")},
	{Tfluentattr("dx-velocity-dz")},
	{Tfluentattr("dy-velocity-dz")},
	{Tfluentattr("dz-velocity-dz")},
	{Tfluentattr("dp-dx")},
	{Tfluentattr("dp-dy")},
	{Tfluentattr("dp-dz")},
};

const int _fluentattr_len = sizeof (_fluentattr) / sizeof (Tfluentattr);

int Tfluentparser::getvalueoffset (int index)
{
	// vrati offset pro 0 - maxvalueovou polozku
	index = absmod (index, maxvalue);
	return values[index].offset;
}

string Tfluentparser::getvaluename (int index)
{
	return _fluentattr [getvalueoffset (index)].name;
}

Tfluentvalue *Tfluentparser::getvalueptr (int index)
{
	int offset = getvalueoffset (index);
	return values + offset;
}

int Tfluentdsparser::getpointer (int x, int y, int z)
{
	if (x >= dssx) x = dssx - 1; if (y >= dssy) y = dssy - 1; if (z >= dssz) z = dssz - 1;
	return x + dssx * (y + z * dssy);
}

void Tfluentdsparser::displayinfo (bool brief)
{
	int i,j,k,l;
	int offset;
	float val1, val2;

	Tfluentparser::displayinfo ();

	for (l = 0; l < maxvalue; l++)
	{
		offset = getvalueoffset (l);
		cout << "Found attribute: " << getvaluename (l) << " (" << offset << ") " << "\n";
		if (brief)
			continue;

		for (i = 0; i < dssx; i++)
		{
			for (j = 0; j < dssy; j++)
			{
				for (k = 0; k < dssz; k++)
				{
					val1 = getdsvalue (i, j, k, offset);
					val2 = getdsvaluebyindex (i, j, k, l);
					if (val1 == val2)
                        cout << val1 << " ";
				}
			}
			cout << "\n";
		}
		cout << "\n\n";
	}
}

float Tfluentdsparser::getdsvelocity (int x, int y, int z)
{
	float val, totval = 0;

	for (int i = 0; i < dim; i++)
	{
		val = getdsvalue (x,y,z, Tfluentattrenum::x_velocity + i);
		totval += val * val;
	}

	return (float) sqrt (totval);
}

float Tfluentdsparser::getdsvaluebyindex (int x, int y, int z, int value)
{
	if (x < 0) x = 0; if (x >= dssx) x = dssx - 1;
	if (y < 0) y = 0; if (y >= dssy) y = dssy - 1;
	if (z < 0) z = 0; if (z >= dssz) z = dssz - 1;

	int pointer = getpointer (x, y, z);

	value = absmod (value, maxvalue);
	return dsvalues [pointer][value];
}

float Tfluentdsparser::getdsvalue (int x, int y, int z, int value)
{
	if (x < 0) x = 0; if (x >= dssx) x = dssx - 1;
	if (y < 0) y = 0; if (y >= dssy) y = dssy - 1;
	if (z < 0) z = 0; if (z >= dssz) z = dssz - 1;

	int pointer = getpointer (x, y, z);

	if ((value = attr[value]->valueoffset) >= 0)
		return dsvalues [pointer][value];
	else return -1;
}

float Tfluentparser::getavgvalue (int value)
{
//	if (value < 0) value = 0; if (value > maxvalue)	value = maxvalue - 1;
	
	if (value)
		return values [value].avg;
	else
	{
		float v1 = values [x_velocity].avg;
		float v2 = values [y_velocity].avg;
		return (float) sqrt (v1 * v1 + v2 * v2);
	}
}

void Tfluentdsparser::downsample (int ssx, int ssy, int ssz)
{
	int i, j, pointer;
	float x,y,z;
	float dx, dy, dz;
	Tfluentnode *tn;

	dssx = ssx; dssy = ssy; dssz = ssz; dss = ssx * ssy * ssz;
	
	dsvalues = new float * [dss];
	dscounts = new int [dss];

	for (i = 0; i < dss; i++)
	{
		dsvalues [i] = new float [maxvalue];
		for (j = 0; j < maxvalue; j++)
		{
			dsvalues [i] [j] = 0;
		}
		dscounts [i] = 0;
	}

	dx = dxmax - dxmin; dy = dymax - dymin; dz = dzmax - dzmin;

	for (i = 0; i < maxnode; i++)
	{
		tn = nodes + i;

		if (tn->px >= dxmin && tn->py >= dymin && tn->pz >= dzmin)
		if (tn->px <= dxmax && tn->py <= dymax && tn->pz <= dzmax)
		{
			x = dx ? dssx * (tn->px - dxmin) / dx : 0;
			y = dy ? dssy * (tn->py - dymin) / dy : 0;
			z = dz ? dssz * (tn->pz - dzmin) / dz : 0;

			if (x >= 0 && y >= 0 && z >= 0)
			{
				pointer = getpointer ((int) x, (int) y, (int) z);
	
				for (j = 0; j < maxvalue; j++)
				{
					dsvalues [pointer][j] += tn->values [j];
				}
				dscounts [pointer]++;
			}
		}
	}

	for (i = 0; i < dss; i++)
	{
		for (j = 0; j < maxvalue; j++)
		{
			if (dscounts[i])
			{
				dsvalues [i][j] /= dscounts [i];
			}
		}
	}
}

Tfluentdsparser::Tfluentdsparser (string &filename, int dsx, int dsy, int dsz, float minsx, float minsy, float minsz, float maxsx, float maxsy, float maxsz) : Tfluentparser (filename)
{
	if (minsx >= 0 && minsy >=0 && minsz >= 0)
	{
		dxmin = minsx; dymin = minsy; dzmin = minsz;
	}
	if (maxsx >= 0 && maxsy >=0 && maxsz >= 0)
	{
		dxmax = maxsx; dymax = maxsy; dzmax = maxsz;
	}

	downsample (dsx, dsy, dsz);
}

Tfluentdsparser::~Tfluentdsparser (void)
{
	int i;

	if (dsvalues)
	{
		for (i = 0; i < dss; i++)
			delete dsvalues [i];

		delete dsvalues, dsvalues = NULL;
	}
	if (dscounts)
		delete dscounts, dscounts = NULL;

	deletenodes ();
}

void Tfluentparser::deletenodes (void)
{
	if (nodes)
	{
		for (int i = 0; i < maxnode; i++)
		{
			delete nodes [i].values;
		}
		delete nodes;
	}
	nodes = NULL;
}

Tfluentparser::~Tfluentparser (void)
{
	int i;

	if (attr)
	{
		for (i = 0; i < _fluentattr_len; i++)
		{
			delete attr[i];
		}
		delete attr;
	}
}

void Tfluentparser::displayvalue (int index)
{
	Tfluentvalue *value;

	if ((index >= 0) && (index < maxvalue))
	{
		value = getvalueptr (index);
		cout << getvaluename (index) << ": Min=" << value->min << ",Max=" << value->max << ",Avg=" << value->avg << ".\n";
	}
	else
		cout << "Value of index " << index << "out of bounds [0.." << maxvalue << "].\n";
}

void Tfluentparser::displayvalues (void)
{
	int i;

	for (i = 0; i < maxvalue; i++)
		displayvalue (i);
}

void Tfluentparser::displayinfo (bool brief)
{
	cout << "Detected " << dim << "D boiler (" << dxmax - dxmin << " x " << dymax - dymin << " x " << dzmax - dzmin << " m) with " << maxvalue << " values and " << maxnode << " nodes.\n\n";
}

int Tfluentparser::findoffset (string value)
{
	int i;

	for (i = 1; i < _fluentattr_len; i++)
	{
		if (value == _fluentattr[i].name)
		{
			return i;
		}
	}
	printf ("Warning: unknown attribute %s !\n", value.c_str ());

	return 0;
}

void Tfluentparser::addattr (string newattr)
{
	int i;

	Tfluentvalue *value = values + maxvalue;
	i = findoffset (newattr);
	value->offset = i;
	attr[i]->valueoffset = maxvalue;
	maxvalue++;
}

Tfluentparser::Tfluentparser (string &filename)
{
	string s;
	int count;
	int len;
	int i;
	float v, vx, vy;
	bool firsttime = true;
	Tfluentnode *mynode;
	Tfluentvalue *value;

	fluentfilename = filename;

	maxvalue = 0; dim = 0;
	ifstream fin (filename.c_str());
	if (!fin.is_open ())
	{
		cout << "File " << filename << "not found !";
		exit (1);
	}

	dxmin = dymin = dzmin = 1e10; dxmax = dymax = dzmax = -1e10;
	manualvelocitylevel = 2;
	
	nodes = NULL;
	attr = new Tfluentattr * [_fluentattr_len];

	for (i = 0; i < _fluentattr_len; i++)
	{
		attr [i] = new Tfluentattr;
		attr [i]->name = _fluentattr [i].name;
		attr [i]->valueoffset = -1;
	}

	while (fin.good ())
	{
		fin >> s;
		if (firsttime && s != "nodenumber")
		{
			cout << "File" << filename << " was not created using FLUENT ASCII export.\n";
			exit (2);
		}
		else
		{
			if (firsttime)
			{
				firsttime = false;
				continue;
			}
		}

		if (s == "1")
		{
			if (manualvelocitylevel == 2)
			{
				addattr ("velocity-magnitude");
				addattr ("velocity-angle");
			}
			if (manualvelocitylevel == 1)
			{
				addattr ("velocity-magnitude");
			}
			break;
		}

		if (s == "velocity-magnitude" || s=="velocity-angle")
		{
			manualvelocitylevel--;
		}

		if (s == "z-coordinate" || s == "y-coordinate" || s == "x-coordinate")
			dim++;
		else
		{
			addattr (s);
		}
	}

	if ((len = filesize (filename)) > 0)
	{
		// odhadnout pocet polozek
		approxmaxnode = len / ((maxvalue + dim) * 7);
		nodes = new Tfluentnode [approxmaxnode];
		maxnode = 0;

		for (i = 0; i < maxvalue; i++)
		{
			value = getvalueptr (i);
			value->min = 1e10;
			value->avg = 0;
			value->max = -1e10;
		}

		while (fin.good ())
		{
			mynode = &nodes [maxnode];

	//		cout << "Reading node " << s << "\n";

			mynode->px = mynode->py = mynode->pz = 0;
			if (dim >= 1) fin >> mynode->px; 
			if (dxmax < mynode->px) dxmax = mynode->px; if (dxmin > mynode->px) dxmin = mynode->px;
			if (dim >= 2) fin >> mynode->py; 
			if (dymax < mynode->py) dymax = mynode->py; if (dymin > mynode->py) dymin = mynode->py;
			if (dim >= 3) fin >> mynode->pz; 
			if (dzmax < mynode->pz) dzmax = mynode->pz; if (dzmin > mynode->pz) dzmin = mynode->pz;

			mynode->values = new float [maxvalue];

			count = 0;
			while (count < maxvalue)
			{
				if (count >= maxvalue - manualvelocitylevel)
				{
					if (maxvalue - count == manualvelocitylevel && maxvalue - manualvelocitylevel >= 2)
					{
						vx = mynode->values [0];
						vy = mynode->values [1];
						v = sqrt (vx * vx + vy * vy);
					} else v = 0;
				}
				else
					fin >> v;

				mynode->values [count] = v;
				value = getvalueptr (count);
				value->avg += v;
				if (value->min > v)
					value->min = v;
				if (value->max < v)
					value->max = v;

	//			cout << v << " ";
				count++;
			}
	//		cout << "\n";

			maxnode++;

			if (approxmaxnode < maxnode)
			{
				cout << "Approximative memory allocation for nodes failed !\n";
				exit (3);
			}

			if (fin.good ())
			{
				i = maxnode + 1;
				fin >> i;
				if (i != maxnode + 1)
				{
					cout << "Warning ! Invalid node specified - " << i;
				}
			}
		}

		for (i = 0; i < maxvalue; i++)
		{
			value = getvalueptr (i);
			value->avg /= maxnode;
		}
	}
}

